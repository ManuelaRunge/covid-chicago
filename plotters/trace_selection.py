"""
Assigns negative log-likelihoods to each trace in a set of trajectories.
"""
import argparse
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
import sys

sys.path.append('../')
from load_paths import load_box_paths
import matplotlib as mpl
import matplotlib.dates as mdates
from datetime import date, timedelta, datetime
import seaborn as sns
from processing_helpers import *
from sample_parameters import make_identifier, gen_combos

def parse_args():

    description = "Simulation run for modeling Covid-19"
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        "-s",
        "--stem",
        type=str,
        help="Name of simulation experiment"
    )
    parser.add_argument(
        "-loc",
        "--Location",
        type=str,
        help="Local or NUCLUSTER",
        default = "Local"
    )
    parser.add_argument(
        "--deaths_weight",
        type=float,
        help="Weight of deaths in negative log likelihood calculation. Default is 1.0.",
        default=0.0
    )
    parser.add_argument(
        "--crit_weight",
        type=float,
        help="Weight of ICU population in negative log likelihood calculation. Default is 1.0.",
        default=1.0
    )
    parser.add_argument(
        "--non_icu_weight",
        type=float,
        help="Weight of non-ICU population in negative log likelihood calculation. Default is 1.0.",
        default=1.0
    )
    parser.add_argument(
        "--cli_weight",
        type=float,
        help="Weight of CLI admissions in negative log likelihood calculation. Default is 1.0.",
        default=0.0
    )
    parser.add_argument(
        "--plot",
        action='store_true',
        help="If specified, plots with top 50% best-fitting trajectories will be generated.",
    )
    return parser.parse_args()

def sum_nll(df_values, ref_df_values):
    try:
        x = -np.log10(scipy.stats.poisson(mu=df_values).pmf(k=ref_df_values))
    except ValueError:
        print('ERROR: The simulation and reference arrays may not be the same length.')
        print('Length simulation: ' + str(len(df_values)))
        print('Length reference: ' + str(len(ref_df_values)))
    x[np.abs(x) == np.inf] = 0
    return np.nansum(x)

def compare_sim_and_ref(df, ems_nr, ref_df, channels, data_channel_names, titles, region_label,
                     first_day, last_day, ymax=10000, logscale=True, weights_array=[1.0,1.0,1.0,1.0], plot_trajectories=False):
    #Creation of rank_df
    [deaths_weight, crit_weight, non_icu_weight, cli_weight] = weights_array
    ref_df_trunc = ref_df[(ref_df['date'] > first_day) & (ref_df['date'] < last_day)]
    df_trunc = df[(df['date'] > first_day) & (df['date'] < last_day)]

    """ Ensure common dates"""
    df_trunc_dates = df_trunc[df_trunc['date'].isin(ref_df_trunc['date'].unique())].date.unique()
    ref_df_trunc_dates = ref_df_trunc[ref_df_trunc['date'].isin(df_trunc['date'].unique())].date.unique()
    common_dates = df_trunc_dates[np.isin(df_trunc_dates, ref_df_trunc_dates)]
    df_trunc = df_trunc[df_trunc['date'].isin(common_dates)]
    ref_df_trunc = ref_df_trunc[ref_df_trunc['date'].isin(common_dates)]

    run_sample_scen_list = list(df_trunc.groupby(['run_num','sample_num','scen_num']).size().index)
    rank_export_df = pd.DataFrame({'run_num':[], 'sample_num':[], 'scen_num':[], 'nll':[]})
    for x in run_sample_scen_list:
        total_nll = 0
        (run_num, sample_num, scen_num) = x
        df_trunc_slice = df_trunc[(df_trunc['run_num'] == run_num) & (df_trunc['sample_num'] == sample_num) & (df_trunc['scen_num'] == scen_num)]
        total_nll += deaths_weight*sum_nll(df_trunc_slice['new_detected_deaths'].values, ref_df_trunc['deaths'].values)
        total_nll += crit_weight*sum_nll(df_trunc_slice['crit_det'].values, ref_df_trunc['confirmed_covid_icu'].values)
        total_nll += cli_weight*sum_nll(df_trunc_slice['new_detected_hospitalized'].values, ref_df_trunc['inpatient'].values)
        total_nll += non_icu_weight*sum_nll(df_trunc_slice['hosp_det'].values, ref_df_trunc['covid_non_icu'].values)
        rank_export_df = rank_export_df.append(pd.DataFrame({'run_num':[run_num], 'sample_num':[sample_num], 'scen_num':[scen_num], 'nll':[total_nll]}))
    rank_export_df['norm_rank'] = (rank_export_df['nll'].rank()-1)/(len(rank_export_df)-1)
    rank_export_df = rank_export_df.sort_values(by=['norm_rank']).reset_index(drop=True)
    rank_export_df.to_csv(os.path.join(output_path,'traces_ranked_region_' + str(ems_nr) + '.csv'), index=False)

    #Creation of plots
    if plot_trajectories:
        plot_path = os.path.join(wdir, 'simulation_output',exp_name, '_plots')
        df = pd.merge(rank_export_df[0:int(len(rank_export_df)/2)],df)
        if ems_nr == 0:
            region_suffix = "_All"
            region_label = 'Illinois'
        else:
            region_suffix = "_EMS-" + str(ems_nr)
            region_label = region_suffix.replace('_EMS-', 'COVID-19 Region ')

        fig = plt.figure(figsize=(13, 6))
        palette = sns.color_palette('husl', 8)
        k = 0
        for c, channel in enumerate(channels):
            ax = fig.add_subplot(2, 3, c + 1)

            mdf = df.groupby('date')[channel].agg([CI_50,CI_2pt5, CI_97pt5, CI_25, CI_75]).reset_index()
            ax.plot(mdf['date'], mdf['CI_50'], color=palette[k])
            ax.fill_between(mdf['date'], mdf['CI_2pt5'], mdf['CI_97pt5'],
                            color=palette[k], linewidth=0, alpha=0.2)
            ax.fill_between(mdf['date'], mdf['CI_25'], mdf['CI_75'],
                            color=palette[k], linewidth=0, alpha=0.4)

            ax.set_title(titles[c], y=0.8, fontsize=12)

            ax.xaxis.set_major_formatter(mdates.DateFormatter('%d\n%b'))
            ax.set_xlim(date(2020, 2, 13), date.today() + timedelta(15))
            ax.grid(b=True, which='major', color='#999999', linestyle='-', alpha=0.3)
            if logscale :
                ax.set_ylim(0.1, ymax)
                ax.set_yscale('log')

            ax.plot(ref_df['date'], ref_df[data_channel_names[c]], 'o', color='#303030', linewidth=0, ms=1)
            ax.plot(ref_df['date'], ref_df[data_channel_names[c]].rolling(window = 7, center=True).mean(), c='k', alpha=1.0)

        fig.suptitle(region_label, y=1, fontsize=14)
        fig.tight_layout()
        fig.subplots_adjust(top=0.88)

        plot_name = 'compare_to_data_covidregion_' + str(ems_nr)
        if logscale == False :
            plot_name = plot_name + "_nolog"
        plot_name = plot_name + "_best_fit"
        plt.savefig(os.path.join(plot_path, plot_name + '.png'))
        plt.savefig(os.path.join(plot_path,'pdf', plot_name + '.pdf'), format='PDF')


def compare_ems(exp_name, ems_nr,first_day,last_day,weights_array,plot_trajectories=False):

    if ems_nr == 0:
        region_suffix = "_All"
        region_label = 'Illinois'
    else:
        region_suffix = "_EMS-" + str(ems_nr)
        region_label = region_suffix.replace('_EMS-', 'COVID-19 Region ')

    column_list = ['time', 'startdate', 'scen_num', 'sample_num','run_num']
    outcome_channels = ['hosp_det_cumul', 'hosp_cumul', 'hosp_det', 'hospitalized',
                        'crit_det_cumul', 'crit_cumul', 'crit_det', 'critical',
                        'death_det_cumul', 'deaths']

    for channel in outcome_channels:
        column_list.append(channel + region_suffix)

    df = load_sim_data(exp_name, region_suffix=region_suffix, column_list=column_list)
    df = df[(df['date'] >= date(2020, 2, 13)) & (df['date'] <= date.today() + timedelta(15))]
    df['critical_with_suspected'] = df['critical']

    ref_df = load_ref_df(ems_nr)

    channels = ['new_detected_deaths', 'crit_det', 'hosp_det', 'new_deaths','new_detected_hospitalized',
                'new_detected_hospitalized']
    data_channel_names = ['deaths',
                          'confirmed_covid_icu', 'covid_non_icu', 'deaths','inpatient', 'admissions']
    titles = ['New Detected\nDeaths (LL)', 'Critical Detected (EMR)', 'Inpatient non-ICU\nCensus (EMR)', 'New Detected\nDeaths (LL)',
              'Covid-like illness\nadmissions (IDPH)', 'New Detected\nHospitalizations (LL)']

    compare_sim_and_ref(df, ems_nr, ref_df, channels=channels, data_channel_names=data_channel_names, titles=titles,
                     region_label=region_label,first_day= first_day, last_day= last_day, logscale=False, weights_array=weights_array, plot_trajectories=plot_trajectories)


def extract_sample_traces(traces_to_keep_ratio=4, min_traces_to_keep=100, n_traces_to_keep=None):

    sample_df = pd.read_csv(os.path.join(output_path, 'sampled_parameters.csv'))
    """Drop parameter columns that have equal values in all scenarios (rows) to assess fitted parameters"""
    nunique = sample_df.apply(pd.Series.nunique)
    cols_to_drop = nunique[nunique == 1].index
    sample_df = sample_df.drop(cols_to_drop, axis=1)

    sampleDat = pd.DataFrame()
    for ems_region in range(1,12):

        rank_export_df = pd.read_csv(os.path.join(output_path, f'traces_ranked_region_{str(ems_region)}.csv'))
        n_traces_to_keep = int(len(rank_export_df) / traces_to_keep_ratio)
        if n_traces_to_keep < min_traces_to_keep and len(rank_export_df) >= min_traces_to_keep:
            n_traces_to_keep = min_traces_to_keep
        if len(rank_export_df) < min_traces_to_keep:
            n_traces_to_keep = len(rank_export_df)
        rank_export_df_sub = rank_export_df[0:n_traces_to_keep]

        sample_df_sub = pd.merge(how='left', left=rank_export_df_sub[['scen_num','norm_rank']], left_on=['scen_num'],
                                 right=sample_df, right_on=['scen_num'])
        sample_df_sub = sample_df_sub.sort_values(by=['norm_rank']).reset_index(drop=True)
        sample_df_sub.columns = sample_df_sub.columns + f'_EMS_{str(ems_region)}'
        sample_df_sub['row_num'] = sample_df_sub.index

        if sampleDat.empty:
            sampleDat = sample_df_sub
        else:
            sampleDat = pd.merge(how='left', left=sampleDat, left_on=['row_num'], right=sample_df_sub, right_on=['row_num'])
        del sample_df_sub

    """Export fitting parameters"""
    sampleDat.to_csv(os.path.join(output_path, f'fitted_parameters_ntraces{n_traces_to_keep}.csv'), index=False)
    sampleDat.head(n=1).to_csv(os.path.join(output_path, f'fitted_parameters_besttrace.csv'), index=False)

    """Export all parameter columns to be used as simulation input"""
    sample_df = pd.read_csv(os.path.join(output_path, 'sampled_parameters.csv'))
    sample_df = sample_df.loc[:n_traces_to_keep+1 , cols_to_drop]
    sample_df['scen_num'] = sample_df.reset_index().index
    sampleDat_ntraces = gen_combos(sample_df, sampleDat)
    sampleDat_besttrace = gen_combos(sample_df, sampleDat.head(n=1))
    sampleDat_ntraces.to_csv(os.path.join(output_path, f'sample_parameters_ntraces{n_traces_to_keep}.csv'), index=False)
    sampleDat_besttrace.to_csv(os.path.join(output_path, f'sample_parameters_besttrace.csv'), index=False)


if __name__ == '__main__':

    args = parse_args()
    stem = "20210120_IL_quest_currentModel2_varyParamForFit"
    Location = args.Location
    weights_array = [args.deaths_weight, args.crit_weight, args.non_icu_weight, args.cli_weight]

    first_plot_day = date(2020, 3, 25)
    last_plot_day = date(2021, 1, 1)

    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)

    exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output')) if stem in x]
    for exp_name in exp_names:
        output_path = os.path.join(wdir, 'simulation_output',exp_name)
        for ems_nr in range(1,12):
            print("Start processing region " + str(ems_nr))
            compare_ems(exp_name, ems_nr=int(ems_nr),first_day=first_plot_day,last_day=last_plot_day,weights_array=weights_array, plot_trajectories=args.plot)
    extract_sample_traces()
