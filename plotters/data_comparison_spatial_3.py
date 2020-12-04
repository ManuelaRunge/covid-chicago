"""
Compare COVID-19 simulation outputs to data.
Allow lines per varying input parameter to be specified
"""
import argparse
import os
import pandas as pd
import matplotlib.pyplot as plt
import sys

sys.path.append('../')
from load_paths import load_box_paths
import matplotlib as mpl
import matplotlib.dates as mdates
from datetime import date, timedelta, datetime
import seaborn as sns
from processing_helpers import *

mpl.rcParams['pdf.fonttype'] = 42
today = datetime.today()
datetoday = date(2021,1,1) #date(today.year, today.month, today.day)


def parse_args():
    description = "Simulation run for modeling Covid-19"
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        "-stem",
        "--stem",
        type=str,
        help="Name of simulation experiment"
    )
    parser.add_argument(
        "-loc",
        "--Location",
        type=str,
        help="Local or NUCLUSTER",
        default="Local"
    )
    parser.add_argument(
        "-t", "--trajectoriesName",
        type=str,
        help="Name of trajectoriesDat file, could be trajectoriesDat.csv or trajectoriesDat_trim.csv",
        default='trajectoriesDat.csv',
    )
    return parser.parse_args()


def load_sim_data(exp_name, ems_nr, input_wdir=None, fname='trajectoriesDat.csv', input_sim_output_path=None,
                  column_list=None):
    input_wdir = input_wdir or wdir
    sim_output_path_base = os.path.join(input_wdir, 'simulation_output','_overflow_simulations', exp_name)
    sim_output_path = sim_output_path_base

    df = pd.read_csv(os.path.join(sim_output_path, fname), usecols=column_list)
    df['run_num']=-9
    # df.columns = df.columns.str.replace('_All', '')
    df.columns = df.columns.str.replace('_EMS-' + str(ems_nr), '')
    #df['infected_cumul'] = df['infected'] + df['recovered'] + df['deaths']
    #df = calculate_incidence(df)

    df['new_detected_hospitalized'] = count_new(df, 'hosp_det_cumul')
    df['new_hospitalized'] = count_new(df, 'hosp_cumul')
    df['new_critical'] = count_new(df, 'crit_cumul')
    df['new_detected_critical'] = count_new(df, 'crit_det_cumul')
    df['new_detected_deaths'] = count_new(df, 'death_det_cumul')
    df['new_deaths'] = count_new(df, 'deaths')

    return df


def plot_sim_and_ref(df, ems_nr, ref_df, channels, data_channel_names, titles, first_day=date(2020, 2, 22),
                     ymax=40, logscale=True):
    fig = plt.figure(figsize=(17, 8))
    palette = sns.color_palette('husl', 12)
    k = 0
    for c, channel in enumerate(channels):
        ax = fig.add_subplot(2, 3, c + 1)

        mdf = df.groupby(['time','reopening_multiplier_4'])[channel].agg([CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75]).reset_index()

        for i, rtc in enumerate(mdf['reopening_multiplier_4'].unique()):
            mdf_sub = mdf[mdf['reopening_multiplier_4'] == rtc]
            dates = [first_day + timedelta(days=int(x)) for x in mdf_sub['time']]
            ax.plot(dates, mdf_sub['CI_50'], color=palette[i], label=rtc)

        mdf_all = df.groupby(['time'])[channel].agg([CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75]).reset_index()
        ax.fill_between(dates, mdf_all['CI_2pt5'], mdf_all['CI_97pt5'], color=palette[k], linewidth=0, alpha=0.2)

        ax.set_title(titles[c], y=0.8, fontsize=12)
        # ax.legend()

        formatter = mdates.DateFormatter("%m-%d")
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_locator(mdates.MonthLocator())
        ax.set_xlim(first_day, datetoday)
        ax.grid(b=True, which='major', color='#999999', linestyle='-', alpha=0.3)
        if logscale:
            ax.set_ylim(0.1, ymax)
            ax.set_yscale('log')

        ax.plot(ref_df['date'], ref_df[data_channel_names[c]], 'o', color='#303030', linewidth=0, ms=1)
        ax.plot(ref_df['date'], ref_df[data_channel_names[c]].rolling(window=7, center=True).mean(), c='k', alpha=1.0)
        if c == len(channels)-1:
            ax.legend()

    fig.suptitle(f'Covidregion {ems_nr}', y=1, fontsize=14)
    fig.tight_layout()
    fig.subplots_adjust(top=0.88)

    plot_name = 'compare_to_data_covidregion_' + str(ems_nr)
    if logscale == False:
        plot_name = plot_name + "_nolog"

    if not os.path.exists(plot_path):
        os.makedirs(plot_path)
    if not os.path.exists(os.path.join(plot_path,'pdf')):
        os.makedirs(os.path.join(plot_path,'pdf'))
    plt.savefig(os.path.join(plot_path, plot_name + '.png'))
    plt.savefig(os.path.join(plot_path, 'pdf', plot_name + '.pdf'), format='PDF')
    # return a


def compare_ems(exp_name, fname, ems_nr=0):
    column_list = ['time', 'startdate', 'scen_num', 'sample_num']

    outcome_channels = [ 'hosp_det_cumul', 'hosp_cumul',  'crit_cumul',
                        'crit_det_cumul', 'death_det_cumul','deaths', 'crit_det',
                         'critical', 'hosp_det', 'hospitalized']

    for channel in outcome_channels:
        column_list.append(channel + "_EMS-" + str(ems_nr))

    df = load_sim_data(exp_name, ems_nr, fname=fname, column_list=column_list)
    first_day = datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['critical_with_suspected'] = df['critical']
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    df = df[df['date'].dt.date <= datetoday]

    param="reopening_multiplier_4"
    sampled_df = pd.read_csv(os.path.join(exp_dir, "sampled_parameters.csv"), usecols=['scen_num', param])
    df = pd.merge(how='left', left=df, left_on='scen_num', right=sampled_df, right_on='scen_num')

    ref_df = load_ref_df(ems_nr)

    channels = ['new_detected_deaths', 'crit_det', 'hosp_det', 'new_deaths', 'new_detected_hospitalized',
                'new_detected_hospitalized']
    data_channel_names = ['confirmed_covid_deaths_prev_24h',
                          'confirmed_covid_icu', 'covid_non_icu', 'deaths', 'inpatient', 'admissions']
    titles = ['New Detected\nDeaths (EMR)', 'Critical Detected (EMR)', 'Inpatient non-ICU\nCensus (EMR)',
              'New Detected\nDeaths (LL)',
              'Covid-like illness\nadmissions (IDPH)', 'New Detected\nHospitalizations (LL)']

    plot_sim_and_ref(df, ems_nr, ref_df, channels=channels, data_channel_names=data_channel_names, titles=titles,
                     ymax=10000, first_day=first_day, logscale=True)
    #plot_sim_and_ref(df, ems_nr, ref_df, channels=channels, data_channel_names=data_channel_names, titles=titles,
    #                 ymax=10000, first_day=first_day, logscale=False)

    # return ref_df_emr, ref_df_ll


if __name__ == '__main__':

    #args = parse_args()
    trajectoriesName = 'trajectoriesDat.csv' #args.trajectoriesName
    Location = 'Local' #args.Location
    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)

    #stem = args.stem
    #exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output')) if stem in x]
    exp_names = ['20201110_IL_baseline_gradual_reopening']

    for exp_name in exp_names:
        exp_dir = os.path.join(wdir, 'simulation_output','_overflow_simulations', exp_name)
        plot_path = os.path.join(exp_dir, '_plots')
        for ems_nr in range(1, 12):
            print("Start processing region " + str(ems_nr))
            compare_ems(exp_name, fname=trajectoriesName, ems_nr=int(ems_nr))