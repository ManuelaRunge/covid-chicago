import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import seaborn as sns
import matplotlib as mpl
import matplotlib.dates as mdates
from datetime import date, timedelta, datetime
import sys

sys.path.append('../')
from processing_helpers import *
from load_paths import load_box_paths

datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths()
analysis_dir = os.path.join(wdir, 'simulation_output', '_overflow_simulations')

mpl.rcParams['pdf.fonttype'] = 42

plot_first_day = pd.to_datetime('2020/3/1')
plot_last_day = pd.to_datetime('2021/4/1')


def parse_args():
    description = "Process simulation outputs to send to Civis"
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        "-e", "--exp_name",
        type=str,
        help="Name of experiment and folder name",
        default=None,
    )
    parser.add_argument(
        "-p", "--processStep",
        type=str,
        help="Only required if files are too large to process regions in a loop",
        default='generate_outputs',
    )
    parser.add_argument(
        "-l", "--Location",
        type=str,
        help="Local or NUCLUSTER",
        default='Local',
    )
    parser.add_argument(
        "-t", "--trajectoriesName",
        type=str,
        help="Name of trajectoriesDat file, could be trajectoriesDat.csv or trajectoriesDat_trim.csv",
        default='trajectoriesDat.csv',
    )

    return parser.parse_args()

def load_sim_data(exp_name,  region_suffix ='_All', input_wdir=None, fname='trajectoriesDat.csv', input_sim_output_path=None,
                  column_list=None):
    input_wdir = input_wdir or wdir
    sim_output_path_base = os.path.join(analysis_dir, exp_name)
    sim_output_path = sim_output_path_base

    df = pd.read_csv(os.path.join(sim_output_path, fname), usecols=column_list)
    df['run_num']=-9
    # df.columns = df.columns.str.replace('_All', '')
    df.columns = df.columns.str.replace(region_suffix, '')

    df['new_detected_hospitalized'] = count_new(df, 'hosp_det_cumul')
    df['new_hospitalized'] = count_new(df, 'hosp_cumul')
    df['new_critical'] = count_new(df, 'crit_cumul')
    df['new_detected_critical'] = count_new(df, 'crit_det_cumul')
    df['new_detected_deaths'] = count_new(df, 'death_det_cumul')
    df['new_deaths'] = count_new(df, 'deaths')

    return df

def get_scenarioName(exp_suffix):
    scenarioName = exp_suffix
    if exp_suffix == "regreopen100perc_0daysdelay_sm4": scenarioName = "100perc_0daysdelay_sm4"
    if exp_suffix == "regreopen100perc_3daysdelay_sm4": scenarioName = "100perc_3daysdelay_sm4"
    if exp_suffix == "regreopen100perc_7daysdelay_sm4": scenarioName = "100perc_7daysdelay_sm4"
    if exp_suffix == "regreopen50perc_0daysdelay_sm4": scenarioName = "50perc_0daysdelay_sm4"
    if exp_suffix == "regreopen50perc_3daysdelay_sm4": scenarioName = "50perc_3daysdelay_sm4"
    if exp_suffix == "regreopen50perc_7daysdelay_sm4": scenarioName = "50perc_7daysdelay_sm4"

    if exp_suffix == "regreopen100perc_0daysdelay_sm6": scenarioName = "100perc_0daysdelay_sm6"
    if exp_suffix == "regreopen100perc_3daysdelay_sm6": scenarioName = "100perc_3daysdelay_sm6"
    if exp_suffix == "regreopen100perc_7daysdelay_sm6": scenarioName = "100perc_7daysdelay_sm6"
    if exp_suffix == "regreopen50perc_0daysdelay_sm6": scenarioName = "50perc_0daysdelay_sm6"
    if exp_suffix == "regreopen50perc_3daysdelay_sm6": scenarioName = "50perc_3daysdelay_sm6"
    if exp_suffix == "regreopen50perc_7daysdelay_sm6": scenarioName = "50perc_7daysdelay_sm6"

    return (scenarioName)

def plot_sim(dat, suffix, channels):
    if suffix not in ["All", "central", "southern", "northeast", "northcentral"]:
        suffix_nr = str(suffix.split("-")[1])
    if suffix == "All":
        suffix_nr = "illinois"
    capacity = load_capacity(suffix_nr)

    fig = plt.figure(figsize=(18, 12))
    fig.subplots_adjust(right=0.97, wspace=0.2, left=0.07, hspace=0.15)
    palette = sns.color_palette('Set1', len(channels))

    for c, channel in enumerate(channels):
        ax = fig.add_subplot(3, 3, c + 1)

        ax.plot(dat['date'], dat['%s_median' % channel], color=palette[c])
        ax.fill_between(dat['date'].values, dat['%s_95CI_lower' % channel], dat['%s_95CI_upper' % channel],
                        color=palette[c], linewidth=0, alpha=0.2)
        ax.fill_between(dat['date'].values, dat['%s_50CI_lower' % channel], dat['%s_50CI_upper' % channel],
                        color=palette[c], linewidth=0, alpha=0.4)

        if channel in capacity.keys():
            ax.plot([np.min(dat['date']), np.max(dat['date'])],
                    [capacity[channel], capacity[channel]], '--', linewidth=2, color=palette[c])

        ax.set_title(channel, y=0.85)
        formatter = mdates.DateFormatter("%m-%d")
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_locator(mdates.MonthLocator())

    plotname = f'{scenarioName}_{suffix}'
    plotname = plotname.replace('EMS-', 'covidregion_')

    plt.suptitle(f'Covidregion {suffix_nr}', y=1, fontsize=14)
    plt.tight_layout()
    plt.subplots_adjust(top=0.88)

    #plt.savefig(os.path.join(plot_path, plotname + '.png'))
    plt.savefig(os.path.join(plot_path, 'pdf', plotname + '.pdf'), format='PDF')
    # plt.show()


def load_and_plot_data(ems_region, fname, input_sim_output_path,savePlot=True):
    column_list = ['startdate', 'time', 'scen_num', 'sample_num', 'run_num','capacity_multiplier']
    #column_list = ['startdate', 'time', 'scen_num', 'sample_num', 'run_num','reopening_multiplier_4']

    #'infected', 'recovered', 'infected_cumul',
    outcome_channels = ['hosp_det_cumul', 'hosp_cumul',  'crit_cumul',
                        'crit_det_cumul', 'death_det_cumul',
                        'deaths', 'crit_det', 'critical', 'hosp_det', 'hospitalized']

    for channel in outcome_channels:
        column_list.append(channel + "_" + str(ems_region))

    df = load_sim_data(exp_name, region_suffix='_' + ems_region, fname=fname, column_list=column_list,
                       input_sim_output_path= input_sim_output_path)

    df['ems'] = ems_region
    first_day = datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    df = df[(df['date'] >= plot_first_day) & (df['date'] <= plot_last_day)]

    df['ventilators'] = get_vents(df['crit_det'].values)
    channels = ['new_deaths', 'new_detected_deaths', 'hospitalized','critical', 'hosp_det', 'crit_det']

    adf = pd.DataFrame()
    for c, channel in enumerate(channels):
        mdf = df.groupby(['date', 'ems','capacity_multiplier'])[channel].agg([np.min, CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75, np.max]).reset_index()
        #mdf = df.groupby(['date', 'ems', 'reopening_multiplier_4'])[channel].agg([np.min, CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75, np.max]).reset_index()


        mdf = mdf.rename(columns={'amin': '%s_min' % channel,
                                  'CI_50': '%s_median' % channel,
                                  'CI_2pt5': '%s_95CI_lower' % channel,
                                  'CI_97pt5': '%s_95CI_upper' % channel,
                                  'CI_25': '%s_50CI_lower' % channel,
                                  'CI_75': '%s_50CI_upper' % channel,
                                  'amax': '%s_max' % channel})
        if adf.empty:
            adf = mdf
        else:
            adf = pd.merge(left=adf, right=mdf, on=['date', 'ems','capacity_multiplier'])
            #adf = pd.merge(left=adf, right=mdf, on=['date', 'ems', 'reopening_multiplier_4'])


    #if savePlot :
    #    plot_sim(adf, ems_region, channels)

    return adf


def process_and_save(adf, ems_region, SAVE=True):
    #col_names = civis_colnames(reverse=False)
    #adf = adf.rename(columns=col_names)
    adf['geography_modeled'] = adf['ems']
    adf.geography_modeled = adf.geography_modeled.str.replace('-', "")
    adf.geography_modeled = adf.geography_modeled.str.lower()
    adf.geography_modeled = adf.geography_modeled.str.replace('all', "illinois")

    adf['scenario_name'] = scenarioName
    dfout = adf[adf['date'] > min(adf['date'])]


    if SAVE:
        filename = "trajectories_aggregated_" + ems_region + ".csv"
        rename_geography_and_save(dfout, filename=filename)

    return dfout


def rename_geography_and_save(df, filename):
    dfout = df.copy()
    if "geography_modeled" not in dfout.columns:
        dfout.rename(columns={'ems': 'covid_region'}, inplace=True)
        dfout['covid_region'] = dfout['covid_region'].str.replace('EMS-', '')

    if "geography_modeled" in dfout.columns:
        dfout['geography_modeled'] = dfout['geography_modeled'].str.replace('ems', 'covidregion_')

    dfout.to_csv(os.path.join(sim_output_path, filename), index=False)


if __name__ == '__main__':

    # args = parse_args()
    #stem = args.stem
    stem = '20201112_IL_600_baseline'
    exp_names = [x for x in os.listdir(os.path.join(analysis_dir)) if stem in x]

    for exp_name in exp_names :
        #exp_name = '20200919_IL_regreopen100perc_0daysdelay_sm7'
        simdate = exp_name.split("_")[0]
        processStep = 'generate_outputs'
        trajectoriesName = 'trajectoriesDat.csv'

        regions = ['All', 'EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10',
                   'EMS-11']

        exp_suffix = exp_name.split("_IL_")[-1]
        scenarioName = get_scenarioName(exp_suffix)

        sim_output_path = os.path.join(analysis_dir, exp_name)
        plot_path = os.path.join(sim_output_path, '_plots')

        if processStep == 'generate_outputs':
            dfAll = pd.DataFrame()
            for reg in regions:
                print(f'Start processing {reg}')
                tdf = load_and_plot_data(reg, fname=trajectoriesName, savePlot=True,input_sim_output_path=sim_output_path)
                adf = process_and_save(tdf, reg, SAVE=True)
                dfAll = pd.concat([dfAll, adf])
                del tdf

            if len(regions) == 12:
                filename = f'trajectories_aggregated.csv'
                rename_geography_and_save(dfAll, filename=filename)

        ### Optional
        if processStep == 'combine_outputs':

            for reg in ['All', 'EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10',
                        'EMS-11']:
                print("Start processing" + reg)
                filename = "trajectories_aggregated_" + reg + ".csv"
                adf = pd.read_csv(os.path.join(sim_output_path, filename))
                dfAll = pd.concat([dfAll, adf])

            filename = f'trajectories_aggregated.csv'
            rename_geography_and_save(dfAll, filename=filename)

