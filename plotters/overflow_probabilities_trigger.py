import os
import sys
sys.path.append('../')
from load_paths import load_box_paths
from processing_helpers import *

import pandas as pd
import datetime as dt
import seaborn as sns
import numpy as np
import matplotlib as mpl

mpl.rcParams['pdf.fonttype'] = 42
import statistics as st
from statsmodels.distributions.empirical_distribution import ECDF
import scipy
import gc
import re
import datetime
datetime.datetime.strptime

datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths()


def load_sim_data(exp_name, fname='trajectoriesDat.csv',input_wdir=None, input_sim_output_path=None, column_list=None):
    input_wdir = input_wdir or wdir
    sim_output_path_base = os.path.join(input_wdir, 'simulation_output','_overflow_simulations', exp_name)
    sim_output_path = input_sim_output_path or sim_output_path_base

    df = pd.read_csv(os.path.join(sim_output_path, fname), usecols=column_list)
    return df


def exceeds(trajectory, metric, lower_limit, upper_limit, maximum):
    return (trajectory[metric].values[lower_limit:upper_limit] > maximum).any()


def when_exceeds(trajectory, metric, lower_limit, upper_limit, maximum):
    ii = lower_limit
    while trajectory[metric].values[ii] <= maximum:
        ii += 1
    return ii


column_list = ['scen_num','time','startdate','capacity_multiplier', 'hosp_det_All', 'crit_det_All']  # 'reopening_multiplier_4'
for ems_region in range(1, 12):
    column_list.append('hosp_det_EMS-' + str(ems_region))
    column_list.append('crit_det_EMS-' + str(ems_region))
    # column_list.append('death_det_cumul_EMS-' + str(ems_region))


def get_latest_filedate(file_path=os.path.join(datapath, 'covid_IDPH', 'Corona virus reports',
                                               'hospital_capacity_thresholds'), extraThresholds=False):
    files = os.listdir(file_path)
    files = sorted(files, key=len)
    if extraThresholds == False:
        files = [name for name in files if not 'extra_thresholds' in name]
    if extraThresholds == True:
        files = [name for name in files if 'extra_thresholds' in name]

    filedates = [item.replace('capacity_weekday_average_', '') for item in files]
    filedates = [item.replace('.csv', '') for item in filedates]
    latest_filedate = max([int(x) for x in filedates])
    fname = f'capacity_weekday_average_{latest_filedate}.csv'
    if extraThresholds == True:
        fname = f'capacity_weekday_average_{latest_filedate}__extra_thresholds.csv'
    return fname


def get_probs(exp_name,  file_str = 'hospitaloverflow'):
    trajectories = load_sim_data(exp_name, column_list=column_list, fname='trajectoriesDat_trim.csv')
    trajectories = trajectories.dropna()
    template_fname = get_latest_filedate()
    civis_template = pd.read_csv(os.path.join(datapath, 'covid_IDPH', 'Corona virus reports', 'hospital_capacity_thresholds', template_fname))
    civis_template = civis_template[civis_template['resource_type'] == 'icu_availforcovid']
    civis_template = civis_template[civis_template['date_window_upper_bound'] == civis_template['date_window_upper_bound'].max()]
    civis_template = civis_template[civis_template['overflow_threshold_percent'] == 1]

    for ems_region in range(1, 12):
        trajectories['total_hosp_census_EMS-' + str(ems_region)] = trajectories['hosp_det_EMS-' + str(ems_region)] + \
                                                                   trajectories['crit_det_EMS-' + str(ems_region)]

    lower_limit = (dt.datetime(month=10, day=1, year=2020) - dt.datetime(month=2, day=13, year=2020)).days
    unique_scen = np.array(list(set(trajectories['scen_num'].values)))
    df_prob = pd.DataFrame(columns=['geography_modeled', 'icu_availforcovid','capacity_multiplier', 'prob'])

    for index, geography in enumerate(civis_template.geography_modeled.unique()):
        thresh = civis_template[civis_template['geography_modeled'] == geography]
        thresh = int(thresh['avg_resource_available'])

        for capacity in list(trajectories.capacity_multiplier.unique()):
            print(capacity)
            trajectories_sub = trajectories[trajectories['capacity_multiplier']== capacity]
            upper_limit = (dt.datetime(month=12, day=31, year=2020) - dt.datetime(month=2, day=13, year=2020)).days
            metric_root = 'crit_det_EMS-'
            region = int(re.sub('[^0-9]', '', geography))
            prob = 0
            for scen in unique_scen:
                new = trajectories_sub[(trajectories_sub['scen_num'] == scen)].reset_index()
                if exceeds(new, metric_root + str(region), lower_limit, upper_limit, thresh):
                    prob += 1 / len(unique_scen)
            df_prob = df_prob.append({'geography_modeled': geography,
                                      'icu_availforcovid': thresh,
                                      'capacity_multiplier': capacity,
                                      'prob': prob, }, ignore_index=True)
    df_prob.capacity_multiplier.value_counts()
    df_prob.geography_modeled.value_counts()

    df_prob['exp_name'] = exp_name
    df_prob.to_csv(os.path.join(wdir, 'simulation_output','_overflow_simulations',  exp_name, file_str, '.csv'), index=False)


def get_dates(exp_name, file_str= 'hospitaloverflow'):
    df = load_sim_data(exp_name, fname='trajectories_aggregated.csv')
    df = df.dropna()
    df['date'] = pd.to_datetime(df['date'])
    df['capacity_multiplier'] = df['capacity_multiplier'].round(decimals=2)
    df = df[(df['date'] > pd.Timestamp(2020, 10, 1)) & (df['date'] < pd.Timestamp(2020, 12, 31))]

    df_prob = pd.read_csv(os.path.join(wdir, 'simulation_output','_overflow_simulations',  exp_name, file_str, '.csv'))
    df_prob.reset_index()
    metric_root = 'crit_det'
    df_probAll = pd.DataFrame()

    for tresh_multiplier in [0, 0.25, 0.5, 0.75, 1]:

        for index, row in df_prob.iterrows():
            region = row['geography_modeled']
            thresh = row['icu_availforcovid'] * tresh_multiplier
            capacity = round(row['capacity_multiplier'],2)

            df_prob.loc[index, 'icu_availforcovid_threshold'] = thresh

            new = df[(df['geography_modeled'] == region) & (df['capacity_multiplier'] == capacity)].reset_index()

            for stat in ['median', '95CI_lower', '95CI_upper', '50CI_lower', '50CI_upper', 'max', 'min']:
                exceed = new[new[f'{metric_root}_{stat}'] >= thresh].reset_index()

                if len(exceed) > 0 :
                    exceed = exceed[(exceed['date'] == pd.Timestamp(exceed['date'].min()))]
                    df_prob.loc[index, f'{metric_root}_exceed_date_{stat}'] = dt.date(exceed['date'][0].year, exceed['date'][0].month, exceed['date'][0].day)
                else:
                    df_prob.loc[index, f'{metric_root}_exceed_date_{stat}'] = None

                new_sub = new[(new[f'{metric_root}_{stat}'] == new[f'{metric_root}_{stat}'].max())].reset_index()
                if len(new_sub) > 1:
                    new_sub = new_sub[ (new_sub['date'] == pd.Timestamp(new_sub['date'].min()))]
                if len(new_sub) == 0:
                    break

                df_prob.loc[index, f'{metric_root}_peak_date_{stat}'] = dt.date(new_sub['date'][0].year, new_sub['date'][0].month, new_sub['date'][0].day)
                df_prob.loc[index, f'{metric_root}_peak_date_{stat}'] = dt.date(new_sub['date'][0].year, new_sub['date'][0].month, new_sub['date'][0].day)

            del region, thresh, capacity, new, exceed

        if df_probAll.empty:
            df_probAll = df_prob
        else:
            df_probAll = df_probAll.append(df_prob)

    df_probAll.to_csv(os.path.join(wdir, 'simulation_output','_overflow_simulations',  exp_name, file_str, '_dates.csv'), index=False)

    return df_prob

def get_numbers(exp_name, file_str = 'hospitaloverflow'):
    df = load_sim_data(exp_name, fname='trajectories_aggregated.csv')
    df = df.dropna()
    df['date'] = pd.to_datetime(df['date'])
    df['capacity_multiplier'] = df['capacity_multiplier'].round(decimals=2)
    df = df[(df['date'] > pd.Timestamp(2020, 10, 1)) & (df['date'] < pd.Timestamp(2020, 12, 31))]

    df_prob = pd.read_csv(os.path.join(wdir, 'simulation_output','_overflow_simulations',  exp_name, file_str , '.csv'))
    df_prob.reset_index()
    metric_root = 'crit_det'

    for index, row in df_prob.iterrows():
        region = row['geography_modeled']
        thresh = row['icu_availforcovid']
        capacity = round(row['capacity_multiplier'],2)

        new = df[(df['geography_modeled'] == region) & (df['capacity_multiplier'] ==capacity) ].reset_index()
        new = new[(new[f'{metric_root}_median'] == new[f'{metric_root}_median'].max())].reset_index()

        if len(new) > 1 :
            new = new[ (new['date'] == pd.Timestamp(new['date'].min()))]
        if len(new) ==0  :
            break

        df_prob.loc[index, f'{metric_root}_median'] = int(new[f'{metric_root}_median'])
        df_prob.loc[index, f'{metric_root}_50CI_lower'] = int(new[f'{metric_root}_50CI_lower'])
        df_prob.loc[index, f'{metric_root}_50CI_upper'] = int(new[f'{metric_root}_50CI_upper'])
        df_prob.loc[index, f'{metric_root}_95CI_lower'] = int(new[f'{metric_root}_95CI_lower'])
        df_prob.loc[index, f'{metric_root}_95CI_upper'] = int(new[f'{metric_root}_95CI_upper'])

        df_prob.loc[index, 'number_that_exceed_median'] = thresh - int(new[f'{metric_root}_median'])
        df_prob.loc[index, 'number_that_exceed_50CI_lower'] = thresh - int(new[f'{metric_root}_50CI_lower'])
        df_prob.loc[index, 'number_that_exceed_50CI_upper'] = thresh - int(new[f'{metric_root}_50CI_upper'])
        df_prob.loc[index, 'number_that_exceed_95CI_lower'] = thresh - int(new[f'{metric_root}_95CI_lower'])
        df_prob.loc[index, 'number_that_exceed_95CI_upper'] = thresh - int(new[f'{metric_root}_95CI_upper'])

        del region, thresh, capacity, new

    df_prob.to_csv(os.path.join(wdir, 'simulation_output','_overflow_simulations',  exp_name, file_str, '.csv'), index=False)

    return df_prob


if __name__ == '__main__':
    #stem = sys.argv[1]
    stem = '20200919_IL_regreopen50'
    exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output','_overflow_simulations')) if stem in x]

    for exp_name in exp_names:
        get_probs(exp_name)
        get_numbers(exp_name)
        get_dates(exp_name)
