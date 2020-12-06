import os
import sys
sys.path.append('../')
from load_paths import load_box_paths
import re
import pandas as pd
import datetime as dt
import numpy as np
import matplotlib as mpl
from processing_helpers import *

datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths()

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


def get_probs(exp_name, file_str='hospitaloverflow', fname='trajectoriesDat_trim.csv'):
    trajectories = pd.read_csv(os.path.join(analysis_dir, fname), usecols=column_list)
    trajectories = trajectories.dropna()
    first_day = dt.datetime.strptime(trajectories['startdate'].unique()[0], '%Y-%m-%d')
    trajectories['date'] = trajectories['time'].apply(lambda x: first_day + timedelta(days=int(x)))

    first_plot_day = dt.date(2020, 10, 1)
    last_plot_day = dt.date(2020, 12, 31)
    trajectories['date'] = pd.to_datetime( trajectories['date']).dt.date
    trajectories = trajectories[(trajectories['date'] >= first_plot_day) & (trajectories['date'] <= last_plot_day)]

    template_fname = 'capacity_weekday_average_20200915.csv'#'capacity_weekday_average_20200901.csv' #'#get_latest_filedate()
    civis_template = pd.read_csv(os.path.join(datapath, 'covid_IDPH', 'Corona virus reports', 'hospital_capacity_thresholds', template_fname))
    civis_template = civis_template[civis_template['resource_type'] == 'icu_availforcovid']
    civis_template = civis_template[civis_template['date_window_upper_bound'] == civis_template['date_window_upper_bound'].max()]
    civis_template = civis_template[civis_template['overflow_threshold_percent'] == 1]
    civis_template['avg_resource_available'] = civis_template['avg_resource_available_prev2weeks']

    for ems_region in range(1, 12):
        trajectories['total_hosp_census_EMS-' + str(ems_region)] = trajectories['hosp_det_EMS-' + str(ems_region)] + \
                                                                   trajectories['crit_det_EMS-' + str(ems_region)]

    df_prob = pd.DataFrame(columns=['geography_modeled', 'icu_availforcovid','capacity_multiplier', 'prob'])

    for index, geography in enumerate(civis_template.geography_modeled.unique()):
        thresh = civis_template[civis_template['geography_modeled'] == geography]
        thresh = int(thresh['avg_resource_available'])

        for capacity in list(trajectories.capacity_multiplier.unique()):
            #(capacity)
            trajectories_sub = trajectories[trajectories['capacity_multiplier']== capacity]
            unique_scen = np.array(list(set(trajectories_sub['scen_num'].values)))
            metric_root = 'crit_det_EMS-'
            region = int(re.sub('[^0-9]', '', geography))
            prob = 0
            for scen in unique_scen:
                new = trajectories_sub[(trajectories_sub['scen_num'] == scen)].reset_index()
                if new[metric_root + str(region)].max() > thresh :  # (new, metric_root + str(region), lower_limit, upper_limit, thresh):
                    prob += 1
            prob_overflow = prob / len(unique_scen)
            df_prob = df_prob.append({'geography_modeled': geography,
                                      'icu_availforcovid': thresh,
                                      'capacity_multiplier': capacity,
                                      'prob': prob_overflow,
                                      'n_overflow': prob,
                                      'nscen':  len(unique_scen)}, ignore_index=True)
    df_prob.capacity_multiplier.value_counts()
    df_prob.geography_modeled.value_counts()

    df_prob['exp_name'] = exp_name
    df_prob.to_csv(os.path.join(analysis_dir, file_str + '.csv'), index=False)

def get_dates(file_str= 'hospitaloverflow',fname='trajectories_aggregated.csv'):
    df = pd.read_csv(os.path.join(analysis_dir, fname))
    df = df.dropna()
    df['date'] = pd.to_datetime(df['date'])
    df['capacity_multiplier'] = df['capacity_multiplier'].round(decimals=2)
    df = df[(df['date'] > pd.Timestamp(2020, 10, 1)) & (df['date'] < pd.Timestamp(2020, 12, 31))]

    df_prob = pd.read_csv(os.path.join(analysis_dir, file_str +  '.csv'))
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

    df_probAll.to_csv(os.path.join(analysis_dir, file_str + '_dates.csv'), index=False)

    return df_prob

def get_numbers(file_str = 'hospitaloverflow', fname='trajectories_aggregated.csv'):
    df = pd.read_csv(os.path.join(analysis_dir, fname))
    df = df.dropna()
    df['date'] = pd.to_datetime(df['date'])
    df['capacity_multiplier'] = df['capacity_multiplier'].round(decimals=2)
    df = df[(df['date'] > pd.Timestamp(2020, 10, 1)) & (df['date'] < pd.Timestamp(2020, 12, 31))]

    df_prob = pd.read_csv(os.path.join(analysis_dir, file_str + '.csv'))
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

    df_prob.to_csv(os.path.join(analysis_dir, file_str + '.csv'), index=False)

    return df_prob

def get_duration(file_str= 'hospitaloverflow', fname='trajectories_aggregated.csv'):
    df = pd.read_csv(os.path.join(analysis_dir, fname))
    df = df.dropna()
    df['date'] = pd.to_datetime(df['date'])
    df['capacity_multiplier'] = df['capacity_multiplier'].round(decimals=2)
    df = df[(df['date'] > pd.Timestamp(2020, 10, 1)) & (df['date'] < pd.Timestamp(2020, 12, 31))]

    df_dur = pd.read_csv(os.path.join(analysis_dir, file_str +  '.csv'))
    df_dur.reset_index()
    metric_root = 'crit_det'
    df_durAll = pd.DataFrame()

    for tresh_multiplier in [0.25, 0.5, 0.75, 1]:

        for index, row in df_dur.iterrows():
            region = row['geography_modeled']
            thresh = row['icu_availforcovid'] * tresh_multiplier
            capacity = round(row['capacity_multiplier'],2)

            df_dur.loc[index, 'icu_availforcovid_threshold'] = thresh

            new = df[(df['geography_modeled'] == region) & (df['capacity_multiplier'] == capacity)].reset_index()

            for stat in ['median', '95CI_lower', '95CI_upper', '50CI_lower', '50CI_upper', 'max', 'min']:
                exceed = new[new[f'{metric_root}_{stat}'] >= thresh].reset_index()

                if len(exceed) > 0 :
                    duration = (exceed['date'].max() - exceed['date'].min()).days
                    df_dur.loc[index, f'{metric_root}_exceed_duration_{stat}'] = duration
                else:
                    df_dur.loc[index, f'{metric_root}_exceed_duration_{stat}'] = None

            del region, thresh, capacity, new, exceed

        df_dur = df_dur[['geography_modeled', 'icu_availforcovid', 'capacity_multiplier','exp_name',
                         'crit_det_exceed_duration_median','crit_det_exceed_duration_95CI_lower',
                         'crit_det_exceed_duration_95CI_upper','crit_det_exceed_duration_50CI_lower',
                         'crit_det_exceed_duration_50CI_upper','crit_det_exceed_duration_max',
                         'crit_det_exceed_duration_min']]
        if df_durAll.empty:
            df_durAll = df_dur
        else:
            df_durAll = df_durAll.append(df_dur)

    df_durAll.to_csv(os.path.join(analysis_dir, file_str + '_duration.csv'), index=False)

    return df_dur


if __name__ == '__main__':
    #stem = sys.argv[1]
    stem = '20200919_IL_regreopen50perc'
    exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output','_overflow_simulations', '20200919')) if stem in x]

    for exp_name in exp_names:
        analysis_dir = os.path.join(wdir, 'simulation_output','_overflow_simulations','20200919',exp_name)
        print("1 - get probs for " + exp_name)
        get_probs(exp_name)
        print("2 - get numbers for " + exp_name)
        get_numbers()
        print("3 - get dates for " + exp_name)
        get_dates()
        print("4 - get duration for " + exp_name)
        get_duration()
