import os
import sys
import argparse
sys.path.append('../')
from load_paths import load_box_paths
import re
import pandas as pd
import datetime as dt
import numpy as np
import matplotlib as mpl
import random
from processing_helpers import *

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
        default = "NUCLUSTER"
    )

    return parser.parse_args()
    
def load_sim_data(exp_name,  region_suffix ='_All', sim_output_path=None, fname=None, column_list=None):
    df = pd.read_csv(os.path.join(sim_output_path, fname), usecols=column_list)
    df = df.dropna()
    df.columns = df.columns.str.replace(region_suffix, '')
    first_day = datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    df['date'] = pd.to_datetime(df['date']).dt.date

    return df

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
def get_capacity():
    template_fname = 'capacity_weekday_average_20200915.csv'  # 'capacity_weekday_average_20200901.csv' #'#get_latest_filedate()
    capacity_df = pd.read_csv(
        os.path.join(datapath, 'covid_IDPH', 'Corona virus reports', 'hospital_capacity_thresholds',template_fname))
    capacity_df = capacity_df[capacity_df['date_window_upper_bound'] == capacity_df['date_window_upper_bound'].max()]
    capacity_df = capacity_df[capacity_df['overflow_threshold_percent'] == 1]
    capacity_df['avg_resource_available'] = capacity_df['avg_resource_available_prev2weeks']
    capacity_df = capacity_df[['geography_modeled', 'avg_resource_available','resource_type']].reset_index()
    capacity_df['region'] = capacity_df['geography_modeled'].str.replace('covidregion_', '')
    capacity_df = capacity_df[capacity_df['resource_type'] == 'icu_availforcovid']
    return capacity_df

def get_probs(exp_name, file_str='hospitaloverflow', regions=range(1, 12),
              first_plot_day = dt.date(2020, 10, 1),last_plot_day = dt.date(2020, 12, 31),
              random_sub=None):

    capacity_df = get_capacity()
    df_prob = pd.DataFrame(columns=['geography_modeled', 'icu_availforcovid', 'capacity_multiplier', 'prob'])
    for ems_region in regions:
        print(ems_region)
        column_list = ['scen_num','sample_num', 'time', 'startdate','capacity_multiplier']
        #column_list.append('hosp_det_EMS-' + str(ems_region))
        column_list.append('crit_det_EMS-' + str(ems_region))

        trajectories_name = 'trajectoriesDat_region_' + str(ems_region) + '.csv'
        if os.path.exists(os.path.join(analysis_dir,trajectories_name)) == False:
            trajectories_name = 'trajectoriesDat_trim.csv'
        if os.path.exists(os.path.join(analysis_dir,trajectories_name)) == False:
            trajectories_name = 'trajectoriesDat.csv'

        trajectories = load_sim_data(exp_name,
                                     fname=trajectories_name,
                                     column_list=column_list,
                                     sim_output_path=analysis_dir)

        trajectories = trajectories[(trajectories['date'] >= first_plot_day) & (trajectories['date'] <= last_plot_day)]
        #trajectories['total_hosp_census_EMS-' + str(ems_region)] = trajectories['hosp_det_EMS-' + str(ems_region)] + \
        #                                                          trajectories['crit_det_EMS-' + str(ems_region)]

        trajectories.groupby('capacity_multiplier')['sample_num'].unique()
        #trajectories.groupby('capacity_multiplier')['sample_num'].unique().to_csv(os.path.join(analysis_dir, 'samples_test.csv'), index=True)

        capacity_df_i = capacity_df[capacity_df['region'] == str(ems_region)]
        thresh = int(capacity_df_i[capacity_df_i['resource_type'] == 'icu_availforcovid']['avg_resource_available'])
        #thresh_hosp = int(capacity_df_i[capacity_df_i['resource_type'] == 'hb_availforcovid']['avg_resource_available'])

        capacity_df = capacity_df[capacity_df['resource_type'] == 'icu_availforcovid']

        for capacity in list(trajectories.capacity_multiplier.unique()):
            trajectories_sub = trajectories[trajectories['capacity_multiplier'] == capacity]
            if random_sub is not None:
                unique_scen_sub = random.sample(list(trajectories_sub['scen_num'].values), 100)
                trajectories_sub = trajectories_sub[trajectories_sub['scen_num'].isin(unique_scen_sub)]
            unique_scen = np.array(list(set(trajectories_sub['scen_num'].values)))
            metric_root = 'crit_det_EMS-'
            prob = 0
            for scen in unique_scen:
                new = trajectories_sub[(trajectories_sub['scen_num'] == scen)].reset_index()
                if new[metric_root + str(ems_region)].max() > thresh:
                    prob += 1
            prob_overflow = prob / len(unique_scen)
            df_prob = df_prob.append({'geography_modeled': ems_region,
                                      'icu_availforcovid': thresh,
                                      'capacity_multiplier': capacity,
                                      'prob': prob_overflow,
                                      'n_overflow': prob,
                                      'nscen': len(unique_scen)}, ignore_index=True)

    df_prob.capacity_multiplier.value_counts()
    df_prob.geography_modeled.value_counts()

    if random_sub is not None:
        file_str = f'{file_str}_randsub_{random_sub}_rn{random.sample(range(1,1000), 1)}'
    df_prob['exp_name'] = exp_name
    df_prob.to_csv(os.path.join(analysis_dir, file_str + '.csv'), index=False)

if __name__ == '__main__':

    args = parse_args()  
    stem = args.stem
    Location = args.Location
    
    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)
    sim_output_dir = os.path.join('/projects/p30781/covidproject/covid-chicago/_temp/')
    exp_names = [x for x in os.listdir(sim_output_dir) if stem in x]
    
    for exp_name in exp_names:
        analysis_dir = os.path.join(sim_output_dir, exp_name)
        print("1 - get probs for " + exp_name)
        get_probs(exp_name)
        for i in range(1,50):
            get_probs(exp_name,regions=[1,4,11], random_sub=100)