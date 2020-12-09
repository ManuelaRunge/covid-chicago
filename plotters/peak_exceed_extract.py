import os
import sys

sys.path.append('../')
from load_paths import load_box_paths
import re
import pandas as pd
import datetime as dt
import numpy as np
import matplotlib as mpl
import random
from processing_helpers import *

def load_sim_data(exp_name,  region_suffix ='_All', sim_output_path=None, fname='trajectoriesDat.csv', column_list=None):
    df = pd.read_csv(os.path.join(sim_output_path, fname), usecols=column_list)
    df = df.dropna()
    df.columns = df.columns.str.replace(region_suffix, '')
    first_day = datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    df['date'] = pd.to_datetime(df['date']).dt.date

    return df

def custom_rename(mdf, suffix):
    for stats in ['CI_50', 'CI_2pt5', 'CI_97pt5', 'CI_25', 'CI_75']:
        mdf = mdf.rename(columns={stats: suffix + '_' + stats})
    return mdf

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

def get_peak_exceed_table(first_plot_day = dt.date(2020, 10, 1), regions= range(1,12)):

    peak_df_All = pd.DataFrame()
    for ems_region in regions:

        #'time', 'startdate',
        column_list = ['date','ems', 'capacity_multiplier', 'critical_median','critical_95CI_lower','critical_95CI_upper','critical_50CI_lower']

        fname = 'trajectories_aggregated_region_' + str(ems_region) + '.csv'
        if os.path.exists(os.path.join(analysis_dir, fname)) == False:
            fname = 'trajectories_aggregated.csv'

        tdf = pd.read_csv(os.path.join(analysis_dir, fname), usecols=column_list)
        tdf['date'] = pd.to_datetime(tdf['date']).dt.date
        tdf = tdf[(tdf['date'] >= first_plot_day)]
        tdf['region'] = tdf['ems'].str.replace("EMS-","")
        tdf = tdf[tdf['region']==str(ems_region)]

        capacity_df = get_capacity()
        df = pd.merge(how='left', left=tdf, left_on=['region'], right=capacity_df, right_on=['region'])
        del tdf
        del capacity_df

        grp_channels = ['region', 'capacity_multiplier', 'avg_resource_available','resource_type','geography_modeled','ems']
        """Peak"""
        peak_df = df[df['critical_median'] == df.groupby(grp_channels)['critical_median'].transform(max)]
        ## Earliest peak in case multiple dates
        #peak_df = peak_df[peak_df['time'] == peak_df.groupby(grp_channels)['time'].transform(min)]
        peak_df = peak_df.sort_values('date').groupby(grp_channels).head(1)
        peak_df = peak_df.rename(columns={'date': f'peak_date'})

        """Duration from 1st Oct to peak"""
        peak_df['Oct_to_peak'] = peak_df['peak_date'] - first_plot_day
        peak_df['Oct_to_peak'] = peak_df['Oct_to_peak'].dt.days

        """Exceed"""
        exceed_df_All = pd.DataFrame()
        for tresh_multiplier in [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]:
            exceed_df = df[df['critical_median'] >= df['avg_resource_available'] * tresh_multiplier]
            exceed_df = exceed_df[['date','region', 'capacity_multiplier', 'avg_resource_available','resource_type','geography_modeled','ems']]
            #exceed_df_from = exceed_df[exceed_df['time'] == exceed_df.groupby(grp_channels)['time'].transform(min)]
            #exceed_df_to = exceed_df[exceed_df['time'] == exceed_df.groupby(grp_channels)['time'].transform(max)]
            exceed_df_from = exceed_df.sort_values('date').groupby(grp_channels).head(1)
            exceed_df_to = exceed_df.sort_values('date').groupby(grp_channels).tail(1)

            exceed_df_from = exceed_df_from.rename(columns={'date': f'exceed_date_from_{tresh_multiplier}',
                                                            'time': f'exceed_time_from_{tresh_multiplier}'})

            exceed_df_to = exceed_df_to.rename(columns={'date': f'exceed_date_to_{tresh_multiplier}',
                                                        'time': f'exceed_time_to_{tresh_multiplier}'})

            del exceed_df
            exceed_df = pd.merge(how='left', left=exceed_df_from, left_on=grp_channels, right=exceed_df_to, right_on=grp_channels)

            """Duration"""
            exceed_df[f'exceed_diff_{tresh_multiplier}'] = exceed_df[f'exceed_date_to_{tresh_multiplier}'] - \
                                                            exceed_df[f'exceed_date_from_{tresh_multiplier}']
            if not exceed_df.empty :
                exceed_df[f'exceed_diff_{tresh_multiplier}'] = exceed_df[f'exceed_diff_{tresh_multiplier}'].dt.days

            if exceed_df_All.empty:
                exceed_df_All = exceed_df
            else:
                exceed_df_All = pd.merge(how='left', left=exceed_df_All, left_on=grp_channels, right=exceed_df, right_on=grp_channels)

        peak_exceed_df = pd.merge(how='left', left=peak_df, left_on=grp_channels, right=exceed_df_All,right_on=grp_channels)

        if peak_df_All.empty:
            peak_df_All = peak_exceed_df
        else:
            peak_df_All = peak_df_All.append(peak_exceed_df)
    peak_df_All.to_csv(os.path.join(analysis_dir, 'peak_exceed_df.csv'), index=False)


def get_time_since_trigger():

    df = pd.read_csv(os.path.join(analysis_dir, 'peak_exceed_df.csv'))
    df['peak_date'] = pd.to_datetime(df['peak_date'])

    trigger_df = pd.read_csv(os.path.join(analysis_dir, 'Ki_dat_All.csv'))
    first_day = datetime.strptime(trigger_df['startdate'].unique()[0], '%Y-%m-%d')
    trigger_df['trigger_date_CI_50'] = trigger_df['trigger_time_CI_50'].apply(lambda x: first_day + timedelta(days=int(x)))
    trigger_df = trigger_df[['region','startdate','capacity_multiplier','Ki_trigger','trigger_date_CI_50','trigger_time_CI_50','sample_num']]

    adf = pd.merge(how='left', left=df, left_on=['region','capacity_multiplier'], right=trigger_df,
                         right_on=['region','capacity_multiplier'])

    adf['trigger_to_peak'] = adf['peak_date'] - adf['trigger_date_CI_50']
    adf['trigger_to_peak'] = adf['trigger_to_peak'].dt.days

    for tresh_multiplier in [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]:
        adf[f'exceed_date_to_{tresh_multiplier}'] = pd.to_datetime(adf[f'exceed_date_to_{tresh_multiplier}'])
        adf[f'trigger_to_exceed_{tresh_multiplier}'] = adf[f'exceed_date_to_{tresh_multiplier}'] - adf['trigger_date_CI_50']
        adf[f'trigger_to_exceed_{tresh_multiplier}'] = adf[f'trigger_to_exceed_{tresh_multiplier}'].dt.days

    adf.to_csv(os.path.join(analysis_dir, 'trigger_peak_exceed_df.csv'), index=False)


if __name__ == '__main__':

    stem = sys.argv[1]
    Location ='NUCLUSTER'
    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)

    sim_output_dir =  os.path.join('/projects/p30781/covidproject/covid-chicago/_temp/')
    exp_names = [x for x in os.listdir(sim_output_dir) if stem in x]

    for exp_name in exp_names:
        analysis_dir = os.path.join(sim_output_dir, exp_name)
        print("get peak, exceed dates for " + exp_name)
        get_peak_exceed_table(regions=range(1,12))
        get_time_since_trigger()