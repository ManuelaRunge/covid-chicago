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

def get_Ki_dat(first_plot_day = dt.date(2020, 10, 1),  regions = range(1,12)):

    Ki_dat_All = pd.DataFrame()
    for ems_region in regions:
        print(ems_region)
        ki_channel = f'Ki_t_EMS-{str(ems_region)}'
        column_list = ['scen_num','sample_num', 'time', 'startdate','capacity_multiplier',ki_channel]


        trajectories_name = 'trajectoriesDat_region_' + str(ems_region) + '.csv'
        if os.path.exists(os.path.join(analysis_dir, trajectories_name)) == False:
            trajectories_name = 'trajectoriesDat.csv'
        if os.path.exists(os.path.join(analysis_dir, trajectories_name)) == False:
            trajectories_name = 'trajectoriesDat_trim.csv'
        Ki_dat = load_sim_data(exp_name, fname=trajectories_name, column_list=column_list,sim_output_path=analysis_dir)
        Ki_dat = Ki_dat[(Ki_dat['date'] >= first_plot_day)]

        sorted_df = Ki_dat.sort_values('date').groupby(['capacity_multiplier','scen_num','sample_num']).tail(1)
        ## exclude if trigger was not activated
        sorted_df = sorted_df[(sorted_df[ki_channel] == sorted_df[ki_channel].min())]
        sorted_df = sorted_df[['capacity_multiplier', 'scen_num','sample_num', ki_channel]]
        sorted_df = sorted_df.rename(columns={ki_channel: 'Ki_last'})
        df_sub = pd.merge(how='left', left=Ki_dat, left_on=['capacity_multiplier','scen_num','sample_num'],
                          right=sorted_df, right_on=['capacity_multiplier','scen_num','sample_num'])
        df_sub = df_sub[df_sub[ki_channel]==df_sub['Ki_last']]

        df_trigger_date = df_sub.sort_values('date').groupby(['capacity_multiplier','scen_num','sample_num']).head(1)
        df_trigger_date['beforeDec'] = 0
        df_trigger_date.beforeDec[df_trigger_date['date']<=dt.date(2020,12,31)] = 1
        df_trigger_date =df_trigger_date[['scen_num','sample_num','capacity_multiplier','date','time',ki_channel]]
        df_trigger_date = df_trigger_date.rename(columns={'date': 'trigger_date','time': 'trigger_time'})
        #df_trigger_date.to_csv(os.path.join(analysis_dir, f'triggerdate_region_{ems_region}.csv'), index=False)

        Ki_dat['region'] = ems_region
        Ki_dat = Ki_dat.rename(columns={f'Ki_t_EMS-{str(ems_region)}': 'Ki_t'})
        df_trigger_date['region'] = ems_region
        df_trigger_date = df_trigger_date.rename(columns={f'Ki_t_EMS-{str(ems_region)}': 'Ki_trigger'})
        df = pd.merge(how='left', left=Ki_dat, left_on=['capacity_multiplier','scen_num','sample_num','region'],
                          right=df_trigger_date, right_on=['capacity_multiplier','scen_num','sample_num','region'])
        df.to_csv(os.path.join(analysis_dir, f'Ki_dat_region_{ems_region}.csv'), index=False)

        grp_channels = ['time','date','startdate','capacity_multiplier','region','Ki_trigger']
        mdf1 = df.groupby(grp_channels)['trigger_time'].agg([CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75 ]).reset_index()
        mdf2 = df.groupby(grp_channels).agg({"Ki_t": CI_50, "sample_num": pd.Series.nunique}).reset_index()
        mdf1 = custom_rename(mdf1, 'trigger_time')
        mdf = pd.merge(how='left', left=mdf1, left_on=grp_channels,right=mdf2, right_on=grp_channels)

        if Ki_dat_All.empty:
            Ki_dat_All = mdf
        else:
            Ki_dat_All = Ki_dat_All.append(mdf)
    Ki_dat_All.to_csv(os.path.join(analysis_dir, 'Ki_dat_All.csv'), index=False)


if __name__ == '__main__':

    stem =sys.argv[1]
    Location ='NUCLUSTER'
    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)

    sim_output_dir = os.path.join('/projects/p30781/covidproject/covid-chicago/_temp/')
    exp_names = [x for x in os.listdir(sim_output_dir) if stem in x]

    for exp_name in exp_names:
        analysis_dir = os.path.join(sim_output_dir, exp_name)
        print("1 - get Ki for " + exp_name)
        get_Ki_dat(regions=range(1,12))

