import numpy as np
import pandas as pd
import subprocess
import matplotlib.pyplot as plt
import os
import seaborn as sns
import matplotlib as mpl
import matplotlib.dates as mdates
from datetime import date, timedelta
import shutil

import numpy as np


def writeTxt(txtdir, filename, textstring):
    file = open(os.path.join(txtdir, filename), 'w')
    file.write(textstring)
    file.close()

def reprocess(input_fname='trajectories.csv', output_fname=None):
    fname = os.path.join(git_dir, input_fname)
    row_df = pd.read_csv(fname, skiprows=1)
    df = row_df.set_index('sampletimes').transpose()
    run_time = len([x for x in df.columns.values if '{0}' in x])
    num_runs = int((len(row_df)) / run_time)
    df = df.reset_index(drop=False)
    df = df.rename(columns={'index': 'time'})
    df['time'] = df['time'].astype(float)
    adf = pd.DataFrame()
    for run_num in range(num_runs):
        channels = [x for x in df.columns.values if '{%d}' % run_num in x]
        sdf = df[['time'] + channels]
        sdf = sdf.rename(columns={
            x: x.split('{')[0] for x in channels
        })
        sdf['run_num'] = run_num
        adf = pd.concat([adf, sdf])
    adf = adf.reset_index()
    del adf['index']
    return adf


def combineTrajectories(VarsToKeep,Nscenarios_start=0, Nscenarios_stop=1000, time_start=1, time_stop=400, fname='observedparamDat.csv',grpnames=None,grpspecific_params=None):

    if grpnames == None:
        grpnames = ['All', 'EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10', 'EMS-11']
        grpnames_ki = ['EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10','EMS-11']

    if grpspecific_params == None:
        grpspecific_params = ['Ki_t','triggertime','d_Sym_t']  # ['Ki_t', 'triggertime','reopening_multiplier_4']

    column_list = VarsToKeep #+ ['cfr_t' ,'fraction_dead_t','fraction_hospitalized_t','frac_crit_t','d_Sys_t']

    for grpspecific_param in grpspecific_params:
        for grp in grpnames_ki:
            column_list.append(grpspecific_param + "_" + str(grp))

    df_list = []
    n_errors = 0
    for scen_i in range(Nscenarios_start, Nscenarios_stop):
        input_name = "trajectories_scen" + str(scen_i) + ".csv"
        try:
            df_i = reprocess(os.path.join(trajectoriesDat, input_name))
            df_i['scen_num'] = scen_i
            # print("df_length " + str(len(df_i)))
            df_i = df_i.merge(sampledf, on=['scen_num'])
            # print("df_length " + str(len(df_i)))
            df_i = df_i[df_i['time'] > time_start]
            df_i = df_i[df_i['time'] < time_stop]
            df_list.append(df_i)
        except:
            n_errors += 1
            continue
    print("Number of errors:" + str(n_errors))
    dfc = pd.concat(df_list)
    dfc = dfc.dropna()
    dfc = dfc[dfc['time'] > time_start]
    dfc = dfc[dfc['time'] < time_stop]
    dfc = dfc[column_list]
    dfc.to_csv(os.path.join(temp_exp_dir, fname), index=False, date_format='%Y-%m-%d')


if __name__ == '__main__':
    sim_out_dir = "/projects/p30781/covidproject/covid-chicago/_temp/"

    stem = '20200919_IL_regreopen100perc_0daysdelay_sm4'
    exp_names = [x for x in os.listdir(sim_out_dir) if stem in x]

    time_start = 250
    time_end = 380
    additionalVars = ['capacity_multiplier','trigger_delay_days']
    VarsToKeepI = ['startdate',  'scen_num', 'sample_num'] + additionalVars
    VarsToKeep = ['time', 'run_num'] + VarsToKeepI


    for exp_name in exp_names:
        print(exp_name)

        git_dir = os.path.join(sim_out_dir, exp_name)
        trajectoriesDat = os.path.join(git_dir, 'trajectories')
        temp_exp_dir = git_dir
        sampledf = pd.read_csv(os.path.join(temp_exp_dir, "sampled_parameters.csv"))
        sampledf = sampledf[VarsToKeepI]
        Nscenario = max(sampledf['scen_num'])

        Scenario_save_limit = 700

        if Nscenario <= Scenario_save_limit:
            combineTrajectories(VarsToKeep=VarsToKeep,Nscenarios_start=0, Nscenarios_stop=Nscenario+1,time_start=time_start, time_stop=time_end, fname='triggerDate.csv')
        if Nscenario > Scenario_save_limit:
            n_subsets = int(Nscenario/Scenario_save_limit)

            for i in range(1,n_subsets+2):
                if i ==1 : Nscenario_stop=Scenario_save_limit
                if i > 1 : Nscenario_stop = Nscenario_stop + Scenario_save_limit
                print(Nscenario_stop)
                Nscenarios_start = Nscenario_stop-Scenario_save_limit
                combineTrajectories(VarsToKeep=VarsToKeep,
                                    Nscenarios_start=Nscenarios_start,
                                    Nscenarios_stop=Nscenario_stop,
                                    fname='triggerDate_'+str(Nscenario_stop)+'.csv')