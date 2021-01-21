import argparse
import numpy as np
import pandas as pd
import os
import sys
sys.path.append('../')
from load_paths import load_box_paths

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
        default = "Local"
    )
    parser.add_argument(
        "--time_start",
        type=int,
        help="Lower limit of time steps to keep",
        default=1
    )
    parser.add_argument(
        "--time_stop",
        type=int,
        help="Upper limit of time steps to keep",
        default=1000
    )
    parser.add_argument(
        "-limit",
        "--scen_limit",
        type=int,
        help="Number of simulations to combine",
        default = 700
    )
    parser.add_argument(
        "--additional_sample_param",
        type=str,
        nargs='+',
        help="Number of simulations to combine",
        default = ['']
    )


    return parser.parse_args()
    
    
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

def trim_trajectories_Dat(df, fname,sample_param_to_keep, time_start=1, time_stop=1000,
                          channels=None, grpspecific_params=None, grpnames=None):
    """Generate a subset of the trajectoriesDat dataframe
    The new csv file is saved under trajectoriesDat_trim.csv, no dataframe is returned
    """

    if grpnames == None:
        grpnames = ['All', 'EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10', 'EMS-11']
        grpnames_ki = ['EMS-1', 'EMS-2', 'EMS-3', 'EMS-4', 'EMS-5', 'EMS-6', 'EMS-7', 'EMS-8', 'EMS-9', 'EMS-10','EMS-11']

    if channels == None:
        channels = ['susceptible', 'infected', 'recovered', 'infected_cumul', 'detected_cumul',
                    'asymp_cumul', 'asymp_det_cumul',
                    'symp_mild_cumul', 'symptomatic_mild', 'symp_mild_det_cumul',
                    'symp_severe_cumul','symptomatic_severe', 'symp_severe_det_cumul',
                    'hosp_det_cumul', 'hosp_cumul', 'hosp_det', 'hospitalized',
                    'crit_cumul','crit_det_cumul', 'crit_det',  'critical',
                    'death_det_cumul', 'deaths']

    if grpspecific_params == None:
        grpspecific_params = ['Ki_t']

    column_list = ['time', 'run_num'] + sample_param_to_keep
    for channel in channels:
        for grp in grpnames:
            column_list.append(channel + "_" + str(grp))

    for grpspecific_param in grpspecific_params:
        for grp in grpnames_ki:
            column_list.append(grpspecific_param + "_" + str(grp))

    """Trim df and save"""
    df = df[column_list]
    df = df[df['time'] > time_start]
    df = df[df['time'] < time_stop]
    df.to_csv(os.path.join(exp_dir, fname + '_trim.csv'), index=False, date_format='%Y-%m-%d')


def combineTrajectories(sampledf, Nscenarios_start=0, Nscenarios_stop=1000, fname='trajectoriesDat.csv',SAVE=True):

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
    if SAVE:
        dfc.to_csv(os.path.join(exp_dir, fname), index=False, date_format='%Y-%m-%d')

    return dfc


if __name__ == '__main__':

    args = parse_args()  
    stem = args.stem
    time_start = args.time_start
    time_stop = args.time_stop
    Location = args.Location
    additional_sample_param = args.additional_sample_param
    Scenario_save_limit = args.scen_limit

    """Define parameters to keep"""
    sample_param_to_keep = ['startdate', 'scen_num', 'sample_num'] + ['N_EMS_%d' % x for x in range(1, 12)]
    sample_param_to_keep = sample_param_to_keep + additional_sample_param

    datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths(Location=Location)
    sim_out_dir = os.path.join(wdir, "simulation_output")
    if Location == "NUCLUSTER":
        sim_out_dir = os.path.join(git_dir, "_temp")
    exp_names = [x for x in os.listdir(sim_out_dir) if stem in x]

    for exp_name in exp_names:
        print(exp_name)
        exp_dir = os.path.join(sim_out_dir, exp_name)
        trajectoriesDat = os.path.join(exp_dir, 'trajectories')

        sampledf = pd.read_csv(os.path.join(exp_dir, "sampled_parameters.csv"), usecols= sample_param_to_keep)
        Nscenario = max(sampledf['scen_num'])

        if Nscenario <= Scenario_save_limit:
            fname = "trajectoriesDat.csv"
            if not os.path.exists(os.path.join(exp_dir, fname)):
                dfc = combineTrajectories(sampledf=sampledf,
                                          Nscenarios_start=0,
                                          Nscenarios_stop=Nscenario + 1,
                                          fname=fname)
            else:
                dfc = pd.read_csv(os.path.join(exp_dir, fname))

            trim_trajectories_Dat(df=dfc,
                                  sample_param_to_keep = sample_param_to_keep,
                                  time_start=time_start,
                                  time_stop=time_stop,
                                  fname=fname.split(".csv")[0])
        if Nscenario > Scenario_save_limit:
            n_subsets = int(Nscenario/Scenario_save_limit)

            for i in range(1,n_subsets+2):
                if i ==1 : Nscenario_stop=Scenario_save_limit
                if i > 1 : Nscenario_stop = Nscenario_stop + Scenario_save_limit
                print(Nscenario_stop)
                Nscenarios_start = Nscenario_stop-Scenario_save_limit
                fname = 'trajectoriesDat_'+str(Nscenario_stop)+'.csv'
                if not os.path.exists(os.path.join(exp_dir, fname)):
                    dfc = combineTrajectories(sampledf=sampledf,
                                              Nscenarios_start=Nscenarios_start,
                                              Nscenarios_stop=Nscenario_stop,
                                              fname=fname)
                else:
                    dfc = pd.read_csv(os.path.join(exp_dir, fname))

                trim_trajectories_Dat(df=dfc,
                                      sample_param_to_keep=sample_param_to_keep,
                                      time_start=time_start,
                                      time_stop=time_stop,
                                      fname=fname.split(".csv")[0])