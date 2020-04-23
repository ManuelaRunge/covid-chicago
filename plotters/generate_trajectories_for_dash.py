import os
import pandas as pd
import sys
sys.path.append('../')
from load_paths import load_box_paths
from datetime import date, timedelta, datetime
from data_comparison import load_sim_data

datapath, projectpath, wdir,exe_dir, git_dir = load_box_paths()

if __name__ == '__main__' :

    stem = 'scenario1'
    exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output')) if stem in x]

    adf = pd.DataFrame()
    for d, exp_name in enumerate(exp_names):
        sim_output_path = os.path.join(wdir, 'simulation_output', exp_name)
        ems = int(exp_name.split('_')[2])
        df = load_sim_data(exp_name)

        first_day = datetime.strptime(df['first_day'].unique()[0], '%Y-%m-%d')

        df['ems'] = ems
        df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
        adf = pd.concat([adf, df])

    filename = 'dash_EMS_trajectories_separate_endsip_20200419.csv'
    print(adf.columns.values)

    keep = ['date', 'ems']
    output_channels = ['new_detected_hospitalized', 'new_detected_critical', 'new_detected_deaths', 'new_deaths',
                       'infected']
    params_of_interest = ['d_Sym', 'd_Sys',
                          'fraction_symptomatic',
                          'fraction_severe',
                          'cfr',
                          'reduced_inf_of_det_cases',
                          'recovery_rate_hosp', 'recovery_rate_crit',
                          'social_multiplier_3']

    adf = adf[keep + output_channels + params_of_interest]
    adf = adf.rename(columns={ x : 'param_%s' % x for x in params_of_interest})
    adf = adf.rename(columns={ x : 'output_%s' % x for x in output_channels})
    adf.to_csv(os.path.join(wdir,'simulation_output/_csv', filename),
               index=False)