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
#from plotting.colors import load_color_palette


mpl.rcParams['pdf.fonttype'] = 42


datapath, projectpath, wdir,exe_dir, git_dir = load_box_paths()


def trim_trajectories(simpath, scenario, colnames, ems) :

    df = pd.read_csv(os.path.join(simpath, 'trajectoriesDat.csv' ))
    df = df.rename(columns={ 'N_EMS_%d' % ems_num : 'N_EMS-%d' % ems_num for ems_num in range(1,12)})
    df['N_All'] = df['susceptible_All'] + df['exposed_All'] + df['infected_All'] + df['recovered_All']
    keep_cols = ['%s_%s' % (x, y) for x in colnames for y in ems]
    df['startdate'] = pd.to_datetime(df['startdate'])
    df['date'] = df.apply(lambda x : x['startdate'] + timedelta(days=x['time']), axis=1)
    df = df[['date'] + keep_cols]
    df.to_csv(os.path.join(simpath, 'trimmed_trajectoriesDat_%s.csv' % scenario), index=False)


if __name__ == '__main__' :


    exp_name = '20201020_IL_mr_baseline'
    simdate = exp_name.split("_")[0]
    ems = ['EMS-%d' % x for x in range(1, 12)] + ['All']
    colnames = ['infected', 'recovered', 'N']

    column_list = ['time','startdate','scen_num', 'infected_All', 'susceptible_All','exposed_All','recovered_All']
    for ems_region in range(1, 12):
        column_list.append('infected_EMS-' + str(ems_region))
        column_list.append('exposed_EMS-' + str(ems_region))
        column_list.append('recovered_EMS-' + str(ems_region))
        column_list.append('susceptible_EMS-' + str(ems_region))

    #simpath = os.path.join(projectdir, 'NU_civis_outputs', simdate,'trajectories')
    simpath = os.path.join(wdir, 'simulation_output', exp_name)
    #trim_trajectories(simpath, scenario, colnames, ems)
    df = pd.read_csv(os.path.join(simpath, 'trajectoriesDat.csv'), usecols=column_list)
    first_day = datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    fig = plt.figure(figsize=(16,8))
    fig.subplots_adjust(left=0.05, right=0.97, top=0.95, bottom=0.05)
    palette =  sns.color_palette('husl', 8)
    formatter = mdates.DateFormatter("%m")

    for e, ems_num in enumerate(ems) :

        df['N_%s' % ems_num] = df['susceptible_%s' % ems_num] + df['exposed_%s' % ems_num] + df['infected_%s' % ems_num] + df['recovered_%s' % ems_num]

        df['prevalence_%s' % ems_num] = df['infected_%s' % ems_num]/df['N_%s' % ems_num]
        df['seroprevalence_%s' % ems_num] = (df['infected_%s' % ems_num] + df['recovered_%s' % ems_num])/ df['N_%s' % ems_num]

        ax = fig.add_subplot(3,4,e+1)

        channels = ['seroprevalence_%s' % ems_num]
        #channels = ['prevalence_%s' % ems_num]
        for k, channel in enumerate(channels) :
            mdf = df.groupby('date')[channel].agg([np.mean, CI_5, CI_95, CI_25, CI_75]).reset_index()
            ax.plot(mdf['date'], mdf['mean'], color=palette[k], label=channel.split('_')[0])
            ax.fill_between(mdf['date'].values, mdf['CI_5'], mdf['CI_95'],
                            color=palette[k], linewidth=0, alpha=0.2)
            ax.fill_between(mdf['date'].values, mdf['CI_25'], mdf['CI_75'],
                            color=palette[k], linewidth=0, alpha=0.4)
        if ems_num == 'EMS-1' :
            ax.legend()
        ax.set_title(ems_num)
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_locator(mdates.MonthLocator())

    plt.savefig(os.path.join(simpath, '_plots', 'seroprevalence_by_ems.png'))
    plt.savefig(os.path.join(simpath, '_plots', 'seroprevalence_by_ems.pdf'), format='PDF')
    plt.show()
