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

mpl.rcParams['pdf.fonttype'] = 42
testMode = True

datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths()

def calculate_mean_and_CI(adf, channel, output_filename=None) :

    mdf = adf.groupby('time')[channel].agg([np.mean, CI_5, CI_95, CI_25, CI_75]).reset_index()
    if output_filename :
        mdf.to_csv(os.path.join(sim_output_path, output_filename), index=False)

def barplot(sim_output_path, ems) :
    df = pd.read_csv(os.path.join(sim_output_path, 'trajectoriesDat.csv'))

    suffix_names = [x.split('_')[1] for x in df.columns.values if 'susceptible' in x]
    base_names = [x.split('_%s' % suffix_names[0])[0] for x in df.columns.values if suffix_names[0] in x]

    first_day = date(2020, 2, 13)  # datetime.strptime(df['startdate'].unique()[0], '%Y-%m-%d')
    df['date'] = df['time'].apply(lambda x: first_day + timedelta(days=int(x)))
    plot_first_day = date(2020, 3, 1)
    plot_last_day = date(2020, 10, 1)
    df = df[(df['date'] == plot_last_day)]

    df.columns = df.columns.str.replace("_All", "_ageAll")
    df["infected_age0to19"] = df["infected_age0to9"] + df["infected_age10to19"]
    df2 = pd.melt(df, id_vars=['time', 'date'], var_name='metrics', value_name='values')
    df3 = df2["metrics"].str.split("_age", n=1, expand=True)
    df2["metrics"] = df3[0]
    df2["age"] = df3[1]
    df2 = df2[(df2['age'] != "All")]
    df2 = df2[(df2['age'] != "0to9")]
    df2 = df2[(df2['age'] != "10to19")]

    channels = ['infected', 'deaths', 'critical', 'hospitalized', 'symptomatic_mild', 'symptomatic_severe']
    # df['symptomatic_census'] = df['symptomatic_mild'] + df['symptomatic_severe']
    palette = sns.color_palette('Set2', len(channels))

    # fig = plt.figure(figsize=(12, 8))
    fig = plt.figure(figsize=(10, 8))
    fig.subplots_adjust(right=0.97, wspace=0.2, left=0.1, hspace=0.3, top=0.95, bottom=0.07)
    axes = [fig.add_subplot(3, 2, x + 1) for x in range(len(channels))]
    ax = axes[0]

    for c, channel in enumerate(channels):
        ax = axes[c]

        mdf = df2[(df2['metrics'] == channel)]
        mdf = mdf.groupby('age')['values'].agg(CI_50).reset_index()
        ax.bar(mdf['age'], mdf['values'], color=palette[c])
        ax.set_title(channel, y=1)
        # ax.set_ylim(0, max(mdf['values']))

    fig.tight_layout()
    fig.savefig(os.path.join(sim_output_path, 'age_barchart_covidregion_%s.png' % ems))
    fig.savefig(os.path.join(sim_output_path, 'age_barchart_covidregion_%s.png' % ems))
    # plt.show()

if __name__ == '__main__' :

    #stem = sys.argv[1]
    stem = "extendedmodel_age8"
    exp_names = [x for x in os.listdir(os.path.join(wdir, 'simulation_output')) if stem in x]
    #exp_name = '20200825_EMS_9_extendedmodel_age8'
    plot_last_day =  datetime.today().strftime('%Y%m%d') # "20200804"

    for exp_name in exp_names:
        region = exp_name.split('_')[2]
        ems_nr = exp_name.split('_')[2]
        region = exp_name.split('_')[1]

        barplot(sim_output_path = os.path.join(wdir, 'simulation_output', exp_name), ems=ems_nr)







