import os
import pandas as pd
import numpy as np
import sys
import seaborn as sns
import matplotlib.pyplot as plt
sys.path.append('../')
from load_paths import load_box_paths
datapath, projectpath, wdir,exe_dir, git_dir = load_box_paths()
from processing_helpers import *

"""Define function methods"""


def load_data(column_list=None, remove_nas=True):
    """Read in only relevant columns """
    if column_list == None:
        column_list =['icu_length',	'hosp_length', 'age_group','res_county','res_state','hosp_yn', 'icu_yn', 'death_yn']
    df_full = pd.read_csv(os.path.join(datapath, 'covid_IDPH', 'Corona virus reports', 'il_cdc_thru_0811.csv'),
                          usecols=column_list)
    #print(df_full)

    """Remove Missings and Unknowns """
    if remove_nas:
        df = df_full.copy()
        df = df.dropna(subset=["hosp_length"])
        df = df.dropna(subset=["age_group"])
        df = df.dropna(subset=["death_yn"])
        df = df[df['age_group'] != 'Unknown' ]
        df = df[df['icu_yn'] != 'Unknown' ]
        df = df[df['icu_yn'] != 'Missing' ]
        #print(df)

    return df


def LOS_descriptive_tables(groupList, channel='hosp_length', sortByList=None):

    df_summary = df.groupby(groupList)['hosp_length'].agg(
        [np.mean, CI_2pt5, CI_25, CI_50, CI_75, CI_97pt5]).reset_index()
    if sortByList != None:
        df_summary = df_summary.sort_values(by=sortByList)
    return df_summary

### Simple histogram, not age structured\
def plot_hist(df=df, channel='hosp_length') :
    plt.rcParams.update({'figure.figsize':(7,5), 'figure.dpi':100})
    x = df[channel]
    plt.hist(x, bins=50)
    plt.gca().set(title=channel, ylabel='Frequency');
    return plt

### Function for age structured plot
def plot_hist_by_grp(df, channel='hosp_length',groups = None, grp_name = None,) :
    ## Get age groups
    if groups == None:
        groups = list(df['age_group'].unique())
    if grp_name == None:
        grp_name = 'age_group'

    palette = sns.color_palette('husl', len(groups))
    fig = plt.figure(figsize=(10, 6))
    axes = [fig.add_subplot(4, 3, x + 1) for x in range(len(groups))]

    for c, grp in enumerate(groups):
        ax = axes[c]
        mdf = df[df[grp_name] == grp]
        ax.hist(mdf[channel], bins=50, color = palette[c])
        ax.set_title(groups[c], y=0.8, fontsize=12)
        #ax.set(xlabel='hosp_length', ylabel='Frequency')
    return plt

if __name__ == '__main__':

    """Basic descriptive tables"""
    df=load_data()
    pd.crosstab(index=df['age_group'], columns='count')

    LOS_descriptive_tables(groupList=['age_group', 'death_yn'])
    LOS_descriptive_tables(groupList=['age_group', 'icu_yn'], sortByList=['icu_yn','age_group'])

    df = df[df['hosp_length'] !=0 ]
    LOS_descriptive_tables(groupList=['age_group', 'death_yn'])
    LOS_descriptive_tables(groupList=['age_group', 'death_yn'], sortByList=['death_yn','age_group'])

    ## Generate plot and save
    plot_hist_by_grp(df)
    plt.savefig(os.path.join(git_dir,   'hosp_length_age_hist.png'))

    """Compare by region"""
    df = load_data()
    df = df.dropna(subset=["res_county"])
    adf = merge_county_covidregions(df_x=df, key_x='res_county', key_y='County')
    adf['region'] = np.int64(adf['new_restore_region'])

    mylist = list(df.res_county.unique())
    mylist.sort(reverse=False)

    mylist2 = list(df_y.County.unique())
    mylist2.sort(reverse=False)

    plot_hist_by_grp(df=adf, grp_name='region', groups=list(range(1,12)))







