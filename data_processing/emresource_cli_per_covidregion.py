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
today = datetime.today()

first_plot_day = pd.to_datetime( date(2020, 7, 15))
last_plot_day = pd.to_datetime(  date(2020, 10,10))

datapath, projectpath, wdir,exe_dir, git_dir = load_box_paths()
plotdir = os.path.join(projectpath, 'Plots + Graphs', 'Emresource Plots')


def plot_emresource(scale='') :

    ems_regions = {
        'custom' : [1, 2, 5, 10],
    }

    ref_df = pd.read_csv(os.path.join(datapath, 'covid_IDPH', 'Corona virus reports','emresource_by_region.csv'))
    ref_df2  = pd.read_csv(os.path.join(datapath, 'covid_IDPH', 'Corona virus reports','CLI_admissions_by_covidregion.csv'))
    ref_df2['date'] = pd.to_datetime(ref_df2['date'])


    sxmin = '2020-03-24'
    xmin = datetime.strptime(sxmin, '%Y-%m-%d')
    xmax = datetime.today()
    datetoday = xmax.strftime('%y%m%d')

    ref_df['suspected_and_confirmed_covid_icu'] = ref_df['suspected_covid_icu'] + ref_df['confirmed_covid_icu']
    ref_df['date'] = pd.to_datetime(ref_df['date_of_extract'])


    ref_df = pd.merge(how='outer', left=ref_df, left_on=['date','covid_region'], right=ref_df2, right_on=['date','covidregion'])
    ref_df = ref_df.sort_values('date')
    ref_df = ref_df[(ref_df['date'] >= first_plot_day) & (ref_df['date'] <= last_plot_day)]

    ref_df = ref_df.rename(columns={
        'confirmed_covid_deaths_prev_24h' : 'deaths',
        'confirmed_covid_icu' : 'ICU conf',
        'confirmed_covid_on_vents' : 'vents conf',
        'suspected_and_confirmed_covid_icu' : 'ICU conf+susp',
        'covid_non_icu' : 'non ICU'
    })

    channels = ['ICU conf', 'non ICU','inpatient']
    ref_df = ref_df[['date', 'covid_region'] + channels]

    #palette = load_color_palette('wes')
    palette = ("#913058", "#F6851F","#00A08A")  # sns.color_palette('Set1', len(channels))
    formatter = mdates.DateFormatter("%m-%d")

    fig = plt.figure(figsize=(9,10))
    fig.subplots_adjust(left=0.07, right=0.97, top=0.95, bottom=0.05, hspace=0.25)

    def format_plot(ax) :
        ax.set_xlim(xmin, )
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_locator(mdates.MonthLocator())
        if scale == 'log' :
            ax.set_yscale('log')

    for ri, (restore_region, ems_list) in enumerate(ems_regions.items()) :

        for ei, ems in enumerate(ems_list) :
            ax = fig.add_subplot(3,2,ei+1)
            df = ref_df[ref_df['covid_region'] == ems]
            for (c,name) in enumerate(channels):
                df['moving_ave'] = df[name].rolling(window=7, center=True).mean()
                ax.plot(df['date'].values, df['moving_ave'], color=palette[c], label=name)
                ax.scatter(df['date'].values, df[name], s=10, linewidth=0, color=palette[c], alpha=0.7, label='')
            ax.set_title('EMS %d' % ems)
           # format_plot(ax)
            if ems == 10 :
                ax.legend()
                #ax.legend(bbox_to_anchor=(1.5, 1))
            formatter = mdates.DateFormatter("%b")
            ax.xaxis.set_major_formatter(formatter)
            ax.xaxis.set_major_locator(mdates.WeekdayLocator())

    fig.savefig(os.path.join(plotdir, 'EMResource_and_CLI_by_covidregion_for_civis.png'))
    fig.savefig(os.path.join(plotdir, 'EMResource_and_CLI_by_covidregion_for_civis.pdf'), format='PDF')


if __name__ == '__main__' :

    plot_emresource('nolog')
    plot_emresource('log')
    plt.show()
