import numpy as np
import pandas as pd
import os
import sys
import yaml
sys.path.append('../')
from load_paths import load_box_paths
datapath, projectpath, WDIR, EXE_DIR, GIT_DIR = load_box_paths()

snippet_path = os.path.join('experiment_configs/snippets')

def replace_chunk(df, param,chunk_base) :
    """Same param name required in template txt and in csv dataframe that includes the parameter values
    """
    fin = open(os.path.join(snippet_path,'templates', f'{chunk_base}_template.txt'), "rt")
    data_cfg = fin.read()
    all_placeholders  = list(set(re.findall(r'@\w+@', data_cfg)))
    for col in all_placeholders:
        region = int( col.split(param)[1].split('_')[1])
        adf = list(df.loc[df['region']==region,param])[0]
        data_cfg = data_cfg.replace(f'{col}', str(round(adf,5)))
    fin.close()
    fin = open(os.path.join(snippet_path, f'{chunk_base}.txt'), "wt")
    fin.write(data_cfg)
    fin.close()

if __name__ == '__main__':

 """Initial fit """
    exp_name ="20200924_IL__test_initialFit"
    df = pd.read_csv(os.path.join(WDIR, "simulation_output/forFitting",exp_name,"_csv/best_parameters_emsAll.csv"))

    replace_chunk(df =df, param='Ki', chunk_base = "config_locale_Ki_chunk")
    replace_chunk(df =df, param='time_infection_import', chunk_base = "config_locale_time_infection_import_chunk")

"""social multiplier """





