import logging
import os
import subprocess
import shutil
import stat
import sys
from datetime import timedelta

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns

from processing_helpers import CI_5, CI_25, CI_75, CI_95

### GE added 04/10/20 to fix "wdir not defined error"
#import sys
#sys.path.append("C:\\Users\\garrett\\Documents\\GitHub\\covid-chicago") #added for the loadpaths for garrett
from load_paths import load_box_paths
datapath, projectpath, WDIR, EXE_DIR, GIT_DIR = load_box_paths()

log = logging.getLogger(__name__)


def DateToTimestep(date, startdate) :
    datediff = date - startdate
    timestep = datediff.days
    return timestep


def TimestepToDate(timesteps, startdate) :
    dates= startdate + timedelta(days=timesteps)
    return dates


def get_cms_cmd(exe_dir=EXE_DIR, workdir=None, docker_image=None):
    """Generate the command to invoke the CMS executable

    Cross-platform -- generate the appropriate command on Windows, OS X, or Linux.
    On OS X or Linux, run either via a Docker container which contains CMS,
    or with wine.

    Parameters
    ----------
    exe_dir : str, optional
        The directory of your `compartments.exe` executable.
        Not needed if running via Docker.
    workdir : str, optional
        Only needed for non-Windows systems. The working directory, which
        must contain the config file, the emodl file, and the output location.
    docker_image : str, optional
        Only needed for non-Windows systems.
        If provided, generate a run command which invokes this Docker image.
        If not provided, generate a run command which invokes `wine` to run
        `compartments.exe`.

    Returns
    -------
    cmd : str
        A string which can be executed to run CMS
    """

    if sys.platform in ['win32', 'cygwin']:
        log.debug("Generating Windows run command")
        return os.path.join(exe_dir, 'compartments.exe')
    else:
        if docker_image:
            log.debug(f"Generating Docker run command for workdir {workdir}")
            if not workdir:
                raise TypeError("Must provide `workdir` input for running via Docker")
            cmd = f"docker run -v={workdir}:{workdir} {docker_image} -d {workdir}"
        else:
            cmd = f"wine {os.path.join(exe_dir, 'compartments.exe')} -d {workdir}"
        return cmd


def runExp(trajectories_dir, Location = 'Local' ):
    if Location =='Local' :
        log.info("Starting experiment.")
        p = os.path.join(trajectories_dir,  'runSimulations.bat')
        subprocess.call([p])
    if Location =='NUCLUSTER' :
        p = os.path.join(trajectories_dir, 'submit_runSimulations.sh')
        subprocess.call(['sh',p])


def reprocess(trajectories_dir, temp_exp_dir, input_fname='trajectories.csv', output_fname=None):
    fname = os.path.join(trajectories_dir, input_fname)
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
    if output_fname:
        adf.to_csv(os.path.join(temp_exp_dir,output_fname), index=False)
    return adf


def combineTrajectories(Nscenarios,trajectories_dir, temp_exp_dir, deleteFiles=False,addSamples = True, git_dir=GIT_DIR):
    sampledf = pd.read_csv(os.path.join(temp_exp_dir,"sampled_parameters.csv"))
    if addSamples == False:
        sampledf = sampledf[["scen_num","sample_num","startdate"]]
    df_list = []
    for scen_i in range(Nscenarios+1):
        input_name = "trajectories_scen" + str(scen_i) + ".csv"
        try:
            df_i = reprocess(trajectories_dir=trajectories_dir, temp_exp_dir=temp_exp_dir, input_fname=input_name)
            df_i['scen_num'] = scen_i
            df_i = df_i.merge(sampledf, on=['scen_num'])
            df_list.append(df_i)
        except:
            continue

        if deleteFiles == True: os.remove(os.path.join(git_dir, input_name))

    dfc = pd.concat(df_list)
    dfc.to_csv( os.path.join(temp_exp_dir,"trajectoriesDat.csv"), index=False)

    nscenarios = sampledf['scen_num'].max()
    nscenarios_processed = len(dfc['scen_num'].unique())
    trackScen = "Number of scenarios processed n= " + str(nscenarios_processed) + " out of total N= " + str(nscenarios) + " (" + str(nscenarios_processed/ nscenarios)+ " %)"
    writeTxt(temp_exp_dir, "Simulation_report.txt" ,trackScen)

    return dfc


def cleanup(temp_dir, temp_exp_dir, sim_output_path,plot_path, delete_temp_dir=True) :
    # Delete simulation model and emodl files
    # But keeps per default the trajectories, better solution, zip folders and copy
    if delete_temp_dir:
        shutil.rmtree(temp_dir, ignore_errors=True)
        print('temp_dir folder deleted')
    shutil.copytree(temp_exp_dir, sim_output_path)
    if not os.path.exists(plot_path):
        os.makedirs(plot_path)
    # Delete files after being copied to the project folder
    if os.path.exists(sim_output_path):
        shutil.rmtree(temp_exp_dir, ignore_errors=True)
    elif not os.path.exists(sim_output_path):
        print('Sim_output_path does not exists')


def writeTxt(txtdir, filename, textstring) :
    file = open(os.path.join(txtdir, filename), 'w')
    file.write(textstring)
    file.close()


def generateSubmissionFile(scen_num, exp_name, experiment_config, trajectories_dir, temp_dir, temp_exp_dir,
                           exe_dir=EXE_DIR, docker_image="cms", git_dir=GIT_DIR):
    fname = 'runSimulations.bat'
    log.debug(f"Generating submission file {fname}")
    if sys.platform not in ["win32", "cygwin"]:
        file = open(os.path.join(trajectories_dir, fname), 'w')
        # If this is OSX or Linux, mark the file as executable and
        # write a bash script
        os.chmod(fname, stat.S_IXUSR | stat.S_IWUSR | stat.S_IRUSR)
        cfg_fname = os.path.join(temp_dir, 'model_$i.cfg')
        emodl_fname = os.path.join(temp_dir, 'simulation_$i.emodl')
        file.write(f"""#!/bin/bash
echo start
for i in {{1..{scen_num}}} 
  do
    {get_cms_cmd(exe_dir, temp_exp_dir, docker_image)} -c "{cfg_fname}" -m "{emodl_fname}"
  done
echo end""")
    else:
        file = open(os.path.join(trajectories_dir, 'runSimulations.bat'), 'w')
        file.write('ECHO start' + '\n' + 'FOR /L %%i IN (1,1,{}) DO ( "{}" -c "{}" -m "{}") >> "{}/log/log.txt"'.format(
            str(scen_num),
            get_cms_cmd(exe_dir, temp_exp_dir),
            os.path.join(temp_dir, "model_%%i" + ".cfg"),
            os.path.join(temp_dir, "simulation_%%i" + ".emodl"),
            os.path.join(temp_exp_dir)
        ) + "\n ECHO end")

        runParallel = False
        if runParallel :
            ## FIXME use parallel processing for local run,
            #    loop runs simulation in batches of 5 but breaks if single simulation returns error
            #    combine trajectories should only start after all trajectories were generated
            file = open(os.path.join(trajectories_dir, 'runSimulations_loop.bat'), 'w')
            file.write('ECHO Run simulation number %x% to  %y%' + '\n' +
                       '\nFOR /L %%j IN (%x%,1,%y%) DO ( "{}" -c "{}" -m "{}") >> "{}/log/log_%x%_to_%y%.txt"'.format(
                get_cms_cmd(exe_dir, temp_exp_dir),
                os.path.join(temp_dir, "model_%%j" + ".cfg"),
                os.path.join(temp_dir, "simulation_%%j" + ".emodl"),
                os.path.join(trajectories_dir)
            ) + "\n ECHO end")

            file = open(os.path.join(trajectories_dir, 'runSimulations_parallel.bat'), 'w')
            file.write('@echo off' \
                        '\ncd {dir}\nfor /l %%i in (1,5,{nscen}]) do call :loop %%i' \
                        '\ngoto :eof' \
                        '\n\n:loop \ncall :checkinstances \nif %INSTANCES% LSS 5 (' \
                        '\n    echo Starting processing instance for %1' \
                        '\n    @set x=%1' \
                        '\n    @set step=%1+5' \
                        '\n    @set /a y=%x%+%step%' \
                        '\n    copy runSimulations_loop.bat runSimulations_loop%1.bat' \
                        '\n    start /min runSimulations_loop%1.bat 2 sec' \
                        '\n    goto :eof' \
                        '\n) \necho Waiting for instances to close ...\nping -n 2 ::1 >nul 2>&1\ngoto loop\ngoto :eof' \
                        '\n:checkinstances\nfor /f "usebackq" %%t in (`tasklist /fo csv /fi "imagename eq wait.bat"^|find /c /v ""`) do set INSTANCES=%%t' \
                        '\ngoto :eof' \
                        '\npause'.format(nscen=str(scen_num), dir=trajectories_dir))

        ## runDataComparison
        if experiment_config == "spatial_EMS_experiment.yaml" :
            fname = "data_comparison_spatial.py"
        if experiment_config != "spatial_EMS_experiment.yaml" :
            fname = "data_comparison.py"
        file = open(os.path.join(temp_exp_dir, 'runDataComparison.bat'), 'w')
        file.write('cd {} \n python {} "{}"\npause'.format(
            os.path.join(git_dir, "plotters"),
            fname,exp_name
        ))

        if experiment_config == "spatial_EMS_experiment.yaml" :
            file = open(os.path.join(temp_exp_dir, 'runTrimTrajectories.bat'), 'w')
            file.write('cd {} \n python trim_trajectoriesDat.py "{}" "{}" "{}" \npause'.format(
                os.path.join(git_dir, "plotters"),
                exp_name,'120','15',
            ))

            ## runProcessForCivis
            file = open(os.path.join(temp_exp_dir, 'runProcessForCivis.bat'), 'w')
            file.write('cd {} \n python process_for_civis_EMSgrp.py "{}" "{}" \npause'.format(
                os.path.join(git_dir, "plotters"),
                exp_name, "generate_outputs"
            ))

            ## runOverflow_probabilities
            file = open(os.path.join(temp_exp_dir, 'runOverflow_probabilities.bat'), 'w')
            file.write('cd {} \n python overflow_probabilities.py "{}" \npause'.format(
                os.path.join(git_dir, "plotters"),
                exp_name
            ))

        if experiment_config != "EMSspecific_sample_parameters.yaml" :
            ## locale_age_postprocessing
            file = open(os.path.join(temp_exp_dir, 'locale_age_postprocessing.bat'), 'w')
            file.write('cd {} \n python locale_age_postprocessing.py "{}"  \npause'.format(
                os.path.join(git_dir, "plotters"),
                exp_name
            ))


def generateSubmissionFile_quest(scen_num, exp_name, experiment_config, trajectories_dir,  temp_exp_dir) :
        # Generic shell submission script that should run for all having access to  projects/p30781
        # submit_runSimulations.sh
        exp_name_short = exp_name[-20:]
        header = '#!/bin/bash\n' \
                 '#SBATCH -A p30781\n' \
                 '#SBATCH -p short\n' \
                 '#SBATCH -t 02:00:00\n' \
                 '#SBATCH -N 5\n' \
                 '#SBATCH --ntasks-per-node=1\n' \
                 '#SBATCH --mem=18G'
        jobname = '\n#SBATCH	--job-name="' + exp_name_short + '"'
        array = '\n#SBATCH --array=1-' + str(scen_num)
        err = '\n#SBATCH --error=log/arrayJob_%A_%a.err'
        out = '\n#SBATCH --output=log/arrayJob_%A_%a.out'
        module = '\n\nmodule load singularity'
        singularity = '\n\nsingularity exec -B /projects:/projects/ /software/singularity/images/singwine-v1.img wine /projects/p30781/covidproject/binaries/compartments/compartments.exe  -c /projects/p30781/covidproject/covid-chicago/_temp/' + exp_name + '/simulations/model_${SLURM_ARRAY_TASK_ID}.cfg  -m /projects/p30781/covidproject/covid-chicago/_temp/' + exp_name + '/simulations/simulation_${SLURM_ARRAY_TASK_ID}.emodl'
        file = open(os.path.join(trajectories_dir, 'runSimulations.sh'), 'w')
        file.write(header + jobname + array + err + out + module + singularity)
        file.close()

        pymodule = '\n\nml python'
        pycommand = '\npython /projects/p30781/covidproject/covid-chicago/nucluster/combine.py --stem "' +exp_name+ '"' + ' --addsamples "True"' + ' --lagtime_days "15"'
        file = open(os.path.join(temp_exp_dir, 'combineSimulations.sh'), 'w')
        file.write(header + jobname + err + out + pymodule + pycommand)
        file.close()

        pymodule = '\n\nml python'
        pycommand = '\npython /projects/p30781/covidproject/covid-chicago/nucluster/cleanup.py --stem "' +exp_name+ '"' + ' --delete_simsfiles "True"'
        file = open(os.path.join(temp_exp_dir, 'cleanupSimulations.sh'), 'w')
        file.write(header + jobname + err + out + pymodule + pycommand)
        file.close()

        if experiment_config == "spatial_EMS_experiment.yaml" :
            fname = "data_comparison_spatial.py"
        if experiment_config != "spatial_EMS_experiment.yaml" :
            fname = "data_comparison.py"
        pymodule = '\n\nml python'
        pycommand = '\npython /projects/p30781/covidproject/covid-chicago/plotters/'+fname+' --stem "' +exp_name+ '"' + ' --Location "NUCLUSTER"'
        file = open(os.path.join(temp_exp_dir, 'compareToData.sh'), 'w')
        file.write(header + jobname + err + out + pymodule + pycommand)
        file.close()

        pymodule = '\n\nml python'
        pycommand = '\npython /projects/p30781/covidproject/covid-chicago/plotters/process_for_civis_EMSgrp.py --stem "' +exp_name+ '"' + ' --Location "NUCLUSTER"'
        file = open(os.path.join(temp_exp_dir, 'processForCivis.sh'), 'w')
        file.write(header + jobname + err + out + pymodule + pycommand)
        file.close()

        submit_runSimulations = 'cd /projects/p30781/covidproject/covid-chicago/_temp/' + exp_name + '/trajectories/\ndos2unix runSimulations.sh\nsbatch runSimulations.sh'
        file = open(os.path.join(temp_exp_dir, 'submit_runSimulations.sh'), 'w')
        file.write(submit_runSimulations)
        file.write('\n\n#cd ' + temp_exp_dir)
        file.write('\n\n#Submit after simulation are finished using job id\n#sbatch --dependency=afterok:<jobid> combineSimulations.sh')
        file.write('\n\n#Submit after combineSimulations using job id\n#sbatch --dependency=afterok:<jobid> cleanupSimulations.sh')
        file.write('\n\n#Submit after cleanupSimulations using job id\n#sbatch --dependency=afterok:<jobid> compareToData.sh')
        file.write('\n\n#Submit after cleanupSimulations using job id\n#sbatch --dependency=afterok:<jobid> processForCivis.sh')
        file.close()


def makeExperimentFolder(exp_name, emodl_dir, emodlname, cfg_dir, cfg_file, yaml_dir, DEFAULT_CONFIG, experiment_config, temp_exp_dir=None,
                         wdir=WDIR, git_dir=GIT_DIR): ## GE 04/10/20 added exp_name, emodl_dir,emodlname, cfg_dir here to fix exp_name not defined error
    sim_output_path = os.path.join(wdir, 'simulation_output', exp_name)
    plot_path = sim_output_path
    # Create temporary folder for the simulation files
    # currently allowing to run only 1 experiment at a time locally
    if temp_exp_dir == None :
        temp_exp_dir = os.path.join(git_dir, '_temp', exp_name)
    temp_dir = os.path.join(temp_exp_dir, 'simulations')
    trajectories_dir = os.path.join(temp_exp_dir, 'trajectories')
    if not os.path.exists(os.path.join(git_dir, '_temp')):
        os.makedirs(os.path.join(os.path.join(git_dir, '_temp')))
    if not os.path.exists(temp_exp_dir):
        os.makedirs(temp_exp_dir)
        os.makedirs(temp_dir)
        os.makedirs(trajectories_dir)
        os.makedirs(os.path.join(temp_exp_dir, 'log'))
        os.makedirs(os.path.join(trajectories_dir, 'log'))  # location of log file on quest

    ## Copy emodl and cfg file  to experiment folder
    shutil.copyfile(os.path.join(emodl_dir, emodlname), os.path.join(temp_exp_dir, emodlname))
    shutil.copyfile(os.path.join(cfg_dir, cfg_file), os.path.join(temp_exp_dir, cfg_file))
    shutil.copyfile(os.path.join(yaml_dir, experiment_config), os.path.join(temp_exp_dir, experiment_config))
    if DEFAULT_CONFIG != experiment_config :
        shutil.copyfile(os.path.join(yaml_dir, DEFAULT_CONFIG), os.path.join(temp_exp_dir, DEFAULT_CONFIG))

    return temp_dir, temp_exp_dir, trajectories_dir, sim_output_path, plot_path


def sampleplot(adf, allchannels, start_date, plot_fname=None):
    fig = plt.figure(figsize=(8, 6))
    palette = sns.color_palette('Set1', 10)

    axes = [fig.add_subplot(3, 3, x + 1) for x in range(len(allchannels))]
    fig.subplots_adjust(bottom=0.05, hspace=0.25, right=0.95, left=0.1)
    for c, channel in enumerate(allchannels):
        mdf = adf.groupby('time')[channel].agg([np.mean, CI_5, CI_95, CI_25, CI_75]).reset_index()
        ax = axes[c]
        dates = [start_date + timedelta(days=int(x)) for x in mdf['time']]
        ax.plot(dates, mdf['mean'], label=channel, color=palette[c])
        ax.fill_between(dates, mdf['CI_5'], mdf['CI_95'],
                        color=palette[c], linewidth=0, alpha=0.2)
        ax.fill_between(dates, mdf['CI_25'], mdf['CI_75'],
                        color=palette[c], linewidth=0, alpha=0.4)

        ax.set_title(channel, y=0.8)

        formatter = mdates.DateFormatter("%m-%d")
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_locator(mdates.MonthLocator())
        ax.set_xlim(start_date, )

    if plot_fname:
        log.info(f"Writing plot to {plot_fname}")
        plt.savefig(plot_fname)
    plt.show()
