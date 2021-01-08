#!/bin/bash
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=80G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --output=/home/mrm9534/gitrepos/covid-chicago/nucluster//outputs/processForCivis-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/gitrepos/covid-chicago/nucluster//errors/processForCivis-%A_%a.err      # Path for errors must already exist
#SBATCH --job-name="processForCivis"       # Name of job


# load modules you need to use
ml python/anaconda3.6
ml R/4.0.0

# A command you actually want to execute:
#python /home/mrm9534/gitrepos/covid-chicago/plotters/process_for_civis_EMSgrp.py  --exp_name "20200912_IL_baseline" --processStep "generate_outputs" --Location "NUCLUSTER"
python /projects/p30781/covidproject/covid-chicago/plotters//aggregate_by_param.py
#python /home/mrm9534/gitrepos/covid-chicago/plotters/overflow_probabilities_trigger.py
#python /home/mrm9534/gitrepos/covid-chicago/plotters/plot_exp_by_varying_param.py
#Rscript /home/mrm9534/gitrepos/covid-chicago/nucluster/split_csvs_by_region.R
