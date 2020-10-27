#!/bin/bash
#SBATCH -A b1139               # Allocation
#SBATCH -p b1139                # Queue
#SBATCH -t 01:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=80GB              # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --mail-user=manuela.runge@northwestern.edu  # Designate email address for job communications
#SBATCH --mail-type=FAIL     # Events options are job BEGIN, END, NONE, FAIL, REQUEUE
#SBATCH --output=/home/mrm9534/jobs/outputs/combine-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/jobs/errors/combine-%A_%a.err      # Path for errors must already exist
#SBATCH --job-name="combine"       # Name of job



# load modules you need to use
module load python
#cd /projects/p30781/covidproject/covid-chicago/plotters/
cd /home/mrm9534/gitrepos/covid-chicago/_temp/

# A command you actually want to execute:
python combine_and_trim.py

#python  process_for_civis_EMSgrp.py#
#python combine.py "${SLURM_ARRAY_TASK_ID}"
#python trim_trajectoriesDat.py
