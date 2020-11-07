#!/bin/bash
#SBATCH --job-name="Rtloessheatmap"       # Name of job
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=32G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --mail-user=manuela.runge@northwestern.edu  # Designate email address for job communications
#SBATCH --mail-type=FAIL     # Events options are job BEGIN, END, NONE, FAIL, REQUEUE
#SBATCH --output=/home/mrm9534/jobs/outputs/Rtloess-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/jobs/errors/Rtloess-%A_%a.err      # Path for errors must already exist
#SBATCH --array=1-11


# load modules you need to use
module load R/4.0.0
cd /home/mrm9534/gitrepos/covid-chicago/Rfiles/

# A command you actually want to execute:
R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/Rfiles/ct_analysis/heatmap_loess_contactTracing_Rt.R" ""${SLURM_ARRAY_TASK_ID}""
