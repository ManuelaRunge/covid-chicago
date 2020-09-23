#!/bin/bash
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=18G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --mail-user=manuela.runge@northwestern.edu  # Designate email address for job communications
#SBATCH --mail-type=FAIL     # Events options are job BEGIN, END, NONE, FAIL, REQUEUE
#SBATCH --output=/home/mrm9534/jobs/outputs/combine-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/jobs/errors/combine-%A_%a.err      # Path for errors must already exist
#SBATCH --job-name="combine"       # Name of job


# load modules you need to use
module load python/anaconda3.6
cd /home/mrm9534/gitrepos/covid-chicago/

# A command you actually want to execute:
python /home/mrm9534/gitrepos/covid-chicago/_temp/combine.py

