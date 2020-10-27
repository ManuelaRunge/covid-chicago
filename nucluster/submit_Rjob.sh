#!/bin/bash
#SBATCH --job-name="custom_combine1"       # Name of job
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 01:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=82GB              # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --mail-user=manuela.runge@northwestern.edu  # Designate email address for job communications
#SBATCH --mail-type=FAIL     # Events options are job BEGIN, END, NONE, FAIL, REQUEUE
#SBATCH --output=/home/mrm9534/jobs/outputs/custom_combine-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/jobs/errors/custom_combine-%A_%a.err      # Path for errors must already exist
#SBATCH --array=1


# load modules you need to use
module load R/4.0.0

R --vanilla -f "/home/mrm9534/jobs/split_csvs_by_region.R" 


#R --vanilla -f "/home/mrm9534/jobs/custom_combine_csv.R" 
#R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/Rfiles/ct_analysis/describeTrajectoriesDat.R" 
#R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/_temp/trimTrajectories.R" 
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/zip_daily_simFiles.R" 
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/combine_Rt.R" 
#R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_contactTracingSimulations.R" ""${SLURM_ARRAY_TASK_ID}""
#R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/Rfiles/simulation_plotter/fitted_param_plots.R" ""${SLURM_ARRAY_TASK_ID}""


