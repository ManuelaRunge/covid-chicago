#!/bin/bash
#SBATCH --job-name="estimateRt_scens"       # Name of job
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=32G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --output=/projects/p30781/covidproject/covid-chicago/nucluster/outputs/estimateRt-%A_%a.out    # Path for output must already exist
#SBATCH --error=/projects/p30781/covidproject/covid-chicago/nucluster/errors/estimateRt-%A_%a.err      # Path for errors must already exist
#SBATCH --array=1-11


# load modules you need to use
module load R/4.0.0
cd /projects/p30781/covidproject/covid-chicago/Rfiles/

# A command you actually want to execute:
R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R"  ""${SLURM_ARRAY_TASK_ID}"" "20200908_IL_quest_test"

