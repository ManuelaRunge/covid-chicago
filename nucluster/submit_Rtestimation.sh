#!/bin/bash
#SBATCH --job-name="getRt"       # Name of job
#SBATCH -A p30781               # Allocation
#SBATCH -p normal                # Queue
#SBATCH -t 08:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=64G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --mail-user=manuela.runge@northwestern.edu  # Designate email address for job communications
#SBATCH --mail-type=FAIL     # Events options are job BEGIN, END, NONE, FAIL, REQUEUE
#SBATCH --output=/home/mrm9534/jobs/outputs/getRt_raw_scen_num-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/jobs/errors/getRt_raw_scen_num-%A_%a.err      # Path for errors must already exist
#SBATCH --array=1-11


# load modules you need to use
module load R/4.0.0
cd /projects/p30781/covidproject/covid-chicago/Rfiles

# A command you actually want to execute:
#R --vanilla -f "/home/mrm9534/gitrepos/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200831_IL_regreopen50perc_7daysdelay_sm6"

#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_0daysdelay_sm4"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_0daysdelay_sm7"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_3daysdelay_sm4"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_3daysdelay_sm7"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_7daysdelay_sm4"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen100perc_7daysdelay_sm7"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_0daysdelay_sm4"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_0daysdelay_sm7"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_3daysdelay_sm4"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_3daysdelay_sm7"
#R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_7daysdelay_sm4"
R --vanilla -f "/projects/p30781/covidproject/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_rawSimulations.R" ""${SLURM_ARRAY_TASK_ID}"" "20200919_IL_regreopen50perc_7daysdelay_sm7"
