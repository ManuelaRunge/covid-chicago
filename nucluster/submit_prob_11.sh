#!/bin/bash
#SBATCH -A p30781               # Allocation
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=18G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1     # Number of Cores (Processors)
#SBATCH --output=/home/mrm9534/gitrepos/covid-chicago/nucluster/outputs/20201121_100perc_3daysdelay_sm8-%A_%a.out    # Path for output must already exist
#SBATCH --error=/home/mrm9534/gitrepos/covid-chicago/nucluster/errors/20201121_100perc_3daysdelay_sm8-%A_%a.err      # Path for errors must already exist
#SBATCH --job-name="100perc_3daysdelay_sm8"       # Name of job


# load modules you need to use
ml python/anaconda3.6

# A command you actually want to execute:
#python /home/mrm9534/gitrepos/covid-chicago/plotters/aggregate_by_param.py  "20201121_IL_regreopen100perc_3daysdelay_sm8"
#python /home/mrm9534/gitrepos/covid-chicago/plotters/overflow_probabilities_trigger.py  "20201121_IL_regreopen100perc_3daysdelay_sm8"
python /home/mrm9534/gitrepos/covid-chicago/plotters/trigger_Ki_extract.py  "20201121_IL_regreopen100perc_3daysdelay_sm8"
python /home/mrm9534/gitrepos/covid-chicago/plotters/peak_exceed_extract.py  "20201121_IL_regreopen100perc_3daysdelay_sm8"