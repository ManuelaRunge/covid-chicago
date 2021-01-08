#!/bin/bash
#SBATCH -A p30781
#SBATCH -p short
#SBATCH -t 04:00:00
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=60G
#SBATCH	--job-name="prob8"
#SBATCH --error=log/arrayJob_%A_%a.err
#SBATCH --output=log/arrayJob_%A_%a.out

ml python/anaconda3.6
ml R/4.0.0

#python /home/mrm9534/gitrepos/covid-chicago/nucluster/combine_and_trim.py  --stem "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen" --Location "NUCLUSTER"
Rscript "/home/mrm9534/gitrepos/covid-chicago/nucluster/split_csvs_by_region.R" "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen"

cd /home/mrm9534/gitrepos/covid-chicago/plotters/
python /home/mrm9534/gitrepos/covid-chicago/plotters/aggregate_by_param.py  --stem "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen" --Location "NUCLUSTER"
python /home/mrm9534/gitrepos/covid-chicago/plotters/overflow_probabilities_trigger.py  "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen"

python /home/mrm9534/gitrepos/covid-chicago/plotters/trigger_Ki_extract.py  "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen"
python /home/mrm9534/gitrepos/covid-chicago/plotters/peak_exceed_extract.py  "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen"

#cd /home/mrm9534/gitrepos/covid-chicago/Rfiles/
#Rscript /home/mrm9534/gitrepos/covid-chicago/Rfiles/estimate_Rt/get_Rt_from_ICUsimulations.R "20201212_IL_regreopen100perc_7daysdelay_pr8_reopen"
