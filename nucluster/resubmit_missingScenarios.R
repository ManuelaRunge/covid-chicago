
exp_name ="20200903_IL_quest_testSamples_rn91"
expDir <- file.path("/home/mrm9534/gitrepos/covid-chicago/_temp",exp_name)
trajectoriesDir <- file.path(expDir,"trajectories")

csvfiles <- list.files(trajectoriesDir, pattern=".csv", full.names=FALSE, recursive=FALSE)

csvnr <- as.numeric(gsub(".csv","",gsub("trajectories_scen","",csvfiles)))

sampleDat <- read.csv(file.path(expDir,"sampled_parameters.csv"))
scens = max(sampleDat$scen_num)

## Write submission
scennotrun <- c(1:scens)[!( c(1:scens) %in% csvnr)]
paste(as.character(scennotrun),sep="", collapse=",")

