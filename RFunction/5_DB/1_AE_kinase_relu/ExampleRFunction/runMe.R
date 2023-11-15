source("runDocker.R")
source("autoencoderCPUDB2.R")
scratch.folder=getwd()
file=paste(scratch.folder,"setA.csv",sep="/")
separator=","
permutation=2
nEpochs=1000
projectName="Test_CPU"
autoencoderDB2(group=c("docker"), scratch.folder=scratch.folder, file=file,separator=separator, permutation=permutation, nEpochs=nEpochs,projectName=projectName)
