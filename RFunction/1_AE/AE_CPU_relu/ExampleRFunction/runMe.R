source("runDocker.R")
source("autoencoderCPU.R")
scratch.folder=getwd()
file=paste(scratch.folder,"setA.csv",sep="/")
separator=","
bias="kegg"
permutation=2
nEpochs=1000
projectName="Test_CPU"
autoencoder4clusteringCPU(group=c("docker"), scratch.folder=scratch.folder, file=file,separator=separator, bias=bias, permutation=permutation, nEpochs=nEpochs,projectName=projectName)
