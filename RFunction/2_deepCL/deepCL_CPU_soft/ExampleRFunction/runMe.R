source("runDocker.R")
source("deepCL_CPU.R")
scratch.folder=getwd()
file=paste(scratch.folder,"setA.csv",sep="/")
separator=","
bias="kegg"
permutation=40
nEpochs=1000
projectName="Test_CPU"
nCluster=3
deepClusteringCPU(group=c("docker"), scratch.folder=scratch.folder, file=file,separator=separator,nCluster=nCluster, bias=bias, finetune_iters=10, nEpochs=50,projectName=projectName,bN="NULL")
