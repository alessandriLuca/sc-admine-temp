#' @title Dense Space visualization
#' @description The present function compress data using autoencoder partially connected creating pseudoBulk matrix
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param matrixName, a character string indicating the path of the file, with file name and extension included
#' @param cloutput, number of permutations to perform the pValue to evaluate clustering. Suggested minimal number of permutations 10
#' @param separator, separator for matrix file
#' @param perplexity, TSNE parameter,  sets the number of nearest neighbors to consider when constructing a probability distribution over the input data. A higher perplexity value will result in a more accurate probability distribution, while a lower value will result in a simpler distribution that is faster to compute.
#' @param n_iter, TSNE parameter, This parameter sets the number of iterations to run the optimization process. A higher number of iterations will result in a more accurate solution, but will also take longer to compute. A lower number of iterations will result in a faster computation, but may not find the optimal solution
#' @param n_neighbors, UMAP parameter, sets the number of neighbors used in the local neighborhood graph construction. A higher value of n_neighbors will result in a more complex and accurate neighborhood graph, while a lower value will result in a simpler graph that is faster to compute.
#' @param min_dist, UMAP parameter, sets the minimum distance between any two points in the low-dimensional embedding. A higher value of min_dist will result in a more spread-out embedding, while a lower value will result in a more compact embedding. Min_dist can be used to control the clustering of points in the embedding, with smaller values leading to tighter clusters and larger values leading to more dispersed clusters.
#' @param tsneplot, 1 for tsne plot 0 otherwise
#' @param umapplot, 1 for umap plot 0 otherwise
#' @author Luca Alessandri, alessandri [dot] luca1991 [at] gmail [dot] com, University of Torino
#'
#' @examples
#' \dontrun{
#source("autoencoder4Pseudobulk.R")
#file=paste(getwd(),"VandE.csv",sep="/")
#file2=paste(getwd(),"VandE_clustering.output.csv",sep="/")
#print(file)
#print(file2)
#autoencoder4pseudoBulk(group=c("docker"), scratch.folder="/scratch/scratch", file=file,separator=",", permutation=10, nEpochs=1000,projectName="flavor",cloutput=file2, version=2)
#'}
#' @export
denseSpaceVisualization <- function(group=c("sudo","docker"), scratch.folder, file,separator,cloutput,perplexity=30,n_iter=250,n_neighbors=15,min_dist=0.1,tsneplot=c(0,1),umapplot=c(0,1)){
    data.folder=dirname(file)
    positions=length(strsplit(basename(file),"\\.")[[1]])
    matrixNameC=strsplit(basename(file),"\\.")[[1]]
    matrixName=paste(matrixNameC[seq(1,positions-1)],collapse="")
    format=strsplit(basename(basename(file)),"\\.")[[1]][positions]


    #running time 1
    ptm <- proc.time()
    #setting the data.folder as working folder
    if (!file.exists(data.folder)){
      cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
      return(2)
    }

    #storing the position of the home folder
    home <- getwd()
    setwd(data.folder)
    #initialize status
    system("echo 0 > ExitStatusFile 2>&1")

    #testing if docker is running
    test <- dockerTest()
    if(!test){
      cat("\nERROR: Docker seems not to be installed in your system\n")
      system("echo 10 > ExitStatusFile 2>&1")
      setwd(home)
      return(10)
    }



    #check  if scratch folder exist
    if (!file.exists(scratch.folder)){
      cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
      system("echo 3 > ExitStatusFile 2>&1")
      setwd(data.folder)
      return(3)
    }
    tmp.folder <- gsub(":","-",gsub(" ","-",date()))
    scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
    writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
    cat("\ncreating a folder in scratch folder\n")
    dir.create(file.path(scrat_tmp.folder))
    #preprocess matrix and copying files

    if(separator=="\t"){
      separator="tab"
    }

    system(paste("cp ",data.folder,"/",matrixName,".",format," ",scrat_tmp.folder,"/",sep=""))
    system(paste("cp ",cloutput," ",scrat_tmp.folder,"/",sep=""))

    cloutput=basename(cloutput)
    #executing the docker job
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d repbioinfo/densevisualprojection python3 /home/main.py ",matrixNameC,".",format," ",cloutput," ",separator," ",perplexity," ",n_iter," ",n_neighbors," ",min_dist," ",tsneplot," ",umapplot,sep="")

    resultRun <- runDocker(group=group, params=params)

    #waiting for the end of the container work
    if(resultRun==0){
      #system(paste("cp ", scrat_tmp.folder, "/* ", data.folder, sep=""))
    }
    #running time 2
    ptm <- proc.time() - ptm
    dir <- dir(data.folder)
    dir <- dir[grep("run.info",dir)]
    if(length(dir)>0){
      con <- file("run.info", "r")
      tmp.run <- readLines(con)
      close(con)
      tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
      writeLines(tmp.run,"run.info")
    }else{
      tmp.run <- NULL
      tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")

      writeLines(tmp.run,"run.info")
    }

    #saving log and removing docker container
    container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
    system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))


    #Copy result folder
    cat("Copying Result Folder")
    system(paste("cp -r ",scrat_tmp.folder,"/* ",data.folder,sep=""))
    #removing temporary folder
    cat("\n\nRemoving the temporary file ....\n")
    system(paste("rm -R ",scrat_tmp.folder))
    system("rm -fR out.info")
    system("rm -fR dockerID")
    system("rm  -fR tempFolderID")
    setwd(home)
  }

