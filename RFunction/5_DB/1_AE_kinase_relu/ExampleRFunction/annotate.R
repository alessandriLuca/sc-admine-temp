 a=read.table("annotated_matrix.csv",header=TRUE,row.names=1,sep=",")
 rownames(a)=make.unique(sapply(rownames(a),FUN=function(x){strsplit(x,":")[[1]][2]}))
a=a[-grep("[.]",rownames(a)),]
a=a[-which(rownames(a)=="NA"),]
write.table(a,"mat.csv",col.names=NA,sep=",")
