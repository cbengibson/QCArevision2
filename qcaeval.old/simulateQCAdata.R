outcome="OUT"

#randomly sample all causal conditions 

unq<-unique(qca.data) #pick all unique combinations of causal conditions
cond<-data.frame(matrix(nrow=length(qca.data[,1]),ncol=length(conditions))) #make a list with the same number of columns as causal conditions
picked<-sample(1:length(unq[,1]),1)

for (loop in 1:length(qca.data)){
cond[,loop]<-qca.data[,loop]==unq[picked,loop]
}

qca.data[,outcome]<-sample(c(0,1),length(qca.data[,1]),replace=T) #randomly sample outcome 

qca.data[rowSums(cond)==length(conditions),outcome]<-sample(c(0,1), length(qca.data[rowSums(cond)==length(conditions),1]),replace=T, prob=c(.9,.1)) #resample outcome variable based upon causal conditions
