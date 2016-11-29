rm(list=ls())
library("QCA")

#Step 1: Identify relevant cases and causal conditions

#Step 2: Construct the data set

gamson<-read.csv("Gamson.csv")
gamson<-rbind(gamson,gamson)[1:100,]
cov<-matrix(nrow=1000,ncol=10)
inc<-matrix(nrow=1000,ncol=10)
pri<-matrix(nrow=1000,ncol=10)
means.inc<-vector()
means.cov<-vector()

#list of parameters
N<-200 #sample size (in increments of hundreds)
n<-2  #categorical n

for (i in 1:((N-100)/100)){
gamson<-rbind(gamson,gamson)}



for (k in 1:30){

for (j in 1:1000) {
  
  for (i in 1:6){
    gamson[,i]<-sample(c(0,1),length(gamson[,1]),replace=T)}
  
  #ERROR HANDLING
  parsimonious <- tryCatch(
    eqmcc(gamson,  outcome=c("ACP"),  n.cut=k, incl.cut1=.7, 
          conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE),
    error=function(e) e
  )
  
  if(!inherits(parsimonious, "error")){
    #REAL WORK
    inc[j,k]<-parsimonious$IC[[3]][1]
    pri[j,k]<-parsimonious$IC[[3]][2]
    cov[j,k]<-parsimonious$IC[[3]][3]
  }
  
  if(inherits(parsimonious, "error")){
    #REAL WORK
    j<-NA
    pri[j,k]<-NA
    cov[j,k]<-NA
  }
  
}

means.inc[k]<-mean(as.numeric(inc[,k]),na.rm=T)
means.cov[k]<-mean(as.numeric(cov[,k]),na.rm=T)

}

sums.inc<-vector()
sums.cov<-vector()

for(i in 1:10){
sums.inc[i]<-sum(is.na(inc[,i]))
sums.cov[i]<-sum(is.na(inc[,i]))
}

inc[is.na(inc)]<-0

par(mfcol=c(1,2)) 

#pdf("consistency.sim.pdf")
hist(as.numeric(inc), main="Histogram of Consistency Scores of 1000 Samples",xlab="Consistency Scores")
hist(as.numeric(cov), main="Histogram of Coverage Scores of 1000 Samples",xlab="Coverage Scores")
#dev.off()

hist(as.numeric(pri), main="Histogram of PRI Scores of 1000 Samples",xlab="PRI Scores")