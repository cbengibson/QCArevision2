rm(list=ls())
library("QCA")

#Step 1: Identify relevant cases and causal conditions

#Step 2: Construct the data set

gamson<-read.csv("Gamson.csv")
gamson<-rbind(gamson,gamson)[1:10,]
cov<-matrix(nrow=1000,ncol=100)
inc<-matrix(nrow=1000,ncol=100)
pri<-matrix(nrow=1000,ncol=100)
means.inc<-vector()
means.cov<-vector()
sim<-1000



#list of parameters
N<-10 #sample size (in increments of 10)
n<-2  #categorical n




ptm<-proc.time()
for (k in 2:50){
  gamson<-read.csv("Gamson.csv")
  gamson<-rbind(gamson,gamson)[1:10,]
  m<-k*10
  
  for (i in 1:5){
    gamson<-rbind(gamson,gamson)}
  
  gamson<-gamson[1:m,]
  
  for (j in 1:sim) {
    
    for (i in 1:6){
      gamson[,i]<-sample(c(0,1),length(gamson[,1]),replace=T)}
    
    #ERROR HANDLING
    parsimonious <- tryCatch(
      eqmcc(gamson,  outcome=c("ACP"),  n.cut=2, incl.cut1=.7, 
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
      #j<-NA
      pri[j,k]<-NA
      cov[j,k]<-NA
    }
    
  }
  
  means.inc[k]<-mean(as.numeric(inc[,k]),na.rm=T)
  means.cov[k]<-mean(as.numeric(cov[,k]),na.rm=T)
  
}

proc.time()-ptm

sums.inc<-vector()
sums.cov<-vector()

for(i in 1:k){
  sums.inc[i]<-sum(is.na(inc[1:sim,i]))
}

plot(1:50*10,1-(sums.inc/sim),type="b",xlab="N in Sample Size", ylab="Probability", main="Probability of Specifying a Random Configuration with Varying Sample Size")
1:50*10

#lines(means.inc)
#lines(means.cov)

#par(mfcol=c(1,1)) 

#pdf("consistency.sim.pdf")
#hist(as.numeric(inc), main="Histogram of Consistency Scores of 1000 Samples",xlab="Consistency Scores")
#hist(as.numeric(cov), main="Histogram of Coverage Scores of 1000 Samples",xlab="Coverage Scores")
#dev.off()

#hist(as.numeric(pri), main="Histogram of PRI Scores of 1000 Samples",xlab="PRI Scores")