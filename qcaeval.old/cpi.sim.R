rm(list=ls())
library("QCA")

#Step 1: Identify relevant cases and causal conditions

#Step 2: Construct the data set

gamson<-read.csv("Gamson.csv")
gamson<-rbind(gamson,gamson)[1:100,]
cov<-matrix(nrow=1000,ncol=100)
inc<-matrix(nrow=1000,ncol=100)
pri<-matrix(nrow=1000,ncol=100)
means.inc<-vector()
means.cov<-vector()

#list of parameters
N<-200 #sample size (in increments of hundreds)
n<-2  #categorical n
sim<-1000 #number of simulations
inclcut<-.8 #inclusion threshold

for (i in 1:((N-100)/100)){
  gamson<-rbind(gamson,gamson)}

ptm<-proc.time()

for (j in 1:sim) {
  
  for (i in 1:6){
    gamson[,i]<-sample(c(0,1),length(gamson[,1]),replace=T)}
  
  
  ##########parsimonious
  
  
  #ERROR HANDLING
  parsimonious <- tryCatch(
    eqmcc(gamson,  outcome=c("ACP"),  n.cut=n, incl.cut1=inclcut, include = "?",
          conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE),
    error=function(e) e
  )
  
  if(!inherits(parsimonious, "error")){
    #REAL WORK
    inc[j,1]<-parsimonious$IC[[3]][1]
    pri[j,1]<-parsimonious$IC[[3]][2]
    cov[j,1]<-parsimonious$IC[[3]][3]
  }
  
  if(inherits(parsimonious, "error")){
    #REAL WORK
    #j<-NA
    pri[j,1]<-NA
    cov[j,1]<-NA
  }
  
  
  ########complex
  
  #ERROR HANDLING
  parsimonious <- tryCatch(
    eqmcc(gamson,  outcome=c("ACP"),  n.cut=n, incl.cut1=inclcut, 
          conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE),
    error=function(e) e
  )
  
  if(!inherits(parsimonious, "error")){
    #REAL WORK
    inc[j,2]<-parsimonious$IC[[3]][1]
    pri[j,2]<-parsimonious$IC[[3]][2]
    cov[j,2]<-parsimonious$IC[[3]][3]
  }
  
  if(inherits(parsimonious, "error")){
    #REAL WORK
    #j<-NA
    pri[j,2]<-NA
    cov[j,2]<-NA
  }
  
}


proc.time() - ptm

sums.inc<-vector()
for(i in 1:2){
  sums.inc[i]<-sum(is.na(inc[1:sim,i]))
}

plot(1-(sums.inc/sim),type="h",xlab="Consistency Threshold", ylab="Probability", main="Probability of Specifying a Random Configuration with Varying Consistency Thresholds")



#lines(means.inc)
#lines(means.cov)

#par(mfcol=c(1,1)) 


#inc[is.na(inc)]<-0

#par(mfcol=c(1,2)) 

#pdf("consistency.sim.pdf")
#hist(as.numeric(inc), main="Histogram of Consistency Scores of 1000 Samples",xlab="Consistency Scores")
#hist(as.numeric(cov), main="Histogram of Coverage Scores of 1000 Samples",xlab="Coverage Scores")
#dev.off()

#hist(as.numeric(pri), main="Histogram of PRI Scores of 1000 Samples",xlab="PRI Scores")