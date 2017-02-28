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
      eqmcc(gamson,  outcome=c("ACP"),  n.cut=k, incl.cut1=.9, 
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

sums.inc3<-vector()
sums.cov<-vector()

for(i in 1:10){
  sums.inc3[i]<-sum(is.na(inc[,i]))
}

sums.inc1<-c(0.36, 0.33, 0.34, 0.333, 0.35, 
             0.264, 0.333, 0.26, 0.24, 0.22)

sums.inc<-c(0.34, 0.32, 0.318, 0.333, 0.34, 
            0.30, 0.28, 0.27, 0.25, 0.22)


plot(sums.inc,type="b",col="blue",ylim=c(0,1),,lty=1,xlab="N in Configuration", ylab="Probability", main="Probability of Specifying a Random Configuration with Varying Configurational N")
lines(sums.inc1,,type="b",col="red",lty=2)
lines(1-(sums.inc3/1000),type="b", col="green",lty=3)
g_range <- range(0, sums.inc, sums.inc3)
legend(9, .8, c(".7",".8",".9"), cex=0.8, 
       col=c("blue","red","green"),lty=1:3)



#lines(means.inc)
#lines(means.cov)

#par(mfcol=c(1,1)) 

#pdf("consistency.sim.pdf")
#hist(as.numeric(inc), main="Histogram of Consistency Scores of 1000 Samples",xlab="Consistency Scores")
#hist(as.numeric(cov), main="Histogram of Coverage Scores of 1000 Samples",xlab="Coverage Scores")
#dev.off()

#hist(as.numeric(pri), main="Histogram of PRI Scores of 1000 Samples",xlab="PRI Scores")