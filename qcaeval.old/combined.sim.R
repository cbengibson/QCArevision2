rm(list=ls())
library("QCA")
library(foreach)
library(doMC)
registerDoMC(cores=4) 

#Step 1: Identify relevant cases and causal conditions

#Step 2: Construct the data set
gamson1<-read.csv("Gamson.csv")
gamson<-read.csv("Gamson.csv")
gamson<-rbind(gamson,gamson)[1:100,]

sim<-1 #number of simulations

inclcut<-vector()
inclcut[1]<-.50

for (i in 2:11){
  j<-i-1
  inclcut[i]<-inclcut[j] + .05
}

ncut<-10  #categorical n number of categories

pop<-vector()
pop[1]<-10
for (i in 2:10){
  j<-i-1
  pop[i]<-pop[j] + 10
}


rows<-sim*ncut*length(inclcut)*length(pop)*2 #total number of rows
rows


data<-data.frame(CTH=0,CNTH=0,CPI=0,NTH=0,OUT=rep(0,rows))
#data<-list(data,data,data,data,data,data,data,data,data,data,
#           data,data,data,data,data,data,data,data,data,data)
kk<-0


ptm<-proc.time()

#data<-foreach (j = 1:sim) %dopar% {

for (j in 1:sim) {
  
  for (k in 1:length(inclcut)){
    
    for (n in 1:ncut){
      
      for (q in 1:length(pop)){
        
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        
        data[kk,4]<-pop[q]
        
        m<-q*10
        
        gamson<-data.frame(BUR=0,LOW=0,DIS=0,HLP=0,ACP=rep(0,m))
        
        for (i in 1:5){
          prob<-c(sum(gamson1[,i]==0)/(dim(gamson1)[1]),sum(gamson1[,i]==1)/dim(gamson1)[1])
          gamson[,i]<-sample(c(0,1),m,prob=prob,replace=T)}
        
        
        ##########parsimonious
        
        
        #ERROR HANDLING
        parsimonious <- tryCatch(
          eqmcc(gamson,  outcome=c("ACP"),  n.cut=n, incl.cut1=inclcut[k], include = "?",
                conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-1
          data[kk,3]<-0
        }
        
        if(inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-0
          data[kk,3]<-0
        }
        
        
        ########complex
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        
        data[kk,4]<-pop[q]
        
        #ERROR HANDLING
        parsimonious <- tryCatch(
          eqmcc(gamson,  outcome=c("ACP"),  n.cut=ncut[n], incl.cut1=inclcut[k],
                conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){
          #REAL WORK
          
          data[kk,5]<-1
          data[kk,3]<-1
          
        }
        
        if(inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-0
          data[kk,3]<-1
        }
        save(data,file="qcaSim5.Rdata")
      }}}}
save(data,file="qcaSim5.Rdata")
proc.time() - ptm
