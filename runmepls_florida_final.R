setwd("/Users/burrelvannjr/Dropbox/Professional/Research/Projects/Lieberson/full package")
rm(list=ls())
library("QCA")
#source("sim.ltQCA.R")
source("laQCA.R") #Lieberson assessment
source("lrQCA.R") #Lieberson recommendation


data<-read.csv("florida_final.csv",header=TRUE,sep=",")
P<-(data$tprallies>=1)*1
R<-(data$reppct2008>=49.62)*1
C<-(data$pctBA25>=(mean(data$pctBA25)))*1
U<-(data$pctunemp>=(mean(data$pctunemp)))*1

app.data<-data.frame(P,R,C,U)

truth<-truthTable(app.data,outcome="P",sort.by="incl",incl.cut1=0.7,show.cases=TRUE)
truth
mod1 <- eqmcc(truth,details=TRUE,show.cases=TRUE)
mod1

laQCA(mod1) #Lieberson assessment
lrQCA(app.data,outcome="P") #Lieberson recommendation

#s.data<-sim.ltQCA(app.data, outcome="P",sim=10,ncut=1:6) 
#results<-conf.table(s.data, ncut=1:6)
#results #should be the same as above




