setwd("/Users/burrelvannjr/Dropbox/Professional/Research/Projects/Lieberson/full package")
rm(list=ls())
library("QCA")
#source("sim.ltQCA.R")
source("laQCA.R") #Lieberson assessment
source("lrQCA.R") #Lieberson recommendation


rally_raw<-read.csv("/Users/burrelvannjr/Desktop/package/florida_final.csv",header=TRUE,sep=",")
rally_raw$P<-(rally_raw$tprallies>=1)*1
rally_raw$R<-(rally_raw$reppct2008>=49.62)*1
rally_raw$C<-(rally_raw$pctBA25>=(mean(rally_raw$pctBA25)))*1
rally_raw$U<-(rally_raw$pctunemp>=(mean(rally_raw$pctunemp)))*1
#save(rally_raw,file="inst/extdata/rallies.RData")
#save(rally_raw,file="data/rallies.rda")

app.data<-data.frame(rally_raw$P,rally_raw$R,rally_raw$C,rally_raw$U)

truth<-truthTable(app.data,outcome="P",sort.by="incl",incl.cut1=0.7,show.cases=TRUE)
truth
mod1 <- eqmcc(truth,details=TRUE,show.cases=TRUE)
mod1

laQCA(mod1) #Lieberson assessment
lrQCA(app.data,outcome="P") #Lieberson recommendation

#s.data<-sim.ltQCA(app.data, outcome="P",sim=10,ncut=1:6) 
#results<-conf.table(s.data, ncut=1:6)
#results #should be the same as above




