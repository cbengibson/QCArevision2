lrQCA<-function(qca.data, outcome="OUT", type="crisp", inclcut = "", ncut=2:6, neg.out=F, sim=10, verbose=T, conv.diag=F){
source("sim.ltQCA.R")
source("configuration.table.R")
library("QCA")

s.data<-sim.ltQCA(qca.data, outcome, inclcut = inclcut, ncut=ncut, sim=sim, neg.out=F, type=type, verbose=verbose)
results<-conf.table(s.data, ncut)
return(results)
}

rvQCA<-function(qca.data, outcome="OUT", conditions=c(""), type="crisp", ncut=4, sim=100){
  source("combined.Gamson.sim.R")
  source("configuration.table.R")
  source("rvQCA.R")
  library("QCA")
    s.data<-sim.rvQCA(qca.data, outcome, ncut=ncut, sim=sim)
    results<-conf.table(s.data, ncut)
    return(results)
  }