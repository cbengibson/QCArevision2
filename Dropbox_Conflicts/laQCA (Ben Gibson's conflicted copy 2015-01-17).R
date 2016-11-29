laQCA<-function(qca.data, outcome="OUT", type="crisp", inclcut = "", ncut=4, neg.out=F, sim=100, verbose=T, conv.diag=F){
source("sim.ltQCA.R")
source("configuration.table.R")
library("QCA")

s.data<-sim.ltQCA(qca.data, outcome, inclcut = inclcut, ncut=ncut, sim=sim, neg.out=F, type=type, verbose=verbose)

if (incl.cut == ""){
results<-conf.table(s.data, ncut)
if (conv.diag==F){return(results)}
if (conv.diag==T){return(s.data)}

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