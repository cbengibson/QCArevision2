laQCA<-function(qca.data, outcome="OUT", type="crisp", ncut=4, sim=100){
source("combined.Gamson.sim.R")
source("configuration.table.R")
library("QCA")
if (type=="crisp"){
s.data<-sim.ltQCA(qca.data, outcome, ncut=ncut, sim=sim)
}

if (type=="fuzzy"){
s.data<-sim.fsQCA(qca.data, outcome, ncut=ncut, sim=sim)}


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