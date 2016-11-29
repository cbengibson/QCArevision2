laQCA<-function(mod, ncut="", sim=100){
source("sim.ltQCA.R")
source("configuration.table.R")
library("QCA")
#
#if (type=="crisp"){
s.data<-sim.ltQCA(mod,  ncut=ncut, sim=sim)
#}

#if (type=="fuzzy"){
#s.data<-sim.fsQCA(qca.data,  ncut=ncut, sim=sim)}

results<-conf.table(s.data[[1]], ncut=s.data[[2]])

return(results)

}