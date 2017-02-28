rm(list=ls())
qca.data<-read.csv("Gamson.csv")
qca.data<-qca.data[,1:5]
source("laQCA.R")
results<-laQCA(qca.data, outcome="ACP", type="crisp", ncut=3:5, sim=50) #something seems off with the results...

results<-rvQCA(qca.data, outcome="ACP", type="crisp", ncut=3:5, sim=5) #something seems off with the results...



#fs QCA example
british<-read.csv("british.csv")
results<-laQCA(qca.data, outcome="ACP",  type="fuzzy", ncut=3, sim=5)


mod<-eqmcc(qca.data,  outcome=c("ACP"),  n.cut=5, incl.cut1=.84,details = TRUE, show.cases = TRUE)
mod






