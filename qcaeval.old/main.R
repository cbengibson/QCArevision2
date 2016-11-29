rm(list=ls())
library("QCA")
library("bootstrap")
source("complete_laQCA.R")
qca.data<-read.csv("Gamson.csv")
qca.data<-qca.data[,1:5]
mod<-eqmcc(qca.data,  outcome=c("ACP"),  n.cut=4, incl.cut1=.85, include="?", details = TRUE,  show.cases = TRUE, all.sol=T)
ltQCA(mod, sim=100)
