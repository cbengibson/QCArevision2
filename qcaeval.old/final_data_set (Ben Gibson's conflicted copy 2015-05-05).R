#QCA Plots
#btw, small modifications of this could replace the current scripts for returning the final data set
#need to change the configuration.table file though (and the regression analysis)
source("sim.ltQCA.R")
library(QCA)
library(foreach)


#create a data set with varying distributions, and varying number of variables 
dvdists<-seq(.1,.9, by=.2)
dists<-seq(.5,.9, by=.1)
num.vars<-2:7
var.names<-c("AA","BB","CC","DD","EE","FF","GG","HH","II","KK")
sam.sizes<-seq(10,100, by=20)
counter<-0

data.list<-vector(mode="list", length=length(num.vars)*length(sam.sizes)*length(dists))



results<-foreach (dist in dists) %dopar% {
for (dvdist in dvdists) {
for (num.var in num.vars){
for (sam.size in sam.sizes){
 
counter<-counter+1

qca.data<-as.data.frame(matrix(nrow=sam.size,ncol=num.var + 1))

for (col in 1:ncol(qca.data)){qca.data[,col]<-sample(c(0,1), sam.size, replace=T,prob=c(1-dist,dist))} #simulate data set
names(qca.data)<-c(var.names[1:num.var],"OUT")
qca.data$OUT<-sample(c(0,1), sam.size, replace=T,prob=c(1-dvdist,dvdist))

data.list[[counter]]<-sim.ltQCA(qca.data, outcome="OUT", sim=10, ncut=1:6)
data.list[[counter]]$nvar<-num.var
data.list[[counter]]$sam.size<-sam.size
data.list[[counter]]$dist<-dist
data.list[[counter]]$dvdist<-dvdist
save(data.list,file=paste("data.list_",dist,".Rdata",sep=""))
}}}}
