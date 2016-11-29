# 1) include more options for laQCA

#2) include bootstrap procedure for standard errors

#6) Apply convergence diagnostics to simulation sample sizes
 #i) r-hat
 #ii) plot standard deviations of bootstrap over time

#3) integrate rvQCA

load("final_data_set.Rdata")
class(data.list)

4) Make tables for results using: 
  i)   n cut sizes



ii)  parsimonious versus complex solutions 
  iii) incl.cut
#  iv)  using a directional expectation?
  v)   sample sizes 
  vi)  number of variables
  vii) rhat values 
  viii) standard errors from bootstrapping

5) Create a general table of results that can be a general reference for others 
   
7) have a class and summary that lists: 
    the probability that this configuration is random 

8) Apply procedure to the case studies used in other studies (steal!)



#source("laQCA.R")
#source("complete_laQCA.R")
#source("conv.diag.complete.R")


rm(list=ls())
qca.data<-read.csv("Gamson.csv")
qca.data<-qca.data[,1:5]


mod<-eqmcc(qca.data,  outcome=c("ACP"),  n.cut=4, incl.cut1=.85, include="?", details = TRUE,  show.cases = TRUE, all.sol=T)
mod

source("sim.ltQCA.R")
source("configuration.table.R")
library("QCA")

s.data<-sim.ltQCA(mod,  ncut=ncut, sim=sim)
results<-conf.table(s.data[[1]], ncut=s.data[[2]])


prob<-ltQCA(mod)


prob<-rvQCA(mod)
Rhatresults<-cdQCA(mod, sim=2000)

plot(Rhatresults[[1]], type="l", ylim=c(0,2), col="red")
#lines(Rhatresults[[2]], type="l", ylim=c(0,2), col="blue")

meanprop<-sapply(1:length(Rhatresults[[3]][,1]), function(x) mean(Rhatresults[[3]][x,]))
lines(meanprop - Rhatresults[[2]]/2, type="l", ylim=c(0,1), col="blue")
lines(meanprop)
lines(meanprop + Rhatresults[[2]]/2, type="l", ylim=c(0,1), col="blue")



states <- read.csv("states_new.csv",header=TRUE,sep=",")
states

W<-(states$r_2010_win==1)*1  	#W = Win, or the Republican won the election
P<-(states$pct_tp_primary>=50.00)*1		#T = TP Primary Win but Election Loss
#O<-calibrate(states$orgs_million,type="fuzzy",thresholds=c(4,8,11),logistic=TRUE)		#O = Orgs
O<-(states$orgs_million>=8)*1			#O = Orgs
#R<-calibrate(states$rallies_million,type="fuzzy",thresholds=c(1.5,2.25,3),logistic=TRUE)		#R = Rallies
R<-(states$rallies_million>=2.25)*1		#R = Rallies
C<-(states$r_context==1)*1		#C = Republican Partisan Context
E<-(states$endorsement==1)*1		#E = Endorsement
I<-(states$incumbent==1)*1		#I = Incumbent

data<-data.frame(I,C,E,O,R,W)
summary(data)

#row.names(data)<-(states$state_name)		#row names

truth<-truthTable(data,outcome="W",sort.by="incl",incl.cut0=.85,neg.out=T, n.cut=3, show.cases=TRUE)

mod <- eqmcc(truth,details=TRUE,show.cases=TRUE)
mod
ltQCA(mod)

#prob<-rvQCA(mod1)
Rhatresults<-cdQCA(mod, sim=2000)

plot(Rhatresults[[1]], type="l", ylim=c(0,2), col="red")
meanprop<-sapply(1:length(Rhatresults[[3]][,1]), function(x) mean(Rhatresults[[3]][x,]))
lines(meanprop - Rhatresults[[2]]/2, type="l", ylim=c(0,1), col="blue")
lines(meanprop)
lines(meanprop + Rhatresults[[2]]/2, type="l", ylim=c(0,1), col="blue")






