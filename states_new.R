setwd("/Users/burrelvannjr/Dropbox/Professional/Research/Projects/Lieberson")
library(QCA)
source("ltQCA.R")

states <- read.csv("states_new.csv",header=TRUE,sep=",")
states

W<-(states$r_2010_win==1)*1		#W = Win, or the Republican won the election
P<-(states$pct_tp_primary>=50.00)*1		#T = TP Primary Win but Election Loss
#O<-calibrate(states$orgs_million,type="fuzzy",thresholds=c(4,8,11),logistic=TRUE)		#O = Orgs
O<-(states$orgs_million>=8)*1			#O = Orgs
#R<-calibrate(states$rallies_million,type="fuzzy",thresholds=c(1.5,2.25,3),logistic=TRUE)		#R = Rallies
R<-(states$rallies_million>=2.25)*1		#R = Rallies
C<-(states$r_context==1)*1		#C = Republican Partisan Context
E<-(states$endorsement==1)*1		#E = Endorsement
I<-(states$incumbent==1)*1		#I = Incumbent

data<-data.frame(I,C,E,O,R,W)
data

#row.names(data)<-(states$state_name)		#row names

truth<-truthTable(data,outcome="W",sort.by="incl",incl.cut1=0.95,show.cases=TRUE)

mod1 <- eqmcc(truth,details=TRUE,show.cases=TRUE)
ltQCA(mod1)