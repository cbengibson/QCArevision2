# make the data set
library(QCA)
library(QCAGUI)
library(dplyr)

rallies<-read.csv("/Users/burrelvannjr/Desktop/package/florida_final.csv",header=TRUE,sep=",")
rallies$P<-(rallies$tprallies>=1)*1
#rallies$R<-(rallies$reppct2008>=49.62)*1
rallies$R<-(rallies$reppct2008>rallies$dempct2008)*1
rallies$C<-(rallies$pctBA25>=(mean(rallies$pctBA25)))*1
rallies$U<-(rallies$pctunemp>=(mean(rallies$pctunemp)))*1
rallies$E<-(rallies$pctevang>=(mean(rallies$pctevang)))*1
rallies$B<-(rallies$pctblack>=(mean(rallies$pctblack)))*1
rallies<-rallies %>% select(tprallies,reppct2008,dempct2008,pctBA25,pctunemp,pctevang,pctblack,P,R,C,U,E,B)
save(rallies,file="/Users/burrelvannjr/Desktop/package/iaQCA/data/rallies.rda")


# Code to add to the example page
data(rallies)
P<-rallies$P
R<-rallies$R
C<-rallies$C
U<-rallies$U
E<-rallies$E
B<-rallies$B

app.data<-data.frame(P,R,C,U,E,B)

truth<-truthTable(app.data,outcome="P",sort.by="incl",incl.cut1=0.85,show.cases=TRUE)
truth
mod1 <- eqmcc(truth,details=TRUE,show.cases=TRUE)
mod1

iraQCA(mod1) #Lieberson assessment
irrQCA(app.data,outcome="P",ncut=min(truth$tt$n):max(truth$tt$n)) #Lieberson recommendation
