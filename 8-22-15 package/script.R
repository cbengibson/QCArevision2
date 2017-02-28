# make the data set

rallies<-read.csv("/Users/burrelvannjr/Desktop/package/florida_final.csv",header=TRUE,sep=",")
rallies$P<-(rallies$tprallies>=1)*1
rallies$R<-(rallies$reppct2008>=49.62)*1
rallies$C<-(rallies$pctBA25>=(mean(rallies$pctBA25)))*1
rallies$U<-(rallies$pctunemp>=(mean(rallies$pctunemp)))*1
save(rallies,file="/Users/burrelvannjr/Desktop/package/laQCA/data/rallies.rda")


# Code to add to the example page
data(rallies)
P<-rallies$P
R<-rallies$R
C<-rallies$C
U<-rallies$U

app.data<-data.frame(P,R,C,U)

truth<-truthTable(app.data,outcome="P",sort.by="incl",incl.cut1=0.7,show.cases=TRUE)
truth
mod1 <- eqmcc(truth,details=TRUE,show.cases=TRUE)
mod1

laQCA(mod1) #Lieberson assessment
lrQCA(app.data,outcome="P") #Lieberson recommendation






