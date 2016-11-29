#rm(list=ls())
library("QCA")
library(foreach)
library(doMC)
registerDoMC(cores=2) 



sims<-c(seq(from=100,to=10000, by=100)) #number of simulations
inclcut1s<-seq(from=.50, to=.99, by=.05)
n.cuts<-1:10  #categorical n number of categories
pops<-seq(from=10,to=100,by=10)  #sample size
probs<-seq(from=.1,to=.9,by=.1)

rhatmepars<-data.frame(sim1 = rep(0,10000), sim2 = 0, sim3 =0 , sim4= 0, sim5= 0, sim6= 0)
rhatmecomp<-data.frame(sim1 = rep(0,10000), sim2 = 0, sim3 =0 , sim4= 0, sim5= 0, sim6= 0)
rhatmepars<-do.call("list", replicate(sim, rhatmepars, simplify = FALSE))
rhatmecomp<-do.call("list", replicate(sim, rhatmecomp, simplify = FALSE))
rhatcounter<-0

#for (sim in 10000){ #just alter the amount of sims you count in the mean to test this
ptm<-proc.time()
sim<-20
rows<-length(inclcut1s) * length(n.cuts) * length(pops) * length(probs) * 8 * 10 * sim * 2
data<-data.frame(comp=NA,pars=NA,pop=NA,nvar=NA,n.cut=NA,prob=NA,inclcut1=NA)

 foreach (inclcut1 = inclcut1s) %dopar% {
    for (n.cut in n.cuts){
      for (pop in pops){
        for (prob in probs){
          for (nvar in 3:10){
            for (type in c("crisp")){
              for (trial in 1:6){

qca.data<-data.frame(matrix(ncol=nvar,nrow=pop))
qca.data[is.na(qca.data)]<-0
s.qca.data<-do.call("list", replicate(sim, qca.data, simplify = FALSE))
       
          if (type=="crisp"){  
            for (j in 1:sim){
            for (i in 1:nvar){ #simulate random causal conditions
              s.qca.data[[j]][,i]<-sample(c(0,1),pop,prob=c(prob, 1-prob),replace=T)
            }
            names(s.qca.data[[j]])[i]<-c("OUT")
          }
          }
          
        parsimonious <- sapply(s.qca.data, function(x){tryCatch( #trap error
          eqmcc(x,  outcome=names(x)[length(x)],  n.cut=n.cut, incl.cut1=inclcut1, include = c("?", "C"), 
                conditions = c(names(x[,!(names(x) %in% 'OUT')])))[[4]]   ,
          error=function(e) e
        )})
        
        pars<-rep(NA, sim)
        pars[sapply(parsimonious, function(x) !inherits(x,"error"))]<-1
        errs<-sapply(parsimonious,  function(x) grepl("Nothing to explain",x) |   grepl("dimensions", x))
        pars[sapply(errs, function(x) sum(x))==1]<-0
        errs<-sapply(parsimonious, function(x) grepl("incorrect number of dimensions",x, ignore.case=T) )
        pars[sapply(errs, function(x) sum(x))==1]<-NA

 
rhatcounter<-rhatcounter + 1

for (rhatcol in 1:sim){
  rhatmepars[[rhatcounter]][rhatcol,trial]<-mean(sample(pars, size=rhatcol),na.rm=T)
}

names(rhatmepars)[[rhatcounter]]<-(paste(pop,inclcut1,n.cut,nvar,prob,sep=","))

data<-rbind(data,cbind(pars,pop,nvar,n.cut,prob,inclcut1))
            
write.csv(paste(round(length(data[,1])/rows, digits=4),"% done", sep=""),"percentdone.csv")

write.csv(rhatcol,"rhatcol.csv")
              }}}}}}}


