rm(list=ls())
library("QCA")

sims<-c(seq(from=100,to=10000, by=100)) #number of simulations
inclcut1s<-seq(from=.50, to=.99, by=..05)
n.cuts<-1:10  #categorical n number of categories
pops<-seq(from=10,to=100,by=10)  #sample size
probs<-seq(from=.1,to=.9,by=.1)


for (sim in 10000){ #just alter the amount of sims you count in the mean to test this
  for (inclcut1 in inclcut1s){
    for (n.cut in n.cuts){
      for (pop in pops){
        for (prob in probs){
          for (nvar in 3:10){
            for (type in c("crisp","fuzzy")){

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
          
          if (type == "fuzzy"){
            for (j in 1:sim){
            for (i in 1:length(qca.data)){ #simulate random causal conditions
              ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop
              prob<-c(rnorm(10, mean=1))
              s.qca.data[[j]][,i]<-sample(ranges,pop,prob=prob,replace=T)
            }
            names(s.qca.data[[j]])[i]<-c("OUT")
            }
          }
       
        
        ##########parsimonious
        parsimonious <- sapply(s.qca.data, function(x){tryCatch( #trap error
          eqmcc(x,  outcome=names(x)[length(x)],  n.cut=n.cut, incl.cut1=inclcut1, include = c("?", "C"), 
                conditions = c(names(x[,!(names(x) %in% 'OUT')])))   ,
          error=function(e) e
        )})
        
       pars<-rep(NA, sim)
        pars[sapply(parsimonious, function(x) !inherits(x,"error"))]<-1
        errs<-sapply(parsimonious, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
        pars[sapply(errs, function(x) sum(x))==1]<-0
        errs<-sapply(parsimonious, function(x) grepl("incorrect number of dimensions",x, ignore.case=T) )
        pars[sapply(errs, function(x) sum(x))==1]<-NA
        
        ########complex
        
        complex <- sapply(s.qca.data, function(x){tryCatch( #trap error
          eqmcc(x,  outcome=c("OUT"),  n.cut=n.cut, incl.cut1=inclcut1,
                conditions= c(names(x[,!(names(x) %in% 'OUT')])),details = F, show.cases = F),
          error=function(e) e
        )})
        
        comp<-rep(NA, sim)
        comp[sapply(complex, function(x) !inherits(x,"error"))]<-1
        errs<-sapply(complex, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
        comp[sapply(errs, function(x) sum(x))==1]<-0
        
        for (i in seq(from=100, to= 10000, by= 100)){
          
        }
        
      }}}}
save(data,file="qcaSim5.Rdata")
proc.time() - ptm
