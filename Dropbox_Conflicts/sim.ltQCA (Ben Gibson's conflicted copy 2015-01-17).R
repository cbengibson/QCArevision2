###Function to return a data set indicating whether a qca model with a certain set of covariates (conf. n thresh, ncut, etc) returned a random configuration.


sim.ltQCA<-function(mod, sim=100, inclcut = "", ncut = "", verbose=T){
ptm <- proc.time()


nconf<-rownames(mod$IC$overall$incl.cov) #names of the configuration(s)
incl.cut1<-mod$tt$incl.cut1 #consistency score of the configuration 
incl.cut0<-mod$tt$incl.cut0 #consistency score of the configuration 
n.cut<-min(mod$tt$tt$n)  #configurational n 
pop<-dim(mod[[1]]$initial.data)[1] #population size
relation<-mod$relation


notconditions<-names(mod[[1]][[1]])== c("OUT") |  names(mod[[1]][[1]])== "n" | names(mod[[1]][[1]])==   "incl" | names(mod[[1]][[1]])== "PRI"  | names(mod[[1]][[1]])== "cases"
conditions<-names(mod$tt$tt)[!notconditions]
outcome<-mod$tt$outcome
qca.data<-mod$tt$initial.data
neg.out<-mod$tt$neg.out


out<-qca.data[,outcome] #outcome vector
qca.data<-qca.data[,(names(qca.data) %in% conditions)] #matrix of causal conditions

if (sum(outcome > 0 && outcome < 1) == 0){
  type = "crisp"}

if (sum(outcome > 0 && outcome < 1) != 0){
  type = "fuzzy"}

if (all(conditions == c(""))) {
  conditions <- names(qca.data[,!(names(qca.data) %in% outcome)]) #use all coniditions that are not the outcome, if no conditions are specified
}


if (inclcut == ""){
inclcut<-seq(from=.5, to = 1, by=.01) #use precise consistency thresholds
}

if (ncut == ""){
  ncut<-2:max(mod$tt$tt["n"])
}

#OUT ~ CTH + CNTH + CPI

rows<-sim*length(ncut)*length(inclcut)*length(pop)*2 #total number of rows
data<-data.frame(CTH=NA,CNTH=NA,CPI=NA,pop=NA,OUT=rep(NA,rows)) #empty data set to simulate into


kk<-0 #set counter to 0

for (j in 1:sim) {
  
  for (k in 1:length(inclcut)){
    
    for (n in ncut){
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        data[kk,4]<-pop
        
        s.qca.data<-qca.data
        
        if (type=="crisp"){
        
        for (i in 1:length(qca.data)){ #simulate random causal conditions
          prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
          s.qca.data[,i]<-sample(c(0,1),pop,prob=prob,replace=T)} 
        
        #simulate random outcome variable
          prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
          s.qca.data$OUT<-sample(c(0,1),pop,prob=prob,replace=T)
        }
        
        if (type == "fuzzy"){
          
          for (i in 1:length(qca.data)){ #simulate random causal conditions
            ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop
            prob<-hist(qca.data[,i])[[2]]/dim(qca.data)[1]
            s.qca.data[,i]<-sample(ranges,pop,prob=prob,replace=T)
          } 
          
          #simulate random outcome variable
          prob<-hist(out)[[2]]/length(out)
          s.qca.data$OUT<-sample(ranges,pop,prob=prob,replace=T)
        }
      
        ##########parsimonious
  
        parsimonious <- tryCatch( #trap error
          eqmcc(s.qca.data,  outcome=c("OUT"),  n.cut=n, incl.cut1=inclcut[k], include = "?", neg.out=neg.out,
                conditions= c(names(s.qca.data[,!(names(s.qca.data) %in% 'OUT')])),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){
          
          data[kk,5]<-1 #1 = it returned a configuration with random data!
          data[kk,3]<-0 # parsimonious solution
        }
        
        if(grepl("Nothing to explain",parsimonious)[1] | grepl("All combinations have been included into analysis", parsimonious)[1]){
          #REAL WORK
          data[kk,5]<-0 #0 = it can't find the pattern that isn't there!
          data[kk,3]<-0 # parsimonious solution
        }
        
        
        ########complex
        
        kk<-kk+1 # increment row
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        data[kk,4]<-pop
        
        complex <- tryCatch( #trap error
          eqmcc(s.qca.data,  outcome=c("OUT"),  n.cut=n, incl.cut1=inclcut[k], neg.out=neg.out,
                conditions = c(names(s.qca.data[,!(names(s.qca.data) %in% 'OUT')])), details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(complex, "error")){  
          data[kk,5]<-1 #1 = it returned a configuration with random data!
          data[kk,3]<-1 # complex solution
        }
        
        if(grepl("Nothing to explain",complex)[1] | grepl("All combinations have been included into analysis", complex)[1]){
          data[kk,5]<-0 #0 = it can't find the pattern that isn't there!
          data[kk,3]<-1 # complex solution
        }
      if (verbose == T){
        print(paste(round(100*kk/rows, digits=2),"% done", sep=""))
        print(proc.time()-ptm)
        flush.console()}
      }}}
return(list(data,ncut))
}


sim.rvQCA<-function(qca.data, outcome="OUT", sim=100, ncut=4){
  
  qca.data<-read.csv("Gamson.csv")
  outcome="ACP"
  type="crisp"
  ncut=3
  sim=50
  
  ptm <- proc.time()
  inclcut<-seq(from=.5, to = 1, by=.01)
  pop<-dim(qca.data)[1]  #sample size
  rows<-sim*length(ncut)*length(inclcut)*length(pop)*2 #total number of rows
  out<-qca.data[,outcome]
  qca.data<-qca.data[,!(names(qca.data) %in% outcome)]
  data<-data.frame(CTH=0,CNTH=0,CPI=0,NTH=0,OUT=rep(0,rows))
  
  kk<-0 #set counter to 0
  
  for (j in 1:sim) {
    
    for (k in 1:length(inclcut)){
      
      for (n in ncut){
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        data[kk,4]<-pop
        
        s.qca.data<-qca.data
        
        for (i in 1:length(qca.data)){ #simulate random causal conditions
          prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
          s.qca.data[,i]<-sample(c(0,1),pop,prob=prob,replace=T)} 
        
        #simulate random outcome variable
        prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
        s.qca.data$OUT<-sample(c(0,1),pop,prob=prob,replace=T)
        
        ##########parsimonious
        
        parsimonious <- tryCatch( #trap error
          eqmcc(s.qca.data,  outcome=c("OUT"),  n.cut=n, incl.cut1=inclcut[k], include = "?",
                conditions= c(names(qca.data)),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-1
          data[kk,3]<-0
        }
        
        if(inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-0
          data[kk,3]<-0
        }
        
        
        ########complex
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        data[kk,4]<-pop
        
        parsimonious <- tryCatch( #trap error
          eqmcc(s.qca.data,  outcome=c("OUT"),  n.cut=ncut[n], incl.cut1=inclcut[k],
                conditions= c(names(qca.data)),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){  
          data[kk,5]<-1
          data[kk,3]<-1
        }
        
        if(inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-0
          data[kk,3]<-1
        }
        print(paste(round(100*kk/rows, digits=2),"% done", sep=""))
        print(proc.time()-ptm)
        flush.console()
      }}}
  return(data)
}


