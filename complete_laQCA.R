####FREQUENTIST ROBUSTNESS ASSESSMENT FOR QCA####

ltQCA<-function(mod, sim=2000, verbose=T, include=c(""), row.dom=F, all.sol=F, omit=c(), dir.exp=c(), conv.diag=F  ){
  
  ################################################################################################################################
  # 0) set-up: configure the parameters set by the researcher, and the data used from the analysis, directly from the QCA object #
  ################################################################################################################################
  
  require(QCA) #need this to run QCA, obviously
  require(bootstrap) #need this for standard errors
  ptm <- proc.time() #this is to track the time it's taking to run the analysis
  
  s.qca.data<-do.call("list", replicate(sim, matrix(nrow=20,ncol=4), simplify = FALSE)) #create a list of "zero-matrices" to pre-allocate memory in later steps
  type="crisp"
  
  #########################################################################################################################
  # 1) simulation of a list of data sets with variables that match exactly the distributions of the researcher's data set #
  #########################################################################################################################
  
  for (j in 1:sim) {   #for j in 1:how many simulations the researcher specified (default is 2000 simulations)

    if (type=="crisp"){ #have to specify
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
       # prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
        s.qca.data[[j]][,i]<-rep(c(0,1),each=10)} 
      #simulate random outcome variable
      #prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
      s.qca.data[[j]]$OUT<-rep(c(0,1),each=10)
    }
    
    if (type == "fuzzy"){
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
        ranges<-seq(from=0.1, to=1, by=.1) 
        prob<-hist(qca.data[,i])[[2]]/dim(qca.data)[1]
        s.qca.data[[j]][,i]<-sample(ranges,pop,prob=prob,replace=T)
      } 
      #simulate random outcome variable
      prob<-hist(out)[[2]]/length(out)
      s.qca.data[[j]]$OUT<-sample(ranges,pop,prob=prob,replace=T)
    }
  }
  
  ####################################################################################################################################
  # 2a) run a qca analysis on each simulated data set in turn, and trap the result in a list (should have sim number of qca analyses #
  ####################################################################################################################################
  
  
  confList <- sapply(s.qca.data, function(x){tryCatch( #trap error
    eqmcc(x,  outcome=c("OUT"),  n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$opts$explain,
          include=include, row.dom=row.dom, all.sol=all.sol, omit=omit, dir.exp=dir.exp,
          conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    error = function(e) e
  )})
  
  
  ######################################################################################
  # 2b) decide if the qca actually returned a configuration, and aggregate the results #
  ######################################################################################
  
  pars<-rep(NA, sim) #make sure to add an NA for any other error that may show up 
  pars[sapply(confList, function(x) !inherits(x,"error"))]<-1 #if no error, it returned a 1 = returned a configuration from random data (not good)
  errs<-sapply(confList, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x)) #if an error, it returned a 0 = didn't return a configuration from random data (very good)
  pars[sapply(errs, function(x) sum(x))==1]<-0 #actually apply the 0 for the errors
  
  results<-mean(pars, na.rm=T) #this returns the proportion of times qca returned a random result (which is interpreted as a psuedo-p-value in this analyis)
  
  #######################################################################################################################################################################
  # 3) use bootstrapping to get a measure of standard deviation (this maybe could also just be done by taking the standard deviation, but this is more robust, I think) #
  #######################################################################################################################################################################
  
  bsp<-bootstrap(pars,mean,nboot=1000, na.rm=T)[[1]]
  confInt<-quantile(bsp, prob=c(0.025, .975), na.rm=T) 
  
  
  #return the result
  
  returnme<-list(results, confInt)
  names(returnme)<-c("Probability of Randomness","Confidence Interval") #could be prettier.
  return(returnme)

}

rvQCA<-function(mod, sim=2000, verbose=T, include=c(""), row.dom=F, all.sol=F, omit=c(), dir.exp=c() ){
  require(QCA)
  require(bootstrap)
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
  
  rows<-sim #total number of rows
  out<-qca.data[,outcome] #outcome vector
  qca.data<-qca.data[,(names(qca.data) %in% conditions)] #matrix of causal conditions
  data<-data.frame(cname=0,OUT=rep(NA,rows)) #empty data set to simulate into
  
  splitted<-list()
  
  for (solnum in 1:length(mod$solution)){
  splitted[[solnum]]<-strsplit(mod$solution[[solnum]], "[*]")
  splitted[[solnum]]<-unlist(strsplit(as.character(splitted[[solnum]][[1]]), "[+]"))
  }
  
  splitted<-unlist(splitted)
  splitted<-gsub(" ","", splitted , fixed=TRUE)
  splitted<-unique(splitted)

  if (sum(outcome > 0 && outcome < 1) == 0){
    type = "crisp"}
  
  if (sum(outcome > 0 && outcome < 1) != 0){
    type = "fuzzy"}
  

  results<-vector()
  confInt<-list()
  
  for (cond in splitted){
    s.qca.data<-do.call("list", replicate(sim, qca.data, simplify = FALSE))
    
  
    if (type=="crisp"){
      for (j in 1:sim) {
      
      #simulate random causal condition
        prob<-c(sum(qca.data[,(grep(cond, names(qca.data), ignore.case=T))])/(dim(qca.data)[1]), 
                1-sum(qca.data[,(grep(cond, names(qca.data), ignore.case=T))])/(dim(qca.data)[1])) #match distributions of data set
        s.qca.data[[j]][,(grep(cond, names(qca.data), ignore.case=T))]<-sample(c(0,1),pop,prob=prob,replace=T)
        s.qca.data[[j]]$OUT<-out
    }} 
    
    if (type == "fuzzy"){
      for (j in 1:sim) {
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
        ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop
        prob<-hist(qca.data[,i])[[2]]/dim(qca.data)[1]
        s.qca.data[[j]][,i]<-sample(ranges,pop,prob=prob,replace=T)
      } 
      #simulate random outcome variable
      prob<-hist(out)[[2]]/length(out)
      s.qca.data[[j]]$OUT<-sample(ranges,pop,prob=prob,replace=T)
    }
  }
  
  confList <- sapply(s.qca.data, function(x){tryCatch( #trap error
    eqmcc(x,  outcome=c("OUT"),  n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$opts$explain,
          include=include, row.dom=row.dom, all.sol=all.sol, omit=omit, dir.exp=dir.exp, show.details=F,
          conditions= c(names(x[,!(names(x) %in% 'OUT')])))[[4]],
    error=function(e) e
  )})
  
  pars<-rep(NA, sim)
  pars[sapply(confList,function(x) grepl(cond,as.character(x[[1]]), ignore.case=F))]<-1
  pars[is.na(pars)]<-0
  
  results[cond]<-mean(pars, na.rm=T)
  
  bsp<-bootstrap(pars,mean,nboot=1000)[[1]]
  confInt[[cond]]<-quantile(bsp, prob=c(0.25, .975),na.rm=T) 
  
  #this is where I should assign values to a ltQCA "class"
  
}
returnme<-list(results, confInt)
names(returnme)<-c("Probability","Confidence Interval")
if (conv.diag==F){return(returnme)}
if (conv.diag==T){return(pars)}

#returned<-mod$IC$incl.cov

}

