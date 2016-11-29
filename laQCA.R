laQCA<-function(mod, sim=2000, verbose=T, include=c(""), row.dom=F, all.sol=F, omit=c(), dir.exp=c() ){
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
  
  rows<-sim*2 #total number of rows
  out<-qca.data[,outcome] #outcome vector
  qca.data<-qca.data[,(names(qca.data) %in% conditions)] #matrix of causal conditions
  data<-data.frame(cname=0,OUT=rep(NA,rows)) #empty data set to simulate into
  
  if (sum(outcome > 0 && outcome < 1) == 0){
    type = "crisp"}
  
  if (sum(outcome > 0 && outcome < 1) != 0){
    type = "fuzzy"}
  
  s.qca.data<-do.call("list", replicate(sim, qca.data, simplify = FALSE))
  
  for (j in 1:sim) {
    
    if (type=="crisp"){
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
        prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
        s.qca.data[[j]][,i]<-sample(c(0,1),pop,prob=prob,replace=T)} 
      #simulate random outcome variable
      prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
      s.qca.data[[j]]$OUT<-sample(c(0,1),pop,prob=prob,replace=T)
    }
    
    if (type == "fuzzy"){
      
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
  
  suppressWarnings(confList <- sapply(s.qca.data, function(x){tryCatch( #trap error
    eqmcc(x,  outcome=c("OUT"),  n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$opts$explain,
          conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    error=function(e) e
  )}))
  
  pars<-rep(NA, sim)
  pars[sapply(confList, function(x) !inherits(x,"error"))]<-1
  pars[sapply(confList, function(x) inherits(x,"error"))]<-0
  #errs<-sapply(confList, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
  #pars[sapply(errs, function(x) sum(x))==1]<-0
  
  results<-mean(pars, na.rm=T)
  
  bsp<-bootstrap(pars,mean,nboot=1000)[[1]]
  confInt<-quantile(bsp, prob=c(0.05, .95)) 
  
  returnme<-list(results, confInt)
  names(returnme)<-c("Probability","Confidence Interval")
  return(returnme)
  
  #this is where I should assign values to a ltQCA "class"
  
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
    confInt[[cond]]<-quantile(bsp, prob=c(0.05, .95)) 
    
    #this is where I should assign values to a ltQCA "class"
    
  }
  returnme<-list(results, confInt)
  names(returnme)<-c("Probability","Confidence Interval")
  return(returnme)
  
  #returned<-mod$IC$incl.cov
  
}