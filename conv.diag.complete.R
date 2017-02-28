cdQCA<-function(mod, trials=6, sim=2000, verbose=T, include=c(""), row.dom=F, all.sol=F, omit=c(), dir.exp=c() ){
  require(QCA)
  require(bootstrap)
  ptm <- proc.time()
  
  resultmat<-matrix(ncol=trials, nrow=sim)
  bsp<-matrix(ncol=trials, nrow=sim)
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
  
  for (trial in 1:trials){
  
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
  
  confList <- sapply(s.qca.data, function(x){tryCatch( #trap error
    eqmcc(x,  outcome=c("OUT"),  n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$opts$explain,
          include=include, row.dom=row.dom, all.sol=all.sol, omit=omit, dir.exp=dir.exp,
          conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    error=function(e) e
  )})
  
  pars<-rep(NA, sim)
  pars[sapply(confList, function(x) !inherits(x,"error"))]<-1
  errs<-sapply(confList, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
  pars[sapply(errs, function(x) sum(x))==1]<-0
  

 # resultmat[,trial]<-sapply(seq(from=10,to=sim,by=10), function(x) mean(sample(pars, size=x, replace=F), na.rm=T))
 resultmat[,trial]<-sapply(1:sim, function(x) mean(pars[1:x], na.rm=T))
 confInt<-list()
 
 bsp<-sapply(1:sim, function(x) bootstrap(pars[1:x],mean,nboot=100)[[1]])
 
 for (i in 1:length(bsp[1,])){
   confInt[[i]]<-quantile(bsp[,i], prob=c(0.05, .95),na.rm=T)
 }
 

dist<-sapply(1:length(confInt), function(x) confInt[[x]][2] - confInt[[x]][1])
#plot(dist) 


  
}


Rhat1 <- function(mat) {
  m <- ncol(mat)
  n <- nrow(mat)
  b <- apply(mat,2,mean)
  B <- sum((b-mean(mat))^2)*n/(m-1)
  w <- apply(mat,2,var)
  W <- mean(w)
  s2hat <- (n-1)/n*W + B/n
  Vhat <- s2hat + B/m/n 
  covWB <- n /m * (cov(w,b^2)-2*mean(b)*cov(w,b))
  varV <- (n-1)^2 / n^2 * var(w)/m +
    (m+1)^2 / m^2 / n^2 * 2*B^2/(m-1) +
    2 * (m-1)*(n-1)/m/n^2 * covWB
  df <- 2 * Vhat^2 / varV
  R <- sqrt((df+3) * Vhat / (df+1) / W)
  return(R)
}

Rhat <- function(arr) {
  dm <- dim(arr)
  if (length(dm)==2) return(Rhat1(arr))
  if (dm[2]==1) return(NULL)
  if (dm[3]==1) return(Rhat1(arr[,,1]))
  return(apply(arr,3,Rhat1))
}

Rhatresults<-vector()
for (simsz in 1:dim(resultmat)[1]){
Rhatresults[simsz]<-tryCatch(Rhat1(resultmat[1:simsz,]), error=function(e) e)
}
Rhatresults<-as.numeric(unlist(Rhatresults))
names(Rhatresults)<-paste("sim=", 1:sim, sep="")


return(list(Rhatresults,dist,resultmat))

}





