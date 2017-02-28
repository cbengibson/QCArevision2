

################################################################################################################################
# 0) set-up: configure the parameters set by the researcher, and the data used from the analysis, directly from the QCA object #
################################################################################################################################


require(QCA) #need this to run QCA, obviously
require(bootstrap) #need this for standard errors
ptm <- proc.time() #this is to track the time it's taking to run the analysis

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

s.qca.data<-do.call("list", replicate(sim, data.frame(AA=rep(0,20),BB=0,CC=0,DD=0), simplify = FALSE)) #create a list of "zero-matrices" to pre-allocate memory in later steps
#########################################################################################################################
# 1) simulation of a list of data sets with variables that match exactly the distributions of the researcher's data set #
#########################################################################################################################

for (j in 1:sim) {   #for j in 1:how many simulations the researcher specified (default is 2000 simulations)
  
  if (type=="crisp"){ #have to specify
    
    for (i in 1:length(s.qca.data[[1]])){ #simulate random causal conditions
      # prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
      s.qca.data[[j]][,i]<-rep(c(0,1),each=10)} 
    #simulate random outcome variable
    #prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
    s.qca.data[[j]]$OUT<-rep(c(0,1),each=10)
  }
  
  if (type == "fuzzy"){
    
    for (i in 1:length(s.qca.data[[1]])){ #simulate random causal conditions
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
        conditions= c(names(x[,!(names(x) %in% 'OUT')])))[[8]],
  error = function(e) e
)})


######################################################################################
# 2b) decide if the qca actually returned the correct configuration, and aggregate the results #
######################################################################################

confList[[1]]=="AA*BB*CC*DD"

