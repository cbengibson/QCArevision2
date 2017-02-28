sim.ltQCA <-
function(qca.data, outcome="OUT", conditions=c(""),sim=100, ncut=4){
ptm <- proc.time()

if (all(conditions == c(""))) {
  conditions <- names(qca.data[,!(names(qca.data) %in% outcome)])
}

inclcut<-seq(from=.5, to = 1, by=.01)
pop<-dim(qca.data)[1]  #sample size
rows<-sim*length(ncut)*length(inclcut)*length(pop)*2 #total number of rows
out<-qca.data[,outcome]
qca.data<-qca.data[,!(names(qca.data) %in% outcome)]
data<-data.frame(CTH=0,CNTH=0,CPI=0,NTH=0,OUT=rep(NA,rows))

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
                conditions= c(names(s.qca.data[,!(names(s.qca.data) %in% 'OUT')])),details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(parsimonious, "error")){
          #REAL WORK
          data[kk,5]<-1
          data[kk,3]<-0
        }
        
        if(grepl("Nothing to explain",parsimonious)[1]){
          #REAL WORK
          data[kk,5]<-0
          data[kk,3]<-0
        }
        
        
        ########complex
        
        kk<-kk+1
        
        data[kk,1]<-inclcut[k]
        data[kk,2]<-n
        data[kk,4]<-pop
        
        complex <- tryCatch( #trap error
          eqmcc(s.qca.data,  outcome=c("OUT"),  n.cut=n, incl.cut1=inclcut[k],
                conditions = c(names(s.qca.data[,!(names(s.qca.data) %in% 'OUT')])), details = TRUE, show.cases = TRUE),
          error=function(e) e
        )
        
        if(!inherits(complex, "error")){  
          data[kk,5]<-1
          data[kk,3]<-1
        }
        
        if(grepl("Nothing to explain",complex)[1]){
          data[kk,5]<-0
          data[kk,3]<-1
        }
        
        print(paste(round(100*kk/rows, digits=2),"% done", sep=""))
        print(proc.time()-ptm)
        flush.console()
      }}}
return(data)
}
