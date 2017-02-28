dvdists
counter<-0
master.results<-vector(mode="list")
for (i in dists){
  counter<-counter+1
load(paste("data.list_",i,".Rdata",sep=""))
master.results[[counter]]<-data.list
}

library (plyr)
df <- ldply (master.results[[1]], data.frame)

for (i in 2:length(master.results)){
  df<-rbind(df, ldply (master.results[[1]], data.frame))
}
save(df,file="final_data_set2.Rdata")
