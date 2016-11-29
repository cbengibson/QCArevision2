
library(plyr)
load("final_data_set.Rdata")
df <- ldply(data.list, data.frame)
sam<-sample(1:length(df[,1]),length(df[,1])/3,replace=T)
df<-df[sam,]
tmp<-df[df$CTH==.8,]
counter<-0
prop<-matrix(nrow=5,ncol=2)
for (i in unique(tmp$dist)){
  counter<-counter+1
  prop[counter,1]<-i
  prop[counter,2]<-sum(tmp$OUT[tmp$dist==i]==1, na.rm=T)/length(tmp$OUT[tmp$dist==i])
}
prop


plot(prop[,1],prop[,2], type="l", ylim=c(0,1))


tmp<-df[df$CTH==.9,]
counter<-0
prop<-matrix(nrow=5,ncol=2)
for (i in unique(tmp$dist)){
  counter<-counter+1
  prop[counter,1]<-i
  prop[counter,2]<-sum(tmp$OUT[tmp$dist==i]==1, na.rm=T)/length(tmp$OUT[tmp$dist==i])
}
prop


lines(prop[,1],prop[,2], col="red")






tmp<-df[df$CTH==1,]
counter<-0
prop<-matrix(nrow=5,ncol=2)
for (i in unique(tmp$dist)){
  counter<-counter+1
  prop[counter,1]<-i
  prop[counter,2]<-sum(tmp$OUT[tmp$dist==i]==1, na.rm=T)/length(tmp$OUT[tmp$dist==i])
}
prop


lines(prop[,1],prop[,2], col="blue")
