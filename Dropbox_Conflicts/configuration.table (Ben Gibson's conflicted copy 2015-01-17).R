conf.table<-function(data, ncut=4){
#logistic regression predicting probability of returning a spurious relationship
  if (length(ncut)==1){suppressWarnings(modp<-glm(OUT ~ CTH + CPI, family="binomial", data=data))}
  if (length(ncut)>2){suppressWarnings(modp<-glm(OUT ~ CTH + CNTH + CPI, family="binomial", data=data))}
  
  
data$pred<-predict(modp,data, type="response")

data<- cbind(data, predict(modp, newdata = data, type = "link", se = TRUE))

data$UL<-plogis(data$fit - (1.96 * data$se.fit))
data$LL<-plogis(data$fit + (1.96 * data$se.fit))
                                    

df<-data.frame(p=c("p < .10","","p < .05","","p < .01","","p < .001",""),pc=rep(c("parsimonious","complex"),4),LL.incl.cut=rep(0,8),fitted.incl.cut=rep(0,8),
               UL.incl.cut=rep(0,8))
output<-list(df)[rep(1L, times=length(ncut))]

names(output)<-paste("ncut=",ncut,sep="")

plevels<-c(.10,.05,.01,.001,-Inf)
# & data$pred > plevels[(i +1)]]

for (q in 1:length(ncut)){
  j<-0
for (i in 1:4){

j<-j+1
output[[q]]$fitted.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$pred < plevels[i]],na.rm=T))
output[[q]]$UL.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$LL < plevels[i]],na.rm=T))
output[[q]]$LL.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$UL < plevels[i]],na.rm=T))

j<-j+1

output[[q]]$fitted.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$pred < plevels[i]],na.rm=T))
output[[q]]$UL.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$LL < plevels[i]],na.rm=T))
output[[q]]$LL.incl.cut[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$UL < plevels[i]],na.rm=T))

}

output[[q]][,3][output[[q]][,3] == Inf]<-NA

output[[q]]$fitted.incl.cut[output[[q]]$fitted.incl.cut == Inf] <- NA
output[[q]]$LL.incl.cut[output[[q]]$LL.incl.cut == Inf] <- NA
output[[q]]$UL.incl.cut[output[[q]]$UL.incl.cut == Inf] <- NA

}

return(output)
}