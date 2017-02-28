  

  range01<-function(x){(x-min(x))/(max(x)-min(x))}
  library(xtable)
  library(MASS)
  library(plyr)
  
  dvdists<-seq(.1,.9, by=.2) #make sure this mataches QCA_plots.R
  dists<-seq(.5,.9, by=.1) #make sure this mataches QCA_plots.R
  
  counter<-0
  master.results<-vector(mode="list")
  for (i in dists){
    counter<-counter+1
    load(paste("data.list_",i,".Rdata",sep=""))
    master.results[[counter]]<-data.list
  }

  df <- ldply (master.results[[1]], data.frame)
  
  for (i in 2:length(master.results)){
    df<-rbind(df, ldply (master.results[[1]], data.frame))
  }
  save(df,file="final_data_set2.Rdata")
  load("final_data_set2.Rdata")
  #df <- ldply(data.list, data.frame)
  #df<-df[df$sam.size<=50,]
  #sam<-sample(1:length(df[,1]),length(df[,1])/10,replace=T)
  
  #df<-df[sam,]
  #mod2<-glm(OUT~NTH+nvar+dvdist, data=df, family="binomial")
  mod1<-glm(OUT~CTH+CNTH+CPI+NTH+nvar+dvdist, data=df, family="binomial")
  mod<-glm(OUT~(CTH+CNTH+CPI+NTH+nvar+dvdist)*(CTH+CNTH+CPI+NTH+nvar+dvdist), data=df, family="binomial")
  #dist+ *CTH+dvdist*CNTH+ nvar*CTH + nvar*CNTH + NTH*CTH + NTH*CNTH

  mod1<-stepAIC(mod1)
  save(list=ls(),file="QCA_plots_data.Rdata")
  summary(mod)
  xtable(mod, digits=2)
  df$pred<-plogis(predict(mod, newdata = df , type = "link", se=T)[[1]])
  #lines(plogis(pred$fit + (2*pred$se.fit)), type="l", col=colors[i],lwd=6, lty=3) #confidence interval
  #lines(plogis(pred$fit - (2*pred$se.fit)), type="l", col=colors[i],lwd=6, lty=3)
  
  
  

  #df$pred[df$dvdist==.5 & df$CNTH==6 & df$CTH==.9]
  
  library(RColorBrewer)

  colors<-brewer.pal(11,name="BrBG")[c(9,10,11,9,10,11)]
  colors[c(1,2,3)]<-brewer.pal(11,name="RdYlGn")[c(9,10,11)]
  colors<-gray.colors(3,start=0,end=.3)
  colors<-c(colors,colors)
  #colors<-1:10
  png("Plots/regression_plots.png", width=960)
  par(mar=c(9,8,4,4) +.01, mfrow=c(1,2))

  #plot for researcher choice 
  i<-1
  df$CNTH.stan<-range01(df$CNTH)
  plot(aggregate(pred~CNTH.stan, df, mean ), type="l", ylim=c(0,.8), col=colors[i],lwd=6,
       main="Predicted Effects of Researcher Choice on Spuriousness",xlab="",ylab="Exp. Probability of a Spurious Result",
       axes=F, cex.main=1.3, cex.lab=1.5)
  axis(2,seq(0,1,by=.2),las=1, lwd=4, cex.axis=1.2)
  axis(1,aggregate(pred~CNTH.stan, df, mean )[[1]],labels=aggregate(pred~CNTH, df, mean )[[1]],line=1,  lwd=4,cex.axis=1, col=colors[i],col.ticks=colors[i],col.axis=colors[i],tick=T)
  mtext("Configurational N",1,line=1,at=-0.20,col=colors[i],font=2)
  
  i<-2
  df$CTH.stan<-round(range01(df$CTH), digits=1)
  df$CTH.round<-round(df$CTH, digits=1)
  lines(aggregate(pred~CTH.stan, df, mean ), type="l", ylim=c(0,.8), col=colors[i],lwd=4,lty=2)
  axis(1,aggregate(pred~CTH.stan, df, mean )[[1]],labels=seq(.5,1,by=.05),line=3.3,  lwd=4,lty=2,cex.axis=1, col=colors[i],col.ticks=colors[i],col.axis=colors[i],tick=T)
  mtext("Cons. Threshold",1,line=3.3,at=-0.20,col=colors[i],font=2)
  
  i<-3
  
  lines(aggregate(pred~abs(CPI-1), df, mean ), type="l", ylim=c(0,.8), col=colors[i],lwd=4,lty=3)
  axis(1,c(0,1),labels=c("Parsimonious","Complex"),line=6.,  lwd=4,lty=3,cex.axis=1, col=colors[i],col.ticks=colors[i],col.axis=colors[i],tick=T)
  mtext("Pars./Comp.",1,line=6.,at=-0.20,col=colors[i],font=2)
  
  
  
  
  #data structure 
  i<-4
  df$sam.size.stan<-range01(df$sam.size)
  plot(aggregate(pred~sam.size.stan, df, mean ), type="l", ylim=c(0,.8), col=colors[i],lwd=6,
       main="Predicted Effects of Data Structure on Spuriousness",xlab="",ylab="Exp. Probability of a Spurious Result",
       axes=F, cex.main=1.3, cex.lab=1.5)
  axis(2,seq(0,1,by=.2),las=1, lwd=4, cex.axis=1.2)
  axis(1,aggregate(pred~sam.size.stan, df, mean )[[1]],labels=aggregate(pred~sam.size, df, mean )[[1]],line=1,  lwd=4, cex.axis=1, col=colors[i],col.ticks=colors[i],col.axis=colors[i],tick=T)
  mtext("Sample Size",1,line=1,at=-0.14,col=colors[i],font=2)
  
  i<-5
  df$nvar.stan<-range01(df$nvar)
  lines(aggregate(pred~nvar.stan, df, mean ), type="l", ylim=c(0,.2), col=colors[i],lwd=4,lty=2)
  axis(1,aggregate(pred~nvar.stan, df, mean )[[1]],labels=aggregate(pred~nvar, df, mean )[[1]],line=3.5,col=colors[i],col.ticks=colors[i],col.axis=colors[i], lwd=4,lty=2)
  mtext("Num. Vars.",1,line=3.3,at=-0.14,col=colors[i],font=2)
  
  i<-6
  df$dvdist.stan<-range01(df$dvdist)
  lines(aggregate(pred~dvdist.stan, df, mean ), type="l", col=colors[i],lwd=4,lty=3)
  axis(1,aggregate(pred~dvdist.stan, df, mean )[[1]],labels=aggregate(pred~dvdist, df, mean )[[1]],line=6,col=colors[i],col.ticks=colors[i],col.axis=colors[i], lwd=4,lty=3)
  mtext("Distribution",1,line=6,at=-0.14,col=colors[i],font=2)
  
  dev.off()
  
  
  xtable(aggregate(pred~CTH.round+CNTH+dvdist+sam.size, df, mean ),digits=2,include.rownames=F)
  
  df$CPI.lab<-NA
  df$CPI.lab[df$CPI==1]<-"Parsimonious"
  df$CPI.lab[df$CPI==0]<-"Complex"
  
  
png("Plots/interaction_plots_dvdist.png",width=960,height=760)
  
  par(mar=c(5,5,4,3) +.01, mfrow=c(2,2))
  
  baseline<-aggregate(pred~dvdist,df,mean)

  plot(aggregate(pred~dvdist,df,mean), type="l",main="Predicted Spuriousness versus Outcome Dist.",xlab="Outcome Distribution",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7,  cex.axis=1.7, ylim=c(0,.84))
  
  interaction.plot(df$dvdist, df$CTH.round, df$pred,main="Interaction between Outcome Dist. and Cons. Score Threshold",xlab="Outcome Distribution",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84), 
                   trace.label ="Cons. Score")
 # matlines(baseline$pred, type="l", col="red") #grayscale sort of negates this
  
  interaction.plot(df$dvdist, df$CNTH, df$pred,main="Interaction between Outcome Dist. and Conf. N Threshold",xlab="Outcome Distribution",ylab="Exp. Probability of a Spurious Result",
                    cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84),
                   trace.label ="Conf. N")
 # matlines(baseline$pred, type="l",  col="red")
  
  interaction.plot(df$dvdist, df$CPI.lab, df$pred,main="Interaction between Outcome Dist. and Complex/Pars. Solutions",xlab="Outcome Distribution",ylab="Exp. Probability of a Spurious Result",
                    cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84), 
                   trace.label ="")
 # matlines(baseline$pred, type="l", col="red")
  
dev.off()
  
  
  

png("Plots/interaction_plots_nvar.png",width=990,height=760)
  
  par(mar=c(5,5,4,3) +.01, mfrow=c(2,2))
  
  baseline<-aggregate(pred~nvar,df,mean)

  plot(aggregate(pred~nvar,df,mean), type="l",main="Predicted Spuriousness versus Num. of Variables",xlab="Number of Causal Conditions",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, ylim=c(0,.94))
  
  interaction.plot(df$nvar, df$CTH.round, df$pred,main="Interaction between Num. of Variables and Cons. Threshold",xlab="Number of Causal Conditions",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.94), 
                   trace.label ="Cons. Score")
  #matlines(baseline$pred, type="l", col="red")
  
  interaction.plot(df$nvar, df$CNTH, df$pred,main="Interaction between Num. of Variables and Conf. N Threshold",xlab="Number of Causal Conditions",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.94), 
                   trace.label ="Conf. N")
  #matlines(baseline$pred, type="l",  col="red")
  
  interaction.plot(df$nvar, df$CPI.lab, df$pred,main="Interaction between Num. of Variables and Complex/Pars. Solutions",xlab="Number of Causal Conditions",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.94), 
                   trace.label ="")
  #matlines(baseline$pred, type="l", col="red")
  
dev.off()
  
  
  
png("Plots/interaction_plots_sam_size.png",width=960,height=760)
  
  par(mar=c(5,5,4,3) +.01, mfrow=c(2,2))
  
  baseline<-aggregate(pred~sam.size,df,mean)

  plot(aggregate(pred~sam.size,df,mean), type="l",main="Predicted Spuriousness versus Sample Size",xlab="Sample Size",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, ylim=c(0,.84))
  
  interaction.plot(df$sam.size, df$CTH.round, df$pred,main="Interaction between Sample Size and Cons. Threshold",xlab="Sample Size",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84), 
                   trace.label ="Cons. Score")
  #matlines(baseline$pred, type="l", col="red")
  
  interaction.plot(df$sam.size, df$CNTH, df$pred,main="Interaction between Sample Size and Conf. N Threshold",xlab="Sample Size",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84), 
                   trace.label ="Conf. N")
 # matlines(baseline$pred, type="l",  col="red")
  
  interaction.plot(df$sam.size, df$CPI.lab, df$pred,main="Interaction between Sample Size and Complex/Pars. Solutions",xlab="Sample Size",ylab="Exp. Probability of a Spurious Result",
                   cex.main=1.4, cex.lab=1.7, cex.axis=1.7, lwd=1.5, ylim=c(0,.84), 
                   trace.label ="")
  #matlines(baseline$pred, type="l", col="red")
  
dev.off()
  
  
  
  
#create final table
  
  xt1<-xtable(mod1)
  xt2<-xtable(mod)
  varnames<-c("Intercept","Cons. Threshold","Conf. N Threshold","Complex Solution","Sample Size","Num. Variables","Dependent Variable Dist.")
  mod1xt<-as.data.frame(cbind(varnames,paste(round(xt1[[1]],digits=2),"(",round(xt1[[2]],digits=2),")","***",sep=""))) #mod1
  
  xt2
  intnames<-cbind(rep(varnames[-1],each=length(varnames[-1])),varnames[-1])
  intnames<-intnames[intnames[,1]!=intnames[,2],]
  removeme<-vector()
  for(i in 5:length(intnames[,1])){
    temp<-intnames[1:(i-1),]
    if(sum(paste(intnames[i,2],intnames[i,1])==paste(temp[,1],temp[,2]))>0){removeme<-c(removeme,i)}
  }
  intnames<-intnames[-removeme,]
  intnames<-paste(intnames[,1],intnames[,2],sep=" * ")
  intnames<-unique(intnames)
  intnames<-c(varnames,intnames)
  printme<-as.data.frame(cbind(intnames,c(paste(round(xt1[[1]],digits=2),"(",round(xt1[[2]],digits=2),")","***",sep=""),rep(NA,15)),paste(round(xt2[[1]],digits=2),"(",round(xt2[[2]],digits=2),")","***",sep="")))
  colnames(printme)<-c("Variable","Model 1","Model 2")
  print(xtable(printme),include.rownames=FALSE)
  
  
  df$pred[df$CTH==1 & df$CNTH==3 & df$CPI == 0 & df$NTH==20 & df$nvar==5 & df$dvdist==.7]
  df$pred[df$CTH==1 & df$CNTH==3 & df$CPI == 0 & df$NTH==20 & df$nvar==5 & df$dvdist==.5]
  df$pred[df$CTH==1 & df$CNTH==3 & df$CPI == 0 & df$NTH==20 & df$nvar==5 & df$dvdist==.5][1]
  
    idtypes=data.frame("Consistency Score"=1,"Configurational N Threshold"=3,"N"=20,"Number of Variables"=5,"Dependent Variable Dist."=.5,"Predicted p"=  df$pred[df$CTH==1 & df$CNTH==3 & df$CPI == 0 & df$NTH==20 & df$nvar==5 & df$dvdist==.5][1])
    combs<-expand.grid(c(0.8,.9,1),CNTH=2:5,seq(10,30,10),3:5,seq(.1,.9,.4))
    combs$Predicted.p<-NA
    names(combs)<-names(idtypes)
    for (i in 1:dim(combs)[1]){
      combs$Predicted.p[i]<-c(df$pred[df$CTH==combs$Consistency.Score[i] & df$CNTH==combs$Configurational.N.Threshold[i] & df$CPI == 0 & df$NTH==combs$N[i] & df$nvar==combs$Number.of.Variables[i] & df$dvdist==combs$Dependent.Variable.Dist.[i]][1])
    }
    
  combs[126,]
  print(xtable(combs),include.rownames=F)
  
  idtypes[2,]<-c(  df$pred[df$CTH==1 & df$CNTH==3 & df$CPI == 0 & df$NTH==20 & df$nvar==5 & df$dvdist==.5][1])
  
  
  
  #i<-4
  #plot(exp(sum(mod$coef[c(1:6)])-mod$coef[[i]] + mod$coef[[i]]*unique(quantile(df[,i])))~c(1,5), type="l", ylim=c(0,.2), col=colors[i],lwd=6,
  #     main="Relative Effects of Researcher Choice on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  #for (i in c(2:3)){
  #  lines(exp(sum(mod$coef[c(1:6)])-mod$coef[[i]] + mod$coef[[i]]*quantile(df[,i])), type="l", col=colors[i], lwd=6)
  #}
  
  #plot for data structure
  
  #i<-5
  #plot(exp(sum(mod$coef[c(1:6)])-mod$coef[[i]] + mod$coef[[i]]*unique(quantile(df[,i]))), type="l", ylim=c(0,.2), col=colors[i],lwd=6,
  #     main="Relative Effects of Data Structure on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  #lines(exp(sum(mod$coef[c(1:6)])-mod$coef[[i]] + mod$coef[[i]]*unique(quantile(df[,i]))), type="l", ylim=c(0,.2), col=colors[i],lwd=6,
  #      main="Relative Effects of Data Structure on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  
  #for (i in c(6:7)){
  #  lines(exp(sum(mod$coef[c(1:6)])-mod$coef[[i]] + mod$coef[[i]]*quantile(df[,i])), type="l", col=colors[i], lwd=6)
  #}
  
  #plot for data structure
  
  #i<-5
  #exp(sum(mod$coef[c(1:7)][c(-1,-i)] * apply(df,2, mean)[c(-1,-i)]) + mod$coef[[i]]*unique(quantile(df[,i]))/(1-sum(mod$coef[c(1:7)][c(-1,-i)] * apply(df,2, mean)[c(-1,-i)]) + mod$coef[[i]]*unique(quantile(df[,i]))))
  #predict(mod)
  
  #pred<-cbind(df,predict(mod, newdata = df, type = "link", se=T))
  
  #newdata3 <- within(pred, {
  #  PredictedProb <- plogis(fit)
  #  LL <- plogis(fit - (1.96 * se.fit))
  #  UL <- plogis(fit + (1.96 * se.fit))
  #})
  
  
  #df2<-(data.frame(t(colMeans(df))))
  #i<-5
  #df3<-cbind(df2[,-i],quantile(df[,i]))
  #names(df3)[length(df3)]<-names(df)[i]
  #pred<-predict(mod, newdata = df3 , type = "link", se=T)
  #plot(plogis(pred$fit),ylim=c(0,.8), col=colors[i],lwd=6, type="l",
  #     main="Relative Effects of Researcher Choice on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  
  #for (i in 6:7){
  #  df3<-cbind(df2[,-i],quantile(df[,i]))
  #  names(df3)[length(df3)]<-names(df)[i]
  #  pred<-predict(mod, newdata = df3 , type = "link", se=T)
  #  lines(plogis(pred$fit), type="l", ylim=c(0,.2), col=colors[i],lwd=6,
  #        main="Relative Effects of Data Structure on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
    
  #}
  
    
  #i<-3
  #df3<-cbind(df2[,-i],quantile(df[,i]))
  #names(df3)[length(df3)]<-names(df)[i]
  #pred<-predict(mod, newdata = df3 , type = "link", se=T)
  #plot(plogis(pred$fit)[3:4]~c(0,5),ylim=c(0,.8), col=colors[i],lwd=6, type="l",
  #     main="Relative Effects of Researcher Choice on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  
  #for (i in 2:3){
  #  df3<-cbind(df2[,-i],quantile(df[,i]))
  #  names(df3)[length(df3)]<-names(df)[i]
  #  pred<-predict(mod, newdata = df3 , type = "link", se=T)
  #  lines(plogis(pred$fit), type="l", ylim=c(0,.2), col=colors[i],lwd=6,
  #        main="Relative Effects of Data Structure on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  #}
  
  
  
  #i<-4
  #df3<-cbind(df2[,-i], seq(0,1,.20))
  #names(df3)[length(df3)]<-names(df)[i]
  #pred<-predict(mod, newdata = df3 , type = "link", se=T)
  #plot(plogis(pred$fit)[1:6]~c(1:6),ylim=c(0,.8), col=colors[i],lwd=6, type="l",
  #     main="Relative Effects of Researcher Choice on the Odds of a Spurious Result",xlab="Quantile",ylab="Odds")
  
