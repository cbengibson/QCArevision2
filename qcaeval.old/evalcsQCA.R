rm(list=ls())

sim<-6


data1<-list()

load("qcaSim5.Rdata")
data1[[1]]<-data
for (i in 5:sim){
  load(paste("qcaSim",i,".Rdata",sep=""))
  j<-i+1
  data1[[j]]<-data
}

library(plyr)
df<-ldply(data1,data.frame)

df<-df[df$CTH!=0,]

library("QCA")
outcome<-"OUT"
conditions<-c(names(data)[1:4])
cons<-.95 # set consistency score threshold
type = "fuzzy"





##############################################################################################
##############################################################################################
#################                 CALIBRATE            #######################################
##############################################################################################
##############################################################################################


data<-df
summary(data)

par(mfrow=c(2,3))  

for (i in 1:length(data[1,])){
  hist(data[,i], main=names(data)[i])
}

par(mfrow=c(1,1))
#pdf("plot.big.pdf")
#plot(data)
#dev.off()

type = "fuzzy"

par(mfrow=c(2,2)) 

th<-list()


for (i in 1:4){
  
  #i<-5
  # base variable; random draw from standard normal distribution
  x <- data[,i]
  
  # calibration thresholds
  th[i] <- quantile(x, seq(from = 0.10, to = .90, length = 5))
  
  
  #datacal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]))
  
  #plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5])), 
  #    ylab = "Fuzzy Set Membership", xlab=names(data)[i])
}

datacal<-list()


i<-1 #consistency thresh
#th<-thresh[[i]]
x <- data[,i]

hist(data$OUT)
datacal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(.65, .75, .85))



#plot(data[,i], calibrate(x, type = "fuzzy", thresholds = c(.7, .8, .95)), 
#     ylab = "Fuzzy Set Membership", xlab=names(data)[i])


i<-2 #n thresh
#th<-thresh[[i]]
x <- data[,i]
datacal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(2, 5, 9))

#plot(data[,i], calibrate(x, type = "fuzzy", thresholds = c(2, 5, 9)), 
#     ylab = "Fuzzy Set Membership", xlab=names(data)[i])


i<-4 #pop.sam.size
#th<-thresh[[i]]
x <- data[,i]
datacal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(150, 300, 470))

#plot(data[,i], calibrate(x, type = "fuzzy", thresholds = c(20, 50, 100)), 
#     ylab = "Fuzzy Set Membership", xlab=names(data)[i])





data2<-matrix(nrow=length(data[,1]),ncol=5)

for (i in c(1:2,4)){
  
  data2[,i]<-datacal[[i]]}

data2<-as.data.frame(data2)
names(data2)<-c(names(data))
#plot(data2)



data2[,3]<-data[,3]
data2[,5]<-data[,5]

counts<-as.data.frame(matrix(nrow=8,ncol=5))


for (j in 1:length(data)){
  levelscc<-unique(data[,j])
  k<-0
for (i in levelscc){
  k<-k+1
  temp<-subset(data, data[,j]==i,select=OUT)
counts[k,j] <- (sum(temp))/(length(temp[,1]))
}}


names(counts)<-names(data)
plot(counts)

par(mfrow=c(2,2)) 

xlab<-c("Consistency Threshold","Configurational N","Parismonious versus Complex","Sample Size")
main=c("Prob. Specifying a Random Configuration across Consistency Thresholds",
       "Prob. Specifying a Random Configuration across Configurational Ns",
       "Prob. between Pars(0) versus Complex(1) Solutions",
       "Prob. Specifying a Random Configuration across Sample Sizes")

for (i in 1:4){
barplot(counts[,i][!is.na(counts[,i])], names.arg=c(unique(data[,i])), 
        xlab=xlab[i], ylab="Probability", main=main[i])
}
##############################################################################################
##############################################################################################
#################                 PRE SHIT             #######################################
##############################################################################################
##############################################################################################


###############################
#shared antecendent conditions#
###############################

superSubset(data2, outcome = "OUT", neg.out = T, conditions = c(names(data2)[1:4]), 
            relation = "nec", incl.cut = .90)

superSubset(data2, outcome = outcome, neg.out = F, conditions = c(names(data2)[1:4]), 
            relation = "nec", incl.cut = .95, cov.cut = 0)


########################################################
# makes crosstabs of all 2x2 combinations of variables #
########################################################

mytables<-list()
temp<-list()
k<-0

for (i in 1:length(data2)){
  for (j in 1:length(data2)){
    if (i != j){
      k<-k+1
      mytables[[k]] <- prop.table(table(data2[,i],data2[,j]),1) # i will be rows, j will be columns
      temp[[k]]<-c(names(data2)[i],names(data2)[j])
    }}
  names(mytables)<-temp}

mytables # print tables -- only really need to pay attention to those with dependent variables as rows


####################################################################
# select only those crosstabs that satisfy a consistency threshold #
####################################################################

cons<-.95 # set consistency score threshold

k<-0
nec.cond.tables<-list()
temp<-list()

for (i in 1:length(mytables)){
  if(sum(mytables[[i]]>cons)>0){
    k<-k+1
    nec.cond.tables[[k]]<-mytables[[i]]
    temp[[k]]<-names(mytables[i])
  }
}
names(nec.cond.tables)<-temp

nec.cond.tables #print tables with consistency threshold parameter


#####################################################################
#### Step 4: Construct the truth table and resolve contradictions ###
#####################################################################

#4-1. Construct a "truth table" based on the causal conditions specified in step 1 or some reasonable subset of these conditions
#A truth table sorts cases by the combinations of causal conditions they exhibit. All logically possible combinations of conditions 
#are considered, even those without empirical instances. 

data2.tt<-truthTable(data2, outcome=c(outcome), conditions= conditions,
                     show.cases=F, details=T, neg.out=T, incl.cut1=.95)
data2.tt$tt

dd<-as.data.frame(cbind(data2.tt$tt$incl,data2.tt$tt$PRI))
names(dd)<-c("incl","PRI")
dd[with(dd, order(PRI,decreasing=T)), ] #do the PRI and incl rank similarly? 

data2.tt$tt[which(data2.tt$tt$n > 2), ] # which configurations have more than 2 cases?

for (i in 1:length(data2.tt$tt[which(data2.tt$tt$n > 2),1 ])){
  cases[[i]]<-data2[strsplit(data2.tt$cases[i],","),]
}


#4-2. Assess the consistency of the cases in each row with respect to the outcome: Do they uniformly share the outcome? 
#A simple measure of consistency for crisp sets is the percentage of cases in each row of the truth 
#table displaying the outcome. Consistency scores of either 1 or 0 indicate perfect consistency for a given row. 
#A score of 0.50 indicates perfect inconsistency. 

cond<- which(data2.tt$tt$incl > 0 && data2.tt$tt$incl < 1)  
data2.tt$tt[cond, ]

#4-3. Identify contradictory rows. Technically, a contradictory row is any row with a 
#consistency score that is not equal to 1 or 0. 

cond<- which(data2.tt$tt$n > 2 & data2.tt$tt$incl > 0 & data2.tt$tt$incl < 1)
contradictory.rows<-data2.tt$tt[cond, ]

#4-4. Compare cases within each contradictory rows. If possible, identify decisive 
#differences between positive and negative cases, and then revise the truth table accordingly.

cases<-list()

for (i in 1:length(contradictory.rows[,1])){
  cases[[i]]<-data2[which(data2[,1:5]==as.numeric(contradictory.rows[i,1:5])),]
}

for (j in 1:5){
  
  cases[[i]]<-data2[which(sum(data2[,j]==as.numeric(contradictory.rows[i,j]))==5,]}

####################################
#Step 5: Analyze the truth table####
####################################


#5-5. fsQCA presents three solutions to each truth table analysis: 

#(1) a "complex" solution that avoids using any counterfactual cases (rows without cases-"remainders"); 

complex<-eqmcc(data2,  outcome=outcome,  n.cut=2, incl.cut1=.95,
               neg.out = T, conditions= conditions, details = T, show.cases = F)
complex

#(2) a "parsimonious" solution, which permits the use of any remainder that will yield simpler (or fewer) recipes; and 

parsimonious<-eqmcc(data2,  outcome=outcome,  n.cut=2, incl.cut1=.95, incl.cut0=.95, include="?",
                    neg.out = F, conditions= conditions,details = TRUE, show.cases = F)
parsimonious




#(3) an "intermediate" solution, which uses only the remainders that survive counterfactual analysis based on theoretical 
#and substantive knowledge (which is input by the user).

#do what you will....

#need to perform counterfactuals here 

eqmcc(data2,  outcome=outcome, neg.out = T, conditions= conditions)


#Step 6: Evaluate the Results

Step 6: Evaluate the Results 
6-1. Interpret the results as causal recipes. Do the combinations make sense? 
What causal mechanisms do they imply or entail? How well do they relate to 
existing theory? Do they challenge or refine existing theory? 

6-2. Identify the cases that conform to each causal recipe. Often some cases will 
conform to more than one recipe and sometimes there are more cases that 
combine two (or more) recipes than there are "pure" instances. Do the recipes 
group cases in a meaningful way? Do the groupings reveal aspects of cases that 
had not been considered before? 

6-3. Conduct additional case-level analysis with an eye toward the mechanisms 
implied in each recipe. Causal processes can be studied only at the case level, 
so it is important to evaluate them at that level. 



#Step 7: Return to step 3 and repeat the analysis for the negation of the outcome. 


save(data,file="sim.data.Rdata")
