data<-british<-read.csv("british.csv")
library("QCA")
outcome<-"MCLCON"
conditions<-
type = "fuzzy"


#subset 5 conditions


data<-subset(british, select=c("MCLCON", "UNEMPL", "MINMAN","MANUAL","LABR70","CARSPC"))


##############################################################################################
##############################################################################################
#################                 CALIBRATE            #######################################
##############################################################################################
##############################################################################################


#Chirot<-read.csv("Chirot_data.csv")
Chirot<-data
summary(Chirot)

par(mfrow=c(2,3))  

for (i in 1:length(Chirot[1,])){
  hist(Chirot[,i], main=names(Chirot)[i])
}

plot(Chirot)

type = "fuzzy"

par(mfrow=c(1,2)) 

thresh<-list()
Chirotcal<-list()

for (i in 1:6){
  
  #i<-5
  # base variable; random draw from standard normal distribution
  x <- Chirot[,i]
  
  # calibration thresholds
  thresh[[i]] <- quantile(x, seq(from = 0.10, to = .90, length = 5))
  

  
  Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]))
  
  plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5])), 
       ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])
}


i<-1
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .7, th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .7, th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])



i<-2
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .01, th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .01, th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])

Unemployment should not be calibrated from 0 percent to 100 percent, obviously. Given my extensive case knowledge, I knew
that in the political climate at the time, an unemployment rate of .01 was considered high. That will be the membership score. 


i<-3
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, .80))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, .90)), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])

For minman, although the stock calibration procedure put in the middle value as .5184, I will shift this value to the right some
to account for my theoretical knowledge and the bimodal distribution. I will change the membership threshold to .67, so that 
fully two-thirds of the district needs to be employed in mining or manufacturing to be considered that type of district. 


i<-4
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])

Similar to minman, I will shift the membership value to about two thirds of the district. 


i<-5
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .67, th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])


Membership threshold will be at about .5 for this variable, or a simply majority vote. 


i<-6
th<-thresh[[i]]
x <- Chirot[,i]
Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], .2, th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], .2, th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])

The average household was 5 at this time (not real data), so I will calibrate membership of "cars per capita" as have about one car 
per household, or about .2. 



Chirot2<-matrix(nrow=199,ncol=6)

for (i in 1:6){
  
  Chirot2[,i]<-Chirotcal[[i]]}

Chirot2<-as.data.frame(Chirot2)
names(Chirot2)<-c("MCLCON", "UNEMPL", "MINMAN","MANUAL","LABR70","CARSPC")
plot(Chirot2)






##############################################################################################
##############################################################################################
#################                 PRE SHIT             #######################################
##############################################################################################
##############################################################################################


###############################
#shared antecendent conditions#
###############################

superSubset(Chirot2, outcome = outcome, neg.out = T, conditions = c(names(Chirot2)[2:6]), 
            relation = "nec", incl.cut = .8, cov.cut = 0)





########################################################
# makes crosstabs of all 2x2 combinations of variables #
########################################################

gamson<-Chirot2#select data set

mytables<-list()
temp<-list()
k<-0

for (i in 1:length(gamson)){
  for (j in 1:length(gamson)){
    if (i != j){
      k<-k+1
      mytables[[k]] <- prop.table(table(gamson[,i],gamson[,j]),1) # i will be rows, j will be columns
      temp[[k]]<-c(names(gamson)[i],names(gamson)[j])
    }}
  names(mytables)<-temp}

mytables # print tables -- only really need to pay attention to those with dependent variables as rows


####################################################################
# select only those crosstabs that satisfy a consistency threshold #
####################################################################

cons<-.90 # set consistency score threshold

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

gamson.tt<-truthTable(gamson, outcome=c(outcome), conditions= c(names(gamson)[2:6]),
                      show.cases=T,details=T, neg.out=T, incl.cut1=.5)
gamson.tt$tt

dd<-as.data.frame(cbind(gamson.tt$tt$incl,gamson.tt$tt$PRI))
names(dd)<-c("incl","PRI")
dd[with(dd, order(PRI,decreasing=T)), ] #do the PRI and incl rank similarly? 

gamson.tt$tt[which(gamson.tt$tt$n > 2), ] # which configurations have more than 2 cases?

for (i in 1:length(gamson.tt$tt[which(gamson.tt$tt$n > 2),1 ])){
  cases[[i]]<-gamson[strsplit(gamson.tt$cases[i],","),]
}


#4-2. Assess the consistency of the cases in each row with respect to the outcome: Do they uniformly share the outcome? 
#A simple measure of consistency for crisp sets is the percentage of cases in each row of the truth 
#table displaying the outcome. Consistency scores of either 1 or 0 indicate perfect consistency for a given row. 
#A score of 0.50 indicates perfect inconsistency. 

cond<- which(gamson.tt$tt$incl > 0 && gamson.tt$tt$incl < 1)  
gamson.tt$tt[cond, ]

#4-3. Identify contradictory rows. Technically, a contradictory row is any row with a 
#consistency score that is not equal to 1 or 0. 

cond<- which(gamson.tt$tt$n > 2 & gamson.tt$tt$incl > 0 & gamson.tt$tt$incl < 1)
contradictory.rows<-gamson.tt$tt[cond, ]

#4-4. Compare cases within each contradictory rows. If possible, identify decisive 
#differences between positive and negative cases, and then revise the truth table accordingly.

cases<-list()

for (i in 1:length(contradictory.rows[,1])){
  cases[[i]]<-gamson[which(gamson[,1:5]==as.numeric(contradictory.rows[i,1:5])),]
}

for (j in 1:5){
  
  cases[[i]]<-gamson[which(sum(gamson[,j]==as.numeric(contradictory.rows[i,j]))==5,]}

####################################
#Step 5: Analyze the truth table####
####################################


#5-5. fsQCA presents three solutions to each truth table analysis: 

#(1) a "complex" solution that avoids using any counterfactual cases (rows without cases-"remainders"); 

complex<-eqmcc(gamson,  outcome=outcome,  n.cut=5, incl.cut1 = .8,  incl.cut0=.77,
               neg.out = T, conditions= c(names(gamson)[2:6]),details = TRUE, show.cases = TRUE)
complex

#(2) a "parsimonious" solution, which permits the use of any remainder that will yield simpler (or fewer) recipes; and 

parsimonious<-eqmcc(gamson,  outcome=outcome,  n.cut=4, incl.cut1 = .8, incl.cut0=.77, include="?",
                    neg.out = F, conditions= c(names(gamson)[2:6]),details = TRUE, show.cases = TRUE)
parsimonious




#(3) an "intermediate" solution, which uses only the remainders that survive counterfactual analysis based on theoretical 
#and substantive knowledge (which is input by the user).

#do what you will....

#need to perform counterfactuals here 

eqmcc(gamson,  outcome=outcome, neg.out = T, conditions= c(names(gamson)[2:6]))


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
