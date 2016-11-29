##########################################################################
###### Crisp-Set QCA with direct quotes from Charles Ragin's slides ######
##########################################################################

library("QCA")

#Step 1: Identify relevant cases and causal conditions

#Step 2: Construct the data set
rm(list=ls())
gamson<-read.csv("Gamson.csv")

#Step 3: Test for necessary conditions

#The simplest way to assess necessary conditions is the use crosstabs procedure and ask for row percentages. 
#Look across the row showing the presence of the outcome; if all instances (or almost all instances) agree 
#in displaying a particular causal condition, then that condition might be interpreted as a necessary condition 
#(shared antecedent condition)


########################################################
# makes crosstabs of all 2x2 combinations of variables #
########################################################

gamson<-gamson #select data set

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

gamson.tt<-truthTable(gamson, outcome=c("ACP"), conditions= c(names(gamson)[1:4]),show.cases=T,details=T)
gamson.tt

gamson.tt$noflevels # list the number of levels (values) 

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

complex<-eqmcc(gamson,  outcome=c("ACP"),  n.cut=4, incl.cut1 = .8,  incl.cut0=.77,
               neg.out = F, conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE)
complex

#(2) a "parsimonious" solution, which permits the use of any remainder that will yield simpler (or fewer) recipes; and 

parsimonious<-eqmcc(gamson,  outcome=c("ACP"),  n.cut=4, incl.cut1 = .8, incl.cut0=.77, include="?",
                      neg.out = T, conditions= c(names(gamson)[1:4]),details = TRUE, show.cases = TRUE)
parsimonious

#1000 simulation of random data 





#(3) an "intermediate" solution, which uses only the remainders that survive counterfactual analysis based on theoretical 
#and substantive knowledge (which is input by the user).

#do what you will....

#need to perform counterfactuals here 

eqmcc(gamson,  outcome=c("ADV"), neg.out = T, conditions= c(names(gamson)[1:5]))


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


#Write-up for assignment
attach(gamson)
V1<-BUR  		
V2<-LOW
V3<-DIS
V4<-HLP

venn.plot <- draw.quad.venn(
  area1 = sum(V1),
  area2 = sum(V2),
  area3 = sum(V3),
  area4 = sum(V4),
  n12 = sum(V1+V2==2),
  n13 = sum(V1+V3==2),
  n14 = sum(V1+V4==2),
  n23 = sum(V2+V3==2),
  n24 = sum(V2+V4==2),
  n34 = sum(V3+V4==2),
  n123 = sum(V1+V2+V3==3),
  n124 = sum(V1+V2+V4==3),
  n134 = sum(V1+V3+V4==3),
  n234 = sum(V2+V3+V4==3),
  n1234 = sum(V1+V2+V3+V4==4),
  category = c("Bureaucratic", "Lower Class", "Displacement", "Help from Outsiders"),
  fill = c("white", "white", "white", "white"),
  lty = "solid",
  cex = 2,
  cat.cex = 2,
  cat.col = c("black", "black", "black", "black")
)






