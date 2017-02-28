rm(list=ls())

library("QCA")

Chirot<-read.csv("Chirot_data.csv")

summary(Chirot)

par(mfrow=c(2,4))  
hist(Chirot[,1], main="Histogram of Population")
hist(Chirot[,2], main="Histogram of Commercialism")
hist(Chirot[,3], main="Histogram of Traditionalism")
hist(Chirot[,4], main="Histogram of Middle Peasants")
hist(Chirot[,5], main="Histogram of Gini Coefficients")
hist(Chirot[,6], main="Histogram of Spread")
hist(Chirot[,7], main="Histogram of Violence")
hist(Chirot[,8], main="Histogram of Intensity")


plot(Chirot)


type = "fuzzy"

par(mfrow=c(1,2)) 

thresh<-list()
Chirotcal<-list()

for (i in 1:3){
  
  i<-5
# base variable; random draw from standard normal distribution
x <- Chirot[,i]

# calibration thresholds
thresh[[i]] <- quantile(x, seq(from = 0.15, to = .80, length = 5))

th<-thresh[[i]]

Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])
}

Chirot2<-list()

for (i in 1:5){

  Chirot2[,i]<-Chirotcal[[i]]}

plot(Chirot2[,1:5])



# base variable; random draw from standard normal distribution

plot(Chirot$spread ~ Chirot$intensity)

plot(Chirot$violence ~ Chirot$intensity)

par(mfrow=c(1,1)) 

i<-8

x <- Chirot[,i]

# calibration thresholds
thresh[[i]] <- quantile(x, seq(from = 0.15, to = .95, length = 5))

th<-thresh[[i]]

Chirotcal[[i]]<-calibrate(x, type = "fuzzy", thresholds = c(-.1, th[3], th[5]))

plot(Chirot[,i], calibrate(x, type = "fuzzy", thresholds = c(-.5, th[3], th[5])), 
     ylab = "Fuzzy Set Membership", xlab=names(Chirot)[i])

