ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop


normprob<-c(.01,.01,.02,.03,.05,.04,.03,.02,.01,.01)
rightskew<-c(.08,.09,.07,.05,.04,.03,.02,.01,.01,.01)
leftskew<-c(.01,.01,.01,.02,.02,.03,.05,.07,.09,.08)

hist(sample(ranges, size=pop, replace=T, prob = normprob)) #even probabilities
hist(sample(ranges, size=pop, replace=T, prob = rightskew)) #even probabilities



