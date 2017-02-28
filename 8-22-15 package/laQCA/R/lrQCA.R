#' Lieberson Recommendation
#' 
#' Post-QCA, this function takes in the parameters of QCA data set, the configurational n thresholds identified in the truth table, and the range of consistency scores. LrQCA simulates all possible combinations of this information to provide recommendations to improve upon randomness in the QCA solutions (diminish random results). 
#' @param qca.data the data frame of causal conditions
#' @param outcome the outcome variable (object name) in the QCA data frame of causal conditions; "OUT" is the outcome variable for an application of QCA
#' @param type type of QCA application, \code{"crisp"} or \code{"fuzzy"} sets. The default is set to \code{type = "crisp"}
#' @param inclcut minimum consistency score for inclusion.
#' @param ncut configurational n levels to simulate. The default is set to 2. Can be altered to the maximum value that the truth table yields, by naming the the truth table object (e.g. \code{truth}) and calling the maximum number of cases, using \code{ncut="max(truth$tt$incl)"} the way up to the max score identified by the truth table.
#' @return Significance levels reached (.05, .01, .001) by improving the original QCA model's consistency score and configurational n threshold.
#' @examples 
#' lrQCA(conditions, outcome="P", ncut = "max(truth$tt$incl)")
#' @export
lrQCA<-function(qca.data, outcome="OUT", type="crisp", inclcut = "", ncut=2, neg.out=F, sim=10, verbose=T, conv.diag=F){
source("R/sim.ltQCA.R")
source("R/configuration.table.R")
library("QCA")

s.data<-sim.ltQCA(qca.data, outcome, inclcut = inclcut, ncut=ncut, sim=sim, neg.out=F, type=type, verbose=verbose)
results<-conf.table(s.data, ncut)
return(results)
}

rvQCA<-function(qca.data, outcome="OUT", conditions=c(""), type="crisp", ncut=4, sim=100){
  source("combined.Gamson.sim.R")
  source("configuration.table.R")
  source("rvQCA.R")
  library("QCA")
    s.data<-sim.rvQCA(qca.data, outcome, ncut=ncut, sim=sim)
    results<-conf.table(s.data, ncut)
    return(results)
  }