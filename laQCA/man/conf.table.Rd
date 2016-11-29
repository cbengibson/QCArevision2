\name{conf.table}
\alias{conf.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
conf.table(data, ncut = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{ncut}{
%%     ~~Describe \code{ncut} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, ncut = 4) 
{
    if (length(ncut) == 1) {
        suppressWarnings(mod <- glm(OUT ~ CTH + CPI, family = "binomial", 
            data = data))
    }
    if (length(ncut) > 2) {
        suppressWarnings(mod <- glm(OUT ~ CTH + CNTH + CPI, family = "binomial", 
            data = data))
    }
    data$pred <- predict(mod, data, type = "response")
    data <- cbind(data, predict(mod, newdata = data, type = "link", 
        se = TRUE))
    data$UL <- plogis(data$fit - (1.96 * data$se.fit))
    data$LL <- plogis(data$fit + (1.96 * data$se.fit))
    df <- data.frame(p = c("p < .10", "", "p < .05", "", "p < .01", 
        "", "p < .001", ""), pc = rep(c("parsimonious", "complex"), 
        4), LL.incl.cut = rep(0, 8), fitted.incl.cut = rep(0, 
        8), UL.incl.cut = rep(0, 8))
    output <- list(df)[rep(1L, times = length(ncut))]
    names(output) <- paste("ncut=", ncut, sep = "")
    plevels <- c(0.1, 0.05, 0.01, 0.001, -Inf)
    for (q in 1:length(ncut)) {
        j <- 0
        for (i in 1:4) {
            j <- j + 1
            output[[q]]$fitted.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                0 & data$CNTH == ncut[q] & data$pred < plevels[i]], 
                na.rm = T))
            output[[q]]$UL.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                0 & data$CNTH == ncut[q] & data$LL < plevels[i]], 
                na.rm = T))
            output[[q]]$LL.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                0 & data$CNTH == ncut[q] & data$UL < plevels[i]], 
                na.rm = T))
            j <- j + 1
            output[[q]]$fitted.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                1 & data$CNTH == ncut[q] & data$pred < plevels[i]], 
                na.rm = T))
            output[[q]]$UL.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                1 & data$CNTH == ncut[q] & data$LL < plevels[i]], 
                na.rm = T))
            output[[q]]$LL.incl.cut[j] <- suppressWarnings(min(data$CTH[data$CPI == 
                1 & data$CNTH == ncut[q] & data$UL < plevels[i]], 
                na.rm = T))
        }
        output[[q]][, 3][output[[q]][, 3] == Inf] <- NA
        output[[q]]$fitted.incl.cut[output[[q]]$fitted.incl.cut == 
            Inf] <- NA
        output[[q]]$LL.incl.cut[output[[q]]$LL.incl.cut == Inf] <- NA
        output[[q]]$UL.incl.cut[output[[q]]$UL.incl.cut == Inf] <- NA
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
