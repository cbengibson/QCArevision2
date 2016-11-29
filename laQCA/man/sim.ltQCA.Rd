\name{sim.ltQCA}
\alias{sim.ltQCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sim.ltQCA(qca.data, outcome = "OUT", conditions = c(""), sim = 100, ncut = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qca.data}{
%%     ~~Describe \code{qca.data} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
}
  \item{conditions}{
%%     ~~Describe \code{conditions} here~~
}
  \item{sim}{
%%     ~~Describe \code{sim} here~~
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
function (qca.data, outcome = "OUT", conditions = c(""), sim = 100, 
    ncut = 4) 
{
    ptm <- proc.time()
    if (all(conditions == c(""))) {
        conditions <- names(qca.data[, !(names(qca.data) \%in\% 
            outcome)])
    }
    inclcut <- seq(from = 0.5, to = 1, by = 0.01)
    pop <- dim(qca.data)[1]
    rows <- sim * length(ncut) * length(inclcut) * length(pop) * 
        2
    out <- qca.data[, outcome]
    qca.data <- qca.data[, !(names(qca.data) \%in\% outcome)]
    data <- data.frame(CTH = 0, CNTH = 0, CPI = 0, NTH = 0, OUT = rep(NA, 
        rows))
    kk <- 0
    for (j in 1:sim) {
        for (k in 1:length(inclcut)) {
            for (n in ncut) {
                kk <- kk + 1
                data[kk, 1] <- inclcut[k]
                data[kk, 2] <- n
                data[kk, 4] <- pop
                s.qca.data <- qca.data
                for (i in 1:length(qca.data)) {
                  prob <- c(sum(qca.data[, i] == 0)/(dim(qca.data)[1]), 
                    sum(qca.data[, i] == 1)/dim(qca.data)[1])
                  s.qca.data[, i] <- sample(c(0, 1), pop, prob = prob, 
                    replace = T)
                }
                prob <- c(sum(out == 0)/(length(out)), sum(out == 
                  1)/length(out))
                s.qca.data$OUT <- sample(c(0, 1), pop, prob = prob, 
                  replace = T)
                parsimonious <- tryCatch(eqmcc(s.qca.data, outcome = c("OUT"), 
                  n.cut = n, incl.cut1 = inclcut[k], include = "?", 
                  conditions = c(names(s.qca.data[, !(names(s.qca.data) \%in\% 
                    "OUT")])), details = TRUE, show.cases = TRUE), 
                  error = function(e) e)
                if (!inherits(parsimonious, "error")) {
                  data[kk, 5] <- 1
                  data[kk, 3] <- 0
                }
                if (grepl("Nothing to explain", parsimonious)[1]) {
                  data[kk, 5] <- 0
                  data[kk, 3] <- 0
                }
                kk <- kk + 1
                data[kk, 1] <- inclcut[k]
                data[kk, 2] <- n
                data[kk, 4] <- pop
                complex <- tryCatch(eqmcc(s.qca.data, outcome = c("OUT"), 
                  n.cut = n, incl.cut1 = inclcut[k], conditions = c(names(s.qca.data[, 
                    !(names(s.qca.data) \%in\% "OUT")])), details = TRUE, 
                  show.cases = TRUE), error = function(e) e)
                if (!inherits(complex, "error")) {
                  data[kk, 5] <- 1
                  data[kk, 3] <- 1
                }
                if (grepl("Nothing to explain", complex)[1]) {
                  data[kk, 5] <- 0
                  data[kk, 3] <- 1
                }
                print(paste(round(100 * kk/rows, digits = 2), 
                  "\% done", sep = ""))
                print(proc.time() - ptm)
                flush.console()
            }
        }
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
