\name{rvQCA}
\alias{rvQCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rvQCA(qca.data, outcome = "OUT", conditions = c(""), type = "crisp", ncut = 4, sim = 100)
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
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{ncut}{
%%     ~~Describe \code{ncut} here~~
}
  \item{sim}{
%%     ~~Describe \code{sim} here~~
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
function (qca.data, outcome = "OUT", conditions = c(""), type = "crisp", 
    ncut = 4, sim = 100) 
{
    source("combined.Gamson.sim.R")
    source("configuration.table.R")
    source("rvQCA.R")
    library("QCA")
    s.data <- sim.rvQCA(qca.data, outcome, ncut = ncut, sim = sim)
    results <- conf.table(s.data, ncut)
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
