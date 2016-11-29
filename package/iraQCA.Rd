\name{iaQCA}

\alias{iaQCA}

\alias{iaQCA}

\title{
The Irvine Robustness Assessment for Qualitative Comparative Analysis
}

\description{
\code{iaQCA} is a package used to test the robustness of QCA solutions to randomness, and suggests alternative consistency score and minimum inclusionary thresholds to reach standard significance levels (i.e. \dfn{p} = .05, 01, .001). 
}

\details{
The \code{iaQCA} package builds on the \code{QCA} package already available (Thiem and Dusa 2013) by suggesting appropriate cutoff levels to ameliorate randomness in QCA solutions. These suggestions, however, should be augmented by a researcher's own case-oriented knowledge such that, where appropriate, the researcher should provide reasoning for why the \code{iaQCA} suggestions do not hold up to their own knowledge of specific cases (Ragin 2014[2008]).

The \code{iaQCA} package comes prepackaged with the \code{iraQCA} and \code{irrQCA} functions. The function \code{\link{iraQCA}} takes the user's QCA solutions, simulates thousands of random data sets using the margins of these solutions, and caculates a "solutions proportion of randomness" point-estimate and confidence interval from these random data. This result demonstrates how robust a given set of QCA solutions are to randomness. Based on the \code{iraQCA} result, the user may chose to improve the solutions by minimizing randomness. Using \code{\link{irrQCA}}, the user will provided with recommendations for improving their solutions to reach standar significance levels. 

For more information about QCA, visit the COMPASSS website at \url{http://www.compasss.org} or check out the Wikipedia entry for QCA at \url{https://en.wikipedia.org/wiki/Qualitative_comparative_analysis}.

The software citation for \code{iaQCA} can be retrieved by using the command \code{citation("iaQCA")} after loading the package.
}

\author{

C. Ben Gibson\cr
Department of Sociology\cr
University of California, Irvine\cr
\email{cbgibson@uci.edu}

Burrel Vann Jr\cr
Department of Sociology\cr
University of California, Irvine\cr
\email{bvann@uci.edu}

\bold{Maintainer}:\cr
C. Ben Gibson
}

\references{

Ragin, Charles C. 2014[2008]. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
Chicago: University of Chicago Press.

Thiem, Alrik, and Adrian Dusa. 2013b. \dQuote{QCA: A Package for Qualitative 
Comparative Analysis.} \emph{The R Journal} 5 (1):87-97. 
URL: \url{http://journal.r-project.org/archive/2013-1/thiem-dusa.pdf}.

Thiem, Alrik, and Adrian Dusa. 2013c. \emph{Qualitative Comparative Analysis with R:
A User's Guide}. New York: Springer.
}

