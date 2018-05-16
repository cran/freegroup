\name{cumsum}
\alias{cumsum}
\alias{cumsum.free}
\title{Cumulative sum}
\description{Cumulative sum of free vectors}
\usage{
\method{cumsum}{free}(x)
}
\arguments{
  \item{x}{Vector of class free}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{sum}}}
\examples{
cumsum(abc(1:6))

x <- rfree(10,2)
cumsum(c(x,-rev(x)))
}