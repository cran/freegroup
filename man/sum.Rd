\name{sum}
\alias{sum}
\alias{sum.free}
\title{Repeated summation by concatenation}
\description{
Concatenates its arguments to give a single free object
}
\usage{
\method{sum}{free}(..., na.rm = FALSE)
}
\arguments{
  \item{\dots}{Objects of class \code{free}, to be summed}
  \item{na.rm}{Boolean, indicating whether to ignore \code{NA} entries}    
}
\author{Robin K. S. Hankin}
\examples{

x <- rfree(10,3)
sum(x)
abelianize(sum(x))

z <- alpha(26)
stopifnot(sum(x^z) == sum(x)^z)

}
