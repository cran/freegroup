\name{abelianize}
\alias{abelianize}
\title{Abelianization of free group elements}
\description{Return the result of modifying a free group element under the assumption of Abelianness}
\usage{abelianize(x)}
\arguments{
  \item{x}{An object of class \code{free}}
}
\details{
Abelianizing a free group element means that the symbols can commute
past one another.  Abelianization is vectorized.
}
\author{Robin K. S. Hankin}
\examples{

x <- rfree(10,20,20)
abelianize(x)

p <- free(rbind(rep(1:5,4),rep(1:4,5)))
abelianize(p)
}
