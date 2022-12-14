\name{getlet}
\alias{getlet}
\title{Get letters of a freegroup object}
\description{Get the symbols in a freegroup object}
\usage{
getlet(x)
}
\arguments{
  \item{x}{Object of class \code{free}}
}
\author{Robin K. S. Hankin}
\note{
  By default, return a list with elements corresponding to the elements
  of \code{x}.  But, if object \code{x} is of length 1, a vector is
  returned.  The result is sorted for convenience.
}
\examples{

(x <- rfree(6,7,3))

getlet(x)

as.free(getlet(x))  

identical(as.free(getlet(abc(1:26))), abc(1:26))

}
