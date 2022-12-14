\name{free}
\alias{free}
\alias{free-class}
\alias{as.free}
\alias{is.free}
\alias{list_to_free}
\title{Objects of class \code{free}}
\description{Generate, and test for, objects of class \code{free}}
\usage{
free(x)
as.free(x)
is.free(x)
list_to_free(x)
}
\arguments{
  \item{x}{Function \code{free()} needs either a two-row matrix, or a
    list of two-row matrices; function \code{as.free()}
    attempts to coerce different types of argument before passing to
    \code{free()} (possibly via \code{list_to_free()})}
}
\details{
The basic structure of an element of the free group is a two-row
matrix.  The top row is the symbols (1=a, 2=b, 3=c, etc)  and the bottom
row is the corresponding power. Thus \eqn{a^2ba^{-1}}{a^2ba^-1} would be

\preformatted{
> rbind(c(1,2,1),c(2,1,-1))
     [,1] [,2] [,3]
[1,]    1    2    1
[2,]    2    1   -1
>
}

Function \code{free()} needs either a two-row matrix or a list of
two-row matrices.  It is the only place in the package that sets the
class of an objet to \code{free}.  Function \code{as.free()} is a bit
more user-friendly and tries a bit harder to do the Right Thing.

The package uses \code{setOldClass("free")} for the \code{dot} methods.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{char_to_free}}}
\examples{
free(rbind(1:5,5:1))

x <- rfree(10,3)
x
x+x
x-x
x[1:5]*(1:5)


as.free(c(4,3,2,2,2))
as.free("aaaabccccaaaaa")
as.free(c("a","A","abAAA"))
}
