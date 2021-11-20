\name{sum}
\alias{sum}
\alias{sum.free}
\alias{prodfree}
\alias{freeprod}
\title{Repeated summation by concatenation}
\description{
Concatenates its arguments to give a single free object
}
\usage{
\method{sum}{free}(..., na.rm = FALSE)
}
\arguments{
  \item{\dots}{Objects of class \code{free}, to be summed}
  \item{na.rm}{Boolean, indicating whether to ignore \code{NA} entries
    (currently ignored)}    
}
\details{
  Concatenates its arguments and gives a single element of the free
  group.  It works nicely with \code{rev()}, see the examples.
}
\author{Robin K. S. Hankin}
\note{

The package uses additive notation, but it is easy to forget this and
wonder why idiom like \code{prod(rfree())} does not work as desired.  Of
course, the package using additive notation means that one probably
wants \code{sum(rfree())}.


  }
\examples{

x <- rfree(10,3)
y <- rfree(10,6)
z <- alpha(26)


sum(x)
abelianize(sum(x))


sum(x,y) == sum(sum(x),sum(y))
x+y  # not the same!

sum(x,-x)
sum(x,rev(-x))


stopifnot(sum(x^z) == sum(x)^z)

}
