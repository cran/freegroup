\name{dot-class}
\docType{class}
\alias{dot}
\alias{.}
\alias{dot-class}
\alias{extract}
\alias{commutator}
\alias{jacobi}
\alias{dot_error}
\alias{[.dot}
\alias{[,dot-method}
\alias{[,dot,ANY,ANY-method}
\alias{[,dot,ANY,missing-method}
\alias{[,dot,missing,ANY-method}
\alias{[,dot,missing,missing-method}
\alias{[,dot,free,ANY-method}
\alias{[,dot,free,ANY,ANY-method}
\alias{[,dot,matrix,matrix-method}
\title{Class \dQuote{dot}}
\description{

  The dot object is defined in the \pkg{freealg} package, and imported
  here, so that idiom like \code{.[x,y]} returns the commutator, that
  is, \code{x^-1 y^-1 xy}.
}
\arguments{
  \item{x}{Object of any class}
  \item{i,j}{elements to commute}
  \item{...}{Further arguments to \code{dot_error()}, currently ignored}
}
\value{
  Always returns an object of the same class as \code{xy}.
}
\author{Robin K. S. Hankin}
\examples{

.[as.free("x"), as.free("y")]

.[abc(1:6),"z"]

x <- rfree()
y <- rfree()
z <- rfree()

.[x,y] == -x-y+x+y   # should be TRUE

abelianize(.[x,y])

## Jacobi identity _not_ satisfied with this definition:
is.id(.[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]])

## But the Hall-Witt identity is:
all(is.id(.[.[x,-y],z]^y + .[.[y,-z],x]^z + .[.[z,-x],y]^x))

}
