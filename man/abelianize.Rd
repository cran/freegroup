\name{abelianize}
\alias{abelianize}
\alias{abelianized}
\alias{is.abelian}
\title{Abelianization of free group elements}
\description{Function \code{abelianize()} returns a word that is
equivalent to its argument under assumption of Abelianness.  The symbols
are placed in alphabetical order.}
\usage{
  abelianize(x)
  is.abelian(x)
}
\arguments{
  \item{x}{An object of class \code{free}}
}
\details{
Abelianizing a free group element means that the symbols can commute
past one another.  Abelianization is vectorized.

Function \code{is.abelian()} is trivial: it just checks to see whether
argument \code{x} has its symbols in alphabetical order.  It might have
been better to call this \code{abelianized()}.

Package \CRANpkg{frab} presents extensive R-centric functionality for
dealing with the free Abelian group.  It is much more efficient than
this package for Abelian operations, and contains bespoke methods for
working with a range of applications such as tables of counts.

}
\author{Robin K. S. Hankin}
\examples{

x <- as.free("aabAA")
x
abelianize(x)

x <- rfree(10,10,2)
x
abelianize(x)

abelianize(.[rfree(),rfree()])


p <- free(rbind(rep(1:5,4),rep(1:4,5)))
p
abelianize(p)
}
