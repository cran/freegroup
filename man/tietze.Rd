\name{tietze}
\alias{tietze}
\alias{Tietze}
\alias{tietze.matrix}
\alias{tietze.free}
\alias{vec_to_matrix}
\title{Tietze form for free group objects}
\description{Translate an object of class free to and from Tietze form}
\usage{
\method{tietze}{free}(x)
\method{tietze}{matrix}(x)
vec_to_matrix(x)
}
\arguments{
  \item{x}{Object to be converted}
}
\details{
  The Tietze form for a word is a list of integers corresponding to the
  symbols of the word; typically \eqn{a=1,b=2}, etc.  Negative integers
  represent the inverses of the symbols.

  Function \code{vec_to_free()} is a low-level helper function that
  returns a two-row integer matrix.  If given \code{0} or \code{NULL},
  it returns a two-row, zero-column matrix.
  }

\author{Robin K. S. Hankin}
\examples{
tietze(rfree(10,3))

vec_to_matrix(c(1,3,-1,-1,-1,2))

as.free(list(c(1,1,8),c(2,-4,-4)))

all(as.free(tietze(abc(1:30)))== abc(1:30))

}
