\name{keep}
\alias{keep}
\alias{discard}
\alias{drop}
\alias{retain}
\alias{drop.free}
\title{Keep or drop symbols}
\description{Keep or drop symbols}
\usage{
keep(a, yes)
discard(a, no)
}
\arguments{
  \item{a}{Object of class \code{free}}
  \item{yes,no}{Specification of symbols to either keep (yes) or discard 
    (no), coerced to a free object}
}
\author{Robin K. S. Hankin}
\note{
  Function \code{keep()} needs an explicit \code{return()} to prevent it
  from returning invisibly.

  The functions are vectorised in the first argument but not the
  second.

  The second argument---the symbols to keep or discard---is formally a
  vector of nonnegative integers, but the functions coerce it to a free
  object.  The symbols kept or dropped are the union of the symbols in
  the elements of the vector.  Function \code{discard()} was formerly
  known as \code{drop()} but this conflicted with \code{base::drop()}.

  These functions have nothing in common with APL's \code{take()} and
  \code{drop()}.

}
\examples{
(x <- rfree(20,5,8))

keep(x,abc(4))           # keep only symbols a,b,c,d
discard(x,as.free('cde'))   # drop symbols c,d,e

keep(x,alpha(3))  # keep only abc 

}
