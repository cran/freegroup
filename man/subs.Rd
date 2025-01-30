\name{subs}
\alias{subs}
\alias{subsu}
\alias{flip}
\title{Substitute and invert symbols}
\description{Substitute and invert specific symbols in a free object}
\usage{
subsu(X, from, to)
subs(X, ...)
flip(X, turn)
}
\arguments{
  \item{X}{Object of class \code{free}}
  \item{from,to,turn}{Objects coerced to class \code{free} specifying symbols
    to alter.  These arguments are coerced to symbols using
    \code{getlet(as.free())}}
  \item{...}{Named arguments for substitution}
}
\details{

  Function \code{subsu(X,from,to)} takes object \code{X} and transforms
  every symbol present in \code{from} into the symbol specified in
  \code{to}.

  Function \code{flip(X,turn)} takes object \code{X} and replaces every
  symbol present in \code{turn} with its inverse.

  Function \code{discard()}, documented at \code{keep.Rd}, effectively
  substitutes a symbol with the identity element (thereby discarding
  it).

  Experimental function \code{subs()} is modelled on similar
  functionality in the \CRANpkg{freealg} package and makes idiom such as
  \code{subs(X,a='z')} work as expected (viz, taking each instance of
  symbol \code{a} and replacing it with \code{x}).
  
}
\author{Robin K. S. Hankin}

\note{

  Functions \code{subs()} and \code{subsu()} substitute for particular
  symbols, not free group elements.  In particular, be careful with
  uppercase (inverse) symbols; because the power is discarded,
  substituting with \code{x} is the same as substituting for \code{X}.
  This behaviour might change in the future.
}

\seealso{\code{\link{abs}},\code{\link{discard}}}
\examples{

subsu(abc(1:10),abc(5),'z')
flip(abc(1:10),abc(5))


o <- rfree(30,5,10)

# Following tests should all be TRUE:
size(flip(o,'a'))   == size(o)
number(flip(o,'a')) == number(o)
total(flip(o,'a'))  == total(o)

size(subsu(o,'a','b'))   <= size(o)
number(subsu(o,'a','b')) <= number(o)
total(subsu(o,'a','b'))  <= total(o)


frog <- rfree()
subs(frog,a='x')

}
