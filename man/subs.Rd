\name{subs}
\alias{subs}
\alias{flip}
\title{Substitute and invert symbols}
\description{Substitute and invert specific symbols in a free object}
\usage{
subs(a, from, to)
flip(a, turn)
}
\arguments{
  \item{a}{Object of class \code{free}}
  \item{from,to,turn}{Objects coerced to class \code{free} specifying symbols
    to alter}
}
\details{

  Function \code{subs(a,from,to)} takes object \code{a} and transforms
  every symbol present in \code{from} into the symbol specified in
  \code{to}.

  Function \code{flip(a,turn)} takes object \code{a} and replaces every
  symbol present in \code{turn} with its inverse.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{abs}}}
\examples{

subs(abc(1:10),abc(5),'z')
flip(abc(1:10),abc(5))


o <- rfree(30,5,10)

# Following tests should all be TRUE:
size(flip(o,'a'))   == size(o)
number(flip(o,'a')) == number(o)
total(flip(o,'a'))  == total(o)

size(subs(o,'a','b'))   <= size(o)
number(subs(o,'a','b')) <= number(o)
total(subs(o,'a','b'))  <= total(o)


}
