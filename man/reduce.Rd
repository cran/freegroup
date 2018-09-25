\name{reduce}
\alias{reduce}
\alias{remove_zero_powers}
\alias{is_reduced}
\alias{consolidate}
\alias{is_proper}
\title{Reduction of a word to reduced form}
\description{Given a word, remove redundant zero-power terms, and
  consolidate adjacent like terms into a single power}
\usage{
reduce(a)
is_reduced(a)
remove_zero_powers(a)
consolidate(a)
is_proper(a)
}
\arguments{
  \item{a}{An object of class \code{free}}
}
\details{
  A word is \dfn{reduced} if no symbol appears next to its own inverse
  and no symbol has zero power.  The essence of the package is to reduce
  a word into a reduced form.  Thus \eqn{a^2b^{-1}ba}{a^2b^-1ba} will
  transformed into \eqn{a^3}.
  
  In the package, reduction happens automatically at creation, in
  function \code{free()}.

  Apart from \code{is_proper()}, the functions all take a \code{free}
  object, but the meat of the function operates on a single two-row
  matrix.

  Reduction is carried out by repeatedly consolidating adjacent terms of
  identical symbol (function \code{consolidate()}), and removing zero
  power terms (function \code{remove_zero_power()}) until the word is in
  reduced form (function \code{is_reduced()}).

  Function \code{is_proper()} checks to see whether a matrix is suitably
  formed for passing to \code{reduce()}.

  A \code{free} object is \dfn{cyclically reduced} iff every cyclic
  permutation of the word is reduced.  A reduced word is cyclically
  reduced iff the first letter is not the inverse of the last one.  A
  reduced word is cyclically reduced if the first and last symbol differ
  (irrespective of power) or, if identical, have powers of opposite
  sign.  For example, \code{abac} and \code{abca} are cyclically reduced
  but \code{abca^{-1}} is not.  Function \code{is.cyclically.reduced()}
  tests for this, documented at \code{cycred.Rd}.

  Whether the identity should be regarded as cyclically reduced is
  problematic.  On the one hand the identity cannot be shortened by a
  combination of cyclic permutation followed by reduction; but on the
  other, I cannot exhibit a symbol at the start of the identity which
  can be reduced by juxtaposition with a symbol at the end (because
  there are no symbols).  Currently it returns \code{NA} but I am open
  to suggestions.

  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{cycred}}}
\examples{

## create a matrix:
M <- rbind(c(1,2,3,3,2,3,2,1),c(1,2,3,-3,5,0,7,0))

## call the print method (note non-reduced form):
as.character_free(M)

## show the effect of reduce():
as.character_free(reduce(M))

## free() calls reduce() automatically:
free(M)
}
