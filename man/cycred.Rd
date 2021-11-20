\name{cycred}
\alias{cycred}
\alias{cyclic}
\alias{cyclically}
\alias{cyclically_reduced}
\alias{cyclically_reduce}
\alias{cyclic_reduction}
\alias{is.cyclically.reduced}
\alias{as.cyclically_reduced}
\alias{is.cyclically_reduced}
\alias{cyclically_reduce_tietze}
\alias{is.conjugate_single}
\alias{conjugate}
\alias{is.conjugate}
\alias{is.conjugate.free}
\alias{\%~\%}
\alias{allconj}
\title{Cyclic reductions of a word}
\description{Functionality to cyclically reduce words and detect conjugacy}
\usage{
is.cyclically_reduced(a)
as.cyclically_reduced(a)
cyclically_reduce(a)
cyclically_reduce_tietze(p)
is.conjugate_single(u,v)
x \%~\% y
\method{is.conjugate}{free}(x,y)
allconj(x)
}
\arguments{
  \item{a,x,y}{An object of class \code{free}}
  \item{p,u,v}{Integer vector corresponding to Tietze form of a word}
}
\details{

  A \code{free} object is \dfn{cyclically reduced} iff every cyclic
  permutation of the word is reduced.  A reduced word is cyclically
  reduced iff the first letter is not the inverse of the last one.  A
  reduced word is cyclically reduced if the first and last symbol differ
  (irrespective of power) or, if identical, have powers of opposite
  sign.  For example, \code{abac} and \code{abca} are cyclically reduced
  but \code{abca^{-1}} is not.  Function \code{is.cyclically_reduced()}
  tests for this.  

  Function \code{as.cyclically_reduced()} takes a vector of free objects
  and returns the elementwise cyclically reduced equivalents.  Function
  \code{cyclically_reduce()} is a synonym with better (English) grammar.

  The identity is cyclically reduced: it cannot be shortened by a
  combination of cyclic permutation followed by reduction.  This ensures
  that \code{is.cyclically_reduced(as.cyclically_reduced(x))} is always
  TRUE.  Also, it is clear that the identity should be conjugate to
  itself.

  Two words \eqn{a,b} are \dfn{conjugate} if there exists a \eqn{x} such
  that \eqn{ax=xb} (or equivalently \eqn{a=x^{-1}bx}{a=x^-1 b x}).  This
  is detected by function \code{is.conjugate()}.  Functions
  \code{is_conjugate_single()} and \code{cyclically_reduce_tietze()} are
  lower-level helper functions.

  Function \code{allconj()} returns all cyclically reduced words
  conjugate to its argument.
    
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{reduce}}}
\examples{

 as.cyclically_reduced(abc(1:9) - abc(9:1))

 a <- rfree(1000,3)
 all(size(as.cyclically_reduced(a)) <= size(a))
 all(total(as.cyclically_reduced(a)) <= total(a))
 all(number(as.cyclically_reduced(a)) <= number(a))



 x <- rfree(1000,2)
 y <- as.free('ab')
 table(conjugate = (x\%~\%y), equal = (x==y))  # note zero at top right

 allconj(as.free('aaaaab'))
 allconj(sum(abc(seq_len(3))))



 x <- rfree(1,10,8,8)
 all(is.id(allconj(x) + allconj(-x)[shift(rev(seq_len(total(x))))]))



}