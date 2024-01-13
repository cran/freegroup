\name{size}
\alias{size}
\alias{total}
\alias{number}
\alias{bigness}
\title{
Bignesses of a free object}
\description{Various metrics to describe how \dQuote{big} a free object is}
\usage{
size(a)
total(a)
number(a)
bigness(a)
}
\arguments{
  \item{a}{Vector of free group objects}
}
\details{

  \itemize{

    \item The \dfn{size} of an object is the number of pure powers in
    it (this is the number of columns of the matrix representation of
    the word)
    
    \item The \dfn{total} of an object is the sum of the absolute
    values of its powers
    
    \item The \dfn{number} of an object is the number of distinct symbols
    in it
  }
  
  Thus \code{size(a^2ba)=3}, \code{total(a^2ba)=4}, and
  \code{number(a^2ba)=2}.

  Function \code{bigness()} is a convenience wrapper that returns all
  three bigness measures.
    
}
\value{These functions return an integer vector.}
\author{Robin K. S. Hankin}
\note{
I would like to thank Murray Jorgensen for his insightful comments which
inspired this functionality.
}
\seealso{\code{\link{abs}}}
\examples{

(a <- rfree(20,6,4))
size(a)
total(a)
number(a)

a <- rfree(20,6,4)
b <- rfree(20,6,4)

## Following should all be TRUE
size(a+b)   <= size(a)  + size(b)
total(a+b)  <= total(a) + total(b)
number(a+b) <= number(a)+ number(b)

bigness(rfree(10,3,3))
bigness(allconj(rfree(1,6,1)))

}
