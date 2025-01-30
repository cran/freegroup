\name{primitive}
\alias{primitive}
\alias{is.primitive}
\alias{is.power}
\title{Primitive elements of the free algebra}
\description{

  A \dfn{primitive word} is one that is not of the form \code{a^m} for
  any \eqn{m>1}.  The concept is used in Lyndon and Schutzenberger 1962.
  
}
\usage{
is.primitive(x)
is.power(d,n)
}
\arguments{
  \item{x}{Freegroup object, coerced to Tietze form}
  \item{d}{Numeric vector}
  \item{n}{Integer}
}
\details{

Function \code{is.primitive()} returns a boolean vector indicating
whether the elements of its argument are primitive.

Function \code{is.power()} is a lower-level helper function.
\code{is.power(d,n)} determines whether \code{d} is an \code{n}-th power
(that is, \code{d} may be written as \code{n} copies of some numeric
vector).

Thus \code{is_power(c(4,5,7,4,5,7,4,5,7,4,5,7),4)} returns \code{TRUE}
because its primary argument is indeed a fourth power (of
\code{c(4,5,7)}).


}
\value{
Returns a boolean.
}
\references{

R. C. Lyndon and M. P. Schutzenberger 1962.  \dQuote{The equation
  \eqn{a^M=b^Nc^P} in a free group}.  Michigan Mathematical Journal,
  9(4): 289--298.

}
\author{
  Robin K. S. Hankin.  The code for finding the factors of an integer
  was (somewhat more than) inspired by the \CRANpkg{numbers} package.
}
\examples{

is.primitive(as.free(c("a","aaaa", "aaaab", "aabaab", "aabcaabcaabcaabc")))

is.power(c(7,8,4,7,8,4,7,8,4,7,8,4),4)


table(is.primitive(rfree(100)))


## primitive with >1 symbols is rare:
x <- rfree(1000)
x[!is.primitive(x) & number(x)>1]
##  try x <- rfree(10000) [but this takes a long time]


}
