\name{abs.free}
\alias{abs.free}
\alias{abs}
\title{Absolute value of a \code{free} object}
\description{
  Replaces every term's power with its absolute value
}
\usage{
\method{abs}{free}(x)
}
\arguments{\item{x}{Object of class \code{free}}}
\details{
  Replaces every term's power with its absolute value
}

\author{Robin K. S. Hankin}
\note{

The function's name is motivated  by the inequality in the examples
section.

}

\seealso{\code{\link{subs}}}
\examples{

abs(abc(-5:5))

a <- rfree(10,4,7)
b <- rfree(10,4,7)

a
abs(a)

## following should all be TRUE:
all(size(abs(a+b))  <=  size(abs(a) + abs(b)))
all(total(abs(a+b)) <=  total(abs(a) + abs(b)))
all(number(abs(a+b)) <= number(abs(a) + abs(b)))

all(size(a+b)   <= size(abs(a) + abs(b)))
all(total(a+b)  <= total(abs(a) + abs(b)))
all(number(a+b) <= number(abs(a) + abs(b)))

}





