\name{backwards}
\alias{backwards}
\title{Write free objects backwards}
\description{Write free objects in reverse order}
\usage{
backwards(x)
}
\arguments{
  \item{x}{Object of class \code{free}}
}
\author{Robin K. S. Hankin}
\note{

  For each element of a \code{free} object, function \code{backwards()}
  writes the symbols in reverse order.  It is distinct from
  \code{rev()}, see examples.

  Function backwards is an involution: it is its own inverse.

}
\examples{
abc(1:5)
backwards(abc(1:5))
rev(abc(1:5))

x <- rfree(10,5)
backwards(backwards(x)) == x  # involution
all(abelianize(x) == abelianize(backwards(x))) # should be TRUE
}
