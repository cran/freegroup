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
  Function \code{backwards()} is distinct from \code{rev()}, see examples.
}
\examples{
backwards(abc(1:5))
rev(abc(1:5))

x <- rfree(10,5)
all(abelianize(x) == abelianize(backwards(x))) # should be TRUE

}
