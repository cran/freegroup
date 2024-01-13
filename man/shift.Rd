\name{shift}
\alias{shift}
\title{Permute elements of a vector in a cycle}
\description{Given a vector, permute the elements with a cyclic permutation}
\usage{
shift(x,i=1)
}
\arguments{
  \item{x}{Vector}
  \item{i}{Integer, number of places to permute.  Negative values mean
    to count from the end}
}
\details{
This function is that of the \CRANpkg{magic} package, where it is
motivated and discussed.
}
\value{Returns a vector}
\author{Robin K. S. Hankin}
\examples{

shift(1:9)
shift(1:9,-1)

shift(1:9,2)


}
