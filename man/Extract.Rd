\name{Extract}
\alias{Extract.free}
\alias{[.free}
\alias{[<-.free}
\title{Extract or replace parts of a free group object}
\description{Extract or replace subsets of free objects}
\arguments{
  \item{x}{A three-vector}
  \item{index}{elements to extract or replace}
  \item{value}{replacement value}
}
\details{
These methods (should) work as expected: an object of class
\code{free} is a list but standard extraction techniques should work.

}
\examples{

a <- rfree(10,2)

a[5:6]
a[1:5]  <- a[1]
a
}
