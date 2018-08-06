\name{Extract}
\alias{Extract.free}
\alias{[.free}
\alias{[<-.free}
\title{Extract or replace parts of a free group object}
\description{Extract or replace subsets of free objects}
\arguments{
  \item{x}{Object of class \code{free}}
  \item{index}{elements to extract or replace}
  \item{value}{replacement value}
}
\details{
These methods (should) work as expected: an object of class
\code{free} is a list but standard extraction techniques should work.
}
\examples{

x <- rfree(20,8,8)

x[5:6]
x[1:2]  <- -x[11:12]


x[1:5] \%<>\%  keep(1:3)


}
