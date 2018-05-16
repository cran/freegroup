\name{identity}
\alias{identity}
\alias{is.identity}
\alias{neutral}
\title{The identity element}
\description{Create and test for the identity element}
\usage{
is.identity(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class \code{free}}
}
\details{
Returns a Boolean indicating whether an element is the identity or not
}
\author{Robin K. S. Hankin}
\examples{
as.free(0)   # convenient R idiom for creating the identity

x <- rfree(10,3)
stopifnot(all(x == x + as.free(0)))
stopifnot(all(is.identity(x-x)))

}
