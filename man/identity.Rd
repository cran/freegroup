\name{identity}
\alias{identity}
\alias{id}
\alias{is.id}
\alias{is.identity}
\alias{is.id.free}
\alias{neutral}
\title{The identity element}
\description{Create and test for the identity element}
\usage{
is.id(x)
id(n)
\method{is.id}{free}(x)
}
\arguments{
  \item{x}{Object of class \code{free}}
  \item{n}{Strictly positive integer}
}
\details{

Function \code{id()} returns a vector of free objects, all of which are
the identity element.  Do not ask what happens if \eqn{n=0}.

Function \code{is.id()} returns a Boolean indicating whether an element
is the identity or not.  The identity can also be generated using
\code{as.free(0)}.

}
\author{Robin K. S. Hankin}
\examples{
id()
as.free(0)   # convenient R idiom for creating the identity

x <- rfree(10,3)
stopifnot(all(x == x + as.free(0)))
stopifnot(all(is.id(x-x)))

}
