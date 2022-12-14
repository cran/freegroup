\name{c}
\alias{c}
\alias{c.free}
\alias{c}
\alias{rep.free}
\title{Concatenation of free objects}
\description{
Concatenate free objects together 
}
\usage{
\method{c}{free}(...)
\method{rep}{free}(x, ...)
}
\arguments{
  \item{\dots}{In the method for \code{c()}, objects to be
    concatenated.  Should all be of the same type}
  \item{x}{In the method for \code{rep()}, a free object}
}
\author{
Robin K. S. Hankin
}

\examples{
(x <- abc(1:3))
(y <- alpha(22:25))

c(x,y,x,x)


## NB: compare
rep(x,2)  
x*2  
}
