\name{alpha}
\alias{alpha}
\alias{alphabet}
\title{Single-symbol words}
\description{Produces a vector of single-symbol words}
\usage{
alpha(v)
}
\arguments{
  \item{v}{Vector of integers}
}
\author{Robin K. S. Hankin}
\examples{
alpha(1)  # just the letter 'a'

z <- alpha(26)  # variable 'z' is symbol 26, aka 'z'.

alpha(1:26)  # the whole alphabet; compare abc(1:16)

alpha(1:26)*6

abc(1:10) ^ z
}
