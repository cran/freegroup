\name{rfree}
\alias{rfree}
\alias{rfreee}
\alias{rfreeee}
\title{Random free objects}
\description{Creates a vector of random free objects.  Intended as a
  quick \dQuote{get you going} example of free group objects}
\usage{
rfree(n = 7, size = 4, number = size, powers = seq(from = -size, to = size))
rfreee(n = 30, size = 8, number = size, powers = seq(from = -size, to = size))
rfreeee(n = 40, size = 25, number = size, powers = seq(from = -size, to = size))
}
\arguments{
  \item{n}{Length of random vector to generate}
  \item{size}{Maximum length of each element}
  \item{number}{How many distinct letters to sample from}
  \item{powers}{Powers in resulting polynomial.  An integer \code{n} is
    interpreted (via \code{sample()}) as \code{seq_len(n)}}
}
\details{
The auxiliary arguments specify the general complexity of the returned
object with small meaning simpler.

Functions \code{rfreee()} and \code{rfreeee()} give, by default,
successively more complicated expressions.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{size}}}
\examples{

rfree()

abelianize(rfree())

rfree(10,2)
rfree(10,30,26)

rfree(powers=5)
rfree(powers=5:6)

rfree(20,2)^alpha(26)

}
