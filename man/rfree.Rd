\name{rfree}
\alias{rfree}
\title{Random free objects}
\description{Creates a vector of random free objects.  Intended as a
  quick \dQuote{get you going} example of free group objects}
\usage{
rfree(n=7, size=4, number = size, powers = seq(from = -size, to = size))
}
\arguments{
  \item{n}{Length of random vector to generate}
  \item{size}{Maximum length of each element}
  \item{number}{How many distinct letters to sample from}
  \item{powers}{Powers to sample from}
}
\details{
The auxiliary arguments specify the general complexity of the returned
object with small  meaning simpler.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{size}}}
\examples{

rfree()

rfree(10,2)
rfree(10,30,26)

rfree(20,2)^alpha(26)

}
