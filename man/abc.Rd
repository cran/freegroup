\name{abc}
\alias{abc}
\title{Create an alphabetical free group element}
\description{Create a simple free group element}
\usage{
abc(n)
}
\arguments{
  \item{n}{An integer specifying the length of the word; if a vector,
    return the appropriate free vector}
}
\author{Robin K. S. Hankin}
\examples{
abc(8)

abc(1:26)   # compare alpha(1:26)

abc(26) ^ alpha(1:9)

}
