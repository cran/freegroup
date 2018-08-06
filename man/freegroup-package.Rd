\name{freegroup-package}
\alias{freegroup-package}
\alias{freegroup}
\docType{package}
\title{
\packageTitle{freegroup}
}
\description{
\packageDescription{freegroup}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{freegroup}
\packageIndices{freegroup}

}
\author{
\packageAuthor{freegroup}

Maintainer: \packageMaintainer{freegroup}
}
\keyword{ package }
\examples{
a <- rfree(10,6,3)
x <- as.free('x')

a+x

a^x

sum(a)

abelianize(a)

drop(a+x,'a')

}
