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
p <- rfree(10,6,3)
x <- as.free('x')

p+x

p^x

sum(p)

abelianize(p)

subsu(p,"ab","z")
subs(p,a='z')


discard(p+x,'a')

}
