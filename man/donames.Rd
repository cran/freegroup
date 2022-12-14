\name{donames}
\alias{donames}
\alias{names}
\title{Names attributes of free group elements}
\description{Get and set names of free group elements and arithmetic operations}
\usage{
donames(f,e1,e2)
}
\arguments{
  \item{f}{A vector, typically of class \code{free}}
  \item{e1,e2}{Objects of class \code{free}, possibly with names}
}
\details{

  Function \code{donames()} is a low-level helper function that ensures
  that the result of arithmetic operations such as \code{+} and \code{^}
  have the correct names attributes.  The behaviour is inherited from
  that of \code{base::`+`}.
  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{Ops.free}}}
\examples{

x <- rfree(9,4)
x
names(x) <- letters[1:9]
x

z <- as.free('z')
x + x 
x^z
z^x

n <- 1:9
names(n) <- LETTERS[1:9]

x*n
n*x  # note different names 


}