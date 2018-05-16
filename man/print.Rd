\name{print}
\alias{print.free}
\alias{as.character_free}
\title{Print free objects}
\description{Print methods for free objects}
\usage{
\method{print}{free}(x,...)
as.character_free(m,latex=getOption("latex"))
}
\arguments{
  \item{x}{Object of class \code{free} in the print method}
  \item{m}{A two-row matrix in function \code{as.character_free()}}
  \item{latex}{Boolean, with code{TRUE} meaning to print latex-friendly
    output including curly braces, and default \code{NULL} option
    meaning to give a nicer-looking output that latex would typeset
    incorrectly}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\note{
The default print method uses multiplicative notation (powers) which is
inconsistent with the juxtaposition method \dQuote{\code{+}}.
}
\examples{

## default symbols:

abc(26)
rfree(1,10)


# if we need more than 26:

options(symbols=apply(expand.grid(letters[1:10],letters[1:10]),1,paste,collapse=""))
rfree(1,100)

options(symbols=NULL)  #   NULL is interpreted as letters

}
