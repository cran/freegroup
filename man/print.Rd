\name{print}
\alias{print.free}
\alias{print.freegroup}
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
  \item{latex}{Boolean, with \code{TRUE} meaning to print latex-friendly
    output including curly braces, and default \code{NULL} option
    meaning to give a nicer-looking output that latex would typeset
    incorrectly}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\note{
The print method does not change the internal representation of a
\code{free} object, which is a list of integer matrices.

The default print method uses multiplicative notation (powers) which is
inconsistent with the juxtaposition method \dQuote{\code{+}}.

The print method has special dispensation for length-zero free objects
but these are not handled entirely consistently.

The default print method uses lowercase letters a-z, but it is possible
to override this using \code{options("freegroup_symbols" = foo)}, where
\code{foo} is a character vector.  This is desirable if you have more
than 26 symbols, because unallocated symbols appear as \code{NA}.

The package will allow the user to set
\code{options("freegroup_symbols")} to unhelpful things like
\code{rep("a",20)} without complaining (but don't actually do it, you
crazy fool).

}
\seealso{\code{\link{char_to_free}}}  
\examples{

## default symbols:

abc(26)
rfree(1,10)


# if we need more than 26:
options(freegroup_symbols=state.name)
rfree(10,4)

# or even:
jj <- letters[1:10]
options(freegroup_symbols=apply(expand.grid(jj,jj),1,paste,collapse=""))
rfree(10,10,100,4)

options(freegroup_symbols=NULL)  #  NULL is interpreted as letters a-z
rfree(10,4)            #  back to normal
}
