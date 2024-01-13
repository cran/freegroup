\name{char_to_free}
\alias{char_to_free}
\alias{char_to_matrix}
\title{Convert character vectors to free objects}
\description{Convert character vectors to free objects}
\usage{char_to_matrix(x)}
\arguments{
  \item{x}{A character vector}
}
\details{
  Function \code{char_to_matrix()} gives very basic conversion between
  character vectors and free objects.  Current functionality is limited
  to strings like \dQuote{\code{aaabaacd}}, which would give
  \eqn{a^3ba^2cd}{a^3.b.a^2.c.d}.  It would be nice to take a string
  like \dQuote{\code{a^3b^(-3)}} but this is not yet implemented.

  Function \code{char_to_free()} is a vectorized version that coerces
  output to \code{free}.

}
\note{
  
  The function is not particularly robust; for example, passing anything
  other than letters a-z or A-Z will give possibly undesirable
  behaviour.

  Upper-case letters A-Z are interpreted by \code{char_to_matrix()} as
  the inverse of their corresponding lower-case equivalents.  This
  behaviour is inherited by \code{char_to_free()} and \code{as.free()},
  so that \code{as.free("A") == inverse(as.free("a"))}.
  
  Function \code{char_to_free()} is consistent with the default print
  options (which are that the symbols are the lowercase letters a-z).
  If you change the symbols' names, for example
  \code{options(freegroup_symbols=sample(letters))}, then things can get
  confusing.  The print method does not change the internal
  representation of a \code{free} object, which is a list of integer
  matrices.
}

\author{Robin K. S. Hankin}
\seealso{\code{\link{print.free}}}
\examples{

char_to_matrix("aaabcABC")

rfree(10,3) + as.free('xxxxxxxxxxxx')

as.free(letters)*7

all(is.id(as.free(letters) + as.free(LETTERS)))


as.free('')  # identity element

}
