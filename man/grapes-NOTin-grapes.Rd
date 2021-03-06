% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_operators.R
\name{\%NOTin\%}
\alias{\%!in\%}
\alias{\%NOTin\%}
\title{[!] Operator "not in"}
\usage{
x \%!in\% table
}
\arguments{
\item{x}{vector or \code{NULL}: the values to be matched.}

\item{table}{vector or \code{NULL}: the values NOT to be matched against.}
}
\value{
A vector of the same length as \code{x}. \cr
\code{\%!in\%}: A logical vector, indicating if a match was located for
 each element of \code{x}: thus the values are \code{TRUE} or \code{FALSE}
 and never \code{NA}.
}
\description{
Operator, oposite to \link[base]{\%in\%}.
}
\examples{

1:10 \%!in\% c(1,3,5,9)
#> [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE

}
\seealso{
\code{\link[spAddins]{insertNotIn_Addin}}
}

