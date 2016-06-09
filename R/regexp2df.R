
# Regular expression (named tokens) --------------------------------------------

#' [!+] Capture information to a dataframe by regular expressions
#'
#' Capture information in substrings of \code{text} that match \bold{named} and
#' \bold{unnamed tokens} of regular expressions and convert the result to a
#' data frame.
#'
#' @export
#'
#' @param text The text to be parsed: a character vector where matches are
#'        sought, or an object which can be coerced by \code{as.character}
#'        to a character vector.
#' @param pattern Perl-like regular expression.
#' @param ... Other arguments to be passed to \code{\link[base]{gregexpr}}.
#'
#'
#' @param stringsAsFactors	logical, passed to \code{\link[base]{as.data.frame}}.
#'
#' @inheritParams base::gregexpr
#'
#' @return A data frame with extracted information.
#'
#' @details The syntax how to use \bold{named tokens} is: \cr
#' '\emph{...}\bold{(?<}\emph{...}>\emph{...})\emph{...}' \cr
#' '\emph{expr1}\bold{(?<}\emph{Name}\bold{>}\emph{expr-to-capture}\bold{)}\emph{expr2}'
#'
#' \itemize{
#'   \item{\bold{Name} - the name of the token.\emph{Note:} that spaces and
#'   other special symbols,
#'   inappropriate for variable names, are not allowed and will result in error.}
#'   \item{\bold{expr-to-capture} - regular expression to be captured as a
#'   value of a variable.}
#'   \item{\bold{expr1, expr2} - (optional) expressions, that must match, but
#'   that are not captured.}
#' }
#' \cr
#'
#' \bold{Unnamed tokens} are simply groups of expressions inside round
#' brackets "\code{()}". The expressions outside brackets are not captured.
#'
#'
#' @seealso More about regular expressions used in R: \link[base]{regex}\cr
#'
#'          Website handy for creating and testing Perl-like regular expressions
#'          (library \emph{pcre}, version 1, not 2)
#'          \url{https://regex101.com/r/dS3iP1/1#pcre} \cr
#'
#'          Functions \code{\link[base]{gregexpr}},
#'          operator from package \pkg{magrittr} \code{\link[magrittr]{\%>\%}}.
#'
#' @note Call to function \code{gregexpr} with parameter \code{perl = TRUE}
#' is used.
#'
#' @family \pkg{spMisc} utilities
#' @author Vilmantas Gegzna

regexp2df <- function(text, pattern, ignore.case = FALSE,
                      perl = TRUE, ## TRUE not FALSE, see if description is correct
                      stringsAsFactors = default.stringsAsFactors(),
                      ...)  {
    ParsedData <- gregexpr(pattern, text, ignore.case = ignore.case,
                           perl = perl, ...)
    as_a_list  <- regcapturedmatches(text, ParsedData)
    rbind_DF <- function(...)
    {
        rbind.data.frame(...,
                         stringsAsFactors = stringsAsFactors,
                         make.row.names = TRUE)
    }
    df <- do.call(rbind_DF, as_a_list)
    return(df)
}

