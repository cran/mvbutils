\name{search.for.regexpr}
\alias{search.for.regexpr}
\title{Find functions/objects/flatdoc-documentation containing a regexp.}
\description{Search one or more environments for objects that contain a regexp. Within each environment, check either (i) all functions, or (ii) the "doc" attributes of all functions, plus any character objects whose name ends in ".doc" or matches a specified regexp.
}
\usage{search.for.regexpr( pattern, where=1, lines=FALSE, doc=FALSE, code.only=FALSE, ...)
}
\arguments{
\item{ pattern}{the regexp}
\item{ where}{an environment, something that can be coerced to an environment (so the default corresponds to \code{.GlobalEnv}), or a list of environments or things that can be coerced to environments.}
\item{ lines}{if FALSE, return names of objects mentioning the regexp. If TRUE, return the actual lines containing the regexp.}
\item{ doc}{if FALSE, search function source code only. Otherwise, search the usual \code{\link{flatdoc}} places, i.e. "doc" attributes of functions, and certain character objects. If \code{doc==TRUE}, the name of those objects must end in ".doc"; otherwise, if \code{doc} is a string (length-1 character vector), then the names of the character object must grep that string; hence, \code{doc="[.]doc$"} is equivalent to \code{doc=TRUE}.}
\item{ code.only}{if FALSE, search only the deparsed version of "raw" code, so ignoring e.g. comments and "flatdoc" documentation}
\item{ ...}{passed to \code{grep}-- e.g. "fixed", "ignore.case".}
}
\value{A list with one element per environment searched, containing either a vector of object names that mention the regexp, or a named list of objects & the actual lines mentioning the regexp.
}
\seealso{\code{\link{flatdoc}}, \code{\link{find.docholder}}, \code{\link{find.documented}}
}
\examples{
\dontrun{
# On my own system's ROOT task (i.e. workspace--- see ?cd)
search.for.regexpr( 'author', doc=FALSE)
# $.GlobalEnv
# [1] "cleanup.refs"
# the code to function 'cleanup.refs' contains "author"
search.for.regexpr( 'author', doc=TRUE)
# $.GlobalEnv
# [1] "scrunge"
# 'scrunge' is a function with a character attribute that contains "author"
search.for.regexpr( 'author', doc='p')
#$.GlobalEnv
# [1] "scrunge" "p1"      "p2"
## 'scrunge' again, plus two character vectors whose names contain 'p'
}
}
\keyword{misc
}
