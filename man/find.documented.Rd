\name{find.documented}
\alias{find.documented}
\title{Support for flat-format documentation}
\description{Finds functions with flat-format documentation.}
\usage{find.documented( pos=1, doctype=c( "Rd", "casual", "own", "any"))}
\arguments{
\item{ pos}{search path position(s), any length numeric or character}
\item{ doctype}{Defaults to "Rd". If supplied, it is partially matched against the choices in USAGE. "Rd" functions are named in the alias list at the start of any \code{doc} attribute of a function in \code{pos} (see \code{\link{doc2Rd}}); "casual" functions have their own \code{doc} attribute, and will be found by the replacement of \code{help}; "own" functions (a subset of "casual") have their own character-mode \code{doc} attribute, and are suitable for \code{doc2Rd}; "any" combines \code{casual} and \code{Rd}.}
}
\value{Character vector of function names.}
\note{
\code{doctype="Rd"} looks for the alias names, i.e. the first word of all lines occurring before the first blank line. This may include non-existent objects.

Start informal documentation (i.e. not intended for \code{doc2Rd}) with a blank line to avoid confusion.
}
\seealso{ \code{\link{doc2Rd}}, \code{\link{dochelp}}}
\author{Mark Bravington}
\keyword{documentation}
\keyword{programming}
\keyword{utilities}
