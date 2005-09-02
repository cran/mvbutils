\name{find.documented}
\alias{find.documented}
\alias{find.docholder}
\title{Support for flat-format documentation}
\description{Finds functions with flat-format documentation.}
\usage{
find.documented( pos=1, doctype=c( "Rd", "casual", "own", "any"))
find.docholder( funs, pos=<<search env containing funs>>)
}
\arguments{
\item{ pos}{search path position(s), numeric or character. In \code{find.documented}, any length; in \code{find.docholder}, only \code{pos[1]} will be used.}
\item{ doctype}{Defaults to "Rd". If supplied, it is partially matched against the choices in USAGE. "Rd" functions are named in the alias list at the start of any \code{doc} attribute of a function in \code{pos} (see \code{\link{doc2Rd}}); "casual" functions have their own \code{doc} attribute, and will be found by the replacement of \code{help}; "own" functions (a subset of "casual") have their own character-mode \code{doc} attribute, and are suitable for \code{doc2Rd}; "any" combines \code{casual} and \code{Rd}.}
\item{ funs}{names of objects whose documentation you're trying to find.}
}
\value{
\item{ find.documented}{Character vector of function names.}
\item{ find.docholder}{list whose names are \code{funs}; each element is a character vector showing which objects have documentation for the named object. Normally you'd expect either 0 or 1 entries in the character vector; more than 1 would imply duplication.}
}
\note{
\code{doctype="Rd"} looks for the alias names, i.e. the first word of all lines occurring before the first blank line. This may include non-existent objects.

Start informal documentation (i.e. not intended for \code{doc2Rd}) with a blank line to avoid confusion.
}
\seealso{ \code{\link{doc2Rd}}, \code{\link{dochelp}}}
\author{Mark Bravington}
\keyword{documentation}
\keyword{programming}
\keyword{utilities}
