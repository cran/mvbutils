\name{make.usage.section}
\alias{make.usage.section}
\alias{make.arguments.section}
\title{Construct sections of documentation}
\description{These are really intended for expediting documentation of large numbers of "internal" functions in a proto-package. \code{make.usage.section} and \code{make.arguments.section} form prototype USAGE and ARGUMENTS section for the specified functions. These are ready for pasting into flat-format documentation (and subsequent editing).}
\usage{
make.usage.section( funs=, file=stdout())
make.arguments.section( funs=, file=stdout())
}
\arguments{
\item{ funs}{character vector, defaulting to \code{find.funs() \%except\% find.documented( doctype="Rd")}}
\item{ file}{where to put the output (\code{"clipboard"} is useful)}
}
\details{
The default \code{funs} argument will find all functions not mentioned in flat-format ready-for-doc2Rd documentation. This is useful for documenting a group of "internal" functions.

\code{make.usage.section} simply puts the name of each function before its deparsed and concatenated argument list, one function per line.

\code{make.arguments.section} puts one argument per line, then a colon, then the name of the function in parentheses. The idea is that something about the argument should be added manually in a text editor.
}
\examples{
make.usage.section( c( "make.usage.section", "find.funs")) # charvec returned invisibly
make.arguments.section( c( "make.usage.section", "find.funs")) # ditto
}
\seealso{ \code{\link{flatdoc}}, \code{\link{formalize.package}}}
\author{Mark Bravington}
\keyword{documentation}
