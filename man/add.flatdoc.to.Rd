\name{add.flatdoc.to}
\alias{add.flatdoc.to}
\title{Skeletal flat-format documentation}
\description{Adds skeleton flat-format documentation to a function, suitable for conversion to Rd-format using \code{\link{doc2Rd}}. The result should pass RCMD CHECK (but won't be much use until you actually edit the documentation).}
\usage{
# See *Examples* for practical usage
add.flatdoc.to(x, char.x = NULL, pkg=NULL)
}
\arguments{
\item{ x}{unquoted function name, sought in \code{parent.frame()} unless \code{pkg} is set}
\item{ char.x}{[string] function name}
\item{ pkg}{[string] name of maintained package where \code{x} lives (optional)}
}
\value{A function with attribute \code{docattr} containing the flat-format documentation.}
\details{You don't \bold{have} to write Rd-compatible documentation from the outset. You can write documentation that's as free-form as you wish, and there's no need to use \code{add.flatdoc.to} to do it-- you can write the doco directly in your text editor provided that you can \code{source} the resultant melange OK (see \code{\link{fixr}}). I find \code{add.flatdoc.to} useful, though, because I can never remember the headings or mild layout conventions of \code{\link{doc2Rd}} and Rd-format itself.}
\seealso{\code{\link{flatdoc}}, \code{\link{fixr}}, \code{\link{doc2Rd}}}
\examples{
myfun <- function( ...) ...
myfun <- add.flatdoc.to( myfun)
# 'fixr( myfun)' will now allow editing of code & doco together
# Or, in a maintained package:
# ..mypack$myfun <<- add.flatdoc.to( myfun, pkg='mypack')
}
\keyword{internal}
