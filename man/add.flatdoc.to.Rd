\name{add.flatdoc.to}
\alias{add.flatdoc.to}
\alias{docskel}
\title{Skeletal flat-format documentation}
\description{Adds skeleton flat-format documentation to a function, suitable for conversion to Rd-format using \code{\link{doc2Rd}}. The result should pass RCMD CHECK (but won't be much use until you actually write the documentation!).}
\usage{
# See *Examples* for practical usage
add.flatdoc.to(x = get(char.x), char.x = NULL)
}
\arguments{
\item{ x}{unquoted function name}
\item{ char.x}{function name as character string}
}
\value{A function with attribute \code{docattr} containing the flat-format documentation.}
\details{You don't \bold{have} to use Rd-compatible. You can write documentation that's as free-form as you wish, and there's no need to use \code{add.flatdoc.to} to do it-- you can write the doco directly in your text editor provided that you can \code{source} the resultant melange OK (see \code{\link{fixr}}). I find \code{add.flatdoc.to} useful, though, because I can never remember the headings or mild layout conventions of \code{doc2Rd} and Rd-format itself.}
\seealso{\code{\link{flatdoc}}, \code{\link{fixr}}, \code{\link{doc2Rd}}}
\examples{
myfun <- function( ...) ...
myfun <- add.flatdoc.to( myfun)
# 'fixr( myfun)' will now allow editing of code & doco together
}
\keyword{internal}
