\name{help2flatdoc}
\alias{help2flatdoc}
\title{Convert help files to flatdoc format}
\description{Converts a vanilla R help file (as used by the internal pager) to plain-text format. The output conventions are those in \code{\link{doc2Rd}}, so the output can be turned into Rd-format by running it through \code{doc2Rd}. This function is useful if you have existing Rd-format documentation and want to try out the \code{\link{flatdoc}} system of integrated code and documentation.}
\usage{
help2flatdoc( fun.name, pkgpath)
}
\arguments{
\item{ fun.name}{function name (a character string)}
\item{ pkgpath}{path of top-level directory of the package, i.e. containing the DESCRIPTION file.}
}
\details{
\code{help2flatdoc} looks first for a file with the name \code{fun.name} in the "help" subdirectory; failing that, it tries to unpack the file from the "Rhelp.zip" archive. It \bold{doesn't} work on Rd-format files, but instead on the "built" pager-style help files produced by RCMD INSTALL etc. If you write documentation using \code{\link{flatdoc}}, prepare the package with \code{\link{pre.install}}, build it with RCMD BUILD or INSTALL, and run \code{help2flatdoc} on the result, you should largely recover your original flat-format documentation.

Aliasses are deduced from function calls in the USAGE section.

(Link-triggering phrases aren't explicitly created-- could look thru lists of linkable things I guess.)
}
\examples{
all.libs <- library()$results
mvbutils.loc <- all.libs[ all.libs[,"Package"]=="mvbutils", "LibPath"][1]
cd.doc <- help2flatdoc( "cd", file.path( mvbutils.loc, "mvbutils")) # gives me back almost the original
print( cd.doc)
cd.Rd <- doc2Rd( cd.doc)
# now could cat cd.Rd to a file and invoke RCMD Rdconv/Rd2dvi to get back the cd doco.
}
\seealso{\code{\link{doc2Rd}}}
\keyword{programming}
