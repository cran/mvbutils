\name{formalize.package}
\alias{formalize.package}
\title{Create a formal package skeleton from an informal package}
\description{\code{formalize.package} is useful when you have a set of functions with flat-format documentation that you want to make into a formal package. It creates a bare-bones package with \R source and Rd documentation ready for RCMD CHECK.}
\usage{
 formalize.package()
 formalize.package( funs=find.funs( where), package, where=1, dir.=,
 description.file=)
}
\arguments{
\item{ funs}{character vector, defaulting to all functions in \code{where}.}
\item{ package}{name to give test package-- see DETAILS for default.}
\item{ where}{position in search path to make into package.}
\item{ dir.}{directory to contain test package. Defaults to \code{path} attribute of search pos \code{where}}
\item{ description.file}{filename of template DESCRIPTION file. If this doesn't exist, a default DESCRIPTION file will be concocted.}
}
\details{
A bare-bones package skeleton is created (no C code etc.) in directory \code{dir.}. The skeleton will have a DESCRIPTION file, a single \R source file with name "<<package>>.r" in the "R" subdirectory, and the individual function-by-function Rd files in the "man" subdirectory. More details of the package structure are given below.

The recommended way to use \code{formalize.package} is to have your collection of functions as a task called e.g. \code{my.proto.package}, use \code{cd(my.proto.package)} to bring it to the top workspace, and then call \code{formalize.package()}. This will create the formal skeleton in the "my.proto.package" subdirectory of the task directory.

The name of the package is determined by the \code{package} parameter, which will default to the obvious, as above. If \code{package} is not supplied and no sensible default can be deduced, there will be an error message. The gory details are: \code{package} defaults to the \code{name} attribute of search pos \code{where}, without preceding "package:" if any; if this is NULL, \code{package} defaults to the \code{names} attribute of the \code{path} attribute of search pos \code{where}, which will have been set if the thing being packaged is a task.

The \code{mvbutils} and \pkg{debug} packages started life as simple tasks, automatically loaded by my \code{.First} as proto-packages using \code{mlibrary}. Eventually they acquired flat-format documentation-- informal at first, then massaged towards Rd format as CRAN release approached. I then used \code{formalize.package} and (repeatedly!) RCMD CHECK.

Package structure details:

All pre-existing files in the "man" and "R" subdirectories (only) will be removed. No other files or subdirectories are created or removed. You will certainly want to edit the DESCRIPTION file before actual CRAN release.

By default, the \R source file will only contain functions, but you can include other objects too by naming them in the \code{funs} argument. For functions, only source code will be included; in other words, any attributes except \code{source} are removed before printing. In particular, flat-format documentation in \code{doc} attributes does not go into the \R file. However, it is used to create the Rd files, by\code{\link{doc2Rd}}.

If any of the Rd files starts with a period (e.g. ".README.mvbutils.Rd"), it will be duplicated as e.g. "X.README.mvbutils.Rd". This results in a message during RCMD CHECK, but avoids a more serious bug whereby the function either doesn't appear in the help, or in the package manual.
}
\seealso{ \code{\link{cd}}, \code{\link{flatdoc}}, \code{\link{doc2Rd}}, \code{\link{README.mvbutils}}, \code{\link{make.usage.section}}, \code{\link{make.arguments.section}}}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
