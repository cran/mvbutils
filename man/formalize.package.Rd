\name{formalize.package}
\alias{formalize.package}
\title{Create a simple formal package from an informal package}
\description{\code{formalize.package} is useful when you have a set of functions with flat-format documentation that you want to make into a formal package. It creates a package with \R source and Rd documentation (and optionally a NAMESPACE file) ready for RCMD CHECK and RCMD BUILD.}
\usage{
 formalize.package()
 formalize.package( funs=find.funs( where), package, where=1, dir.=,
 description.file=, new.index=TRUE, README.goes.first=TRUE, keep.flatdoc=FALSE)
}
\arguments{
\item{ funs}{character vector, defaulting to all functions in \code{where}.}
\item{ package}{name to give test package-- see DETAILS for default.}
\item{ where}{position in search path to make into package.}
\item{ dir.}{directory to contain test package. Defaults to \code{path} attribute of search pos \code{where}}
\item{ description.file}{filename of template DESCRIPTION file. If this doesn't exist, a default DESCRIPTION file will be concocted.}
\item{ new.index}{should it create a new INDEX file using \code{Rdindex}}
\item{ README.goes.first}{if there's a function whose name starts with \code{README}, should its documentation file be renamed so that it's found first? See "Package structure details".}
\item{ keep.flatdoc}{FALSE for general distribution (flatdoc attributes discarded), TRUE if you're updating your own stuff.}
}
\details{
A basic package is created (no C code etc.) in directory \code{dir.}. The package will have a DESCRIPTION file, a single \R source file with name "<<package>>.r" in the "R" subdirectory, and the individual function-by-function Rd files in the "man" subdirectory. Mostly, any other files that you created beforehand in \code{dir.} will be left alone, so you can set up \code{inst} directories, etc. More details of the package structure, including control of namespacing, are given below.

The recommended way to use \code{formalize.package} is to have your collection of functions as a task called e.g. \code{my.proto.package}, use \code{cd(my.proto.package)} to bring it to the top workspace, and then call \code{formalize.package()}. This will create the formal skeleton in the "my.proto.package" subdirectory of the task directory.

The name of the package is determined by the \code{package} parameter, which will default to the obvious, as above. If \code{package} is not supplied and no sensible default can be deduced, there will be an error message. The gory details are: \code{package} defaults to the \code{name} attribute of search pos \code{where}, without preceding "package:" if any; if this is NULL, \code{package} defaults to the \code{names} attribute of the \code{path} attribute of search pos \code{where}, which will have been set if the thing being packaged is a task.

The \code{mvbutils} and \pkg{debug} packages started life as simple tasks, automatically loaded by my \code{.First} as proto-packages using \code{mlibrary}. Eventually they acquired flat-format documentation-- informal at first, then massaged towards Rd format as CRAN release approached. I then used \code{formalize.package} and (repeatedly!) RCMD CHECK. Using \code{formalize.package} and the \code{flatdoc} documentation system has made it much easier for me to get new versions ready for release.

Package structure details:

All pre-existing files in the "man" and "R" subdirectories of the package will be removed. If there is a "changes.txt" file in the \code{dir.} directory, it will be copied to the \code{inst} subdirectory of the package. Similarly, any DESCRIPTION file in \code{dir.} will be copied to the package directory, after removing the "Built:" line. If there is no DESCRIPTION file in \code{dir.}, a default DESCRIPTION file will be created in the package directory, but you'd certainly want to edit it before CRAN release. No other files or subdirectories in the package directory will be created or removed, but some essential files will be modified.

If there is already a NAMESPACE file in the package directory, \code{formalize.package} will overwrite it by calling \code{\link{make.NAMESPACE}}, which makes sensible decisions about what to import, export, and S3methodize. However, if no NAMESPACE file is present, none will be created. Hence, to make a namespaced package, just include a dummy NAMESPACE file in the package directory (not \code{dir.}) before calling \code{formalize.package}; to make a non-namespaced package, just make sure there is no NAMESPACE file present.

By default, the \R source file will only contain functions, but you can include other objects too by naming them in the \code{funs} argument. For functions, only source code will be included; in other words, any attributes except \code{source} are removed before printing. In particular, any \code{export.me} attribute (see \code{\link{make.NAMESPACE}} (qv)) and any flat-format documentation in the \code{doc} attribute do not go into the \R file. However, the \code{doc} attribute is used to create the Rd files, by\code{\link{doc2Rd}}.

If any of the Rd files starts with a period (e.g. ".dotty.name"), it will be renamed to "01.dotty.name.Rd" (to avoid some problems with rcmd).

Any undocumented functions (i.e. functions not in \code{find.documented( doctype="any")}) will receive skeletal documentation in a \code{my.proto.package-internals.Rd} file. The doco is OK for RCMD CHECK, but says little more than "don't use these functions yourself".

If \code{README.goes.first} is TRUE, any Rd file starting with \code{README} will be renamed \code{00README\dots}. This will ensure that the README\dots function appears first in the "package manual", and top of the list when \code{library(help=mypackage)} is called. Package users often find it helpful to have something like a README.xxx function, rather than just an alphabetical list of functions in the package.
}
\seealso{ \code{\link{cd}}, \code{\link{flatdoc}}, \code{\link{doc2Rd}}, \code{\link{README.mvbutils}}, \code{\link{make.usage.section}}, \code{\link{make.arguments.section}}}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
