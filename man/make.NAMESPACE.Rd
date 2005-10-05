\name{make.NAMESPACE}
\alias{make.NAMESPACE}
\title{Auto-create a NAMESPACE file}
\description{Called by \code{\link{formalize.package}} for would-be packages that have an existing NAMESPACE file and that have been informally documented using \code{\link{flatdoc}}. It tries to work out which packages should be imported, which functions are meant to be exported, and which should be registered as S3 methods.}
\usage{make.NAMESPACE( pos=1, path=attr( pos.to.env( pos), "path"))}
\arguments{
\item{ pos}{character or numeric position on search path}
\item{ path}{directory where proto-package lives}
}
\details{
Normally, \code{formalize.package} will call \code{make.NAMESPACE} for you if necessary. If \code{make.NAMESPACE} is applied to proto-package "mypack", then it will produce a file called NAMESPACE in the "mypack/mypack" directory. The NAMESPACE file will consist of (maybe) a call to \code{import}, (definitely) a call to \code{export} with all the function names in quotes, and (maybe) a series of calls to \code{S3Method}. There is no attempt to handle S4 methods.

The imported packages are those listed in the "Depends:" field of the DESCRIPTION file.

The exported functions are all those in \code{find.documented(doctype="any")}, plus any functions that have a non-NULL \code{export.me} attribute. The attribute mechanism is useful for a function such as \code{evaluator} in the \pkg{debug} package, which has to be exported but should never be used directly and therefore doesn't need explicit documentation. \code{\link{formalize.package}} will incorporate any undocumented \code{export.me} functions in the "mypack-internal.Rd" file, so that RCMD CHECK will be happy.

The S3 methods are all the functions whose names start "generic.", where "generic" is any element of the character vector \code{.knownS3Generics} in \code{package:base}-- \emph{except} any functions with a non-NULL \code{export.me} attribute. You can use the attribute mechanism to prevent "methodization" of functions that look like methods but aren't really, such as \code{\link{as.data.frame.I}} in the \pkg{mvbutils} package.

If you have defined new S3 generics and want to make sure that appropriate methods are exported in \code{S3Method} statements rather than \code{export}, you must ensure that the generic name has been added to \code{.knownS3Generics}.
}
\seealso{ \code{\link{formalize.package}}, \code{\link{flatdoc}}}
\keyword{utilities}
\keyword{programming}
