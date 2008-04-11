\name{dont.lockBindings}
\alias{dont.lockBindings}
\title{Helper for live-editing of packages}
\description{Normally, objects in a NAMESPACEd package are locked and can't be changed. Sometimes this isn't what you want; you can prevent by calling \code{dont.lockBindings} in the \code{.onLoad} for the package.}
\usage{
dont.lockBindings( what, pkgname)
}
\arguments{
\item{  what}{(character) the names of the objects to not lock.}
\item{  pkgname}{(string) the name of the package. As you will only use this inside \code{.onLoad}, you can just set this to \code{pkgname} which is an argument of \code{.onLoad}.}
}
\seealso{\code{\link{lockBinding}}}
\keyword{misc}
