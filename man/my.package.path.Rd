\name{my.package.path}
\alias{my.package.path}
\title{Given a function in a loaded package, find the path of the package.}
\description{Useful for finding the path of special files in a package. Usually this will be called inside a function which lives in a package. If the package has a namespace, the \code{path} object stored in the \code{.__NAMESPACE__.} environment is used; if not, the \code{path} attribute of the attached package in the search path is used. The package has to be loaded, of course. Doesn't work for things in \code{baseenv()}.
}
\usage{my.package.path( func=sys.function( sys.parent()))
}
\arguments{\item{ func}{a function in a package. If you're calling it from a function in that package, the default will usually do.}
}
\value{Path: a character string, or NULL if not found.
}
\section{Strange usage}{
If you are doing something weird, then the default \code{func} might not you work. Instead, you can set \code{func} manually beforehand, e.g.:

\preformatted{
me <- sys.function()
my.path <- lapply( list( me), my.package.path)[[1]]
}
}
\examples{
\dontrun{
my.package.path( ks.test)
mypack:::show.description.file <- function() as.cat( readLines(
    file.path( my.package.path(), 'DESCRIPTION')))
}
}
\keyword{misc}
