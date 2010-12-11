\name{as.data.frame.I}
\alias{as.data.frame.I}
\title{Coerce to data.frame, preserving mode of columns}
\description{\code{as.data.frame.I(x)} protects all columns in \code{x}, except factors and numerics, using \code{I()} before calling \code{as.data.frame(x)}.}
\usage{
as.data.frame.I( x, row.names=NULL, optional=FALSE, ...)
}
\arguments{
As per \code{as.data.frame}.
\item{ x}{a list}
\item{ row.names}{\code{NULL} or a character vector giving the row names for the data frame. Missing values are not allowed.}
\item{ optional}{logical. If \code{TRUE}, setting row names and converting column names (to syntactic names) is optional.}
\item{ ...}{passed to \code{as.data.frame} after protecting columns.}
}
\value{A \code{data.frame} with the same columns as \code{x}.}
\examples{
class( as.data.frame( list( x=letters[1:3]))$x) # factor
mode( as.data.frame( list( x=letters[1:3]))$x) # numeric
class( as.data.frame.I( list( x=letters[1:3]))$x) # AsIs
mode( as.data.frame.I( list( x=letters[1:3]))$x) # character
}
\seealso{\code{data.frame}, \code{I}}
\keyword{classes}
\keyword{methods}
