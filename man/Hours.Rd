\name{Hours}
\alias{Hours}
\alias{Minutes}
\alias{Seconds}
\title{Sorting out times and time difference}
\description{These functions enforce seconds-based calculations of time differences. \code{Hours}, \code{Minutes}, and \code{Seconds} all return the obvious number of seconds, together with the attribute \code{unit="seconds"}. Further, when you load \code{mvbutils}, the system functions \code{-.POSIXt}, \code{+.POSIXt}, and \code{difftime}, get silently replaced by equivalents that force all calculations to be done in seconds. Actually, you \bold{can} still force \code{difftime} to calculate in different units, via its \code{units} argument, but the default is now "seconds" rather than "auto".}
\usage{
 Hours( x)
 Minutes( x)
 Seconds( x)
}
\arguments{\item{  x}{numeric vector in the units of the name of the called function.}}
\value{A numeric vector of the same length as \code{x}, giving the equivalent timespan in seconds, together with an attribute \code{unit="seconds"}.}
\author{Mark Bravington}
\keyword{utilities}
