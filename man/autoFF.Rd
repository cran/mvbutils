\name{autoFF}
\alias{autoFF}
\title{Callback function for automatic updates of edited functions.}
\description{\code{autoFF} should not be called directly. It can be installed via \code{addTaskCallback( autoFF)}, after which it will be invoked after every successful top-level command (i.e. things you type). It automatically updates any functions being edited via \code{fixr}.}
\usage{
autoFF(\dots) # but don't call it directly
# Use it like this: addTaskCallback( autoFF)
}
\details{\code{autoFF} just calls \code{\link{FF}}. Just typing <ENTER> isn't enough to trigger \code{autoFF}; 0<ENTER> will do, though. To use \code{autoFF}, include the USAGE call in your \code{.First}.}
\arguments{\item{ \dots}{a placeholder that absorbs the arguments passed into \code{autoFF} by the callback mechanism}}
\value{\item{ TRUE (for the benefit of callback handling)}{}}
\examples{
\dontrun{
## Need to set up "fixr" first
## If you use "fixr", try putting this in your ".First"
addTaskCallback( autoFF)
}
}
\seealso{ \code{\link{addTaskCallback}}, \code{\link{fixr}}}
\author{Mark Bravington}
\keyword{utilities}
\keyword{device}
