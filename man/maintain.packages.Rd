\name{maintain.packages}
\alias{maintain.packages}
\alias{unmaintain.packages}
\title{Not documented yet!}
\description{Set up packages for live-editing.}
\usage{
# In your .First, after library( mvbutils)
maintain.packages(..., character.only = FALSE)
}
\arguments{
\item{ ...}{names of your packages, unquoted unless\dots}
\item{ character.only}{\dots is TRUE}
}
\details{copious details needed}
\section{Faq}{
How to set up a package from scratch

How to convert an existing package so it can be live-edited etc
}
\seealso{\code{\link{fixr}}, \code{\link{pre.install}}, \code{\link{patch.installed}}}
\examples{
# whatever they are...
\dontrun{
# In your .First:
library( mvbutils)
maintain.packages( myfirstpack, mysecondpack, mythirdpack)
# or...
live.edit.list <- c( 'myfirstpack', 'mysecondpack', 'mythirdpack')
maintain.packages( live.edit.list, character.only=TRUE)
}
}
\keyword{internal}
