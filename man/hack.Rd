\name{hack}
\alias{hack}
\alias{assign.to.base}
\title{Modify standard R functions, including tweaking their default arguments}
\description{You probably shouldn't use these\dots{} \code{hack} lets you easily change the argument defaults of a function. \code{assign.to.base} replaces a function in \code{base} or \code{utils} (or any other package and its namespace) with a modified version, possibly produced by \code{hack}. Package \pkg{mvbutils} uses these two to change the default position for library attachment, etc; see the code of \code{.First.lib}.}
\usage{
 hack( fun, ...)
 assign.to.base( x, what=,  where=-1, in.imports=, override.env = TRUE)
}
\arguments{
\item{ fun}{a function (not a character string)}
\item{ ...}{pairlist of arguments and new default values, e.g. arg1=1+2. Things on RHS of equal signs will \bold{not} be evaluated.}
\item{ x}{function name (a character string)}
\item{ what}{function to replace \code{x}, defaulting to \code{"replacement." \%&\% x}}
\item{ where}{where to find the replacement function, defaulting to usual search path}
\item{ in.imports}{usually TRUE, if this is being called from an \code{.onLoad} method in a namespace. Make sure any copies of the function to be changed that are in the "imports" namespace also get changed.}
\item{ override.env}{should the replacement use its own environment, or the one that was originally there?}
}
\examples{
\dontrun{
hack( dir, all.files=option.or.default( "ls.all.files", TRUE)) # from '.First.lib' in 'mvbutils'
assign.to.base( "dir", hack( dir, all.files=TRUE))
}
}
\keyword{programming}
