\name{my.index}
\alias{my.index}
\alias{my.index.assign}
\alias{my.index.exists}
\title{Arbitrary-level retrieval from and modification of recursive objects}
\description{\code{my.index} and \code{my.index.assign} are designed to replace \code{[[} and \code{[[<-} \emph{within} a function, to allow arbitrary-depth access into any recursive object. In order to avoid conflicts with system usage and/or slowdowns, it may be wise to do this only inside a function definition where they are needed. \code{my.index.exists} tests whether the indexed element actually exists.}
\usage{
# Use them like this, inside a function definition:
# assign( "[[", my.index); var[[i]]
# assign( "[[<-", my.index.assign); var[[i]] <- value
my.index( var, \dots) # not normally called by name
my.index.assign( var, \dots, value) # not normally called by name
my.index.exists( i, var)
}
\arguments{
\item{ var}{a recursive object of any mode (not just \code{list}, but e.g. \code{call} too)}
\item{ value}{anything}
\item{ \dots}{one or more numeric index vectors, to be concatenated}
\item{ i}{numeric index vector}
}
\details{
Although \R allows arbitrary-level access to lists, this does not (yet) extend to \code{call} objects or certain other language objects-- hence these functions. They are written entirely in \R, and are probably very slow as a result. It is probably \emph{unwise} to replace system \code{[[} and \code{[[<-} with these replacements at a global level, i.e. outside the body of a function-- these replacements do not dispatch based on object class, for example.

Note that \code{my.index} and \code{my.index.assign} distort strict \R syntax, by concatenating their \code{\dots} arguments before lookup. Strictly speaking, \R says that \code{x[[2,1]]} should extract one element from a matrix list; however, this doesn't really seem useful because the same result can always be achieved by \code{x[2,1][[1]]}. With \code{my.index}, \code{x[[2,1]]} is the same as \code{x[[c(2,1)]]}. The convenience of automatic concatentation seems slightly preferable.

\code{my.index.exists} checks whether \code{var} is "deep enough" for \code{var[[i]]} to work. Unlike the others, it does not automatically concatenate indices.

At present, there is no facility to use a mixture of character and numeric indexes, which you can in S+ via "list subscripting of lists".
}
\examples{
assign( "[[", my.index)
assign( "[[<-", my.index.assign)
ff <- function() \{ a <- b + c \}
body( ff)[[2,3,2]] # as.name( "b")
my.index.exists( c(2,3,2), body( ff)) # TRUE
my.index.exists( c(2,3,2,1), body( ff)) # FALSE
body( ff)[[2,3,2]] <- quote( ifelse( a>1,2,3))
ff # function () \{ a <- ifelse(a > 1, 2, 3) + c \}
my.index.exists( c(2,3,2,1), body( ff)) # now TRUE
remove( list=c( "[[", "[[<-"))
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
