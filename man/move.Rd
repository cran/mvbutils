\name{move}
\alias{move}
\title{Organizing \R workspaces}
\description{\code{move} shifts one or more objects around the task hierarchy (see \code{\link{cd}}), whether or not the source and destination are currently attached on the search path.}
\synopsis{move( x='.', from='.', to='.', what, overwrite.by.default=FALSE, copy=FALSE)}
\usage{
move( x, from, to)
move( what=, from, to)
move( x, from, to, copy=, overwrite.by.default=)
}
\arguments{
\item{ x}{unquoted name}
\item{ from}{unquoted path specifier}
\item{ to}{unquoted path specifier}
\item{ what}{character vector}
\item{ overwrite.by.default}{logical(1)}
\item{ copy}{logical(1)}
}
\details{
The normal invocation is something like \code{move( myobj, ., 0/another.task)}-- note the lack of quotes around \code{myobj}. To move objects with names that have to be quoted, or to move several objects at the same time, specify the \code{what} argument: e.g. \code{move( what=c( "myobj", "\%myop\%"), ., 0/another.task)}. Note that \code{move} is playing fast and loose with standard argument matching here; it correctly interprets the \code{.} as \code{from}, rather than \code{x}. This well-meaning subversion can lead to unexpected trouble if you deviate from the paradigms in EXAMPLES. If in doubt, you can always name \code{from} and \code{to}.

If \code{move} finds an object with the same name in the destination, you will be asked whether to overwrite it. If you say no, the object will not be moved. If you want to force overwriting of a large number of objects, set \code{overwrite.by.default=TRUE}.

By default, \code{move} will delete the original object after it has safely arrived in its destination. It's normally only necessary (and more helpful) to have just one instance of an object; after all, if it needs to be accessed by several different tasks, you can just \code{move} it to an ancestral task. However, if you really do want a duplicate, you can avoid deletion of the original by setting \code{copy=TRUE}.

You will be prompted for whether to \code{save} the source and destination tasks, if they are attached somewhere, but not in position 1. Normally this is a good idea, but you can always say no, and call \code{save.pos} or \code{Save.pos} later. If the source and/or destination are not attached, they will of course be \code{save}d automatically. The top workspace (i.e. current task) \code{.GlobalEnv} is never saved automatically; you have to call \code{save.image} or \code{Save} yourself.

\code{move} is not meant to be called within other functions.
}
\examples{
\dontrun{
move( myobj, ., 0) # back to the ROOT task
move( what="\%myop\%", 0/first.task, 0/second.task)
# neither source nor destination attached. Funny name requires "what"
move( what=c( "first.obj", "second.obj"), ., ../sibling.task)
# multiple objects require "what"
}
}
\seealso{ \code{\link{cd}}}
\author{Mark Bravington}
\keyword{utilities}
