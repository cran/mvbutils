\name{Save}
\alias{Save}
\alias{Save.pos}
\title{Save \R objects}
\description{These function resemble \code{save} and \code{save.image}, except that any functions which have been \code{mtrace}d (see the \pkg{debug} package) will be temporarily untraced during saving (though the \pkg{debug} package is not needed to run them). \code{Save()} is like \code{save.image()} \code{Save.pos(i)} saves all objects from the \code{i}th position on the search list in the corresponding .Rdata file. There is less flexibility in the arguments than for the system equivalents.}
\usage{
Save()
Save.pos( pos, path, ascii=FALSE)
}
\arguments{
\item{ pos}{character or numeric position on search path}
\item{ path}{directory or file to save into (see DETAILS).}
\item{ ascii}{file type, as per \code{\link{save}}}
}
\details{
If a function \code{f} is saved while \code{mtrace}d and then restored into a different \R session, there will be an error message when you try to run \code{f}. Assuming that the \pkg{debug} package is loaded, this can be fixed by \code{mtrace(f,FALSE)} or \code{mtrace(f)} if you really want to debug \code{f}. However, you can avoid the problem in the first place by always calling \code{Save} and \code{Save.pos}; this is faster than manually untracing and retracing, and avoids losing any breakpoints. If you use the \code{cd} system in \code{mvbutils}, you will rarely need to call \code{Save.pos} directly; \code{cd}, \code{move} and \code{FF} will do it for you.

To exclude objects from saving, store their names in \code{options( dont.save)}. The default exclusion is \code{.packageName}; good additions might be \code{last.warning} and \code{.Last.value}.

\code{path} is normally inferred from the \code{path} attribute of the \code{pos} workspace. If no such attribute can be found (e.g. if the attached workspace was a list object), you will be prompted. If \code{path} is a directory, the file will be called ".Rdata". If you specify \code{path}, it must be a complete directory path or file path (i.e. it will not be interpreted relative to a \code{path} attribute).
}
\seealso{ \code{\link{save}}, \code{\link{save.image}}, mtrace in package \pkg{debug}}
\examples{
\dontrun{
Save() #
Save.pos( "package:mvbutils") # binary image of exported functions
Save.pos( 3, path="temp.Rdata") # path appended to attr( search()[3], "path")
}
}
\author{Mark Bravington}
\keyword{debugging}
\keyword{file}
