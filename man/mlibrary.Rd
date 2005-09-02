\name{mlibrary}
\alias{mlibrary}
\title{Loading and listing of informal packages}
\description{\code{mlibrary} lets you use tasks as informal proto-packages. The documentation etc. requirements are much less stringent than for proper packages.}
\usage{
# Usual: mlibrary( task=)
mlibrary( \dots, character.only=FALSE, logical.return=FALSE, task, pos=lib.pos(), execute.First=TRUE)
}
\arguments{
\item{ task}{an unquoted character string naming a task.}
\item{ character.only}{if TRUE, \code{task} will be interpreted as a normal quoted character string.}
\item{ logical.return}{if TRUE and there is a "fatal" problem, FALSE will be returned; if FALSE and there is a "fatal" problem, an error will occur.}
\item{ pos}{a position on the search path; no reason not to use the default}
\item{ execute.First}{if FALSE, don't execute any \code{.First.lib} code. ?Is this working?}
\item{ \dots}{other arguments as for \code{library}}
}
\value{As for \code{library}.}
\details{
\code{mlibrary( task=mytask)} loads task \code{mytask} as a library. It sets \code{lib.loc} based on the disk location of \code{mytask} before calling \code{library}. If no objects are found in the loaded package after the \code{library} call (i.e. if there is no code in the file "R/mytask"), then \code{mlibrary} will load the .Rdata file and execute its \code{.First.lib} if any (but see below). This binary-loading feature is extremely convenient if you are developing personal proto-libraries and want to avoid the formality of \R source files, Rd documentation files, and perpetual RCMD INSTALL; it's how I maintain \code{mvbutils}, for example. Of course, for CRAN distribution you must do the job properly; \code{\link{formalize.package}} may help.

To use task \code{XXX} as a proto-package loaded via \code{mlibrary(task=XXX)}, all you need to add to \code{XXX} is a DESCRIPTION file in the task directory; none of the usual package subdirectories (not even "R" or "man") need be present.

After \code{mlibrary( task=mytask)}, you can \code{cd(mytask)} to bring \code{mytask} into the active workspace (top of the search path); see \code{\link{cd}}. Note that \code{mlibrary} calls \code{.First.lib}, while \code{cd} calls \code{.First.task}, and you probably don't want both in a single task.

\code{execute.First=FALSE} is useful for debugging; if there is a problem in \code{.First.lib}, normal \code{library} will just refuse to load the package.

\code{mlibrary} without a \code{task} argument will duplicate \code{library}. In particular, \code{.execute.First=FALSE} will have no effect.
}
\note{
(1) The "binary load" option (see \code{?INSTALL}) available in more recent versions of \R may make \code{mlibrary} partly obsolete.

(2) \code{mlibrary} has not been tried with namespaced proto-libraries.
}
\examples{
\dontrun{
mlibrary( task=mvbutils)
}
}
\seealso{ \code{\link{cd}}, \code{\link{formalize.package}}}
\author{Mark Bravington}
\keyword{utilities}
