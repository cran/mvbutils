\name{maintain.packages}
\alias{maintain.packages}
\title{Set up task package for live editing}
\description{Set up task package(s) for editing and/or live-editing. You need to be \code{\link{cd}}ed into the task above your source-task. \code{maintain.packages} must be called before loading the package via \code{library} or \code{require}.}
\usage{
# E.g. in your .First, after library( mvbutils), or in a '.First.task' above yr source task
maintain.packages(..., character.only = FALSE)
}
\arguments{
\item{ ...}{names of your packages, unquoted unless \code{character.only} is TRUE. Package names must correspond to subtasks of the current task.}
\item{ character.only}{see above}
}
\details{\code{maintain.packages( mypack)} loads a copy of your source-task package "mypack" (as stored in its ".RData" file) into a variable \code{..mypack} in the "mvb.session.info" environment. You don't normally need to know this, because normally you'd modify/create functions in the package via \code{\link{fixr}} or \code{fixr(..., pkg="mypack")}. However, you can directly modify the contents of \code{..mypack}. If you do, then remember to use \code{<<-}, e.g. \code{..mypack$newfun <<- function( x) whatever}, rather than \code{<-}; otherwise, a local copy of \code{..mypack} will be created in the current task. If that does happen, just \code{rm} it; the local copy and the master copy in "mvb.session.info" both point to the same thing, and modifying one implies modifying the other, so that deleting the local copy won't affect the master.}
\section{Maintained packages as tasks}{
If you use \code{mvbutils} to pre-build your package, then your package must exist as a task in the \code{\link{cd}} hierarchy. This means you can in theory \code{\link{cd}} to its workspace. However, thanks to \code{maintain.packages}, there is no compelling need to have the package/task at the top of the search path. In fact, I don't really recommend \code{\link{cd}}ing to a loaded package, because of the potential for confusion; there are then 4 potentially -different versions of objects floating around in R! Nevertheless, if you really want to then it should be possible to \code{\link{cd}} to a package-as-task; this might be useful if you want to add several new objects to the package without having to create & move them.

If do you \code{\link{cd}} to a package/task that is already loaded under \code{maintain.packages}, then the loaded version of the package is "frozen" until you \code{\link{cd}} back up from the package/task. All \code{\link{fixr}} edit operations on objects in the package are redirected to the task version, i.e. the one highest on the search path. \code{\link{pre.install}} and then \code{\link{patch.installed}} will still work, since they operate on the disk image. When you \code{\link{cd}} back up, the task is updated on disk (as \code{\link{cd}} always does), and the maintained-package version is reloaded from disk, and all \code{\link{fixr}} edits are redirected to the maintained-package version.
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
library( myfirstpack) # etc
}
}
\keyword{internal}
