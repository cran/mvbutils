\name{maintain.packages}
\alias{maintain.packages}
\alias{unmaintain.package}
\title{Set up task package for live editing}
\description{Set up task package(s) for editing and/or live-editing. Usually called in \code{.First} or \code{.First.task}. You need to be \code{\link{cd}}ed into the parent task of your task-package. \code{maintain.packages} must be called \emph{before} loading the package via \code{library} or \code{require}. The converse, \code{unmaintain.package}, is rarely needed; it's really only meant for when \code{\link{unpackage}} doesn't work properly, and you want a "clean slate" task package.
}
\usage{
# E.g. in your .First, after library( mvbutils), or in a '.First.task' above yr task-package
maintain.packages(..., character.only = FALSE, autopatch=FALSE)
unmaintain.package( pkg, character.only = FALSE)
}
\arguments{
\item{ ...}{names of packages, unquoted unless \code{character.only} is TRUE. Package names must correspond to subtasks of the current task.}
\item{ character.only}{see above}
\item{ pkg}{name of package, unquoted unless \code{character.only} is TRUE.}
\item{ autopatch}{whether to \code{patch.install} out-of-date installed packages (default FALSE, but TRUE is common).}
}
\details{
\code{maintain.packages( mypack)} loads a copy of your task-package "mypack" (as stored in its ".RData" file) into a environment \code{..mypack}, which itself lives in the "mvb.session.info" environment. You don't normally need to know this, because normally you'd modify/create/delete objects in the package via \code{\link{fixr}} or \code{fixr(..., pkg="mypack")} or \code{rm.pkg( ..., pkg="mypack")}. But to move objects between the package and other tasks, you do need to refer to the in-memory task package, e.g. via \code{move( ..., from=..Splendid, to=subtask/of/current)}. In most cases, you will be prompted afterwards for whether to save the task package on disk, but you can always do yourself via \code{Save.pos( ..Splendid)}. Note that only these updates and saves only update the \emph{task package} and the \emph{loaded package}. To update the \emph{source package} using the task package, call \code{\link{pre.install}}; to update the \emph{installed package} on disk as well as the source package, call \code{\link{patch.install}}.

Sometimes you may also want to directly modify the contents of \code{..mypack}, e.g. via

\preformatted{
..mypack$newfun <<- function( x) whatever
}

If you forget and use \code{<-} instead of \code{<<-}, a local copy of \code{..mypack} will be created in the current task. This is no big deal, and you can just \code{rm} the local copy; the local copy and the master copy in "mvb.session.info" both point to the same thing, and modifying one implies modifying the other, so that deleting the local copy won't lose your changes. \code{\link{Save}} detects accidental local copies of task packages, and omits them from the disk image, so there shouldn't be any problems next time you start R{} even if you completely forget about local/master copies.

If \code{autopatch==TRUE}, then \code{maintain.packages} will check whether the corresponding \emph{installed} packages are older than the ".RData" files of the task packages. If they are, it will do a full \code{\link{patch.install}}; if not, it will still call \code{\link{patch.install}} but only to reverse-update any bundled DLLs (see \code{\link{pre.install}}), not to re-install the R-source. I find \code{autopatch} useful with packages containing C code, where a crash in the C code can cause R{} to die before the most recent R-code changes have been "committed" with \code{\link{patch.install}}. When you next start R{}, a call to \code{maintain.packages} with \code{autopatch=TRUE} will "commit" the changes \emph{before} the package is loaded, because you have to call \code{maintain.packages} before \code{library}; this seems to be more reliable than running \code{\link{patch.install}} manually after \code{library} after a restart.
}
\section{Maintained packages as tasks}{
If you use \code{mvbutils} to pre-build your package, then your package must exist as a task in the \code{\link{cd}} hierarchy. Older versions of \code{mvbutils} allowed you to \code{\link{cd}} to a maintained package, but this is now forbidden because of the scope for confusion. Thanks to \code{maintain.packages}, there is no compelling need to have the package/task at the top of the search path; \code{\link{fixr}}, \code{\link{move}}, etc work just fine without. If you really do want to \code{\link{cd}} to a maintained package, you must call \code{unmaintain.package} first.

One piece of cleanup that I recommend, is to move any subtasks of "mypack" one level up in the task hierarchy, and to remove the \code{tasks} object from "Splendid" itself, e.g. via something like:

\preformatted{
cd( task.above.splendid)
tasks <- c( tasks, combined.file.paths( tasks[ "Splendid"], ..Splendid$tasks))
# ... combined.file.paths is an imaginary function. Watch out if you've used relative paths!
rm.pkg( tasks, pkg="Splendid")
}

% If do you 'cd' to a package/task that is already loaded under 'maintain.packages', then the loaded version of the package is "frozen" until you 'cd' back up from the package/task. All 'fixr' edit operations on objects in the package are redirected to the task version, i.e. the one highest on the search path. 'pre.install' and then 'patch.installed' will still work, since they operate on the disk image. When you 'cd' back up, the task is updated on disk (as 'cd' always does), and the maintained-package version is reloaded from disk, and all 'fixr' edits are redirected to the maintained-package version.
}
\seealso{\code{\link{mvbutils.packaging.tools}}, \code{\link{fixr}}, \code{\link{pre.install}}, \code{\link{patch.installed}}, \code{\link{unpackage}}
}
\examples{
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
\keyword{internal
}
