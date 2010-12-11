\name{get.backup}
\alias{get.backup}
\alias{restore.all.backups}
\alias{create.backups}
\title{Text backups of function source code}
\description{\code{get.backup} retrieves backups of a function. \code{restore.all.backups} creates objects from their most recent backups. \code{create.backups} creates backup files for all hitherto-unbacked-up functions in a search environment. For \code{get.backup} or \code{restore.all.backups} to work, all backups must have been created using the \code{\link{fixr}} system (or \code{create.backups}).}
\usage{
get.backup( name, where=1, rev=TRUE, zap.name=TRUE)
restore.all.backups( where=2)
create.backups( pos=1)
}
\arguments{
\item{ name}{function name (character)}
\item{ where, pos}{position in search path (character or numeric)}
\item{ rev}{if TRUE, most recent backup comes first in the return value}
\item{ zap.name}{if TRUE, the tag \code{"funname" <- } at the start of each backup is removed}
}
\value{
\item{ get.backup}{Either NULL with a warning, if no backups are found, or a list containing the backups, each as a character vector.}
\item{ restore.all.backups}{NULL, but numerous objects will be created or over-ridden}
\item{ create.backups}{NULL}
}
\details{
\code{\link{fixr}} and \code{\link{FF}} are able to maintain text-file backups of source code, in a directory ".Backup.mvb" below the task directory. The directory will contain a file called "index", plus files BU1, BU2, etc. "index" shows the correspondence between function names and BUx files. Each BUx file contains multiple copies of the source code, with the oldest first. Even if a function is removed (or \code{\link{move}}d) from the workspace, its BUx file and "index" entry are not deleted.

The number of backups kept is controlled by \code{options(backup.fix)}, a numeric vector of length 2. The first element is how many backups to keep from the current R session. The second is how many previous R sessions to keep the final version of the source code from. Older versions get discarded. I use \code{c(5,2)}. If you want to use the backup facility, you'll need to set this option in your \code{.First}. If the option is not set, no backups happen. If set, then every call to \code{\link{Save}} or \code{\link{Save.pos}} will create backups for all previously-unbackupped functions, by automatically calling \code{create.backups}. \code{create.backups} can also be called manually, to create the backup directory, index, and backup files for all functions in the currently-top task.

\code{restore.all.backups} always restores to the current workspace, regardless of where it is restoring from. This is a safety feature. If you screw up the .Rdata file in task \code{mytask} and want to restore the functions in it from backups, the safe way to do it is to \code{cd(mytask)}, then \code{cd(restore)} to create a subtask, then \code{restore.all.backups()} to create "masking" versions in \code{mytask/restore}. Then you can check the restorees and \code{move( what=<<some or all>>, ., ..)} to put them back into \code{mytask}. Set \code{overwrite.by.default=TRUE} in the \code{\link{move}} call if you don't want to check each file. The more risky way is just to \code{cd(mytask)} and \code{restore.all.backups(1)}-- this will overwrite any more recent versions that you may want to keep.

\code{get.backup} offers more control, in that you get all (available) backup versions of a given function, but can only be called for one function at a time. You can also examine the backup files in a text editor, of course.

The function \code{\link{fix.order}} uses the access dates of backup files to list your functions sorted by date order.

\code{\link{move}} will also move backup files and update INDEX files appropriately.
}
\examples{
\dontrun{
## Need some backups first
get.backup( "myfun", "package:myfun")[[1]] # returns most recent backup only
cat( get.backup( "myfun", "package:myfun", zap=FALSE)[[1]][1])
# shows "myfun" <- function...
# restore.all.backups(1) # really don't run this
}
}
\seealso{\code{\link{fixr}}, \code{\link{cd}}, \code{\link{move}}}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
\keyword{documentation}
