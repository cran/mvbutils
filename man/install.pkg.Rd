\name{install.pkg}
\alias{install.pkg}
\alias{build.pkg}
\alias{build.pkg.binary}
\alias{check.pkg}
\alias{cull.old.builds}
\alias{set.rcmd.vars}
\title{Package building, distributing, and checking}
\description{These are convenient wrappers for R{}'s package creation and installation tools. They are designed to be used on packages created from tasks via \code{mvbutils}. However, the \code{mvbutils} approach deliberately makes re-installation a rare event, and one call to \code{install.pkg} might suffice for the entire life of a simple package. After that very first installation, you'd probably only need to call \code{install.pkg} if (when...) new versions of R{} entail re-installation of packages, and \code{build.pkg/build.pkg.binary/check.pkg} when you want to give your package to others, either directly or via CRAN etc. Set the argument \code{intern=F} if you want to see progress on-screen (but the result won't be captured); if you can handle the disconcertingly blank wait, set \code{intern=T} to get a character vector result. \code{cull.old.builds} deletes ".tar.gz" and ".zip" files for versions before the current one (as determined by the versions \code{set.rcmd.vars} does nothing (yet).
}
\usage{
  # Usually: build.pkg( mypack) etc
  install.pkg( pkg, character.only=FALSE, dir.above.source='+', lib=.libPaths()[1], flags=character(0), intern=TRUE)
  build.pkg( pkg, character.only=FALSE, dir.above.source='+', flags=character(0), intern=TRUE)
  build.pkg.binary( pkg, character.only=FALSE, dir.above.source='+', flags=character(0), intern=TRUE)
  check.pkg( pkg, character.only=FALSE, dir.above.source='+', build.flags=character(0), check.flags=character( 0),
      CRAN=FALSE, intern=TRUE)
  cull.old.builds( pkg, character.only=FALSE, dir.above.source='+')
  set.rcmd.vars( ...) # not yet implemented. If you need to set env vars eg PATH for R CMD to work, you have to do so yourself; see *Details*
}
\arguments{
See the examples
\item{ pkg}{usually an unquoted package name, but interpretation can be changed by non-default \code{character.only}. You can also get away with eg \code{..mypack}, ie a direct reference to the maintained package.}
\item{ character.only}{default FALSE. If TRUE, treat \code{pkg} as a normal object, which should therefore be a string containing the package's name. If \code{character.only} is itself a string, it will override \code{pkg} and be treated as the name of the package.}
\item{ dir.above.source}{where to look for source package; see \code{\link{pre.install}}}
\item{ intern}{?return the result as a character vector? (See \code{\link{system}}) Set to FALSE if you want to see the output as-it-happens, but in that case it won't be returned.}
\item{ lib}{where to install to; default is the same place R{} would install to, i.e. \code{.libPaths()[1]}.}
\item{ flags}{character vector, by default empty. Any entries should be function-specific flags, such as "--md5" for \code{build.pkg}.}
\item{ build.flags, check.flags}{as per \code{flags} but for the two separate parts of \code{check.pkg} (see \bold{Details}). \code{check.flags} is overridden if \code{CRAN==TRUE}'.}
\item{ CRAN}{if TRUE, set the \code{--as-cran} flag to "RCMD check" and unset all other check flags (except library locations, which are set automatically by all these functions).}
\item{ ...}{name-value pairs of system environment variables (not used for now)}
}
\details{
Before doing any of this, you need to have used \code{\link{pre.install}} to create a source package. (Or \code{\link{patch.install}}, if you've done all this before and just want to re-install/build/check for some reason.)

The only environment variable currently made known to R{} CMD is R_LIBS-- let me know if others would be useful.

\code{install.pkg} calls "R CMD INSTALL" to install from a source package.

\code{build.pkg} calls "R CMD build" to wrap up the source package into a "tarball", as required by CRAN and also for distribution to non-Windows-and-Mac platforms.

\code{build.pkg.binary} (Windows & Mac only) calls "R CMD INSTALL --build" to generate a binary package. A temporary installation directory is used, so your existing installation is \emph{not} overwritten or deleted if there's a problem; R{} CMD INSTALL --build has a nasty habit of doing just that unless you're careful, which \code{build.pkg.binary} is.

\code{check.pkg} calls "R CMD check" after first calling \code{build.pkg} (more efficiently, I should perhaps try to work out whether there's an up-to-date tarball already). It \emph{may} also be possible to do some checks directly from R{} via functions in the \pkg{utils} package, but NB the possibility of interference with your current R{} session. For example, at one stage \code{codoc} (which is the only check that I personally find very useful) tried to unload & load the package, which was very bad; but I think that may no longer be the case.

You \emph{may} have to set some environment variables (eg PATH, and perhaps R_LIBS) for the underlying R{} CMD calls to work. Currently you have to do this manually--- your \code{.First} or \code{.Rprofile} would be a good place. If you really object to changing these for the whole R{} session, let me know; I've left a placeholder for a function \code{set.rcmd.vars} that could store a list of environment variables to be set temporarily for the duration of the R{} CMD calls only, but I haven't implemented it (and won't unless there's demand).

Perhaps it would be desirable to let some flags be set automatically, eg via something in the \code{pre.install.hook} for a package. I'll add this if requested.
}
\value{If \code{intern=TRUE}: the stuff printed out, with class \code{cat} so it prints nicely. If \code{intern=FALSE}: various things about the paths (purely for my programming convenience).
}
\examples{
\dontrun{
# First time package installation
# Must be cd()ed to task above 'mvbutils'
maintain.packages( mvbutils)
pre.install( mvbutils)
install.pkg( mvbutils)
# Subsequent maintenance is all done by:
patch.install( mvbutils)
# For distro to
build.pkg( mvbutils)
# or on Windows (?and Macs?)
build.pkg.binary( mvbutils)
# If you enjoy R CMD CHECK:
check.pkg( mvbutils) # will print "live" as it does them
# If you want the results directly in R, and don't mind not seeing them "live":
check.pkg( mvbutils, intern=T)
# Also legal:
build.pkg( ..mvbutils)
# To do it under programmatic control
for( ipack in all.my.package.names)
  build.pkg( char=ipack)
}
}
\keyword{misc}
