\name{mvbutils.packaging.tools}
\alias{mvbutils.packaging.tools}
\title{How to create & maintain packages with mvbutils}
\description{
This document covers:

\itemize{
\item using \code{mvbutils} to create a new package from scratch;
\item using \code{mvbutils} to maintain a package you've created (e.g. edit it while using it);
\item converting an existing package into \code{mvbutils}-compatible format;
\item how to customize the package-creation process.
}

For clarity, the simplest usage is presented first in each case. For how to do things differently, first look further down this document, then in the documentation for \code{\link{pre.install}} and perhaps \code{\link{doc2Rd}}.

You need to understand \code{\link{cd}} and \code{\link{fixr}} before trying any of this.
}
\section{Setting up a package from scratch}{
First, the simplest case: suppose you have some pure R{} code and maybe data that you'd like to make into a package called "Splendid". The bare-minimum steps you need are:-

\itemize{
\item Make sure all the code & data lives in a single task called "Splendid".
\item \code{\link{cd}} to the task above "Splendid"
\item \code{maintain.packages( Splendid)}
\item \code{pre.install( Splendid)}. This will create a "source package" in a subdirectory of Splendid's task directory. The subdirectory will be called "Splendid".
\item Make sure you have all the R{} build tools installed and on your path-- see "R-exts" for details (and NB that if you need to install Latex, then google MikTex & choose a \emph{minimal} install).
\item \code{install.pkg( Splendid)} to do what you'd expect. On Windows, you can alternatively first do \code{build.pkg.binary( Splendid)}, then use R{}'s menus to "Packages/Install from local zip files".
\item \code{library(Splendid)}; your package will be loaded for use, and is also ready for live-editing.
}

Your package will probably just about work now, but the result won't yet be perfect. The first thing is that you will need to edit the DESCRIPTION file. \code{mvbutils} creates a default text file "DESCRIPTION" in the task folder even if you haven't, but it won't be what you really want, as you'll realize if you type \code{library( help=Splendid)}. Apart from the obvious changes, the most important fields to add are "Imports:" (or "Depends:" for packages that are pre-R2.14 and that also don't have a namespace), to say what other packages are needed by Splendid. The DESCRIPTION file should rarely need to be updated, since the \code{autoversion} feature can be used to take care of version numbering.

The additional steps you'll likely need are these:

\itemize{
\item Provide documentation (see below)
\item Sort out any C/Fortran source code, pre-compiled code, demos, and other additional files (see \code{\link{pre.install}})
\item Move any subtasks of Splendid to one level up the task hierarchy (see \code{\link{maintain.packages}})
}

Once you have set up "Splendid" so that \code{\link{maintain.packages}} works, you should never need to \code{\link{cd}} directly into "Splendid" again.
\subsection{Glossary}{
\emph{Task package} is a folder with at least an ".RData" file, linked into the \code{\link{cd}} hierarchy. It contains master copies of the objects in your package, plus perhaps a few other objects required to build the package (e.g. stand-alone items of documentation).

\emph{In-memory task package} is an environment in the current R{} session that contains an image of the task package. Objects in it are never used directly, only as templates for editing. It is loaded by \code{\link{maintain.packages}}, and \code{\link{Save.pos}} uses it to update the task package (usually automatic).

\emph{Source package} is a folder containing, yes, an R-style source package. It is created initially by \code{\link{pre.install}}, and subsequently by \code{\link{patch.install}} or \code{\link{pre.install}}.

\emph{Installed package} is a folder containing, yes, an R-style installed package. It is always created from the source package, initially by \code{\link{install.pkg}} and subsequently by \code{\link{patch.install}} or \code{\link{install.pkg}}.

\emph{Loaded package} is the in-memory version of an installed package, loaded by \code{library}.

\emph{Tarball package} is a zipped-up version of a source package, for distro on non-Windows-Mac platforms or submission to CRAN and subsequent installation via "R CMD INSTALL". Usually it will not contain DLLs of any low-level code, just the source low-level code. It is created by \code{\link{build.pkg}}.

\emph{Binary package} is a special zipped-up version for distro to Windows or Macs that includes actual DLLs, for installation via e.g. the "Packages/Install from local ZIP" menu. It is created by \code{\link{build.pkg.binary}}.
}
}
\section{Converting an existing package}{
Suppose you have already have a \emph{source} package "hardway", and would like to try maintaining it via \code{mvbutils}. You'll need to create a task package, then create a new version of the source package, then re-install it. The first step is to call \code{unpackage( hardway)} to creat the task package "hardway" in a subdirectory of the current task. Plain-text documentation will be attached to functions, or stored as ".doc" text objects. All functions and documentation must thereafter be edited using \code{\link{fixr}}. The full sequence is something like:

\preformatted{
# Create task package in subdirectory of current:
unpackage( "path/to/existing/source/package/hardway")
#
# Load image into memory:
maintain.packages( hardway)
#
# Make new version of source package:
pre.install( hardway, ...) # use dir= to control where new source pkg goes
#
install.pkg( hardway) # or build.pkg.binary( hardway) followed by "install from local zip file" menu
#
library( hardway) # off yer go
}

If you get problems after \code{\link{maintain.packages}}, you might need \code{unmaintain.package( hardway)} to clear out the in-memory copy of the new task package.
}
\section{Documentation}{
Documentation for functions can be stored as plain text just after a function's source code, as described in \code{\link{flatdoc}}. Just about anything will do-- you don't absolutely have to follow the conventional structure of R{} help if you are really in a hurry. However, the easiest way to add "proper" but skeletal documentation to your function \code{brilliant}, is \code{fixr( brilliant, new.doc=TRUE)}; again, see \code{\link{flatdoc}} and \code{\link{doc2Rd}} if you want to understand what's going on. The format is almost exactly as displayed in plain-text help, i.e. from \code{help(..., help_type="text")}. My recommendation is to just start writing something that looks reasonable, and see whether it works. To test the ultimate appearance, you'll need to run \code{\link{patch.install}} to update the help system, as explained below in MAINTAINING.A.PACKAGE. If you run into problems with writing documentation for your functions, then refer to \code{\link{doc2Rd}} for further details of format, such as how to document several functions in the same file.

You can also provide three other types of documentation, for: (i) general use of your package (please do! it helps the user a lot; packages where the doco PDF consists only of an alphabetical list of functions/objects are a pain); (ii) more specific aspects of usage that are not tied to individual functions, such as this file; and (iii) datasets. These types of documentation should be stored in the package as text objects whose name ends in ".doc"; examples of the three types could be "Splendid.package.doc", "glitzograms.with.Splendid.doc", and "earlobes.doc" if you have a dataset \code{earlobes}. See \code{\link{doc2Rd}} for format details.

You must document every function and dataset that the user will see, but you don't need to document any others. The foregoing applies iff your package is namespaced (see next), which it must be for R{} 2.14 up.
}
\section{Namespaces}{Usually this is automatic. \code{\link{pre.install}} etc automatically creates a "NAMESPACE" file for your package, ensuring inter alia that all documented objects are user-visible. To load DLLs, add a \code{.onLoad} function that contains the body code of \code{\link{generic.dll.loader}} in package \pkg{mvbutils} (thus avoiding dependence on \code{mvbutils}). For more complicated fiddling, see \bold{Customizing package creation}.
\subsection{Packages without namespaces pre r 2 14}{Namespaces only became compulsory with R{} 2.14. If you're setting up your package in an earlier version of R{}, \code{mvbutils} will \emph{not} create a namespace unless it finds a \code{.onLoad} function. To trigger namespacing, just create a \code{.onLoad} with this definition: \code{function( libname, pkgname) {}}.
}
}
\section{Maintaining a package}{
Once you have successfully gotten your \code{Splendid} package installed and loaded the first time, you should rarely need to call \code{\link{install.pkg}} or \code{\link{build.pkg}} etc again, except when you are about to distribute to others. In your own work, after calling \code{\link{maintain.packages}} and \code{library} in an R{} session, you can modify, add and delete functions, datasets, and documentation in your package via the standard functions \code{\link{fixr}}, \code{\link{move}}, and \code{\link{rm.pkg}} (or directly), and these changes will mostly be immediately manifested in the loaded package within your R{} session-- this is "live editing". The changes are made first to the in-memory task package, which will be called e.g. \code{..Splendid}, and then propagated to the loaded package. Don't try to manipulate the loaded package's namespace directly. See \code{\link{maintain.packages}} for details.

To update the installed package (on disk), call \code{patch.install( Splendid)}; this also calls \code{\link{pre.install}} to update the source package, updates the help system in the current session, and does a few other synchronizations. You need to call \code{\link{patch.install}} before quitting R{} to ensure that the changes are manifest in the loaded package the next time you start R{}; otherwise they will only exist in the in-memory task package, and won't be callable.
\subsection{Troubleshooting}{
In rare cases, you may find that \code{maintain.packages( Splendid)} fails. If that happens, there won't be a \code{..Splendid} environment, which means you can't fix whatever caused the load failure. The load failure is (invariably in my experience) caused by a hidden attempt to load a namespaced package, which is failing for yet another reason, usually something in its \code{.onLoad}; that package might or might not be "Splendid" itself. If you can work out what other package is trying to load itself-- say \code{badpack}-- you can temporarily get round the problem by making use of the character vector \code{partial.namespaces}, which lives in the "mvb.session.info" search environment, as follows:

\preformatted{
partial.namespaces <<- c( partial.namespaces, "badpack")
}

That will prevent execution of \code{badpack:::.onLoad}. Consequently \code{badpack} won't be properly loaded, but at least the task package will be loaded into \code{..Splendid}, so that you can make a start on the problem. If you can't work out which package is causing the trouble, try

\preformatted{
partial.namespaces <<- "EVERY PACKAGE"
}

After that, no namespaced package will load properly, so remember to clear \code{partial.namespaces <<- NULL} before resuming normal service.

You might also find \code{\link{find.lurking.envs}} useful, via \code{eapply( ..Splendid, find.lurking.envs)}; this will show any functions (or other things) in \code{..Splendid} that have accidentally acquired a non-standard environment such as a namespace, which can trigger a "hidden" package load attempt. The environment for all functions in \code{..Splendid} \emph{should} probably be \code{.GlobalEnv}; the environments in the \emph{loaded} package will be different, of course.
}
}
\section{Distributing and checking}{
\code{\link{build.pkg}} calls R{} CMD BUILD to create a "tarball" of the package (a ".tar.gz" file), which is the appropriate format for distribution to Unix folk and submission to CRAN. \code{\link{build.pkg.binary}} creates a binary package (a ".zip" file), suitable for Windows or Macs. \code{\link{check.pkg}} runs R{} CMD CHECK, which is required by CRAN and sometimes useful at other times. These \code{.pkg} functions are pretty simple wrappers to the R{} CMD tools with similar names. However, for those with imperfect memories and limited time, there are enough arcane and mutable nuances with the "raw" R{} CMD commands (including the risk of inadvertently deleting existing installations) to make the wrappers in \code{mvbutils} useful.

Various functions in the \pkg{tools} package can be used to check specific aspects of an \emph{installed} package, without needing a full-on (and slowish) R{} CMD CHECK-- \bold{but} note that they will first unload your package and then reload it, so they may disrupt your session especially if compiled code is involved. I find \code{codoc} and \code{undoc} useful, eg via \code{codoc( "debug", file.path( tasks[ "debug"], "debug"))}. Nothing is printed unless a problem is found, so a blank result is good news!
}
\section{Different r versions}{You might need to distribute different versions of your package to go with different R{} versions. (This happened with-- at least-- 2.10, 2.12, and 2.14.) The \code{dir.above.source} argument of \code{\link{pre.install}} can be used to create different source package versions. Presumably you'll install the results into different R{} libraries.
}
\section{Customizing package creation}{You can customize many aspects of the \pkg{mvbutils} package-creation process, by adding a function \code{pre.install.hook.Splendid} to your package. See \code{\link{pre.install}} for further details.
}
\keyword{misc}
