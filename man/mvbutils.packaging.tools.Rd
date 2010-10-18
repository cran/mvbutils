\name{mvbutils.packaging.tools
}
\alias{mvbutils.packaging.tools
}
\title{How to create & maintain packages with mvbutils
}
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
\item call \code{maintain.packages( Splendid)}
\item Call \code{pre.install( Splendid)} to create a "source package" in a subdirectory of Splendid's task directory. The subdirectory will be called "Splendid".
\item Make sure you have all the R{} build tools installed and on your path-- see "R-exts" for details
\item From a command prompt, run "R CMD build Splendid" (Windows: "R CMD build --binary Splendid") to create the installable version-- a file "Splendid_1.0.tar.gz" (Windows: "Splendid_1.0.zip") in the task directory.
\item Install the package however you would normally. In Windows, I do this from inside R{}, via the "Packages/Install from local Zip" option, after "R CMD build --binary" in the previous step.
\item Call \code{library(Splendid)}; your package will be loaded for use, and is also ready for live-editing.
}

Your package will probably just about work now, but the result won't yet be perfect. The first thing is that you will need to give it a sensible DESCRIPTION file. \code{mvbutils} creates a default even if you don't, but it won't be what you really want, as you'll realize if you type \code{library( help=Splendid)}. Easiest is to copy the default DESCRIPTION file from the new subdirectory "Splendid" into Splendid's home directory, then change it with a text editor. Apart from the obvious changes (your name etc), the most important field to add is "Depends:", to say what other packages are needed by Splendid (called "Imports:" if your package is NAMESPACEd-- see below). Once this modified DESCRIPTION file is sitting in Splendid's home directory, you will rarely need to change it again.

The additional steps you'll likely need are these:

\itemize{
\item Copy and edit the DESCRIPTION file (see above)
\item Provide documentation (see below)
\item Namespace the package (see below)
\item Sort out any C/Fortran source code, pre-compiled code, demos, and other additional files (see \code{\link{pre.install}})
\item Move any subtasks of Splendid to one level up the task hierarchy (see \code{\link{maintain.packages}})
}

Once you have set up "Splendid" so that \code{\link{maintain.packages}} works, you should never need to \code{\link{cd}} directly into "Splendid" again.
}
\section{Converting an existing package}{
Suppose you have already have a \emph{source} package "hardway", and would like to try maintaining it via \code{mvbutils}. You'll need to create a task package, then create a new version of the source package, then re-install it. The first step is to call \code{unpackage( hardway)} to creat the task package "hardway" in a subdirectory of the current task. Plain-text documentation will be attached to functions, or stored as ".doc" text objects. All functions and documentation must thereafter be edited using \code{\link{fixr}}. The full sequence is something like:

\preformatted{
# Create task package in subdirectory of current:
unpackage( "path/to/source/package/hardway")
#
# Load image into memory:
maintain.packages( hardway)
#
# Make new version of source package:
pre.install( hardway, ...) # use dir= to control where new source pkg goes
# Now RCMD BUILD and INSTALL "hardway"...
# ...in Windows, I use R CMD build -binary and then the Menu commands...
# ..."Packages/Install from local zip file"
#
# If all goes well, then:
library( hardway) # off yer go
}

If you get problems after \code{\link{maintain.packages}}, you might need \code{unmaintain.package( hardway)} to clear out the in-memory copy of the new task package.
}
\section{Documentation}{
Documentation for functions can be stored as plain text just after a function's source code. The easiest way to add skeletal documentation to your function \code{brilliant}, is \code{fixr( brilliant, new.doc=TRUE)}; see \code{\link{flatdoc}} if you want to understand what's going on. The format is almost exactly as displayed in plain-text help, i.e. from \code{help(..., help_type="text")} in R{} >= 2.10, or \code{help(...,chmhelp=FALSE,htmlhelp=FALSE)} prior to that. My recommendation is to just start writing something that looks reasonable, and see whether it works; to test the appearance, you'll need to run \code{\link{patch.install}} to update the help system, as explained below in MAINTAINING.A.PACKAGE. If you run into problems with writing documentation for your functions, then refer to \code{\link{doc2Rd}} for further details of format, such as how to document several functions in the same file.

You can also provide three other types of documentation, for: (i) general use of your package (please do! it helps the user a lot; packages where the doco PDF consists only of an alphabetical list of functions/objects are a pain); (ii) more specific aspects of usage that are not tied to individual functions, such as this file; and (iii) datasets. These types of documentation should be stored in the package as text objects whose name ends in ".doc"; examples of the three types could be "Splendid.package.doc", "glitzograms.with.Splendid.doc", and "earlobes.doc" if you have a dataset \code{earlobes}. See \code{\link{doc2Rd}} for format details.

If your package is namespaced (see below), you must document every function and dataset that the user will see, but you don't need to document any others.

Documentation of functions can be checked using \code{tools::codoc} (qv).
}
\section{Namespaces}{Packages really should be namespaced. The payoff for the end user is control over name clashes in different packages, and less clutter. The payoff for \emph{you} is that you don't have to waste time documenting functions that aren't visible to the end user. Namespacing isn't compulsory, and the simplest case described above doesn't add a namespace. However, namespacing with \code{mvbutils} is trivial; just add a function \code{.onload} with this definition: \code{function( libname, pkgname) {}}. (If you want to load compiled code, use the body of \code{mvbutils:::generic.dll.loader} instead of the empty braces.) The only user-visible functions and datasets will be those that are named or aliased explicitly in your documentation; all others will be hidden. The only other recommended change if your package is namespaced, is for the DESCRIPTION file to say "Imports:" rather than "Depends:".
}
\section{Maintaining a package}{
Once you have successfully gotten your package installed and loaded the first time, you should rarely need to call RCMD BUILD or INSTALL again, except when you are about to distribute to others. After calling \code{\link{maintain.packages}} and \code{library} in an R{} session, you can modify, add and delete functions, datasets, and documentation in your package, and these changes will mostly be immediately manifested in the package within the session-- this is "live editing". To update the installed package on disk accordingly, call \code{patch.install( Splendid)}-- this also calls \code{\link{pre.install}} to update the source package, updates the help system in the current session, and does a few other synchronizations.

\code{maintain.packages( Splendid)} loads your task package into an environment \code{..Splendid}, which lives in the "mvb.session.info" workspace on the search path. You can move objects to and from other tasks via \code{\link{move}}, with \code{..Splendid} as the \code{from} or \code{to} argument; you can create functions and text via \code{\link{fixr}} with argument \code{pkg="Splendid"}; you can delete objects via \code{\link{rm.pkg}} with argument \code{pkg="Splendid"}. Intrepid users can also create things directly, via \code{..Splendid$newthing <<- ...}. In most cases, you will be prompted afterwards for whether to update the task package, but you can always do yourself via \code{Save.pos( ..Splendid)}. Note that only these updates only update the \bold{task package}. To update the \bold{source package} using the task package, call \code{\link{pre.install}}; to update the \bold{installed package} (and the source package), call \code{\link{patch.install}}.

In rare cases, you may find that \code{maintain.packages( Splendid)} fails. If that happens, there won't be a \code{..Splendid} environment, which means you can't fix whatever caused the load failure. The load failure is (invariably in my experience) caused by a "hidden" attempt to load a namespaced package, which is failing for yet another reason, usually something in its \code{.onLoad}; that package might or might not be "Splendid" itself. If you can work out what other package is trying to load itself-- say "badpack"-- you can temporarily get round the problem by making use of the character vector \code{partial.namespaces}, which lives in the "mvb.session.info" search environment, as follows:

\preformatted{
partial.namespaces <<- c( partial.namespaces, "badpack")
}

That will prevent execution of "badpack"'s \code{.onLoad}. "badpack" won't be properly loaded, but at least the task package will be loaded into \code{..Splendid}, so that you can make a start on the problem. If you can't work out which package is causing the trouble, try

\preformatted{
partial.namespaces <<- "EVERY PACKAGE"
}

After that, no namespaced package will load properly, even if it is flawless; so remember to clear \code{partial.namespaces <<- NULL} before resuming normal service.

You might also find \code{\link{find.lurking.envs}} useful, via \code{eapply( ..Splendid, find.lurking.envs)}; this will show any functions (or other things) in \code{..Splendid} that have accidentally acquired a non-standard environment such as a namespace, which can trigger a "hidden" package load attempt. The environment for all functions in \code{..Splendid} \emph{should} probably be \code{.GlobalEnv}; the environments in the \emph{loaded} package will be different, of course.
}
\section{Different r versions}{You might need to distribute different versions of your package to go with different R{} versions. (This happened with the change from R{} 2.9 to 2.10; the Rdoc format changed unbackcompatibly.) The \code{subdir} argument can be used to create different source package versions. Presumably you'll install the results into different R{} libraries.
}
\section{Customizing package creation}{You can customize many aspects of the \pkg{mvbutils} package-creation process, by adding a function \code{pre.install.hook.Splendid} to your package. See \code{\link{pre.install}} for further details.
}
\keyword{misc}
