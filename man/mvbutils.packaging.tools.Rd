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
First, the simplest case: suppose you have some pure R code and maybe data that you'd like to make into a package called "Splendid". The bare-minimum steps you need are:-

\itemize{
\item Make sure all the code & data lives in a single task called "Splendid".
\item \code{\link{cd}} to the task above "Splendid"
\item Call \code{pre.install( Splendid)} to create a "source package" in a subdirectory of Splendid's task directory. The subdirectory will be called "Splendid".
\item Make sure you have all the R build tools installed and on your path-- see "R-exts" for details
\item From a command prompt, run "R CMD BUILD Splendid" (Windows: "RCMD BUILD --binary Splendid") to create the installable version-- a file "Splendid_1.0.tar.gz" (Windows: "Splendid_1.0.zip") in the task directory.
\item Install the package however you would normally. In Windows, I do this from inside R, via the "Packages/Install from local Zip" option, after "RCMD BUILD --binary" in the previous step.
\item From the task above "Splendid", call \code{maintain.packages(Splendid)}.
\item Call \code{library(Splendid)}; your package will be loaded for use, and is also ready for live-editing.
}

Your package will probably just about work now, but the result won't yet be perfect. The first thing is that you will need to give it a sensible DESCRIPTION file. \code{mvbutils} creates a default even if you don't, but it won't be what you really want, as you'll realize if you type \code{library( help=Splendid)}. The easiest way to do this is to copy the default DESCRIPTION file from the new subdirectory "Splendid" into Splendid's home directory, then change it with a text editor. Apart from the obvious changes (your name etc), the most important field to add is "Depends:", to say what other packages are needed by Splendid (called "Imports:" if your package is NAMESPACEd-- see below). Once this modified DESCRIPTION file is sitting in Splendid's home directory, you will rarely need to change it again.

The additional steps you'll likely need are these:

\itemize{
\item Copy and edit the DESCRIPTION file (see above)
\item Provide documentation (see below)
\item Namespace the package (see below)
\item Sort out any C/Fortran source code, pre-compiled code, demos, and other additional files (see \code{\link{pre.install}})
}

Once you have set up Splendid so that \code{\link{maintain.packages}} works, you should never need to \code{\link{cd}} directly into Splendid again.

\emph{Documentation}

Documentation for functions can be stored as plain text just after a function's source code. The easiest way to add skeletal documentation to your function \code{brilliant}, is \code{fixr( brilliant, new.doc=TRUE)}; see \code{\link{flatdoc}} if you want to understand what's going on. The format is almost exactly as displayed in plain-text help, i.e. from \code{help(..., help_type="text")} in R >= 2.10, or \code{help(...,chmhelp=FALSE,htmlhelp=FALSE)} prior to that. My recommendation is to just start writing something that looks reasonable, and see whether it works; you'll need to run \code{\link{patch.install}} to update the help system, as explained below in MAINTAINING.A.PACKAGE. If you run into problems with writing documentation for your functions, then refer to \code{\link{doc2Rd}} for further details of format, such as how to document several functions in the same file.

You can also provide three other types of documentation, for: (i) general use of your package (please do! it helps the user a lot); (ii) more specific aspects of usage that are not tied to individual functions, such as this file; and (iii) datasets. These types of documentation should be stored in the package as text objects whose name ends in ".doc"; examples of the three types could be "Splendid.package.doc", "glitzograms.with.Splendid.doc", and "earlobes.doc" if you have a dataset \code{earlobes}. See \code{\link{doc2Rd}} for format details.

If your package is namespaced (see below), you must document every function and dataset that the user will see, but you don't need to document any others.

\emph{Namespaces}

Packages really should be namespaced. The payoff for the end user is control over name clashes in different packages, and less clutter. The payoff for \emph{you} is that you don't have to waste time documenting functions that aren't visible to the end user. Namespacing isn't compulsory, and the simplest case described above doesn't add a namespace. However, namespacing with \code{mvbutils} is trivial; just add a function \code{.onload} with this body: \code{function( libname, pkgname) {}}. (If you want to load compiled code, this needs changing; see below.) The only user-visible functions and datasets will be those that are named or aliased explicitly in your documentation; all others will be hidden. The only other recommended change if your package is namespaced, is for the DESCRIPTION file to say "Imports:" rather than "Depends:".
}
\section{Maintaining a package}{
Once you have successfully gotten your package installed and loaded the first time, you should rarely need to call RCMD BUILD or INSTALL again, except when you are about to distribute to others. After calling \code{\link{maintain.packages}} and \code{library} in an R session, you can modify, add and delete functions, datasets, and documentation in your package, and these changes will mostly be immediately manifested in the package within the session-- this is "live editing". To update the installed package on disk accordingly, call \code{patch.install( Splendid)}-- this also calls \code{\link{pre.install}} to update the source package, updates the help system in the current session, and does a few other synchronizations.

\code{maintain.packages( Splendid)} loads your task package into an environment \code{..Splendid}, which lives in the "mvb.session.info" workspace on the search path. You can move objects to and from other tasks via \code{\link{move}}, with \code{..Splendid} as the \code{from} or \code{to} argument; you can create functions and text via \code{\link{fixr}} with argument \code{pkg="Splendid"}; you can delete objects via \code{\link{rm.pkg}} with argument \code{pkg="Splendid"}. Intrepid users can also create things directly, via \code{..Splendid$newthing <<- ...}. In most cases, you will be prompted afterwards for whether to update the task package, but you can always do yourself via \code{Save.pos( ..Splendid)}. Note that only these updates only update the \bold{task package}. To update the \bold{source package} using the task package, call \code{\link{pre.install}}; to update the \bold{installed package} (and the source package), call \code{\link{patch.install}}.
}
\section{Converting an existing package}{Suppose you have already created & installed your package "hardway", and would like to try maintaining it via \code{mvbutils}. Just do \code{unpackage( hardway)}, and a task package "hardway" will be created under the current task. The hope is that \code{maintain.packages(hardway)} and \code{patch.install(hardway)} etc will then work as expected. Plain-text documentation will be attached to functions, or stored as ".doc" text objects. The task package might need a bit of editing.}
\section{Different r versions}{You might need to distribute different versions of your package to go with different R versions. (This happened with the change from R 2.9 to 2.10; the Rdoc format changed unbackcompatibly.) The \code{subdir} argument can be used to create different source package versions. Presumably you'll install the results into different R libraries.}
\section{Customizing package creation}{You can customize many aspects of the \pkg{mvbutils} package-creation process, by adding a function \code{pre.install.hook.Splendid} to your package. See \code{\link{pre.install}} for further details.}
\keyword{misc}
