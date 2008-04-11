\name{mvbutils-package}
\alias{mvbutils}
\alias{mvbutils-package}
\docType{package}
\title{How to use the mvbutils package}
\description{
Package \pkg{mvbutils} is a collection of utilities offering the following main features:

\itemize{
\item Hierarchical organization of projects and sub-projects, allowing switching within a single R session, searching and moving objects through the hierarchy, objects in ancestor projects always visible from child (sub)projects, etc. See \code{\link{cd}}.
\item Improved function & text-object editing facilities, interfacing with whichever text editor you prefer. The R command line is not frozen while editing, and you can have multiple edit windows open. There is also a complete automatic text-format backup system for functions. See \code{\link{fixr}}.
\item "Lazy loading" for individual objects, allowing fast & efficient access to collections of biggish objects where only a few objects are used at a time. See \code{\link{mlazy}}.
\item Support for flat-format (plain-text) documentation, stored along with a function and editable at the same time, and viewable through normal \code{help} (i.e. semi-literate programming). Automatic conversion to Rd format is available if certain rules are followed. See \code{\link{flatdoc}}.
\item Support for transparent "live" editing of your own loaded packages (even/especially with namespaces), and for conversion to formal R source package format; see \code{\link{maintain.packages}}.
\item Support for nesting of \code{source} calls, and for interspersing R code and data in the same file. See \code{\link{source.mvb}}.
\item Support for easy macro-like functions which act in their caller's environment (see also RNews 1/3), and for "dynamically scoped" functions. See \code{\link{mlocal}} and \code{do.in.envir}.
\item Graphical display of which functions call which other functions. See \code{\link{foodweb}}.
}

There are also numerous lower-level utility functions and operators: see \code{?"mvbutils-utils"} and \code{?"mvbutils-operators"}.

To get the full features of the \pkg{mvbutils} package (in particular, the project organization), you need to start R in the \bold{same} directory every time (your "ROOT task"), and then switch to whichever project from inside R; see \code{\link{cd}}. Various \code{options} always need to be set to make \code{fixr} and the \pkg{debug} package work the way you want, so one advantage of the start-in-the-same directory-approach is that you can keep all your project-independent \code{options()}, library loads, etc., in a single \code{.First} function, called automatically when you start R. However, most features (including support for the \pkg{debug} package) will probably work even if you don't follow this suggestion.

On loading, the \pkg{mvbutils} package creates a new environment in the search path, called \code{mvb.session.info}, which stores some housekeeping information. \code{mvb.session.info} is never written to disk, and disappears when the R session finishes. [For Splus users: \code{mvb.session.info} is similar to frame 0.] You should never change anything in \code{mvb.session.info} by hand, but it is sometimes useful to look at some of the variables there:

\itemize{
\item \code{.First.top.search} is the directory R started in (your ROOT task).
\item \code{.Path} shows the currently-attached part of the task hierarchy.
\item \code{base.xxx} is the original copy of an overwritten system function, e.g. \code{help}
\item \code{fix.list} keeps track of functions being edited via \code{\link{fixr}}
\item \code{session.start.time} is the value of \code{Sys.time()} when \code{mvbutils} was loaded
\item \code{source.list} is used by \code{source.mvb} to allow nesting of sources
\item \code{r.window.handle} is used by the \pkg{handy} package (Windows only)
\item things whose name starts with ".." are environments used in live-editing packages
}
}
\note{Version 2.2.0 is partly a maintenance fix, and partly a premature release of a major new feature: the easy package-building and maintenance routines. The documentation for the latter is incomplete, and the routines haven't been thoroughly tested except on my own half-dozen packages. (If anyone is brave enough to try the package-related material, I'd appreciate feedback!) The reason for rushing this version out is to deal with a fatal non-back-compatible change deep inside R 2.8; it also sorts out a number of other mostly minor bugs that have hatched or been unearthed since the last CRAN release. I will release a properly-documented version as soon as I can, hopefully with an R-news article to describe the package-maintenance routines.}
\section{Note2}{
On loading, the present version of package \pkg{mvbutils} compulsorily overwrites a few system functions: \code{library}, \code{rbind.data.frame}, \code{lockEnvironment}. By default, it also overwrites \code{help}, \code{savehistory}, \code{loadhistory}, \code{save.image}, \code{difftime}, \code{+.POSIXt}, and \code{-.POSIXt}. (The original version of routine \code{xxx} can always be obtained via \code{base.xxx} if you really need it.) The modifications should have [almost] no side-effects, and/but I hope to be able to avoid them altogether in future versions of R. Briefly:

\itemize{
\item \code{library} is modified so that its default \code{pos} argument becomes a call to \code{lib.pos()}. This means that packages get attached just below ROOT rather than always in position 2 (needed by \code{cd}).
\item \code{lockEnvironment} gets an X-rated hack to allow editing of "live" packages (see \code{\link{maintain.packages}})-- no change to default behaviour.
\item \code{rbind.data.frame} is modified to work better (IMO) when the first \code{data.frame} has zero rows, to cope with a code-breaking change in R's behaviour several versions ago. Specifically, the modified version does \bold{not} drop zero-row data.frames, and their column attributes are taken account of when \code{rbind} ing to the other args. In other words, \code{rbind( data.frame( x=1, y="a"), data.frame( x=2, y="b"))[-1,]$y} now has the same levels as \code{rbind( data.frame( x=1, y="a")[-1,], data.frame( x=2, y="b"))$y}. Experiment with \code{base.rbind.data.frame} instead, if you want to see the difference.
\item \code{help} is modified so that, if system \code{help} can't find help for a simple object (not a method or package), it will look for the \code{doc} attribute of that object (if any) and display it in a pager using \code{dochelp}.
\item \code{savehistory} and \code{loadhistory} are modified so that they use the \code{R_HISTFILE} environment variable if it set. This can be set dynamically during an R session using \code{Sys.setenv}. Standard R behaviour is to respect \code{R_HISTFILE} iff it is set \bold{before} the R session starts. Note that, by default, \code{cd} will on first use set \code{R_HISTFILE} to what it thinks is the ROOT task, so that same the history file will be used throughout the session; see \code{\link{cd}} if you want to change this.
\item \code{save.image} is modified to call \code{\link{Save}} instead; this will behave exactly the same for workspaces not using \code{mvbutils} task-hierarchy feature or the \pkg{debug} package, but otherwise will prevent problems with \code{mtrace} d functions and \code{mlazy} ed functions.
\item \code{difftime}, \code{+.POSIXt}, and \code{-.POSIXt} are modified to behave more consistently and forgivingly. Results won't break code that doesn't make invalid assumptions.
}

If you are certain that you \bold{don't} want the optional replacements, set \code{options(mvbutils.replacements=FALSE)} before loading \code{mvbutils}. However, this will prevent \code{cd}, \code{fixr}, and the flat-documentation help from working properly. You can undo the modification of an individual function called \code{xxx} with \code{assign.to.base( "xxx", base.xxx)}.

ESS.and.'mvbutils'

For ESS users: I'm not an Emacs user and so haven't tried ESS with the \pkg{mvbutils} package myself, but a read-through of the ESS documentation suggests that a couple of ESS variables may need changing to get the two working optimally. Please check the ESS documentation for further details on these points. I will update this helpfile when/if I receive more feedback on what works.

\itemize{
\item \code{cd} changes the search list, so you may need to alter "ess-change-sp-regex" in ESS.
\item \code{cd} also changes the prompt, so you may need to alter "inferior-ess-prompt". Prompts have the form WORD1/WORD2/\dots/WORDn> where WORDx is a letter followed by one or more letters, underscores, periods, or digits.
\item \code{move} can add/remove objects in workspaces other than the top one, so if ESS relies on stored internal summaries of "what's where", these may need updating.
}

Display bugs: if you have a buggy Unix display where \code{readline()} always returns the cursor to the start of the line, overwriting any prompt, then try \code{options( cd.extra.CR=TRUE)}.
}
\seealso{\code{\link{cd}}, \code{\link{fixr}}, \code{\link{mlazy}}, \code{\link{flatdoc}}, \code{\link{dochelp}}, \code{\link{maintain.packages}}, \code{\link{source.mvb}}, \code{\link{mlocal}}, \code{\link{do.in.envir}}, \code{\link{foodweb}}, \code{mvbutils-operators}, \code{mvbutils-utils}, package \pkg{debug}}
\author{Mark Bravington}
\keyword{documentation}
