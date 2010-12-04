\name{mvbutils-package}
\alias{mvbutils}
\alias{mvbutils-package}
\docType{package}
\title{How to use the mvbutils package}
\description{
Package \pkg{mvbutils} is a collection of utilities offering the following main features:

\itemize{
\item Hierarchical organization of projects (tasks) and sub-tasks, allowing switching within a single R{} session, searching and moving objects through the hierarchy, objects in ancestor tasks always visible from child (sub)tasks, etc. See \code{\link{cd}}.
\item Improved function & text-object editing facilities, interfacing with whichever text editor you prefer. The R{} command line is not frozen while editing, and you can have multiple edit windows open. Documentation can be stored as plain text after the function definition, and will be found by \code{help}. There is also a complete automatic text-format backup system for functions. See \code{\link{fixr}}.
\item Automated package construction, including production of Rd-format from plain text documentation. Packages can be edited & updated while loaded, without needing to quit/rebuild/reinstall. See \code{\link{mvbutils.packaging.tools}}.
\item "Lazy loading" for individual objects, allowing fast and transparent access to collections of biggish objects where only a few objects are used at a time. See \code{\link{mlazy}}.
\item Miscellaneous goodies: local/nested functions (\code{\link{mlocal}}), display of what-calls-what (\code{\link{foodweb}}), multiple replacement (\code{\link{multirep}}), numerous lower-level lower-level utility functions and operators (\code{\link{mvbutils.utils}}, \code{\link{mvbutils.operators}}, \code{\link{extract.named}}, \code{\link{mcut}}, \code{\link{search.for.regexpr}}, \code{\link{strip.missing}}, \code{\link{Hours}}).
}

To get the full features of the \pkg{mvbutils} package-- in particular, the project organization-- you need to start R{} in the same directory every time (your "ROOT task"), and then switch to whichever project from inside R{}; see \code{\link{cd}}. Various \code{options} always need to be set to make \code{\link{fixr}} and the \pkg{debug} package work the way you want, so one advantage of the start-in-the-same directory-approach is that you can keep all your project-independent \code{options()}, library loads, etc., in a single \code{.First} function, called automatically when you start R{}. However, most features (including support for the \pkg{debug} package) may work even if you don't follow this suggestion.
}
\section{Note1}{
On loading, the \pkg{mvbutils} package creates a new environment in the search path, called \code{mvb.session.info}, which stores some housekeeping information. \code{mvb.session.info} is never written to disk, and disappears when the R{} session finishes. [For Splus users: \code{mvb.session.info} is similar to frame 0.] You should never change anything in \code{mvb.session.info} by hand, but it is sometimes useful to look at some of the variables there:

\itemize{
\item \code{.First.top.search} is the directory R{} started in (your ROOT task).
\item \code{.Path} shows the currently-attached part of the task hierarchy.
\item \code{base.xxx} is the original copy of an overwritten system function, e.g. \code{help}
\item \code{fix.list} keeps track of functions being edited via \code{\link{fixr}}
\item \code{session.start.time} is the value of \code{Sys.time()} when \code{mvbutils} was loaded
\item \code{source.list} is used by \code{\link{source.mvb}} to allow nesting of sources
\item \code{r.window.handle} is used by the \pkg{handy} package (Windows only)
\item \code{partial.namespaces} is used to alleviate difficulties with unloadable data files-- see \code{\link{mvbutils.packaging.tools}}
\item things whose name starts with ".." are environments used in live-editing packages
\item \code{maintained.packages} is a list of the above
}
}
\section{Note2}{
On loading, the present version of package \pkg{mvbutils} compulsorily overwrites a few system functions: \code{library}, \code{rbind.data.frame}, \code{lockEnvironment}, \code{loadNamespace}. By default, it also overwrites \code{help}, \code{savehistory}, \code{loadhistory}, \code{save.image}, \code{difftime}, \code{+.POSIXt}, and \code{-.POSIXt}. (The original version of routine \code{xxx} can always be obtained via \code{base.xxx} if you really need it.) The mods are undone when you unload \code{mvbutils}. The mods should have [almost] no side-effects, and/but I hope to be able to avoid them altogether in future versions of R{}. Briefly:

\itemize{
\item \code{library} is modified so that its default \code{\link{pos}} argument becomes a call to \code{lib.pos()}. This means that packages get attached just below ROOT rather than always in position 2 (needed by \code{\link{cd}}).
\item \code{lockEnvironment} is modified to allow live-editing of loaded packages-- no change to default behaviour.
\item \code{loadNamespace} has the default value of its "partial" argument altered, to let you bypass \code{.onLoad} for selected faulty packages-- see \code{\link{mvbutils.packaging.tools}} and look for \code{partial.namespaces}. This allows the loading of certain ".RData" files which otherwise crash from "hidden" attempts to load a namespace.
\item \code{rbind.data.frame} is modified to work better (IMO) when the first \code{data.frame} has zero rows, to cope with a code-breaking change in R{}'s behaviour several versions ago. Specifically, the modified version does \emph{not} drop zero-row data.frames, and their column attributes are taken account of when \code{rbind.data.frame} ing to the other args. This is useful when repeatedly adding rows to an initially-empty data.frame. To see the difference, experiment with \code{rbind( data.frame( x=1, y=factor( "a")), data.frame( x=2, y=factor( "b"))[-1,])$y} vs \code{base.rbind.data.frame(...)} with the same arguments. \code{mvbutils} and \code{debug} rely on the non-default behaviour, so the overwriting is not optional.
}

Optional but recommended replacements are as follows:

\itemize{
\item \code{help} is modified so that, if system \code{help} can't find help for a function (but not a method, dataset, or package), it will look for a \code{doc} attribute of the function to display in a pager using \code{\link{dochelp}}.
\item \code{loadhistory} and \code{savehistory} are modified so that they use the "R_HISTFILE" environment variable if it set. This can be set dynamically during an R{} session using \code{Sys.setenv}. Standard R{} behaviour is to respect "R_HISTFILE" iff it is set \emph{before} the R{} session starts. If "R_HISTFILE" is not set, then \code{\link{cd}} will on first use set "R_HISTFILE" to "<<ROOT task>>/.RHistory", so that same the history file will be used throughout each and every session.
\item \code{save.image} is modified to call \code{\link{Save}} instead; this will behave exactly the same for workspaces not using \code{mvbutils} task-hierarchy feature or the \pkg{debug} package, but otherwise will prevent problems with \code{mtrace} d functions and \code{\link{mlazy}} ed objects.
\item \code{difftime}, \code{+.POSIXt}, and \code{-.POSIXt} are modified to behave more consistently and forgivingly. Results won't break code that doesn't make invalid assumptions. [This should probably be done in a different package. I'm also unsure whether my changes are still necessary, following changes in R{} since about 2.9.]
}

If you are certain that you don't want the optional replacements, set \code{options(mvbutils.replacements=FALSE)} \emph{before} loading \code{mvbutils}. However, this will prevent \code{\link{cd}}, \code{\link{fixr}}, and the flat-documentation help from working properly. You can also set the "mvbutils.replacements" option to a character vector comprising some or all of the above names.

After \code{mvbutils} has loaded, you can undo the modification of an individual function called \code{xxx} with \code{assign.to.base( "xxx", base.xxx)}.

ESS.and.'mvbutils'

For ESS users: I'm not an Emacs user and so haven't tried ESS with the \pkg{mvbutils} package myself, but a read-through of the ESS documentation (as of ~2005) suggests that a couple of ESS variables may need changing to get the two working optimally. Please check the ESS documentation for further details on these points. I will update this helpfile when/if I receive more feedback on what works.

\itemize{
\item \code{\link{cd}} changes the search list, so you may need to alter "ess-change-sp-regex" in ESS.
\item \code{\link{cd}} also changes the prompt, so you may need to alter "inferior-ess-prompt". Prompts have the form WORD1/WORD2/\dots{}/WORDn> where WORDx is a letter followed by zero or more letters, underscores, periods, or digits.
\item \code{\link{move}} can add/remove objects in workspaces other than the top one, so if ESS relies on stored internal summaries of "what's where", these may need updating.
}

Display bugs: if you have a buggy Unix display where \code{readline()} always returns the cursor to the start of the line, overwriting any prompt, then try \code{options( cd.extra.CR=TRUE)}.
}
\author{Mark Bravington}
\seealso{\code{\link{cd}}, \code{\link{fixr}}, \code{\link{mlazy}}, \code{\link{flatdoc}}, \code{\link{dochelp}}, \code{\link{maintain.packages}}, \code{\link{source.mvb}}, \code{\link{mlocal}}, \code{\link{do.in.envir}}, \code{\link{foodweb}}, \code{\link{mvbutils.operators}}, \code{\link{mvbutils.utils}}, package \pkg{debug}}
\keyword{misc}
