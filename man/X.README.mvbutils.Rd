\name{.README.mvbutils}
\alias{.README.mvbutils}
\alias{README.mvbutils}
\title{How to use mvbutils}
\description{For how to use \code{mvbutils}, see the DETAILS section below.}
\usage{
README.mvbutils()
.README.mvbutils()
}
\details{
\code{mvbutils} is a collection of utilities offering the following main features:

\itemize{
\item Hierarchical organization of projects and sub-projects, allowing switching within a single \R session, searching and moving objects through the hierarchy, objects in ancestor projects always visible from child (sub)projects, etc. See \code{\link{cd}}.
\item Improved function editing facilities, interfacing with whichever text editor you prefer. The \R command line is not frozen while editing, and you can have multiple edit windows open. There is also a complete automatic backup system, with (as a side-effect) the ability to sort objects by date. See \code{\link{fixr}}.
\item Support for flat-format (plain-text) documentation, stored along with a function and editable at the same time, and viewable through normal \code{help}. Automatic conversion to Rd format is available if certain rules are followed. See \code{\link{flatdoc}}.
\item Support for nesting of \code{source} calls, and for interspersing \R code and data in the same file. See \code{\link{source.mvb}}.
\item Support for easy macro-like functions which act in their caller's environment (see also RNews 1/3), and for "dynamically scoped" functions. See \code{\link{mlocal}} and \code{\link{do.in.envir}}.
\item Graphical display of which functions call which other functions. See \code{\link{foodweb}}.
}

There are also numerous lower-level utility functions and operators: see \code{?"mvbutils-utils"} and \code{?"mvbutils-operators"}.

To get the full features of \code{mvbutils} (in particular, the project organization), you need to start \R in the \emph{same} directory every time (your "ROOT task"), and then switch to whichever project from inside \R; see \code{\link{cd}}. Various \code{options} always need to be set to make \code{fixr} and the \pkg{debug} package work the way you want, so one advantage of the start-in-the-same directory-approach is that you can keep all your project-independent \code{options()}, library loads, etc., in a single \code{.First} function, called automatically when you start \R. However, most features (including support for the \pkg{debug} package) will probably work even if you don't follow this suggestion.

On loading, \code{mvbutils} creates a new environment in the search path, called \code{mvb.session.info}, which stores some housekeeping information. \code{mvb.session.info} is never written to disk, and disappears when the \R session finishes. [For Splus users: \code{mvb.session.info} is similar to frame 0.] You should never change anything in \code{mvb.session.info} by hand, but it is sometimes useful to look at some of the variables there:

\itemize{
\item \code{.First.top.search} is the directory \R started in (your ROOT task).
\item \code{.Path} shows the currently-attached part of the task hierarchy.
\item \code{base.xxx} is the original copy of an overwritten system function, e.g. \code{help}
\item \code{fix.list} keeps track of functions being edited via \code{\link{fixr}}
\item \code{session.start.time} is the value of \code{Sys.time()} when \code{mvbutils} was loaded
\item \code{source.list} is used by \code{source.mvb} to allow nesting of sources
\item \code{tracees} is used by the \pkg{debug} package to store debugging information
\item \code{r.window.handle} is used by the \pkg{handy} package (Windows only)
}
}
\note{
On loading, the present version of \code{mvbutils} overwrites two system functions: \code{library} and \code{help}. The modifications should have no side-effects, and/but I hope to be able to avoid them altogether in future versions of \R. Briefly:

\itemize{
\item \code{library} is modified so that its default \code{pos} argument becomes a call to \code{lib.pos()}, so packages are attached just below ROOT rather than always in position 2 (needed by \code{cd});
\item \code{help} is modified so that, if system \code{help} can't find help for a simple object (not a method or package), it will look for the \code{doc} attribute of that object (if any) and display it in a pager using \code{dochelp}.
}

If you are certain that you \emph{don't} want these replacements, set \code{options(mvbutils.replacements=FALSE)} before loading \code{mvbutils}. However, this will prevent \code{cd} and the flat-documentation help from working properly.

\emph{Display bugs}: if you have a buggy Unix display where \code{readline()} always returns the cursor to the start of the line, overwriting any prompt, then try \code{options( cd.extra.CR=TRUE)}.
}
\seealso{ \code{\link{cd}}, \code{\link{fixr}}, \code{\link{flatdoc}}, \code{\link{dochelp}}, \code{\link{source.mvb}}, \code{\link{mlocal}}, \code{\link{do.in.envir}}, \code{\link{foodweb}}, \code{\link{clip}}, \code{\link{mvbutils-operators}}, \code{\link{mvbutils-utils}}, package \pkg{debug}}
\author{Mark Bravington}
\keyword{documentation}
