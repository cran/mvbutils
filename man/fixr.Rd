\name{fixr}
\alias{fixr}
\alias{fixtext}
\alias{readr}
\alias{FF}
\alias{autoedit}
\title{Editing functions}
\description{\code{fixr} opens a function (or text stored as a character vector) in your preferred text editor. Control returns immediately to the R command line, so you can keep working in R and can be editing several functions simultaneously (cf \code{edit}). A session-duration list of objects being edited is maintained, so that each object can be easily sourced back into its rightful workspace, either manually by calling \code{FF()} or automatically on file change (see \code{\link{autoedit}} below). There is an optional automatic text backup facility. \code{readr} also opens a file in your text editor, but in read-only mode, and doesn't update the backups or the list of objects being edited. \code{fixtext} is a shorthand form for forcing creation of a text object rather than the default of a function.}
\usage{
  # Usually: fixr( x)
  fixr( x, new=FALSE, install=FALSE, what, fixing, pkg=NULL, character.only=FALSE)
  # fixtext really has exact same args as fixr, but technically it
  fixtext( x, ...)
  # Usually: readr( x) but exact same args as fixr
  readr( x, ...)
  FF()
  autoedit( do=TRUE)
}
\arguments{
\item{ x}{a quoted or unquoted name of a function or text object.}
\item{ new}{(rarely used) if TRUE, edit a blank function template rather than any existing copy in the search path. New edit will go into \code{.GlobalEnv} unless argument \code{pkg} is set.}
\item{ install}{(rarely used) logical indicating whether to go through the process of asking you about your editor}
\item{ what}{if no pre-existing \code{x}, then \code{fixr} creates an empty function template by default. Set \code{what=""} to create an empty character vector instead-- or just use \code{fixtext}.}
\item{ fixing}{FALSE for read-only (i.e. just opening editor to examine the object)}
\item{ pkg}{(string or environment) if non-NULL, then specifies in which package a specific maintained package (see \code{\link{maintain.packages}}) \code{x} should be looked for.}
\item{ character.only}{(boolean) if TRUE, \code{x} is treated as a string naming the object to be edited, rather than the unquoted object name.}
\item{ do}{TRUE => automatically update objects from altered files; FALSE => don't.}
\item{ ...}{other arguments (except \code{what} in \code{fixtext}, and \code{fixing} in \code{readr}) are passed to \code{fixr}.}
}
\note{
\code{fixr} is designed to be used with \code{cd}; I'm not sure it will work independently.

ESS users will not be very interested in \code{fixr}, even if they use \code{cd}!
}
\details{
When \code{fixr} is run for the first time (or if you set \code{install=TRUE}), it will ask you for some basic information about your text editor. In particular, you'll need to know what to type to invoke your text editor from the command shell (MS-DOS prompt on Windows) on a specific file. After supplying these details, \code{fixr} will launch the editor and print a message showing some \code{options} (\code{"backup.fix"}, \code{"edit.scratchdir"} and \code{"program.editor"}), that will need to be set in your \code{.First}. function-- which you can do via \code{fixr(.First)}.

\code{readr} requires a similar installation process. To get the read-only feature, you'll need to add some kind of option/switch on the command line that invokes your text editor; not all text editors support this feature. Similarly to \code{fixr}, you'll need to set \code{options( program.reader=<<something>>)} in your \code{.First}; the installation process will tell you what to use.

\code{fixr} will also edit character vectors. It will "respect" class attributes on character vectors; for example, I often set the class to "cat" so that \code{print} invokes my \code{print.cat} method, which displays text more readably than the default. Any other attributes on character vectors are stripped.

\code{fixr} creates a blank function template if none existed, or if \code{new=TRUE} is specified. If you want to create a new character vector as opposed to a new function, set \code{what=""} when you call \code{fixr}.

If the function has attributes, it's wrapped in a \code{structure(...)} construct. If a \code{doc} attribute exists, it's printed as free-form text at the end of the file, and the call to \code{structure} will end with a line similar to:

,doc=flatdoc( EOF="<<end of doc>>"))

When the file is read later on (see next paragraph, and \code{source.mvb} for technical details), that line will cause the rest of the file-- which should be free-format text, with no escape characters etc.-- to be read in as a \code{doc} attribute, which can be displayed by \code{help}. If you want to add "flat-format documentation", you can also add these lines yourself-- see \code{\link{flatdoc}}.

To read functions back in again, call \code{FF()}, which checks the modification times of any function files. If any have been updated, \code{FF} sources in the code and modifies the appropriate functions. It tries to write functions back into the workspace they came from, which might not be .GlobalEnv. If not, you'll be asked whether you want to \code{save.image} that workspace (provided it's a "task"-- see \code{\link{cd}}). \code{FF} should still put the function in the right place, even if you've called \code{cd} after calling \code{fixr} (unless you've detached the original task) or if you \code{move}d it.

To have \code{FF} run automatically whenever a valid command is executed (e.g. by typing 0<ENTER>; <ENTER> alone doesn't work), run \code{autoedit()}.

If the function was being \code{mtrace}d (see \code{help(package=debug)}), \code{FF} will re-apply \code{mtrace} after loading the edited version.

If there is a problem with parsing, the \code{source} attribute of the function is updated to the new code, but the function body is invisibly replaced with a \code{stop} call, stating that parsing failed. \code{FF} also prints an immediate warning.

The list of functions being edited by \code{fixr} is stored in the variable \code{fix.list} in the \code{mvb.session.info} environment. When you quit and restart R, the function files you have been using will stay open in the editor, but \code{fix.list} will be empty; hence, updating the file "myfun.r" will not update the corresponding R function. If this happens, just type \code{fixr(myfun)} in R and when your editor asks you if you want to replace the on-screen version, say no. Save the file again (some editors require a token modification, such as space-then-delete, first) and R will notice the update.

An automatic text backup facility is available from \code{fixr} and \code{FF}: see \code{?get.backup}. The backup system also allows you to sort functions by edit date; see \code{?fix.order}. Backup only works for functions.

\code{fixtext} returns a text vector with class \code{cat}; the \code{print} method for such objects is just \code{cat( x, sep="\\n")} which avoids all the "[1]" and escape-character clutter.
}
\seealso{\code{\link{.First}}, \code{\link{edit}}, \code{\link{cd}}, \code{\link{get.backup}}, \code{\link{fix.order}}, \code{\link{move}}, \code{\link{autoFF}}}
\keyword{utilities}
\keyword{programming}
