\name{fixr
}
\alias{fixr
}
\alias{fixtext
}
\alias{readr
}
\alias{FF
}
\alias{autoedit
}
\title{Editing functions, text objects, and scriptlets
}
\description{\code{fixr} opens a function, (or text object, or scriptlet for making a single object of any type) in your preferred text editor. Control returns immediately to the R{} command line, so you can keep working in R{} and can be editing several objects simultaneously (cf \code{edit}). A session-duration list of objects being edited is maintained, so that each object can be easily sourced back into its rightful workspace. These objects will be updated automatically on file-change if you've run \code{autoedit( TRUE)}, or manually by calling \code{FF()}. There is an optional automatic text backup facility. \code{readr} also opens a file in your text editor, but in read-only mode, and doesn't update the backups or the list of objects being edited. \code{fixtext} is a shorthand form for forcing creation of a text object rather than the default of a function. For non-function and non-text objects, you can edit a scriptlet that creates the object and is stored as its \code{source} attribute. The object is updated whenever the scriptlet is changed, by running the scriptlet; see separate section below.
}
\usage{
  # Usually: fixr( x) or fixr( x, new.doc=T)
  fixr( x, new=FALSE, install=FALSE, what, fixing, pkg=NULL, character.only=FALSE, new.doc=FALSE)
  # fixtext really has exact same args as fixr, but technically its args are:
  fixtext( x, ...)
  # Usually: readr( x) but exact same args as fixr
  readr( x, ...)
  FF() # manual check and update
  autoedit( do=TRUE) # stick this line in your .First
}
\arguments{
\item{ x}{a quoted or unquoted name of a function, text object, or general object. You can also write \code{mypack$myfun}, or \code{mypack::myfun}, or \code{mypack:::myfun}, or \code{..mypack$myfun}, to simultaneously set the \code{pkg} argument. Note that \code{fixr} uses non-standard evaluation of its \code{x} argument, unless you specify \code{character.only=TRUE}.}
\item{ character.only}{(logical) if TRUE, \code{x} is treated as a string naming the object to be edited, rather than the unquoted object name.}
\item{ new.doc}{(logical) if TRUE, add skeleton plain-text R-style documentatation, as per \code{add.flatdoc.to}. Also use this to create an empty scriptlet for a general (non-function, non-text) object.}
\item{ new}{(logical, seldom used) if TRUE, edit a blank function template rather than any existing copy in the search path. New edit will go into \code{.GlobalEnv} unless argument \code{pkg} is set.}
\item{ install}{(logical, rarely used) logical indicating whether to go through the process of asking you about your editor}
\item{ what}{(logical, rarely used) if no pre-existing \code{x}, then \code{fixr} creates an empty function template by default. Set \code{what=""} to create an empty character vector instead-- or just use \code{fixtext}.}
\item{ fixing}{(logical, rarely used) FALSE for read-only (i.e. just opening editor to examine the object)}
\item{ pkg}{(string or environment) if non-NULL, then specifies in which package a specific maintained package (see \code{\link{maintain.packages}}) \code{x} should be looked for.}
\item{ do}{(logical) TRUE => automatically update objects from altered files; FALSE => don't.}
\item{ ...}{other arguments, except \code{what} in \code{fixtext}, and \code{fixing} in \code{readr}, are passed to \code{fixr}.}
}
\note{
\code{fixr} is designed to be used with \code{\link{cd}}; I'm not sure it will work independently.

Originally, \code{fixr} was only for functions, and not even for functions in packages, so that it was mostly an alternative to e.g. ESS; if you liked ESS, you wouldn't have bothered with \code{fixr}. However, \code{fixr} now has more sophisticated purposes, in particular being the only way of using the package-maintenance features in the \pkg{mvbutils} package. It would be interesting to find out if it can be integrated with e.g. ESS (which I know nothing about). Input welcome.
}
\details{
When \code{fixr} is run for the first time (or if you set \code{install=TRUE}), it will ask you for some basic information about your text editor. In particular, you'll need to know what to type at a command prompt to invoke your text editor on a specific file; in Windows, you can usually find this by copying the Properties/Shortcut/Target field of a shortcut, followed by a space and the filename. After supplying these details, \code{fixr} will launch the editor and print a message showing some \code{options} (\code{"backup.fix"}, \code{"edit.scratchdir"} and \code{"program.editor"}), that will need to be set in your \code{.First}. function. You should now be able to do that via \code{fixr(.First)}.

Changes to the temporary files used for editing can be checked for automatically whenever a valid R{} command is typed (e.g. by typing 0<ENTER>; <ENTER> alone doesn't work). To set this up, call \code{autoedit()} once per session, e.g. in your \code{.First}. The manual version of this is \code{FF()}. If any file changes are detected, the code is sourced back in and the appropriate function(s) are modified. \code{FF} tries to write functions back into the workspace they came from, which might not be \code{.GlobalEnv}. If not, you'll be asked whether you want to \code{\link{Save}} that workspace (provided it's a task-- see \code{\link{cd}}). \code{FF} should still put the function in the right place, even if you've called \code{\link{cd}} after calling \code{fixr} (unless you've detached the original task) or if you \code{\link{move}}d it.

If something goes wrong during an automatic call to \code{FF} (very unusual), the automatic-call feature will stop working. To get it back in the current R{} session, do \code{autoedit(F)} and then \code{autoedit(T)}. It will come back anyway in a new R{} session.

\code{readr} requires a similar installation process. To get the read-only feature, you'll need to add some kind of option/switch on the command line that invokes your text editor; not all text editors support this feature. Similarly to \code{fixr}, you'll need to set \code{options( program.reader=<<something>>)} in your \code{.First}; the installation process will tell you what to use.

\code{fixr}, and of course \code{fixtext}, will also edit character vectors. If the object to be edited exists beforehand and has a class attribute, \code{fixr} will not change its class; otherwise, the class will be set to "cat". This means that \code{print} invokes the \code{\link{print.cat}} method, which displays text more readably than the default. Any other attributes on character vectors are stripped.

\code{fixr} creates a blank function template if the object doesn't exist already, or if \code{new=TRUE}. If you want to create a new character vector as opposed to a new function, call \code{fixtext}, or equivalently set \code{what=""} when you call \code{fixr}.

If the function has attributes, it's wrapped in a \code{structure(...)} construct. If a \code{doc} attribute exists, it's printed as free-form text at the end of the file, and the call to \code{structure} will end with a line similar to:

\preformatted{
,doc=flatdoc( EOF="<<end of doc>>"))
}

When the file is sourced back in, that line will cause the rest of the file-- which should be free-format text, with no escape characters etc.-- to be read in as a \code{doc} attribute, which can be displayed by \code{help}. If you want to add plain-text documentation, you can also add these lines yourself-- see \code{\link{flatdoc}}. Calling \code{fixr( myfun, new.doc=TRUE)} sets up a documentation template that you can fill in, ready for later conversion to Rd format in a package (see \code{\link{mvbutils.packaging.tools}}).

If the function was being \code{mtrace}d (see \code{help(package=debug)}), \code{FF} will re-apply \code{mtrace} after loading the edited version.

If there is a problem with parsing, the \code{source} attribute of the function is updated to the new code, but the function body is invisibly replaced with a \code{stop} call, stating that parsing failed.

The list of functions being edited by \code{fixr} is stored in the variable \code{fix.list} in the \code{mvb.session.info} environment. When you quit and restart R{}, the function files you have been using will stay open in the editor, but \code{fix.list} will be empty; hence, updating the file "myfun.r" will not update the corresponding R{} function. If this happens, just type \code{fixr(myfun)} in R{} and when your editor asks you if you want to replace the on-screen version, say no. Save the file again (some editors require a token modification, such as space-then-delete, first) and R{} will notice the update.

An automatic text backup facility is available from \code{fixr}: see \code{?get.backup}. The backup system also allows you to sort functions by edit date; see \code{?fix.order}. Backup currently only works for functions and character objects.
}
\section{Scriptlets for general objects}{
\code{fixr} can edit an object of any type, within reason; more precisely, it can edit a "scriptlet" of R{} code that generates the object. The scriptlet is evaluated as soon as \code{FF} detects a changed file, and the result is assigned to the object. Only the first complete expression in a scriptlet is evaluated; use braces to group multiple expressions. The scriptlet should not include the assignment to the final object; e.g., the scriptlet for \code{myobj} should be something like \code{1:10}, not \code{myobj <- 1:10}. Evaluation takes place in a temporary environment inheriting from \code{.GlobalEnv}, so side-effect assignments during the scriptlet are discarded. The scriptlet itself is stored as the "source" attribute of the object, of class \code{cat}.

Two cases I find useful are:

\itemize{
\item instructions to create data.frames or matrices by reading from a text file, and maybe doing some initial processing;
\item expressions for complicated calls with particular datasets to model-fitting functions such as \code{glm}.
}

Note that the second case is not meant to be evaluated immediately-- I often just want to save a call, not the result of the call. The solution is to wrap the statement in a call to \code{quote()} or \code{expression()}.

\preformatted{
{ # Brace needed because several expressions are involved
  raw.data <- read.table( "bigfile.txt", header=TRUE, row=NULL)
  # Condense date/time char fields into something more useful:
  raw.data <- within( raw.data, {
    Time <- strptime( paste( DATE, TIME, sep=' '), format="\%Y-\%m-\%d \%H:\%M:\%S")
    rm( DATE, TIME)
  })
  raw.data
}
}

To edit a new general object \code{myobj}, call \code{fixr( myobj, what=list())}; it doesn't matter whether \code{myobj} will actually be a list. If you forget the \code{what} argument, you can just replace the function template with your scriptlet; the \code{fixr} paradigm for general objects is the same as for functions. However, omitting \code{what=list()} can lead to problems if your scriptlet fails to parse.

If you want to use \code{fixr} to edit a general object that wasn't created with \code{fixr}, just set \code{new.doc=TRUE}; you shouldn't need the \code{what} argument.

If you're calling \code{fixr} on an object that already has a scriptlet, the scriptlet will appear and you edit that.

You can use the \pkg{debug} package on scriptlets. To turn debugging on/off, use \code{mtrace( eval.scriptlet)} and \code{mtrace( eval.scriptlet, F)}. Currently, there is no way to select particular scriptlets; either all are debugged (during updating) or none are.

The backup files for general objects show the scriptlet, not the result.
}
\seealso{\code{.First}, \code{edit}, \code{\link{cd}}, \code{\link{get.backup}}, \code{\link{fix.order}}, \code{\link{move}}
}
\keyword{utilities}
\keyword{programming
}
