\name{flatdoc}
\alias{flatdoc}
\title{Flat-format documentation}
\description{\code{flatdoc} lets you edit plain-text documentation in the same file as your function definition. The documentation can be completely informal, or can be close to the standard on-screen appearance of \R help files (and if so can be converted to actual Rd files by \code{doc2Rd}). This can be useful for your own notes, for informal distribution, and for formal distribution when you are trying to avoid managing dozens of separate source and documentation files.}
\synopsis{flatdoc( EOF="<<end of doc>>")}
\usage{structure( function( \dots) \{body\}, doc=flatdoc( EOF="<<end of doc>>"))}
\arguments{
\item{ EOF}{character string showing when plain text ends, as in \code{readlines.mvb}}
\item{ body}{replace with your function code}
\item{ \dots}{replace with your function arg list}
}
\value{Character vector of class \code{docattr}, as read from the \code{current.source()} (qv) connection.}
\details{
A combined function-and-documentation text file should contain your function code wrapped in a \code{structure(} construct, followed by a line like this:

, doc=flatdoc())

The rest of the file is plain-text documentation, which will be formatted and shown by \code{\link{dochelp}} when \code{help} or \code{?} is called. The documentation can be very informal, but if you follow some guidelines you will be able to convert it automatically to an Rd-format file by calling \code{\link{doc2Rd}}.

When such a file is read in using \code{\link{source.mvb}}, the \code{flatdoc} call will cause the rest of the file to be read in as plain text, and assigned to the \code{doc} attribute of the function. Documentation can be terminated with this line:

<<end of doc>>

The above line will causes \code{source.mvb} to revert to normal statement processing mode for the rest of the file. Note that vanilla \code{source} will not respect \code{flatdoc}; you do need to use \code{source.mvb}.

Files in this format can be produced by \code{write.sourceable.function}, or by hand. The \code{fixr} editing system automatically outputs and inputs \code{flatdoc} format for functions with flat-format documentation.

On some text editors, you can modify syntax highlighting so that the "start of comment block" marker is set to the string "doc=flatdoc(".

It's possible to use \code{flatdoc} to read in more than one free-format text attribute. The \code{EOF} argument can be used to distinguish one block of free text from the next.

\code{flatdoc} should never be called from the command line; it should only appear in text files designed for \code{source.mvb}.

The \code{\link{fixr}} editor interface automatically uses the \code{flatdoc} approach to allow editing of source code and plain-text documentation in the same file. If you use a different editor interface, but like the idea of using \code{flatdoc}-style documentation, then you will need to:

\itemize{
\item make sure \code{source.mvb} rather than \code{source} is used to read in text files;
\item if your system produces text files for the editor directly from function objects (which \code{fixr} does), then make sure that the files are produced by \code{\link{write.sourceable.function}} rather than \code{print} or \code{write} or \code{cat}.
}
}
\note{
You can also write combined code-and-documentation functions whose documentation merely \emph{refers} to another function, like an alias in an Rd file. To refer to documentation stored with a function \code{x}, instead of setting \code{doc} to \code{flatdoc()} at the end of your structure(\dots) construct, set \code{doc} to \code{list("x")}. The referencing can be several layers deep; the documentation of \code{x} can refer to \code{y}, whose documentation refers to \code{z}, whose documentation is actually flatdoc()-style.

If you are writing documentation for a group of functions together, you only need to \code{flatdoc} one of them, say \code{myfun1}. Informal help will work if you modify the others to e.g.

myfun2 <- structure( function(\dots) \{ whatever\}, doc=list("myfun1"))

If you are writing with \code{doc2Rd} in mind and a number of such functions are to be grouped together, e.g. a group of "internal" functions in preparation for formal package release, you may find \code{make.usage.section} and \code{make.arguments.section} helpful.

One reason for needing \code{flatdoc}, rather than just putting \code{,doc="giant character string"}, is that \R requires a backslash at the end of each line of a multi-line string. This format is very painful to edit.
}
\seealso{
 \code{\link{source.mvb}}, \code{\link{doc2Rd}}, \code{\link{dochelp}}, \code{\link{write.sourceable.function}}, \code{\link{make.usage.section}},
 \code{\link{make.arguments.section}}, \code{\link{fixr}}, the demo in "flatdoc.demo.R"
}
\examples{
\dontrun{
## Put everything before the next comment into a text file <<your filename>>
structure( function( x) \{
 x*x
\}
,doc=flatdoc("<<end of doc>>"))
Here is some informal documentation for the "SQUARE" function
<<end of doc>>
## Now try SQUARE <- source.mvb( <<your filename>>); ?SQUARE
## Put everything before the next comment into a text file
myfun <- structure( function(\dots) \{\}
, att1=flatdoc( EOF="<<end of part 1>>")
, att2=flatdoc( EOF="<<end of part 2>>"))
This goes into "att1"
<<end of part 1>>
and this goes into "att2"
<<end of part 2>>
## Now "source.mvb" that file, to create "myfun"
cat( attr( myfun, "att1")) # This goes into "att1"
cat( attr( myfun, "att2")) # This goes into "att2"
}
}
\author{Mark Bravington}
\keyword{documentation}
\keyword{programming}
