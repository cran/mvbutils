\name{doc2Rd}
\alias{doc2Rd}
\title{Converts flat documentation to Rd format}
\description{For functions whose \code{help} documentation is stored as a \code{doc} attribute; the text in the \code{doc} attribute is converted into a Rd format character vector. The documentation of the \pkg{mvbutils} package and the \pkg{debug} package was produced this way.}
\usage{ doc2Rd( text)}
\arguments{\item{ text}{character vector of documentation, or a function with a \code{doc} attribute.}}
\value{Character vector containing the text as it would appear in an Rd file. Use \code{cat} with \code{sep="\\\\n"} to display this.}
\details{
Flat-format (plain-text) documentation in \code{doc} attributes can be displayed by the replacement \code{help} in \code{mvbutils} (see \code{\link{dochelp}}). For informal purposes, you can write such documentation any way you want. However, if you want to use \code{doc2Rd} to convert it to Rd format, the documentation must follow the rules below. At first sight, these might seem worse than having to learn how to write "native" Rd format, but in practice they are very simple. Check the file "sample.fun.rrr" in the "demo" subdirectory to see how it's done. The formal description below is harder to follow.

The first line should be the docfile name (without the Rd) followed by a few spaces and the package descriptor, like so:

utility-funs package:mypack

When \code{doc2Rd} runs, the docfile name will appear in both the name\{\} field and the first alias\{\} field. \code{\link{formalize.package}} will actually create the file "utility-funs.Rd".

The next non-blank lines form the other alias entries. Each line should consist of one word, preceded by one or more spaces for safety (not necessary if they have normal names).

After the blank line at the end of the aliases, put a one-line title, without any quotes.

Put section titles in capital letters. With multi-word section titles, replace the spaces by periods: e.g. write SEE.ALSO instead of SEE ALSO. The recognized sections are: DESCRIPTION, USAGE, SYNOPSIS, ARGUMENTS, VALUE, DETAILS, EXAMPLES, AUTHOR, SEE.ALSO, REFERENCES, NOTE, KEYWORDS.

Other unrecognized section titles (in capitals) can be used, and will be sentence-cased and wrapped in a section\{\} construct. If an unrecognized section title ends with a period, the text up to the next section will be treated like ARGUMENTS; otherwise, it will be treated like DESCRIPTION (basically, as plain text).

Bulleted lists within DESCRIPTION, DETAILS, AUTHOR, REFERENCES and NOTE sections are identified by lines starting with one or more spaces, then a "-", then another space. Such lists are converted into itemize\{item \dots, item \dots\} constructs.

Each ARGUMENTS item, and optionally each VALUE item, should be on a line beginning with one or more spaces. Lines that don't start with a space will be interpreted as normal text in these sections. Normal text is fine, but you need to itemize arguments to satisfy RCMD CHECK.

Almost anything between a pair of single quotes will be put into a code\{\} construct, and the quotes will be removed. Anything of the form "See XXX" or "see XXX" or "XXX (qv)", where XXX is in single quotes, will be put into a code\{ link\{ \} \} construct, and the " (qv)" will be removed (see also next para). In USAGE and EXAMPLES, and inside code fragments elsewhere, make sure that only double quotes " are used. The exception to code\{\} is that, with "[pP]ackage \code{x}" and "'x' package", \code{x} will go into a pkg\{\} construct rather than a code construct.

Function names in SEE.ALSO should \emph{not} be in single quotes, and should be separated by commas or semicolons; they will be put into code\{ link\{ \} \} constructs. Single-quoted words (with "-" and "." allowed internally) not terminated by a comma or semicolon go into code\{\} constructs (i.e. no link\{\}), or pkg\{\} constructs if preceded or followed by "package". Unquoted words not terminated by a comma or semicolon or end-of-line will not be put into constructs. End-of-line spaces are stripped first. You can split SEE.ALSO across several lines; this won't matter for pager help, but can help produce tidier output in the file "***-manual.tex" produced by RCMD CHECK.

For some reason, two successive closing braces without a space between them seem to cause problems for RCMD CHECK when creating "***-manual.tex"-- so don't do that!

For "don't run" examples, put a "\#\# Don't run" line before and a "\#\# End don't run" line after.

Start the keyword list with a space, and separate keywords with commas or semicolons.

Lines that start with a \% will have the \% removed before conversion. They aren't displayed by \code{dochelp}, though, so can be used to hide an unhelpful SYNOPSIS, say, or to hide an "\#ifdef windows".

Triple dots are converted to \\\\dots, regardless of whether they're in code or normal text. \R is converted to \\\\R.

Any "*emphatic*" constructions (no quotes, just the asterisks) will go into emph\{\} constructs. No other fancy constructs are supported (yet).
}
\examples{
\dontrun{
## Needs a function with the right kind of "doc" attr
text.doc <- doc2Rd( source.mvb)
cat( doc2Rd( doc2Rd), sep="\\n")
}
}
\seealso{
 To do a whole group at once: \code{\link{formalize.package}}
 The file "sample.fun.rrr" and the demo "flatdoc.demo.r" in subdirectory "demo"
 Also: \code{\link{flatdoc}}, \code{\link{make.usage.section}}, \code{\link{make.arguments.section}}, \code{\link{write.sourceable.function}}
}
\author{Mark Bravington}
\keyword{documentation}
