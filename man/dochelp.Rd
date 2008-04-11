\name{dochelp}
\alias{dochelp}
\title{Documentation}
\description{\code{dochelp(topic)} will be invoked by the replacement \code{help} if conventional \code{help} fails to find documentation for topic \code{topic}. If \code{topic} has a \code{doc} attribute, the latter will be formatted and displayed by \code{file.show}. \code{dochelp} is not usually called directly.}
\usage{
# Not usually called directly
# If it is, then normal usage is: dochelp( topic)
dochelp( topic, doc)
}
\arguments{
\item{ topic}{(character) name of the object to look for help on.}
\item{ doc}{(character or list)-- normally not set, but deduced by default from \code{topic}; see \bold{Details}.}
}
\details{
\code{dochelp} will only be called if the original \code{help} call was a simple \code{help(topic=X,...)} form, with X not a call and with no \code{try.all.packages} or \code{type} or \code{lib.loc} arguments (the other \code{help} options are OK).

The \code{doc} argument defaults to the \code{doc} attribute of \code{get("topic")}. The only reason to supply a non-default argument would be to use \code{dochelp} as a pager; this might have some value, since \code{dochelp} does reformat character vectors to fit nicely in the system pager window, one paragraph per element, using \code{\link{strwrap}}. Elements starting with a "\%" symbol are not displayed.

To work with \code{dochelp}, a \code{doc} attribute should be either:

\itemize{
\item a character vector, of length >=1. New elements get line breaks in the pager. Or:
\item a length-one list, containing the name of another object with a \code{doc} attribute. \code{dochelp} will then use the \code{doc} attribute of that object instead. This referencing can be iterated.
}

If the documentation is very informal, start it with a blank line to prevent \code{find.documented( ..., doctype="Rd")} from finding it.

The file will be re-formatted to fit the pager; each paragraph should be a single element in the character vector. Elements starting with a \% will be dropped (but may still be useful for \code{doc2Rd}).

\code{\link{flatdoc}} offers an easy way to incorporate plain-text (flat-format) documentation-- formal or informal-- in the same text file as a function definition, allowing easy maintenance.
}
\seealso{\code{\link{flatdoc}}, \code{\link{doc2Rd}}, \code{\link{find.documented}}, \code{\link{strwrap}}}
\examples{
myfun <- structure( function() 1,
  doc="Here is some informal documentation for myfun\\n")
dochelp( "myfun")
help( "myfun") # calls dochelp
}
\author{Mark Bravington}
\keyword{documentation}
