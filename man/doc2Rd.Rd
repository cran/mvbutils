\name{doc2Rd}
\alias{doc2Rd}
\title{Converts plain-text documentation to Rd format}
\description{
Converts plain-text documentation into an Rd-format character vector, optionally writing it to a file. It is almost the inverse of "RCMD Rdconv -t=txt". Works either on a function with a character-mode \code{doc} attribute, or on a separate text object (useful for package and dataset documentation). \code{doc2Rd} will produce legal Rd even with completely informal documentation (see \bold{Details}). However, if your documentation looks like plain pager-style R help, then \code{doc2Rd} makes a big effort to generate the special Rd fields (sections, code fragments, links, URLs, etc) that result in nice-looking help.

You probably won't need to call \code{doc2Rd} yourself, because \code{\link{pre.install}} does it for you when you are building a package; the entire documentation of package \pkg{mvbutils} was produced this way. The main point of this helpfile is to describe how to write good plain-text documentation.
}
\usage{
doc2Rd( text, file=NULL, append=, warnings.on=TRUE)
}
\arguments{
\item{ text}{(character or function) character vector of documentation, or a function with a \code{doc} attribute that is a c.v. of d..}
\item{ file}{(string or connection) if non-NULL, write the output to this file}
\item{ append}{(logical) only applies if \code{!is.null(file)}; should output be appended rather than overwriting?}
\item{ warnings.on}{(logical) ?display warnings about apparently informal documentation?}
}
\value{Character vector containing the text as it would appear in an Rd file, with \code{class} of "cat" so it prints nicely on the screen.}
\details{
Flat-format (plain-text) documentation in \code{doc} attributes can be displayed by the replacement \code{help} in \code{mvbutils} (see \code{\link{dochelp}}) without any further ado. This is very useful while developing code before the package-creation stage, and you can write such documentation any way you want. When you want to generate a package, \code{doc2Rd} will convert pretty much anything into a legal Rd file. However, if you can follow a very few rules, using \code{doc2Rd} will actually give nice-looking authentic R help. For this to work, your documentation basically needs to look like a plain-text help file, as displayed by \code{help( ..., pager=T)}.

Rather than wading through this help file to work out how to write plain-text help, just have a look at a couple of R's help screens in the pager (i.e. not HTML or CHM help) and try making one yourself. You can also use \code{\link{help2flatdoc}} to convert an existing plain-text help file. Also check the file "sample.fun.rrr" in the "demostuff" subdirectory of this package (see \bold{Examples}). If something doesn't work, delve more deeply\dots

\itemize{
\item There are no "escape characters"-- the system is "text WYSIWYG". For example, if you type a \\\enc{}{} character in your doc, \code{help} will display a \\\enc{}{} in that spot. Single quotes and percent signs can have special implications, though-- see below.
\item Section titles should either be fully capitalized, or end with a : character. The capitalized version shows up more clearly in informal help. Replace any spaces with periods, e.g. SEE.ALSO not SEE ALSO.
\item "Item lists", such as in the ARGUMENTS section and sometimes the VALUE section, should be indented. and have a colon to separate the item name from the item body.
\item General lists of items, like this bullet-point list, should be indented and should start with a "-" character or a "*" character, followed by a space.
\item Your spacing is generally ignored (exceptions: USAGE, EXAMPLES, multi-line code blocks; see previous point). Text is wrapped, so you should write paragraphs as single lines without hard line breaks. Use blank lines generously, to make your life easier; also, they will help readability of informal helpfiles.
\item To mark \emph{in-line} code fragments (including variable names, package names, etc-- basically things that R could parse), put them in single quotes. Hence you \bold{can't} use single quotes \bold{within} in-line code fragments.
}

#ifdef flub 

This flatdoc help file can't show you an example of what you can't do in a flatdoc help file!

#endif

An example of what you couldn't include:

\code{'myfun( "'No no no!'")'}

\itemize{
\item Single quotes are OK within multi-code blocks, USAGE, and EXAMPLES. For multi-line code blocks in other sections, don't bother with the single-quotes mechanism. Instead, insert a "\%\%\#" line before the first line of the block, and make sure there is a blank line after the block.
\item You can insert "hidden lines", starting with a \% character, which get passed to the Rd conversion routines. If the line starts with \%\%, then the Rd conversion routines will ignore it too. The "\%\%\#" line to introduce multi-line code blocks is a special case of this.
\item Some other special constructs, such as links, can be obtained by using particular phrases in your documentation; see \bold{Special fields} below.
}
}
\note{In rare cases, backslashes and braces inside marked code (fragments, multi-line blocks, and the USAGE and EXAMPLES sections) will cause harmless but annoying warnings about missing links during RCMD CHECK. This is a consequence of buggy behaviour in the Rd conversion routines; the only way I could get these characters to display completely reliably, was by introducing empty links.}
\section{Special fields}{
Almost anything between a pair of single quotes will be put into a \\\enc{}{}code\{\enc{}{}\}\enc{}{} construct, and the quotes will be removed. The first exception is if the quotes are inside USAGE, EXAMPLES, or a multi-line code block. The second is if the first quote is preceded by anything other than " ", "(" or "-". The final semi-exception is that a few special cases are put into other constructs, as follows. In SEE.ALSO, any single word between single quotes becomes a link. Everywhere else, anything of the form "See XXX" or "see XXX" or "XXX (qv)", where XXX is in single quotes, will be put into a \\\enc{}{}code\{\enc{}{}\\\enc{}{}link\{\enc{}{}\}\enc{}{}\}\enc{}{} construct, and the " (qv)" will be removed (see also next para). With "[pP]ackage XXX" and "XXX package", a \\\enc{}{}pkg\{\enc{}{}\}\enc{}{} construct will be used. References to \code{.GlobalEnv} and \code{.BaseNamespaceEnv} go into \\\enc{}{}env\{\enc{}{}\}\enc{}{} constructs.

URLs and email addresses should be enclosed in <\dots>; they are auto-detected and put into \\\enc{}{}url\{\enc{}{}\}\enc{}{} and \\\enc{}{}email\{\enc{}{}\}\enc{}{} constructs respectively.

Lines that start with a \% will have the \% removed before conversion, so their contents will be passed to RCMD Rdconv later (unless you start the line with \%\%). They aren't displayed by \code{dochelp}, though, so can be used to hide an unhelpful USAGE, say, or to hide an "\#ifdef windows".

Triple dots are converted to \\\enc{}{}dots, regardless of whether they're in code or normal text. A solitary capital-R is converted to \\\enc{}{}R.

Any reasonable "*b*old" or "\_emphatic stuff\_" constructions (no quotes, just the asterisks) will go into \\\enc{}{}bold\{\enc{}{}\}\enc{}{} and \\\enc{}{}emph\{\enc{}{}\}\enc{}{} constructs respectively, to give \bold{b}old or \emph{emphatic stuff}. (Those first two didn't, because they are "unreasonable"-- in particular, they're quoted.) No other fancy constructs are supported (yet).
}
\section{Extreme details}{
The first line should be the docfile name (without the Rd) followed by a few spaces and the package descriptor, like so:

utility-funs package:mypack

When \code{doc2Rd} runs, the docfile name will appear in both the \\\enc{}{}name\{\enc{}{}\}\enc{}{} field and the first \\\enc{}{}alias\{\enc{}{}\}\enc{}{} field. \code{pre.install} will actually create the file "utility-funs.Rd". The next non-blank lines form the other alias entries. Each line should consist of one word, preceded by one or more spaces for safety (not necessary if they have normal names).

"Informal documentation" is interpreted as any documentation that doesn't include a "DECRIPTION" line (or a "Description:" line). If this is the case, \code{doc2Rd} first looks for a blank line, treats everything before it as \\\enc{}{}alias\{\enc{}{}\}\enc{}{} entries, and then generates a DESCRIPTION section into which all the rest of your documentation goes. No other sections in your documentation are recognized, but all the special field substitutions above are applied. (If you really don't want them to be, use the multi-line code block mechanism.) Token USAGE, ARGUMENTS, and KEYWORDS sections are appended automatically, to keep RCMD happy.

Section titles built into Rd are: DESCRIPTION, USAGE, SYNOPSIS, ARGUMENTS, VALUE, DETAILS, EXAMPLES, AUTHOR or AUTHOR(S), SEE.ALSO, REFERENCES, NOTE, KEYWORDS and, for data documentation only, FORMAT and SOURCE. Other section titles (in capitals, or terminated with a colon) can be used, and will be sentence-cased and wrapped in a \\\enc{}{}section\{\enc{}{}\}\enc{}{} construct. Cross-refs to sections will be picked up if of the form "see ANOTHER.SECTION" or "ANOTHER.SECTION (qv)".

The \\\enc{}{}docType field is set automatically for data documentation (iff a FORMAT section is found) and for package documentation (iff the name on the first line includes "-package").

Spacing within lines does matter in USAGE, EXAMPLES, and multi-line code blocks, where what you type really is what you get. The main issue is in the package "manual" that RCMD generates for you, where the line lengths are very short and overflows are common. (Overflows are also common with in-line code fragments, but little can be done about that.) The "RCMD Rd2dvi --pdf" utility is helpful for seeing how individual helpfiles come out.

In SEE.ALSO, the syntax is slightly different; names of things to link to should \bold{not} be in single quotes, and should be separated by commas or semicolons; they will be put into \\\enc{}{}code\{\enc{}{}\\\enc{}{}link\{\enc{}{}\}\enc{}{}\}\enc{}{} constructs. You can split SEE.ALSO across several lines; this won't matter for pager help, but can help produce tidier output in the file "***-manual.tex" produced by RCMD CHECK.

In EXAMPLES, to designate "don't run" segments, put a "\#\# Don't run" line before and a "\#\# End don't run" line after.

In KEYWORDS, separate the keywords with commas, semicolons, or line breaks; don't use quotes. A token KEYWORDS section will be auto-generated if you don't include one, to keep RCMD happy.
}
\section{Infrequently asked questions}{
 \bold{Q:} I have written a fancy \emph{displayed} equation using \\\enc{}{}deqn\{\enc{}{}\}\enc{}{} and desperately want to include it. Can I?

 \bold{A:} Yes (though IMO fancy equations don't belong in function doco; how about a vignette?). Just prefix all the lines of your \\\enc{}{}deqn with \%. If you want something to show up in informal help, too, then make sure you also include lines with the text version of the equation.

 \bold{Q:} I have written a fancy \emph{in-line} equation using \\\enc{}{}eqn\{\enc{}{}\}\enc{}{} and desperately want to include it. Can I?

 \bold{A:} No. Sorry.

 \bold{Q:} For some reason I want to see one thing in informal help (i.e. when the package isn't actually loaded but just sitting on the search path), but a different thing in formal help. Can I do that?

 \bold{A:} If you must. Use the \%-line mechanism for the formal help version, and then insert a line "\%\#ifdef flub" before the informal version, and a line "\%\#endif" after it. Your text version will show up in informal help, and your fancy version will show up in all help produced via Rd. (Anyone using the "flub" operating system will see both versions\dots)

 \bold{Q:} How can I insert a file/kbd/samp/option/acronym etc tag?

 \bold{A:} You can't. They all look like single quotes in pager-style help, anyway.

 \bold{Q:} What about S3?

 \bold{A:} S3 methods can be documented just like any other function, except for one small detail: in the USAGE section, the generic name instead of your method name should be used. IE if you are documenting a method \code{print.cat}, the USAGE section should contain a call to \code{print(x,...)} rather than \code{print.cat(x,...)}. \code{doc2Rd} will detect this and produce correct Rd format. If you document several different S3 methods for the same generic in the same piece of documentation (and even if you don't), then it may help the user if you put a comment with the method name after each call, e.g. \code{print(x,...) # print.cat}, especially if the optional arguments are different. Doing that also gives \code{pre.install} a better chance of correctly sorting out the documentation.

 Sometimes you have a function which looks like it might be an S3 method, but isn't (e.g. \code{as.data.frame.I} in package \pkg{mvbutils}). This will be handled correctly by \code{pre.install} because you will of course have used the full name in the USAGE section, because it isn't a method.

 S3 classes themselves need to be documented via a relevant function, using aliasses.

 \bold{Q:} What about S4?

 \bold{A:} I am not a fan of S4 and have found no need for it in many 1000s of lines of R code\dots hence I haven't included any explicit support for it so far. Nevertheless, things might well work anyway, unless special Rd constructs are needed. If \code{doc2Rd} \emph{doesn't} work for your S4 stuff (bear in mind that the \%-line mechanism may help), then for now you'll still have to write S4 Rd files yourself; see \code{\link{pre.install}} for where to put them. However, if anyone would like the flatdoc facility for S4 and is willing to help out, I'm happy to try to add support.
}
\author{Mark Bravington}
\seealso{
The file "sample.fun.rrr" in subdirectory "demostuff", and the demo "flatdoc.demo.r".

To do a whole group at once: \code{\link{pre.install}}.

To check the results: "RCMD Rd2dvi --pdf XXX.Rd" and "RCMD Rdconv -t=html XXX.Rd" and/or "-t=txt" (though \code{\link{patch.installed}} will allow to check the HTML version immediately).

To convert existing Rd documentation: \code{\link{help2flatdoc}}.

If you want to tinker with the underlying mechanisms: \code{\link{flatdoc}}, \code{\link{write.sourceable.function}}
}
\examples{
## Needs a function with the right kind of "doc" attr
## Look at file "demostuff/sample.fun.rrr"
lodlibs <- library()[[2]]
mvb.lib.loc <- lodlibs[ lodlibs[,1]=='mvbutils', 2][1]
sample.fun <- source.mvb( file.path( mvb.lib.loc, 'mvbutils', 'demostuff', 'sample.fun.rrr'))
cat( '***Original plain-text doco:***\\n')
print( as.cat( attr( sample.fun, 'doc'))) # unescaped, ie what you'd actually edit
cat( '\\n***Rd output:***\\n')
sample.fun.Rd <- doc2Rd( sample.fun)
print( sample.fun.Rd) # already "cat" class
}
\keyword{misc}
