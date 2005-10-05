\name{mvbutils-utils}
\alias{mvbutils-utils}
\alias{clip}
\alias{cq}
\alias{deparse.names.parsably}
\alias{empty.data.frame}
\alias{exists.mvb}
\alias{expanded.call}
\alias{find.funs}
\alias{index}
\alias{is.dir}
\alias{legal.filename}
\alias{lib.pos}
\alias{lsall}
\alias{masked}
\alias{masking}
\alias{mkdir}
\alias{my.all.equal}
\alias{named}
\alias{option.or.default}
\alias{pos}
\alias{print.cat}
\alias{print.nullprint}
\alias{put.in.session}
\alias{returnList}
\alias{safe.rbind}
\alias{to.regexpr}
\alias{yes.no}
\title{Miscellaneous utilities}
\description{Miscellaneous utilities.}
\usage{
clip( x, n=1)
cq( \dots)
deparse.names.parsably( x)
empty.data.frame( \dots)
exists.mvb( x, pos = -1, envir = pos.to.env(pos), frame, mode = "any", inherits = FALSE)
expanded.call( nlocal=sys.parent())
find.funs(pos=1, \dots)
index( lvector)
is.dir( dir)
legal.filename( name)
lsall( \dots)
lib.pos() # used only to hack \code{library}
masked( pos)
masking( pos=1)
mkdir( dirlist)
my.all.equal( x, y)
named( x)
option.or.default( opt.name, default=NULL)
pos( substrs, mainstrs, any.case = FALSE, names.for.output)
print.cat( x, \dots)
print.nullprint( x, \dots)
put.in.session( \dots)
returnList( \dots)
safe.rbind( df1, df2)
to.regexpr( x)
yes.no( prompt, default)
}
\arguments{
\item{ x, y, n, \dots, nlocal, lvector, dir, name, pos, envir, frame, mode, inherits, dirlist, opt.name, default, substrs,}{}
\item{ mainstrs, any.case, names.for.output, df1, df2, prompt}{see "Arguments by function"}
}
\section{Arguments by function }{
\item{ clip}{x:a vector or list}
\item{ clip}{n:integer saying how many elements to clip from the end of \code{x}}
\item{ cq}{\dots:quoted or unquoted character strings, to be \code{substitute}d and then concatenated}
\item{ deparse.names.parsably}{x:any object for \code{deparse}-- \code{name} objects treated specially}
\item{ empty.data.frame}{\dots:named length-1 vectors of appropriate mode, e.g. "first.col=''"}
\item{ exists.mvb}{x:string, pos = integer, envir = environment, frame = frame number, mode & inherits as for \code{exists}}
\item{ expanded.call}{nlocal:frame to retrieve arguments from. Normally, use the default; see \code{\link{mlocal}}.}
\item{ find.funs}{\dots:extra arguments for \code{objects}. Usually just "pattern" for regexp searches.}
\item{ index}{lvector:vector of TRUE/FALSE/NA}
\item{ is.dir}{dir:character vector of files to check existence and directoriness of.}
\item{ legal.filename}{name:character string to be modified}
\item{ find.funs}{pos:list of environments, or vector of char or numeric positions in search path.}
\item{ lsall}{\dots:as for \code{ls}, except that \code{all.names} will be coerced to TRUE}
\item{ masking, masked}{pos:position in search path}
\item{ mkdir}{dirlist:character vector of directories to create}
\item{ my.all.equal}{x, y:anything}
\item{ named}{x:character vector which will become its own \code{names} attribute}
\item{ option.or.default}{opt.name:character(1)}
\item{ option.or.default}{default:value to be returned if there is no \code{option} called \code{"opt.name"}}
\item{ pos}{substrs:character vector of patterns (literal not regexpr)}
\item{ pos}{mainstrs:character vector to search for \code{substrs} in.}
\item{ pos}{any.case:logical-- ignore case?}
\item{ pos}{names.for.output:character vector to label rows of output matrix; optional}
\item{ print.cat, print.nullprint}{x:thing to print}
\item{ print.cat, print.nullprint}{\dots:args for \code{print} (ignored)}
\item{ put.in.session}{\dots:a named set of objects, to be \code{assign}ed into the \code{mvb.session.info} search environment}
\item{ returnList}{\dots:named or un-named arguments, just as for \code{return} before \R 1.8.}
\item{ safe.rbind}{df1, df2:\code{data.frame} or \code{list}}
\item{ to.regexpr}{x:character vector}
\item{ yes.no}{prompt:string to put before asking for input}
\item{ yes.no}{default:value to return if user just presses <ENTER>}
}
\details{
\code{clip} removes the last \code{n} elements of \code{x}.

\code{cq} is handy for typing \code{cq( alpha, beta, gamma)} instead of \code{cq( "alpha", "beta", "gamma")}. Certain strings DO still require quotes around them, e.g. \code{cq( "NULL", "1-2")}).

\code{deparse.names.parsably} is like \code{deparse} except that \code{name} objects get wrapped in a call to \code{as.name}, so that they won't be evaluated accidentally.

\code{empty.data.frame} creates a template data frame with 0 rows but with all columns of the appropriate type.

\code{exists.mvb} is pretty much like \code{\link{exists}} but has a \code{pos} rather than a \code{where} argument.

\code{find.funs} finds "function" objects in one or more environments, optionally matching a pattern.

\code{expanded.call} returns the full argument list available to its caller, including defaults where arguments were not set explicitly. The arguments may not be those originally passed, if they were modified before the invocation of \code{expanded.call}. Default arguments which depend on calculations after the invocation of \code{expanded.call} will lead to an error.

\code{index} returns the position(s) of TRUE elements. Unlike \code{which}: attributes are lost; NA elements map to NAs; \code{index(<<length 0 object>>)} is \code{numeric(0)}; \code{index( <<non-logical>>)} is NA.

\code{is.dir} tests for directoriness.

\code{legal.filename} coerces its character argument into a similar-looking string that is a legal filename on any (?) system.

\code{lib.pos} is used by \code{mvbutils} in hacking \code{library}, to make sure packages get attached \emph{below} ROOT task. You'll probably never need to call this directly.

\code{lsall} is like \code{ls} but coerces \code{all.names=TRUE}.

\code{masked} checks which objects in \code{search()[pos]} are masked by identically-named objects higher in the search path. \code{masking} checks objects mask identically-named objects lower in the search path. Namespaces may make the results irrelevant.

\code{mkdir} makes directories; unlike \code{dir.create}, it can do several levels at once.

\code{my.all.equal} is like \code{all.equal}, except that it returns FALSE in cases where \code{all.equal} returns a non-logical-mode result.

\code{named(x)} is just \code{names(x) <- as.character( x); x}; useful for \code{lapply} etc.

\code{option.or.default} returns the named option value if it exists, otherwise the supplied default.

\code{pos} will either search for several literal patterns in a single target, or vice versa-- not both. It returns a matrix showing the positions of the matching substrings, with as many columns as the maximum number of matches. 0 signifies "no match"; there is always at least one column even if there are no matches at all. \code{pos} is very slow and should probably be rewritten.

\code{print}: if \code{class(x)=="cat"}, the character vector \code{x} will be \code{print}ed by \code{cat(x,sep="\\n")}. If \code{class(x)=="nullprint"}, then \code{print(x)} will not print anything.

\code{returnList} returns a list corresponding to old-style (pre-R 1.8) \code{return} syntax. Briefly: a single argument is returned as itself. Multiple arguments are returned in a list. The names of that list are the argument names if provided; or, for any unnamed argument that is just a symbolic name, that symbolic name; or no name at all, for other unnamed arguments. You can duplicate pre-1.8 behaviour of \code{return(\dots)} via \code{return(returnList(\dots))}.

\code{safe.rbind} mimics \code{rbind}, but works round an \R bug (I reckon) where a column appears to be a numeric in one \code{data.frame} but a factor in the other.

\code{to.regexpr} converts literal strings to their equivalent regexprs, e.g. by doubling backslashes.

\code{yes.no} \code{cat}s the \code{prompt} argument and waits for user input. if the user input \code{pmatch}es "yes" or "YES", then \code{yes.no} returns \code{TRUE}; if the input \code{pmatch}es \code{no} or \code{NO} then \code{yes.no} returns \code{FALSE}; if the input is '' and \code{default} is set, then \code{yes.no} returns \code{default}; otherwise it repeats the question. You probably want to put a space at the end of \code{prompt}.
}
\value{
\item{ clip}{vector of the same mode as \code{x}.}
\item{ cq}{character vector.}
\item{ empty.data.frame}{\code{data.frame}}
\item{ expanded.call}{a \code{call} object}
\item{ find.funs}{a character vector of function names}
\item{ is.dir}{logical vector}
\item{ is.nonzero}{TRUE or FALSE}
\item{ legal.filename}{character( 1)}
\item{ masked}{character vector.}
\item{ masking}{character vector.}
\item{ mkdir}{logical vector of success/failure}
\item{ named}{vector of the same mode as \code{x}}
\item{ option.or.default}{option value}
\item{ pos}{numeric matrix, one column per match found plus one ; at least one column guaranteed}
\item{ returnList}{list or single object}
\item{ safe.rbind}{\code{data.frame}}
\item{ to.regexpr}{character}
\item{ yes.no}{TRUE or FALSE}
}
\examples{
clip( 1:5, 2) # 1:3
cq( alpha, beta) # c( "alpha", "beta")
empty.data.frame( a=1, b="yes")
# data.frame with 0 rows of columns "a" (numeric) and
# "b" (a factor with levels ""yes"")
f <- function( a=9, b) expanded.call(); f( 3, 4) # list( a=3, b=4)
find.funs( "package:base", patt="an") # "transform" etc.
is.dir( getwd()) # TRUE
legal.filename( "a:b\\\\c/d&f") # "a.b.c.d&f"
sapply( named( cq( alpha, beta)), nchar) # c( alpha=5, beta=4)
option.or.default( "my.option", 5) # probably 5
pos( cq( quick, lazy), "the quick brown fox jumped over the lazy dog")
# matrix( c( 5, 37), nrow=2)
pos( "quick", c( "first quick", "second quick quick", "third"))
# matrix( c( 7,8,0, 0,14,0), nrow=3)
pos( "quick", "slow") # matrix( 0)
f <- function() \{ a <- 9; return( returnList( a, a*a, a2=a+a)) \}
f() # list( a=9, 81, a2=18)
levels( rbind( data.frame( x=1), data.frame( x="cat"))$x)
# NULL, because "x" acquires mode "character"; a bug, I think
levels( safe.rbind( data.frame( x=1), data.frame( x="cat"))$x)
# c( "1", "cat")
to.regexpr( "a[[") # "a\\\\[\\\\["
\dontrun{
mkdir( "subdirectory.of.getwd")
yes.no( "OK (Y/N)? ")
masking( 1)
masked( 5)
}
}
\author{Mark Bravington}
\keyword{utilities}
\keyword{misc}
