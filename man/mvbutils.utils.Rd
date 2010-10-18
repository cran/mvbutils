\name{mvbutils.utils
}
\alias{mvbutils.utils
}
\alias{as.cat
}
\alias{clip
}
\alias{cq
}
\alias{deparse.names.parsably
}
\alias{empty.data.frame
}
\alias{env.name.string
}
\alias{exists.mvb
}
\alias{expanded.call
}
\alias{everyth
}
\alias{find.funs
}
\alias{find.lurking.envs
}
\alias{index
}
\alias{is.dir
}
\alias{legal.filename
}
\alias{lsall
}
\alias{lib.pos
}
\alias{masked
}
\alias{masking
}
\alias{mkdir
}
\alias{most.recent
}
\alias{my.all.equal
}
\alias{named
}
\alias{option.or.default
}
\alias{pos
}
\alias{print.cat
}
\alias{print.nullprint
}
\alias{put.in.session
}
\alias{returnList
}
\alias{safe.rbind
}
\alias{to.regexpr
}
\alias{yes.no
}
\title{Miscellaneous utilities
}
\description{Miscellaneous utilities.
}
\usage{
as.cat( x)
clip( x, n=1)
cq( ...)
deparse.names.parsably( x)
empty.data.frame( ...)
env.name.string( env)
exists.mvb( x, pos = -1, envir = pos.to.env(pos), frame, mode = "any", inherits = FALSE)
expanded.call( nlocal=sys.parent())
everyth( x, by=1, from=1)
find.funs(pos=1, ..., exclude.mcache = TRUE, mode="function")
find.lurking.envs(obj, delve=FALSE, trace=FALSE)
index( lvector)
is.dir( dir)
legal.filename( name)
lsall( ...)
lib.pos() # used only to hack 'library'
masked( pos)
masking( pos=1)
mkdir( dirlist)
most.recent( lvec)
my.all.equal( x, y)
named( x)
option.or.default( opt.name, default=NULL)
pos( substrs, mainstrs, any.case = FALSE, names.for.output)
\method{print}{cat}( x, ...) # print.cat
\method{print}{nullprint}( x, ...) # print.nullprint
put.in.session( ...)
returnList( ...)
safe.rbind( df1, df2)
to.regexpr( x)
yes.no( prompt, default)
}
\arguments{\item{ x, y, n, ..., by, env, from, exclude.mcache, nlocal, lvector, dir, name, pos, envir, frame, mode, inherits, dirlist, lvec, opt.name, default, substrs, mainstrs, any.case, names.for.output, df1, df2, prompt,  obj, delve, trace}{see "Arguments by function"}
}
\details{
\code{as.cat} makes a character vector print as if it was \code{cat}ted rather than \code{print}ed.

\code{clip} removes the last \code{n} elements of \code{x}.

\code{cq} is handy for typing \code{cq( alpha, beta, gamma)} instead of \code{cq( "alpha", "beta", "gamma")}. Certain strings DO still require quotes around them, e.g. \code{cq( "NULL", "1-2")}).

\code{deparse.names.parsably} is like \code{deparse} except that \code{name} objects get wrapped in a call to \code{as.name}, so that they won't be evaluated accidentally.

\code{empty.data.frame} creates a template data frame with 0 rows but with all columns of the appropriate type. Useful for \code{rbind}ing to later-- see also \code{safe.rbind}.

\code{env.name.string} returns a string naming an environment; its \code{name} attribute if there is one, or the name of its \code{path} attribute if applicable, concatenated with the first line of what would be shown if you \code{print}ed the argument. Unlike \code{environmentName}, this will always return a non-empty string.

\code{exists.mvb} is pretty much like \code{exists} but has a \code{pos} rather than a \code{where} argument.

\code{expanded.call} returns the full argument list available to its caller, including defaults where arguments were not set explicitly. The arguments may not be those originally passed, if they were modified before the invocation of \code{expanded.call}. Default arguments which depend on calculations after the invocation of \code{expanded.call} will lead to an error.

\code{everyth} extracts every \code{by}-th element of \code{x}, starting at position \code{from}.

\code{find.funs} finds "function" objects (or objects of other modes, via the "mode" arg) in one or more environments, optionally matching a pattern.

\code{find.lurking.envs( myobj)} will search through \code{myobj} and all its attributes, returning the size of each sub-object. The size of environments is returned as Inf. The search is completely recursive, except for environments and by default the inner workings of functions; attributes of the entire function are always recursed. Changing the \code{delve} parameter to TRUE ensures full recursion of function arguments and function bodies, which will show e.g. the \code{srcref} structure; try it to see why the default is FALSE. \code{find.lurking.envs} can be very useful for working out e.g. why the result of a model-fitting function is taking up 1000000MB of disk space; sometimes this is due to unnecessary environments in well-concealed places.

\code{index} returns the position(s) of TRUE elements. Unlike \code{which}: attributes are lost; NA elements map to NAs; \code{index(<<length 0 object>>)} is \code{numeric(0)}; \code{index( <<non-logical>>)} is NA.

\code{is.dir} tests for directoriness.

\code{legal.filename} coerces its character argument into a similar-looking string that is a legal filename on any (?) system.

\code{lib.pos} is used by \code{mvbutils} in hacking \code{library}, to make sure packages get attached \bold{below} ROOT task. You'll probably never need to call this directly.

\code{lsall} is like \code{ls} but coerces \code{all.names=TRUE}.

\code{masked} checks which objects in \code{search()[pos]} are masked by identically-named objects higher in the search path. \code{masking} checks for objects mask identically-named objects lower in the search path. Namespaces may make the results irrelevant.

\code{mkdir} makes directories; unlike \code{dir.create}, it can do several levels at once.

\code{most.recent} returns the highest-so-far position of TRUE within a logical vector, or 0 if TRUE has not occurred yet; \code{most.recent( c(F,T,F,T))} returns c(0,2,2,4).

\code{my.all.equal} is like \code{all.equal}, except that it returns FALSE in cases where \code{all.equal} returns a non-logical-mode result.

\code{named(x)} is just \code{names(x) <- as.character( x); x}; useful for \code{lapply} etc.

\code{option.or.default} returns the named option value if it exists, otherwise the supplied default. Obsolete in R2.10; use \code{getOption( ..., default=...)} instead.

\code{pos} is probably to be eschewed in new code, in favour of \code{gregexpr} with \code{fixed=TRUE}, which is likely faster. (And I should rewrite it to use \code{gregexpr}.) It's one of a few legacy functions in \code{mvbutils} that pre-date improvements in base R{}. \code{pos} will either search for several literal patterns in a single target, or vice versa-- but not both. It returns a matrix showing the positions of the matching substrings, with as many columns as the maximum number of matches. 0 signifies "no match"; there is always at least one column even if there are no matches at all.

\code{print}: if \code{class(x)=="cat"}, the character vector \code{x} will be \code{print}ed by \code{cat(x,sep="\\n")}. If \code{class(x)=="nullprint"}, then \code{print(x)} will not print anything.

\code{returnList} returns a list corresponding to old-style (pre-R 1.8) \code{return} syntax. Briefly: a single argument is returned as itself. Multiple arguments are returned in a list. The names of that list are the argument names if provided; or, for any unnamed argument that is just a symbolic name, that symbolic name; or no name at all, for other unnamed arguments. You can duplicate pre-1.8 behaviour of \code{return(...)} via \code{return(returnList(...))}.

\code{safe.rbind} mimics \code{rbind}, but works round an R{} bug (I reckon) where a column appears to be a numeric in one \code{data.frame} but a factor in the other.

\code{to.regexpr} converts literal strings to their equivalent regexps, e.g. by doubling backslashes. Useful if you want "fixed=TRUE" to apply only to a portion of your regexp.

\code{yes.no} \code{cat}s its "prompt" argument and waits for user input. if the user input \code{pmatch}es "yes" or "YES", then \code{yes.no} returns \code{TRUE}; if the input \code{pmatch}es \code{no} or \code{NO} then \code{yes.no} returns \code{FALSE}; if the input is '' and \code{default} is set, then \code{yes.no} returns \code{default}; otherwise it repeats the question. You probably want to put a space at the end of \code{prompt}.
}
\value{
\item{  as.cat}{character vector of class \code{cat}}
\item{  clip}{vector of the same mode as \code{x}}
\item{  cq}{character vector}
\item{  empty.data.frame}{\code{data.frame}}
\item{  env.name.string}{a string}
\item{  expanded.call}{a \code{call} object}
\item{  everyth}{same type as \code{x}}
\item{  find.funs}{a character vector of function names}
\item{  find.lurking.envs}{a \code{data.frame} with columns "what" and "size"}
\item{  is.dir}{logical vector}
\item{  is.nonzero}{TRUE or FALSE}
\item{  legal.filename}{character( 1)}
\item{  masked}{character vector}
\item{  masking}{character vector}
\item{  mkdir}{logical vector of success/failure}
\item{  most.recent}{integer vector the same length as \code{lvec}, with values in the range (0,length(lvec)).}
\item{  named}{vector of the same mode as \code{x}}
\item{  option.or.default}{option's value}
\item{  pos}{numeric matrix, one column per match found plus one; at least one column guaranteed}
\item{  returnList}{list or single object}
\item{  safe.rbind}{\code{data.frame}}
\item{  to.regexpr}{character}
\item{  yes.no}{TRUE or FALSE}
}
\section{Arguments by function}{
\describe{
\item{as.cat}{x: character vector that you want to be displayed via \code{cat( x, sep="\\n")}}
\item{clip}{x: a vector or list}
\item{clip}{n: integer saying how many elements to clip from the end of \code{x}}
\item{cq}{\dots{}: quoted or unquoted character strings, to be \code{substitute}d and then concatenated}
\item{deparse.names.parsably}{x: any object for \code{deparse}- \code{name} objects treated specially}
\item{empty.data.frame}{\dots{}: named length-1 vectors of appropriate mode, e.g. "first.col=''"}
\item{env.name.string}{env: environment}
\item{exists.mvb}{x: string, pos = integer, envir = environment, frame = frame number, mode & inherits as for \code{exists}}
\item{expanded.call}{nlocal: frame to retrieve arguments from. Normally, use the default; see \code{\link{mlocal}}.}
\item{everyth}{x: subsettable thing. by: step between values to extract. from: first position.}
\item{find.funs}{\dots{}: extra arguments for \code{objects}. Usually just "pattern" for regexp searches.}
\item{find.funs}{exclude.mcache: if TRUE (default), don't look at \code{\link{mlazy}} objects}
\item{find.funs}{mode: "function" to look for functions, "environment" to look for environments, etc}
}

 :

\describe{
\item{find.lurking.envs}{delve: whether to recurse into function arguments and function bodies}
}

 :

\describe{
\item{find.lurking.envs}{trace: just a debugging aid-- leave as FALSE}
\item{index}{lvector: vector of TRUE/FALSE/NA}
\item{is.dir}{dir: character vector of files to check existence and directoriness of.}
\item{legal.filename}{name: character string to be modified}
\item{find.funs}{pos: list of environments, or vector of char or numeric positions in search path.}
}

 :

\describe{
\item{find.lurking}{envs obj: what to look in}
}

 :

\describe{
\item{find.lurking}{envs trace: leave as FALSE unless debugging}
\item{lsall}{\dots{}: as for \code{ls}, except that \code{all.names} will be coerced to TRUE}
\item{}{masking, masked pos: position in search path}
\item{mkdir}{dirlist: character vector of directories to create}
\item{most.recent}{logical vector}
\item{my.all.equal}{x, y: anything}
\item{named}{x: character vector which will become its own \code{names} attribute}
}

 :

\describe{
\item{option.or.default}{opt.name: character(1)}
\item{option.or.default}{default: value to be returned if there is no \code{option} called \code{"opt.name"}}
\item{pos}{substrs: character vector of patterns (literal not regexpr)}
\item{pos}{mainstrs: character vector to search for \code{substrs} in.}
\item{pos}{any.case: logical- ignore case?}
\item{pos}{names.for.output: character vector to label rows of output matrix; optional}
\item{}{print.cat, print.nullprint x: thing to print}
\item{}{print.cat, print.nullprint \dots{}: args for \code{print} (ignored)}
\item{put.in.session}{\dots{}: a named set of objects, to be \code{assign}ed into the \code{mvb.session.info} search environment}
\item{returnList}{\dots{}: named or un-named arguments, just as for \code{return} before R{} 1.8.}
\item{safe.rbind}{df1, df2: \code{data.frame} or \code{list}}
\item{to.regexpr}{x: character vector}
\item{yes.no}{prompt: string to put before asking for input}
\item{yes.no}{default: value to return if user just presses <ENTER>}
}
}
\author{Mark Bravington
}
\examples{
clip( 1:5, 2) # 1:3
cq( alpha, beta) # c( "alpha", "beta")
empty.data.frame( a=1, b="yes")
# data.frame with 0 rows of columns "a" (numeric) and
# "b" (a factor with levels ""yes"")
everyth( 1:10, 3, 5) # c( 5, 8)
f <- function( a=9, b) expanded.call(); f( 3, 4) # list( a=3, b=4)
find.funs( "package:base", patt="an") # "transform" etc.
find.lurking.envs( cd)
#                                     what  size
#1                     attr(obj, "source")  5368
#2                                     obj 49556
#3 environment(obj) <: namespace:mvbutils>   Inf
eapply( .GlobalEnv, find.lurking.envs)
is.dir( getwd()) # TRUE
legal.filename( "a:b\\\\c/d&f") # "a.b.c.d&f"
most.recent( c( FALSE,TRUE,FALSE,TRUE)) # c( 0, 2, 2, 4)
sapply( named( cq( alpha, beta)), nchar)  # c( alpha=5, beta=4)
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
to.regexpr( "a{{") # "a\\\\\{\\\\\{"
\dontrun{
mkdir( "subdirectory.of.getwd")
yes.no( "OK (Y/N)? ")
masking( 1)
masked( 5)
}
}
\keyword{misc}
