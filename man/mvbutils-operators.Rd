\name{mvbutils-operators}
\alias{mvbutils-operators}
\alias{\%&\%}
\alias{\%**\%}
\alias{\%!in\%}
\alias{\%is.a\%}
\alias{\%is.not.a\%}
\alias{\%is.an\%}
\alias{\%is.not.an\%}
\alias{\%except\%}
\alias{\%matching\%}
\alias{\%not.in\%}
\alias{\%that.match\%}
\alias{\%that.are.in\%}
\alias{\%without.name\%}
\alias{\%in.range\%}
\alias{\%such.that\%}
\alias{\%SUCH.THAT\%}
\alias{\%upto\%}
\alias{\%downto\%}
\alias{\%where\%}
\title{Utility operators}
\description{Succinct or convenience operators}
\usage{
a \%&\% b
x \%**\% y
a \%!in\% b
x \%is.not.a\% what
x \%is.a\% what
x \%is.not.an\% what
x \%is.an\% what
vector \%except\% condition
x \%matching\% patt
a \%not.in\% b
x \%that.match\% patt
a \%that.are.in\% b
x \%without.name\% what
a \%in.range\% b
a \%such.that\% b
a \%SUCH.THAT\% b
from \%upto\% to
from \%downto\% to
x \%where\% cond
}
\arguments{\item{ a, b, vector, condition, x, y, what, patt, from, to, cond}{see \bold{Arguments by function}.}}
\section{Arguments by function }{
 \code{\%&\%}: a, b: character vectors to be \code{paste}d with no separator

 \code{\%**\%}: x, y: matrices or vectors to be multiplied using \code{\%*\%} but with less fuss about dimensions

 \code{\%!in\%}, \code{\%that.are.in\%}: a, b: vectors (character, numeric, complex, or logical).

 \code{\%except\%}: vector, condition: character or numeric vectors

 \code{\%in.range\%}: a, b: numeric vectors.

 \code{\%is.a\%}, etc.: x: object whose class is to be checked

 \code{\%is.a\%}, etc.: what: class name

 \code{\%matching\%}, \code{\%that.match\%}: x: character vector

 \code{\%matching\%}, \code{\%that.match\%}: patt: character vector of regexps

 \code{\%such.that\%}, \code{\%SUCH.THAT\%}: a: vector

 \code{\%such.that\%}, \code{\%SUCH.THAT\%}: b: expression containing a \code{.}, to subscript \code{a} with

 \code{\%upto\%}, \code{\%downto\%}: from, to: numeric(1)

 \code{\%where\%}: x: data.frame

 \code{\%where\%}: cond: unquoted expression to be \code{eval}ed in context of \code{x}, then in the calling frame of \code{\%where\%} (or \code{.GlobalEnv}). Should evaluate to logical (or maybe numeric or character); NA is treated as FALSE. Wrap \code{cond} in parentheses to avoid trouble with operator precedence.

 \code{\%without.name\%}: x: object with \code{names} attribute

 \code{\%without.name\%}: what: character vector of names to drop
}
\value{
% Don't need the quotes around the item names in VALUE or ARGUMENTS sections (only)
\item{ \%&\%}{string concatenation via \code{paste( a, b, sep="")}}
\item{ \%**\%}{like \code{\%*\%} except (i) matrices with one dimension is 1 are collapsed to vectors, (ii) vectors are transposed if necessary before multiplication, and (iii) results where one dimension is 1 are collapsed to vectors}
\item{ \%!in\%, \%not.in\%}{logical negation of \code{\%in\%}}
\item{ \%is.a\%}{(and \code{\%is.not.a\%}, \code{\%is.an\%}, \code{\%is.not.an\%}) result of \code{inherits} to test class membership}
\item{ \%except\%}{\code{vector} except those elements that match something in \code{condition}}
\item{ \%matching\%, \%that.match\%}{those elements in \code{x} which are regexp-matched with any of the \code{patt}s}
\item{ \%that.are.in\%}{those elements in \code{a} which are in \code{b}}
\item{ \%without.name\%}{\code{x} except those elements whose names match \code{what}}
\item{ \%in.range\%}{those elements of \code{a} that are >=min(b) and <=max(b)}
\item{ \%upto\%}{like \code{:} except that if \code{to<from}, numeric(0) is returned (useful for coding for-loops)}
\item{ \%downto\%}{converse of \code{\%upto\%}}
\item{ \%such.that\%, \%SUCH.THAT\%}{subset of \code{a} for which \code{substitute( b, list( .=a))} is TRUE \code{\%such.that\%} uses direct subscripting; \code{\%SUCH.THAT\%} is more general but slower, using a for-loop}
\item{ \%where\%}{those rows of the data.frame \code{x} where \code{cond} is satisfied (see \bold{Arguments by function})}
}
\seealso{bquote}
\examples{
  "a" \%&\% "b" # "ab"
  matrix( 1:4, 2, 2) \%**\% matrix( 1:2, 2, 1) # c( 7, 10); '\%*\%' gives matrix result
  matrix( 1:2, 2, 1) \%**\% matrix( 1:4, 2, 2) # c( 5, 11); '\%*\%' gives error
  1:2 \%**\% matrix( 1:4, 2, 2) # '\%*\%' gives matrix result
  1:5 \%!in\% 3:4 # c( TRUE, TRUE, FALSE, FALSE, TRUE)
  1:5 \%that.are.in\% 3:4 # c( 3, 4)
  trf <- try( 1+"nonsense")
  if( trf \%is.not.a\% "try-error") cat( "OK\\n") else cat( "not OK\\n")
  1:5 \%except\% c(2,4,6) # c(1,3,5)
  c( alpha=1, beta=2) \%without.name\% "alpha" # c( beta=2)
  1:5 \%in.range\% c( 2, 4) # c(F,T,T,T,F)
  c( "cat", "hat", "dog", "brick") \%matching\% c( "at", "ic") # cat hat brick
  1 \%upto\% 2 # 1:2
  1 \%upto\% 0 # numeric( 0); using \%upto\% rather than : in for-loops can simplify coding
  1 \%downto\% 0 # 1:0
  1 \%downto\% 2 # numeric( 0)
  ff <- function( which.row) \{
    x <- data.frame( a=1:3, b=4:6)
    x \%where\% (a==which.row)
  \}
  ff( 2) # data.frame( a=2, b=5)
  (1:5) \%such.that\% (.>2) # 3,4,5
  listio <- list( a=1,  b=2)
  chars <- cq( a, b)
  chars \%SUCH.THAT\% (listio[[.]]==2) # 'b'; \%such.that\% won't work because [[]] can't handle xtuples
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
\keyword{misc}
