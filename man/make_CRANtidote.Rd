\name{make_CRANtidote}
\alias{make_CRANtidote}
\alias{locally_import}
\title{Suppress stupid CRAN notes, and facilitate use of Suggested packages}
\description{
Sigh... CRAN will \emph{reject} packages with \emph{NOTE} "no visible binding for global variable" from R{} CMD check--- not WARNING, just NOTE. This is stupid because of false-positives whenever \code{\link{mlocal}}, \code{\link{cq}}, or indeed \code{evalq} or \code{local}, are called :/. Arguing with CRAN is about as productive as trying to teach a dead fish to climb trees. So, call \code{make_CRANtidote} instead, to get a bit of code that you can paste at the end of your package's \code{.onLoad} to suppress the non-problem. Your package does not need to import \code{\link{mvbutils}} to use that code.

It works by creating a dummy for each of those variables in the namespace environment, specifically a function that causes an error (\code{stop}) with the message "CRANtidote failure". It has to be a function, since as-CRAN checks look not just for variables, but for functions. Sigh.

The dummy function should never be called, and if you see that "CRANtidote failure" error message, then the dummy has masked something important. Unwanted side-effects are unlikely, but possible: here's two examples. First, when I initially tried this with \code{\link{mvbutils}} itself, it noticed that \code{.onAttach} was mentioned directly somewhere, and helpfully tried to add that--- causing failure on \code{attach()}, since R{} tried to run a non-function \code{.onAttach}. So \code{.onAttach} is now specifically excluded to avoid this, and will need tweaking manually in your code. There might be other special things only created/needed after \code{.onLoad} that need similar attention.

The second side-effect might be more common, but is easily fixed. Suppose you \code{requireNamespace} a package (which is OK, even according to CRAN and even if it lives in a different repo, as long as it's listed in "Suggests"), but you don't want to always refer to its members by name, e.g. \code{otherpak::funcaroo}. Of course, R{} says you're Supposed To do the latter, but that is not always cozza increased verbosity and decreased code clarity; and e.g. what about operators a la \code{\%dopar\%} in \code{parallel}? Then, just call \code{locally_import("otherpak")} instead of \code{requireNamespace("otherpak")}. It will create a local copy (an "import") of all exportees in "otherpak", by default just in the frame it is called from (so the copy disappears as soon as your code finishes). The local copies will mask any dummies created by \code{make_CRANtidote}, so things should work as you expect.

\code{locally_import} might be generally useful for packages in "Suggests", even without \code{make_CRANtidote} (if you don't care about CRAN), since it saves on precious colons--- and it \emph{does} eliminate the unlikely-but-logically-possible problem that a synonymous function from some completely different attached package \emph{might} mask the version you are hoping to call (and without \code{locally_import}, the colonless version would only work if you \code{attach(otherpak)} anyway). You can also "import" just some selected functions.
}
\usage{
make_CRANtidote( checkout)
locally_import( pkg, funs=getNamespaceExports( pkg), to=parent.frame())
}
\arguments{
\item{ checkout}{Result of a call to \code{check.pkg}; AFAIK, it doesn't matter here whether the \code{CRAN} argument is set.}
\item{ pkg}{name of a package, presumably listed in "Suggests" but not necessarily}
\item{ funs}{which things to "import" from \code{pkg}. Default is everything that's exported by \code{pkg}. Usually it's only functions that might be imported, but I think it's conceivable that non-functions might be exported--- which is fine, despite the name \code{funs}.}
\item{ to}{environment to create the "imports" in. The default is whatever frame \code{locally_import} is called from--- so the copy will almost always be temporary. In the somewhat unlikely event that you are calling \code{locally_import} inside \code{lapply} or similar, you might need to specify e.g. \code{to=environment()}.}
}
\value{
\code{make_CRANtidote} returns a character vector of class \code{cat}, which will display nicely so that you can cut-and-paste it into your \code{.onLoad}.
\code{locally_import} returns TRUE or FALSE, like \code{requireNamespace}.
}
\examples{
\dontrun{
if( FALSE && is_very_annoying( CRAN))\{ # to avoid CRAN trying to run this despite dont-run :/
  test <- check.pkg( 'mypak')
  make_CRANtidote( test)
  # Some function defined in your own package.
  # This example is complete nonsense--- its only purpose is to show syntax!
  mypakfun <- function( x)\{
      locally_import( 'doParallel') # everything from 'doParallel'
      locally_import( 'otherpak', c( 'funcaroo', 'bo_diddley')) # just those 2
      # Look mum no double-colons
      thrub <- funcaroo( x \%dopar\% y)
    return( bo_diddley( thrub))
  \}
\}
}
}
\keyword{misc}
