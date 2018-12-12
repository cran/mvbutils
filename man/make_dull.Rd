\name{make_dull}
\alias{make_dull}
\alias{make.dull}
\title{Hide dull columns in data frames}
\description{
\code{make_dull} AKA \code{make.dull} adds a "dull" S3 class to designated columns in a \code{data.frame}. When the \code{data.frame} is printed, entries in those columns will show up just as "...". Useful for hiding long boring stuff like nucleotide sequences, MD5 sums, and filenames. Columns will still print clearly if manually extracted.

The \code{dull} class has methods for \code{format} (used when printing a \code{data.frame}) and \code{[}, so that dullness is perpetuated.
}
\usage{make_dull(df, cols)
}
\arguments{
\item{ df}{a data.frame}
\item{ cols}{columns to designate}
}
\value{A modified data.frame
}
\details{Ask yourself: do you \emph{really} want details of a function called \code{make_dull}? Life may be sweet but it is also short.
\subsection{More details}{\code{make_dull} is both autologous and idempotent.
}
}
\examples{
# Becos more logical syntax:
rsample <- function (n = length(pop), pop, replace = FALSE, prob = NULL)\{
  pop[sample(seq_along(pop) - 1, size = n, replace = replace, prob = prob) + 1]
\}
df <- data.frame( x=1:3,
    y=apply( matrix( rsample( 150, as.raw( 33:127), rep=TRUE), 50, 3), 2, rawToChar),
    stringsAsFactors=FALSE) # s.A.F. value shouldn't matter
df # zzzzzzzzzzzzzzz
df <- make_dull( df, 'y')
df # wow, exciting!
df$y # zzzzzzzzzzzzzz
}
\keyword{misc}
