\name{mlazy}
\alias{mlazy}
\alias{mtidy}
\alias{demlazy}
\alias{mcachees}
\alias{attach.mlazy}
\title{Cacheing objects for lazy-load access}
\description{
\code{mlazy} and friends are designed for handling collections of biggish objects, where only a few of the objects are accessed during any period, and especially where the individual objects might change and the collection might grow or shrink. As with "lazy loading" of packages, and the \pkg{gdata} package, the idea is to avoid the time & memory overhead associated with loading in numerous huge \R binary objects when not all will be needed. Unlike lazy loading and \code{gdata}, \code{mlazy} keeps each lazy-load object in a separate file, so it also avoids the overhead associated with changing/adding/deleting objects when all objects are saved into the same big file. When a workspace is \code{Save}d, the code updates only those individual object files that need updating.

\code{mlazy} does not require any special structure for object collections; in particular, the data doesn't have to go into a package. \code{mlazy} is particularly useful for users of \code{cd} because each \code{cd} to/from a task causes a read/write of the binary image file (usually ".Rdata"), which can be very large if \code{mlazy} is not used. Read DETAILS next. Feedback is welcome.
}
\usage{
mlazy( \dots, what, envir=.GlobalEnv) # cache some objects
mtidy( \dots, what, envir=.GlobalEnv) # (cache and) purge the cache to disk, freeing memory
demlazy( \dots, what, envir=.GlobalEnv) # makes \code{what} into normal uncached objects
mcachees( envir=.GlobalEnv) # shows which objects in envir are cached
attach.mlazy( dir, pos=2, name=) # load mcached workspace into new search environment, or create empty s.e. for cacheing
}
\arguments{
\item{ \dots}{unquoted object names, overridden by \code{what} if supplied}
\item{ what}{character vector of object names, all from the same environment. For \code{mtidy} and \code{demlazy}, defaults to all currently-cached objects in \code{envir}}
\item{ envir}{environment or position on the search path, defaulting to the environment where \code{what} or \code{objs} live.}
\item{ dir}{name of directory, relative to \code{\link{task.home}}.}
\item{ pos}{numeric position of environment on search path, 2 or more}
\item{ name}{name to give environment, defaulting to something like "data:current.task:dir".}
}
\value{These functions are used only for their side-effects, except for \code{cachees} which returns a character vector of object names.}
\details{
All this is geared to working with saved images (i.e. ".Rdata" or \code{all.rda} files) rather than creating all objects anew each session via \code{source}. If you use the latter approach, \code{mlazy} will probably be of little value.

The easiest way to set up cacheing is just to create your objects as normal, then call \code{mlazy( <<objname1>>, <<objname2>>, <<etc>>)} followed by \code{Save()}. This will not seem to do much immediately-- your object can be read and changed as normal, and is still taking up memory. The memory and time savings will come in your next \R session in this workspace.

You should never see any differences (except in time & memory usage) between working with cached and normal uncached objects.

[One minor exception is that cacheing a function may stuff up the automatic backup system, or at any rate the "backstop" version of it which runs when you \code{cd}. This is deliberate, for speeding up \code{cd}. But why would you cache a function anyway?]

\code{mlazy} itself doesn't save the workspace image (the ".Rdata" or "all.rda" file), which is where the references live; that's why you need to call \code{\link{Save}} periodically. \code{save.image} and \code{save} will \emph{not} work (and nor will \code{load}-- see NOTE). \code{Save} doesn't store \code{mcache}d objects directly in the \code{.Rdata} file, but instead stores an index object called something like \code{.mache00} (guaranteed not to conflict with one of your own objects) that triggers the creation of \code{mcache}d objects with promises-to-load, and is then deleted. The actual load process is handled by \code{\link{load.refdb}} but you shouldn't need to call this directly.

\code{mlazy} and \code{Save} do not immediately free any memory, to avoid any unnecessary re-loading from disk if you access the objects again during the current session. To force a "memory purge" \emph{during} an \R session, you need to call \code{mtidy}. \code{mtidy} purges its arguments from the cache, replacing them by \code{promise}s just as when loading the workspace; when a reference is next accessed, its cached version will be re-loaded from disk. \code{mtidy} can be useful if you are looping over objects, and want to keep memory growth limited-- you can \code{mtidy} each object as the last statement in the loop. By default, \code{mtidy} purges the cache of all objects that have previously been cached. \code{mtidy} also caches any formerly uncached arguments, so one call to \code{mtidy} can be used instead of \code{mlazy( \dots); mtidy( \dots)}.

\code{\link{move}} understands cached objects, and will shuffle the files accordingly.

\code{demlazy} will \emph{delete} the corresponding "obj*.rda" file(s), so that only an in-memory copy will then exist; don't forget to \code{Save} soon after.

Cacheing in other search environments

It is possible to cache in search environments other the current top one (AKA the current workspace, AKA'.GlobalEnv'). This could be useful if, for example, you have a large number of simulated datasets that you might need to access, but you don't want them cluttering up \code{.GlobalEnv}. If you weren't worried about cacheing, you'd probably do this by calling \code{attach( "<<filename>>")} (qv). The cacheing equivalent is \code{attach.mlazy( "cachedir")}. The argument is the name of a directory where the cached objects will be (or already are) stored; the directory will be created if necessary. If there is a ".Rdata" file in the directory, \code{attach.mlazy} will load it and set up any references properly; the ".Rdata" file will presumably contain mostly references to cached data objects, but can contain normal uncached objects too.

Once you have set up a cacheable search environment via \code{attach.mlazy} (typically in search position 2), you can cache objects into it using \code{mlazy} with the \code{envir} argument set (typically to 2). If the objects are originally somewhere else, they will be transferred to \code{envir} before cacheing. Whenever you want to save the cached objects, call \code{Save.pos(2)}.

You will probably also want to modify or create the \code{.First.task} (see \code{\link{cd}} (qv)) of the current task so that it calls \code{attach.mlazy("<<cache directory name>>")}. Also, you should create a \code{.Last.task} (see \code{\link{cd}} (qv)) containing \code{detach(2)}, otherwise \code{cd(..)} and \code{cd(0/\dots)} won't work.
}
\note{
The system function \code{load} does not understand cacheing; if you merely \code{load} an image file saved using \code{Save}, cached objects will not be there (and there will be an extra object called something like \code{.mcache00}). Hence, if you have cached objects in your ROOT task, they will not be visible when you start \R until you load the \code{mvbutils} library-- another fine reason to put this in your \code{.First}. The \code{.First.lib} function in \code{mvbutils} calls \code{setup.mcache( .GlobalEnv)} to automatically prepare any references in the ROOT task.

In the unlikely event of needing to "manually" load a cached image file, use \code{\link{load.refdb}}-- \code{cd} and \code{attach.lazy} do this automatically.

In the unlikely event of lost/corrupted data, you can manually reload "obj*.rda" files using \code{load}-- each "obj*.rda" file contains one object stored with its correct name. Before doing that, call \code{demlazy( what=mcachees())} to avoid subsequent trouble. Once you have reloaded the objects, you can call \code{mlazy} again.

To see what object is stored in a particular "obj*.rda" file, the safe way is to load the file into a new environment, e.g. \code{e <- new.env(); load( "obj99.rda", e); ls( e)}.

There is no requirement that cacheing be restricted to search environments 1 or 2. If you have several different clusters of datasets (e.g. a bunch of different datasets, and a bunch of results from fitting different models), you might want to use a different search position for each cluster. Just be sure to specify \code{pos} in function calls, as appropriate.

To see how memory changes when you call \code{mlazy} and \code{mtidy}, call \code{gc()} (qv).

Housekeeping of "obj**.rda" files happens during \code{Save}; any obsolete ones (i.e. of objects that have been \code{remove}d) are deleted.
}
\section{More details}{
What happens: each workspace acquires a \code{mcache} attribute, which is a named numeric vector. The absolute values of the entries correspond to files-- 53 corresponds to a file "obj53.rda", etc., and the names to objects. When an object \code{myobj} is \code{mlazy}ed, the \code{mcache} is augmented by a new element named "myobj" with a new file number, and that file is saved to disk. Also, "myobj" is replaced with an active binding (see \code{\link{makeActiveBinding}}). The active binding is a function which retrieves or sets the object's data within the function's environment. If the function is called in change-value mode, then it also makes negative the file number in \code{mcache}. Hence it's possible to tell whether a function has been changed since last being saved.

When an object is first \code{mlazy}ed, the object data is placed directly into the active binding function's environment so that the function can find/modify the data. When an object is \code{mtidy}ed, or when a cached image is loaded from disk, the thing placed into the A.B.fun's environment is not the data itself, but instead a \code{promise} saying, in effect, "fetch me from disk when you need me". The promise gets forced when the object is accessed for reading or writing. This is how "lazy loading" of packages works, and also the \pkg{gdata} package. However, for \code{mlazy} there is the additional requirement of being able to determine whether an object has been modified; for efficiency, only modified objects should be written to disk when there is a \code{Save}.

There is presumably some speed penalty from using a cache, but experience to date suggests that the penalty is small. Cached objects are saved in compressed format, which seems to take a little longer than an uncompressed save, but loading seems pretty quick compared to uncompressed files.
}
\seealso{ \code{\link{gc}}, package \pkg{gdata}}
\author{Mark Bravington}
\examples{
\dontrun{
biggo <- matrix( runif( 1e6), 1000, 1000)
gc() # lots of memory
mlazy( biggo)
gc() # still lots of memory
mtidy( biggo)
gc() # better
biggo[1,1]
gc() # worse; it's been reloaded
}
}
\keyword{programming}
\keyword{data}
