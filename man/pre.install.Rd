\name{pre.install}
\alias{pre.install}
\alias{patch.installed}
\alias{patch.install}
\alias{pre.install.hook...}
\title{Update a source and/or installed package from a task package}
\description{\code{pre.install} creates a "source package" from a "task package", ready for installation using R CMD INSTALL/BUILD/CHECK. \code{patch.install} can be called after a \code{pre.install}; it makes a quick modification to your already-installed version of a package, and there is then no subsequent need to re-build and re-install via RCMD. It also updates the help system with immediate effect, i.e. during the current R session. \code{patch.installed} is a synonym for \code{patch.install}. Consult \code{?"mvbutils.packaging.tools"} before reading further.}
\usage{
 # 95\% of the time you just need
 # pre.install( pkg)
 # patch.install( pkg)
 # Your own hook: pre.install.hook.<<mypack>>( default.list, <<myspecialargs>>, ...)
 pre.install( pkg, character.only=FALSE, force.all.docs=FALSE, Rd.version, subdir=pkg, ...)
 patch.installed( pkg, character.only=FALSE, force.all.docs=FALSE, help.patch=TRUE, DLLs.only=FALSE, update.installed.cache=option.or.default("mvb.update.installed.cache", TRUE), pre.inst=TRUE, Rd.version=NULL, subdir=pkg)
 patch.install(...) # actually the args are exactly the same as for 'patch.installed'
}
\arguments{
\item{ pkg}{package name. Either quoted or unquoted is OK; unquoted will be treated as quoted unless\dots}
\item{ character.only}{\dots is TRUE}
\item{ force.all.docs}{normally just create help files for objects whose documentation has changed; if TRUE, then recreate help for all documented objects.}
\item{ help.patch}{if TRUE, patch the help of the installed package}
\item{ DLLs.only}{just synchronize the DLLs and don't bother with other steps (see \bold{Compiled code})}
\item{ default.list}{list of various things-- see under "Overriding\dots" below}
\item{ ...}{arguments to pass to your \code{pre.install.hook.XXX} function, usually if you want to be able to build different versions of a package. In \code{pre.install}, \code{...} is just shorthand for the arg list of \code{patch.installed}.}
\item{ update.installed.cache}{defaults to TRUE unless you have set \code{options( "mvb.update.installed.cache")}. If TRUE, then clear the installed-package cache, so that things like \code{installed.packages} work OK. The only reason to set to FALSE could be speed, if you have lots of packages; feedback on this would be appreciated.}
\item{ pre.inst}{?run \code{pre.install} first?}
\item{ Rd.version}{(character) what Rdoc version to create "man" files in? Currently can be "1" (pre-R2.10) or "2" (R2.10 and up). Default is set according to what version of R is running.}
\item{ subdir}{what subdirectory name to put the source package into. Default is same as package. Can be useful to set manually if you are forced to maintain different versions of the package, e.g. for different R versions.}
}
\details{
The minimal ingredient for \code{pre.install} is a "task package"-- a directory with a ".RData" file in it, and possibly other files. [The term "task" is used because there is an expectation that this directory will be linked into the task hierarchy maintained by \code{\link{cd}}; this might not be essential, but I haven't tested it any other way.] \code{pre.install} creates a source package in a subdirectory of task package, for first-time installation. For subsequent maintenance, you would normally just call \code{patch.install}, which calls \code{pre.install}. You can override most of the default behaviour of \code{pre.install} by providing your own hook function \code{pre.install.hook.<<mypack>>} in the task-- see \bold{Overriding defaults}.

[_R pre-2.10 only:_ For help conversion to work, the various R build tools must be in the search path, just as when you're actually building the package. Also, certain environment variables may need to be set, such as "R_LIBS". All this may not automatically be the case. If not, you should set \code{options( rcmd.shell.setup)} to a character vector of commands that will set up the path & environment variables properly, when called as part of a batch file (Windows) or shell script (Linux etc). On my (Windows) system, this is \code{CALL SET-R-BUILD-PATH-GUTS.BAT}. The changes are temporary, just while the conversion is taking place.]
}
\section{Compiled code}{
\code{pre/patch.install} do not compile source code; you need to do that yourself. If you use RCMD to do your compilation, then you can use RCMD INSTALL; this will overwrite your installed package, and probably can't be done during an R session. Alternatively, you may be able to use RCMD SHLIB to create the DLL directly, which you can then copy into the "libs" subdirectory of the installed package, without needing to re-install. I haven't tried this.

If you pre-compile your own DLLs directly (which I do-- not for CRAN, but fine for distribution to other Windows users), then you can put the DLL into a subdirectory "inst/libs" of the task; it will end up in the "libs" subdirectory of the installed package.

To load compiled code in your package, use \code{library.dynam} or \code{dyn.load} in the \code{.onLoad} function (or in the \code{.First.lib} if you really aren't going to use a namespace).

After the package is built, I change my compiler settings so that the DLL is created directly in the installed package's "libs" subdirectory; this means I can use the compiler's debugger while R is running. To accommodate this, \code{patch.install} behaves as follows:

\itemize{
\item any new DLLs in the task package are copied to the source package and installed package;
\item any DLLs in the installed package but not in the task package are deleted;
\item for any DLLs in both task & installed, both copies are synchronized to the \bold{newer} version, which is also copied to the source package's "inst/lib" subdirectory.
}

You can call \code{patch.install( mypack, DLLs.only=TRUE)} if you only want the DLL-synching step.
}
\section{Package structure}{
The source package will contain R source, Rd documentation, optionally a NAMESPACE, and other things. The source package is ready for RCMD BUILD, maybe RCMD CHECK, and \code{patch.install}; see \bold{Package structure}.

The "task directory" means the working directory when you call \code{pre.install}; \code{\link{cd}} will look after this for you. The "package directory" is the subdirectory "<<pkg>>" below that, which will be created if needs be.

The default behaviour of \code{pre.install} is as follows-- to change it, see \bold{Overriding defaults}. A basic source package is created (no C code etc.) in a subdirectory "<<pkg>>" of the current task. The package will have a DESCRIPTION file, a single R source file with name "<<pkg>>.R" in the "R" subdirectory, possibly a "sysdata.rda" file in the same place to contain non-functions, possibly a NAMESPACE file, and a set of Rd files in the "man" subdirectory. Rd files will be auto-created from \code{\link{flatdoc}} style documentation, although precedence will be given to any pre-existing Rd files found in an "Rd" subdirectory of your task, which get copied directly into the package. Any "demo", "src", and "data" subdirectories will be copied straight to the package. An "inst" subdirectory will also be copied, but recursively (i.e. including any of \emph{its} subdirectories). There is no recompilation of source code. For handling of DLLs, see below.

Most objects in the task will go into the package, but there are usually a few you wouldn't want there: objects that are concerned only with how to create the package in the first place, and ephemeral system clutter such as \code{.Random.seed}. The default exceptions are: functions \code{pre.install.hook.<<pkg>>}, \code{.First.task}, and \code{.Last.task}; data \code{forced!exports}, \code{.required}, \code{tasks}, \code{.Traceback}, \code{.packageName}, \code{last.warning}, \code{.Last.value}, \code{.Random.seed}, \code{.SavedPlots}; and any character vector whose name ends with ".doc".

All pre-existing files in the "man", "src", "tests", "exec", "demo", "inst", and "R" subdirectories of the source-package directory will be removed (unless you have some \code{\link{mlazy}} objects; see below). Rd files in the task's "man" directory take precedence over Rd files that are created automatically by \code{pre.install} from \code{\link{flatdoc}}-style documentation. If an \code{.Rbuildignore} file is present in the task directory, it's copied to the package directory (NB I should include a facility in the pre-install hook for this). If there is a "changes.txt" file in the task directory, it will be copied to the "inst" subdirectory of the package, as will any files in the task's own "inst" subdirectory. Similarly, any DESCRIPTION file in the task directory will be copied to the package directory, after removing any "Built:" line. If there is no DESCRIPTION file in the task directory, a default DESCRIPTION file will be created in the package directory, but you'll certainly want to edit it before CRAN release; you can also generate the DESCRIPTION file yourself via the \code{pre.install.hook} override. Any "Makefile.*" in the task directory will be copied, as will any in the "src" subdirectory (not sure why both places are allowed). No other files or subdirectories in the package directory will be created or removed, but some essential files will be modified.

The package is assumed to be namespaced if any of the following apply: there is a NAMESPACE file in the task directory; there is a \code{.onLoad} function in the task; there is an "Imports" directive in the DESCRIPTION file. If a NAMESPACE file is present in the task, then it is copied directly to the package. If not but the package still looks like a namespace candidate, then \code{pre.install} will generate a NAMESPACE file by calling \code{\link{make.NAMESPACE}}, which makes reasonable guesses about what to import, export, and S3methodize. What is & isn't an S3 method is generally deduced OK (see \code{\link{make.NAMESPACE}} for gruesome details), but you can override the defaults via the pre-install hook.

By default, the R source file will only contain functions, but you can include other objects too by naming them in the \code{funs} argument. For functions, any \code{doc} and \code{export.me} attributes are dropped, but source code is kept in the \code{source} attribute.

If any of the Rd files starts with a period, e.g. ".dotty.name", it will be renamed to e.g. "01.dotty.name.Rd" to avoid some problems with RCMD.

If the package is not namespaced (why not?), then any undocumented functions will receive skeletal documentation in a \code{my.proto.package-internals.Rd} file. The doco is OK for RCMD CHECK, but says little more than "don't use these functions yourself". Undocumented functions are those not found by \code{find.documented( doctype="any")}).

To speed up conversion of documentation, a list of raw & converted documentation is stored in the file "doc2Rd.info.rda" in the task directory, and conversion is only done for objects whose raw documentation has changed, unless \code{force.all.docs} is TRUE.

\code{pre.install} creates a file "funs.rda" in the package's "R" subdirectory, which is subsequently used by \code{patch.installed}. RCMD BUILD will omit this file (currently with a complaint, though I'm trying to fix this) but it does not cause trouble.
}
\section{Package documentation}{If there is a text object called "<<packagename>>.package.doc", then it will be passed through \code{\link{doc2Rd}} with an extra "docType\{package\}" field. The first line should start "<<packagename>>-package" and the corresponding ".Rd" file will be put first into the index. This is the recommended way of providing package overviews-- and, speaking as a frequently bewildered would-be user of others' packages, I urge you to make use of it!}
\section{Data objects}{
Data objects are handled a bit differently to the recommendations in "R extensions" and elsewhere-- but the end result for the package user is the same, or better. The changes have been made to speed up package maintenance, and to improve useability. Specifically:

\itemize{
\item Undocumented data objects live only in the package's namespace, i.e. visible only to your functions.
\item Documented data objects appear both in the visible part of the package (i.e. in the search path), and in the namespace. [The R standard is that these should not be visible in the namespace, but this doesn't seem sensible to me.]
\item To document a data object \code{xxx}, include a flat-format text object \code{xxx.doc} in your task. \code{xxx.doc} itself won't appear in the packaged object, but will result in documentation for \code{xxx}.
\item There is no need for the user ever to call \code{\link{data}} to access a dataset in the package, and in fact it won't work;
\item Big data objects can be individually lazy-loaded (see below) to save time & memory, but lazy-loading is otherwise off by default for individual data objects.
}

Note that the \code{data(...)} function has been pretty much obsolete since the advent of lazy-loading in R 2.0; see R-news #4/2.

In terms of package structure, as opposed to operation, there is no "data" subdirectory. Data lives either in the "sysdata.rdb/rdx" files in the "R" subdirectory (but can still be user-visible, which is not normally the case for objects in those files) or in the "mlazy" subdirectory (for individual lazy-loading).
}
\section{Big data objects}{Lazy-loading objects cached with \code{\link{mlazy}} are handled specially, to speed up \code{pre.install}. Such objects get their cache-files copied to "inst/mlazy", and the \code{.onLoad} is prepended with code that will load them on demand. By default, they are exported if and only if documented, and are not locked. The following objects are not packaged by default, even if \code{\link{mlazy}}ed: \code{.Random.seed}, \code{.Traceback}, \code{last.warning}, and \code{.Saved.plots}. These are \code{\link{mlazy}}ed automatically if \code{options( mvb.quick.cd)} is \code{TRUE}-- see \code{\link{cd}}.}
\section{Overriding defaults}{
If a function \code{pre.install.hook.<<pkgname>>} exists in the task "<<pkgname>>", it will be called during \code{pre.install}. It will be passed one list-mode argument, containing default values for various installation things that can be adjusted; it should return a list with the same names. It will also be passed any \code{...} arguments to \code{pre.install}, which can be used e.g. to set "production mode" vs "informal mode" of the end product. The hook can do two things: sort out any file issues not adequately handled by \code{pre.install}, and/or change the following elements in the list that is passed in:

\describe{
\item{copies}{files to copy directly}
\item{dll.paths}{DLLs to copy directly}
\item{extra.docs}{names of character-mode objects that constitute flat-format documentation}
\item{description}{named elements of DESCRIPTION file}
\item{task.path}{path of task (ready-to-install package will be created as a subdirectory in this)}
\item{has.namespace}{should a namespace be used?}
\item{use.existing.NAMESPACE}{ignore default and just copy the existing NAMESPACE file?}
\item{nsinfo}{default namespace information, to be written iff \code{has.namespace==TRUE} and \code{use.existing.NAMESPACE==FALSE}}
\item{exclude.funs}{any functions \bold{not} to include}
\item{exclude.data}{non-functions to exclude from \code{system.rda}}
}

There are two reasons for using a hook rather than directly setting parameters in \code{pre.install}. The first is that \code{pre.install} will calculate sensible but non-obvious default values for most things, and it is easier to change the defaults than to set them up from scratch in the call. The second is that once you have written a hook, you can forget about it-- you don't have to remember special argument values each time you call \code{pre.install} for that task.
}
\seealso{\code{\link{cd}}, \code{\link{doc2Rd}}, \code{\link{maintain.packages}}}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
