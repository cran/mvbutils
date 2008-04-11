\name{pre.install}
\alias{pre.install}
\alias{patch.installed}
\alias{pre.install.hook...}
\title{Create a simple formal package from an informal package}
\description{\code{pre.install} creates a "source package" from a "source task", ready for installation using R CMD INSTALL/BUILD/CHECK. \code{patch.installed} can be called after a \code{pre.install}; it makes a quick modification to your already-installed version of a package, and there is then no subsequent need to re-build and re-install via RCMD. It also updates HTML and pager-help, with immediate effect (i.e. during the current R session).}
\usage{
 pre.install( pkg, character.only=FALSE, force.all.docs=FALSE, ...)
 # Your own hook: pre.install.hook<<mypack>>( default.list, <<myspecialargs>>, ...)
 patch.installed( pkg, character.only=FALSE, force.all.docs=FALSE, help.patch=TRUE)
}
\arguments{
\item{ pkg}{package name. Either quoted or unquoted is OK; unquoted will be treated as quoted unless\dots}
\item{ character.only}{\dots is TRUE}
\item{ force.all.docs}{normally just create help files for objects whose documentation has changed; if TRUE, then recreate help for all documented objects.}
\item{ help.patch}{if TRUE, patch the html and pager-style help of the installed package}
\item{ default.list}{list of various things-- see under "Overriding\dots" below}
\item{ ...}{arguments to pass to your \code{pre.install.hook.XXX} function, usually if you want to be able to build different versions of a package.}
}
\details{
The minimal ingredient for \code{pre.install} is a "source task"-- basically a directory with a ".Rdata" file in it. [The term "task" is used because there is an expectation that this directory will be linked into the "task hierarchy" maintained by \code{\link{cd}}; this might not be essential, but I haven't tested it any other way.] \code{pre.install} is useful in two circumstances: when you have a set of functions that you want to make into a formal package for the first time, and also when you are ready to update a "maintained package" that you have just changed (see \code{\link{maintain.packages}}). It creates a source package with R source, Rd documentation, optionally a NAMESPACE, and other things, ready for RCMD CHECK, RCMD BUILD, and/or \code{patch.installed}; see \bold{Package structure}. You can override some of the defaults by providing your own hook function \code{pre.install.hook.<<mypack>>} in the task-- see \bold{Overriding defaults}.

If you have already built your package and are just using \code{pre.install} to update it, you can follow up with a call to \code{patch.installed}. This patches up your already-installed R package to reflect changes in the "source task", so that the modifications will be incorporated the next time R loads it (NB that if the package is already loaded and you are using \code{maintain.packages}, then code changes to the loaded version are immediate-- but will be lost the next time the package is loaded, unless you call \code{patch.installed}. However, changes to the documentation are currently only triggered by \code{patch.installed}.). Using \code{patch.installed} means that you \bold{don't} need to repeatedly rebuild and re-install the package via RCMD while developing it; those steps are only necessary when giving the package to others, or to CRAN. \code{patch.installed} updates the HTML and pager-style help files, but not CHM help nor the \code{help.search} database (not yet, anyway)-- for that, you need to re-build and re-install. It does not compile C/Fortran code for you, but it does update your DLLs, and re-loads them if they've changed. The DLLs present in the installed "libs" subdirectory will be kept in synch with any DLLs in your task directory or a subdirectory "libs" thereof. \code{patch.installed} also copies new and/or changed files out of the "demo" and "inst" subdirectories of the source package, respectively into the "demo" subdirectory and the installed directory itself; copying of "inst" is recursive. For safety's sake, though, no files are deleted from the installed version.

The recommended way to use \code{pre.install} to \bold{create} the package for the first time, is to have your collection of functions as a task called e.g. "protopack", use \code{cd(protopack)} to bring it to the top workspace, and then call \code{pre.install(protopack)}. This will create the formal skeleton in the "protopack" subdirectory of the task directory. Once you get the package built, use \code{\link{maintain.packages}} to live-edit it and make future changes-- it's better to do that than to keep using \code{cd} to access the original task.
}
\section{Package structure}{
The "task directory" means the working directory when you call \code{pre.install}; \code{cd} will look after this for you. The "package directory" is the subdirectory "<<pkg>>" below that, which will be created if needs be.

The default behaviour of \code{pre.install} is as follows-- to change it, see \bold{Overriding defaults}. A basic source package is created (no C code etc.) in a subdirectory "<<pkg>>" of the current task. The package will have a DESCRIPTION file, a single R source file with name "<<pkg>>.R" in the "R" subdirectory, possibly a "sysdata.rda" file in the same place to contain non-functions, possibly a NAMESPACE file, and a set of Rd files in the "man" subdirectory. Rd files will be auto-created from \code{\link{flatdoc}} style documentation, although precedence will be given to any pre-existing Rd files that in an "Rd" subdirectory of your task, which get copied directly into the package. Any "demo", "src", and "data" subdirectories will be copied straight to the package. An "inst" subdirectory will also be copied, but recursively (i.e. including any of \emph{its} subdirectories). There is no recompilation of source code. For handling of DLLs, see below.

Most objects in the task will go into the package, but there are usually a few you wouldn't want there: objects that are concerned only with how to create the package in the first place, and ephemeral system clutter such as \code{.Random.seed}. The default exceptions are: functions \code{pre.install.hook.<<pkg>>} and \code{.First.task}; data \code{forced!exports}, \code{.required}, \code{tasks}, \code{.Traceback}, \code{.packageName}, \code{last.warning}, \code{.Random.seed}, \code{.SavedPlots}, and any character vector of the form \code{***.doc}.

All pre-existing files in the "man", "inst", and "R" subdirectories of the package directory will be

removed (unless you have some \code{mlazy} objects; see below). If an \code{.Rbuildignore} file is present in the task directory, it's copied to the package directory (NB I should include a facility in the pre-install hook for this). If there is a "changes.txt" file in the task directory, it will be copied to the "inst" subdirectory of the package, as will any files in the task's own "inst" subdirectory. Similarly, any DESCRIPTION file in the task directory will be copied to the package directory, after removing the "Built:" line. If there is no DESCRIPTION file in the task directory, a default DESCRIPTION file will be created in the package directory, but you'll certainly want to edit it before CRAN release; you can also generate the DESCRIPTION file via the \code{pre.install.hook} override. No other files or subdirectories in the package directory will be created or removed, but some essential files will be modified.

The package is assumed to be namespaced if any of the following apply: there is a NAMESPACE file in the task directory; there is a \code{.onLoad} function in the task; there is an "Imports" directive in the DESCRIPTION file. If a NAMESPACE file is present in the task, then it is copied directly to the package. If not but the package still looks like a namespace candidate, then \code{pre.install} will generate a NAMESPACE file by calling \code{\link{make.NAMESPACE}}, which makes reasonable guesses about what to import, export, and S3methodize. What is & isn't an S3 method is generally deduced OK (based on function name, and name of first argument-- if it's not a method, I'd suggest giving the first arg a different name to that in the generic), but if you want to force a plausible S3 method \bold{not} to be listed as a method, then give the function an attribute \code{export.me=TRUE}, or override with the pre-install hook.

By default, any DLLs found in either the task directory or its "libs" subdirectory (if any) are copied to the "inst/libs" package directory. This will ensure that the DLLs ultimately get copied to the "libs" subdirectory of the \emph{installed} package, which is where R expects to see DLLs. This works fine for RCMD BUILD & INSTALL, but horrifies RCMD CHECK; you could use the \code{...} argument and a pre-install hook to pre-install different versions of the package, depending on whether you are distributing it informally or putting it onto CRAN. To change the behaviour in the pre-install hook, you can modify or remove elements from the \code{dll.paths} character vector, whose elements are the original paths of the DLLs and whose names are the names of the DLLs.

By default, the R source file will only contain functions, but you can include other objects too by naming them in the \code{funs} argument. For functions, only source code will be included; in other words, any attributes except \code{source} are removed before printing. In particular, any \code{export.me} attribute (see \code{\link{make.NAMESPACE}}) and any flat-format documentation in the \code{doc} attribute do not go into the R file. However, the \code{doc} attribute is used to create the Rd files, by \code{\link{doc2Rd}}.

If any of the Rd files starts with a period (e.g. ".dotty.name"), it will be renamed to "01.dotty.name.Rd" (to avoid some problems with rcmd).

If the package is not namespaced (and namespacing is definitely a Good Thing-- ), then any undocumented functions (i.e. functions not in \code{find.documented( doctype="any")}) will receive skeletal documentation in a \code{my.proto.package-internals.Rd} file. The doco is OK for RCMD CHECK, but says little more than "don't use these functions yourself".

To speed up conversion of documentation, a list of raw & converted documentation is stored in the file "doc2Rd.info.rda" in the task directory, and conversion is only done for objects whose raw documentation has changed.

\code{pre.install} creates a file "funs.rda" in the package's "R" subdirectory, which is subsequently used by \code{patch.installed}. RCMD BUILD will omit this file (currently with a complaint, though I'm trying to fix this) but it does not cause trouble.
}
\section{Package documentation}{If there is a text object called "<<packagename>>.package.doc", then it will be passed through \code{\link{doc2Rd}} with an extra "docType\{\enc{}{}package\}\enc{}{}" field. The first line should start "<<packagename>>-package" and the corresponding ".Rd" file will be put first into the index. This is the recommended way of providing package overviews-- and, speaking as a frequently bewildered would-be user of others' packages, I beg you to make use of it!}
\section{Data objects}{Most data objects in the task will get stored in the "system.rda" file in the "R" package subdirectory. This is loaded when the package is, but in a NAMESPACEd package the contents only go into the namespace and are not directly visible to the user. You can make them visible (i.e. exported) by providing documentation for individual objects; doco for data object \code{xx} needs to go in a separate text object \code{xx.doc} in the source task.}
\section{Big data objects}{Lazy-loading objects cached with \code{\link{mlazy}} are handled specially, to speed up \code{pre.install}. Such objects (apart from \code{.Random.seed}) get their cache-files copied to "inst/mlazy", and the \code{.onLoad} is prepended with code that will load them on demand. They are exported (unless overriden in the pre-install hook), and are not locked; users don't need to use \code{data()} to access them, and actually can't. If they are exported, you should really provide formal doco for them, at least if you're planning to submit to CRAN, so perhaps \code{pre.install} shouldn't just export them by default.}
\section{Overriding defaults}{
If a function \code{pre.install.hook.<<pkgname>>} exists in the task "pkgname", it will be called during \code{pre.install}. It will be passed one list-mode argument, containing default values for various installation things that can be adjusted; it should return a list with the same names. It will also be passed any \code{...} arguments to \code{pre.install}, which can be used e.g. to set "production mode" vs "informal mode" of the end product. The hook can do two things: sort out any file issues not adequately handled by \code{pre.install}, and/or change the following elements in the list that is passed in:

 copies: files to copy directly

 dll.paths: DLLs to copy directly

 extra.docs: names of character-mode objects that constitute flat-format documentation

 description: named elements of DESCRIPTION file

 task.path: path of task (ready-to-install package will be created as a subdirectory in this)

 has.namespace: should a namespace be used?

 use.existing.NAMESPACE: ignore default and just copy the existing NAMESPACE file?

 nsinfo: default namespace information, to be written iff \code{has.namespace==TRUE} and \code{use.existing.NAMESPACE==FALSE}

 exclude.funs: any functions \bold{not} to include

 exclude.data: non-functions to exclude from \code{system.rda}

There are two reasons for using a hook rather than directly setting parameters in \code{pre.install}. The first is that \code{pre.install} will calculate sensible but non-obvious default values for most things, and it is easier to change the defaults than to . The second is that once you have written a hook, you can forget about it-- you don't have to remember special argument values each time you call \code{pre.install} for that task.

The pre-install hook is probably the best way to tailor the package construction, but there are some other "legacy" ways to include/export specific objects. If a character vector \code{forced!exports} is present in the task, then the objects it names will be exported. If any function has an attributed \code{export.me}, it is also exported (which can be used to prevent something being seen as an S3 method).
}
\seealso{\code{\link{cd}}, \code{\link{doc2Rd}}, \code{\link{maintain.packages}}}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
