\name{mvbutils-internal}
\alias{mvbutils-internal}
\alias{\%**\%}
\alias{as.docattr}
\alias{assign.to.base}
\alias{called.by}
\alias{cdfind.guts}
\alias{cd.change.all.paths.guts}
\alias{cdtree.guts}
\alias{cd.write.mvb.tasks.guts}
\alias{cd..}
\alias{cd.load}
\alias{char.unlist}
\alias{close.selfdeleting.file}
\alias{create.bkind.if.needed}
\alias{deal.with.backups}
\alias{discard.mouldering.backups}
\alias{dos.or.windows}
\alias{exists.mvb}
\alias{expand.match}
\alias{find.path}
\alias{find.prefix}
\alias{find.web}
\alias{get.bkfile}
\alias{get.cd.from.menu}
\alias{get.path.list}
\alias{group}
\alias{hack.help}
\alias{hack.library}
\alias{index}
\alias{install.proged}
\alias{is.nonzero}
\alias{least.mutual.dependency}
\alias{lib.pos}
\alias{load.mvb}
\alias{make.new.cd.task}
\alias{maybe.save.after.move}
\alias{move.backup.file}
\alias{move.fix.list}
\alias{my.all.equal}
\alias{organize.web.display}
\alias{plot.cdtree}
\alias{print.cdtree}
\alias{print.docattr}
\alias{prepare.for.move}
\alias{promote.2.to.1}
\alias{put.in.session}
\alias{read.bkind}
\alias{reattach.placeholder}
\alias{save.pos}
\alias{set.path.attr}
\alias{set.pos.and.path}
\alias{set.test}
\alias{unpaste}
\alias{upper.case}
\alias{what.is.open}
\alias{write.mvb.tasks}
\title{Internal functions for mvbutils}
\description{Internal functions for \code{mvbutils}, not meant to be called directly.}
\usage{
x \%**\% y
as.docattr(x)
assign.to.base( x, what=lapply( named( x),
 function( x, where) get( "replacement." \%&\% x, pos=where),
 where=where), where=-1)
called.by( fname, can.match, where)
cdfind.guts(found, task.dir, task.name, pattern, env)
cd.change.all.paths.guts(found, task.dir, task.name = "??",
 env, old.path, new.path, case)
cdtree.guts(found, task.dir, task.name, env)
cd.write.mvb.tasks.guts(found, task.dir, task.name, env)
cd..(pos, nlocal = sys.parent())
cd.load( taskname, pos, attach.new, nlocal = sys.parent())
char.unlist(x)
close.selfdeleting.file( con, \dots)
create.bkind.if.needed(dir)
deal.with.backups(name, where)
discard.mouldering.backups(nlocal = sys.parent())
dos.or.windows()
exists.mvb(x, pos = -1, envir = pos.to.env(pos), frame, mode = "any",
 inherits = FALSE)
expand.match(short, long, any.case = FALSE)
find.path(rel.path, char.rel.path, return.all = FALSE)
find.prefix(j, nodes, parents)
find.web(nlocal = sys.parent())
get.bkfile(name, bkdir, create = FALSE)
get.cd.from.menu()
get.path.list()
group(m, \dots)
hack.library()
index( lvector)
install.proged()
is.nonzero( x)
least.mutual.dependency(funmat, funs, level)
lib.pos()
load.mvb(filename, name, pos, attach.new, \dots)
make.new.cd.task(task.name, nlocal = sys.parent(), answer, dir.name)
maybe.save.after.move(to.from)
move.backup.file(name, old.dir, new.dir, copy = FALSE)
move.fix.list(nlocal = sys.parent())
my.all.equal(x, y)
organize.web.display(resequence = TRUE, merge01 = FALSE,
 plotmath = FALSE, nlocal = sys.parent())
prepare.for.move(path)
plot.cdtree( x, \dots)
print.cdtree( x, \dots)
print.docattr( x, \dots)
promote.2.to.1()
put.in.session(\dots)
read.bkind(dir)
reattach.placeholder(sn, nlocal = sys.parent())
save.pos( pos, path, ascii=FALSE)
set.path.attr(env, the.path, task.name = character(0))
set.pos.and.path( nlocal=sys.parent())
set.test(a, b)
unpaste(string, sep)
upper.case( s)
what.is.open()
write.mvb.tasks(tasks = get("tasks", env = env),
 env = .GlobalEnv, dir = attr(env, "path"))
}
\note{
The following may be obsolete: \code{expand.match}, \code{char.unlist}, \code{\%**\%}, \code{what.is.open}

or duplicative

\itemize{
\item \code{index} (nearly synonymous with \code{which})
\item \code{my.all.equal} (\code{identical}? \code{all.equal}?)
\item \code{unpaste} (for compatibility with S+)
\item \code{upper.case} (\code{casefold})
\item \code{save.pos} (use \code{Save.pos} instead for more safety)
}

The \code{print} and \code{plot} methods do the obvious thing; this seems to be the least intrusive place to "document" them.

\code{hack.library} and \code{hack.help} produce modified versions of the appropriate system functions, hopefully in a version-independent fashion. See \code{\link{README.mvbutils}}.
}
\author{Mark Bravington}
\keyword{internal}
