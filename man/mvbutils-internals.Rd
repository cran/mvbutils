\name{mvbutils-internal}
\alias{mvbutils-internal}
\alias{**\%}
\alias{.First.lib}
\alias{.onLoad}
\alias{as.docattr}
\alias{assign.to.base}
\alias{cd..}
\alias{cd.change.all.paths.guts}
\alias{cd.load}
\alias{cd.write.mvb.tasks.guts}
\alias{cdfind.guts}
\alias{cdtree.guts}
\alias{char.unlist}
\alias{close.selfdeleting.file}
\alias{create.bkind.if.needed}
\alias{deal.with.backups}
\alias{discard.mouldering.backups}
\alias{dont.save}
\alias{dos.or.windows}
\alias{expand.match}
\alias{find.derefs}
\alias{find.path}
\alias{find.prefix}
\alias{find.web}
\alias{fixr.guts}
\alias{get.bkfile}
\alias{get.cd.from.menu}
\alias{get.image.filename}
\alias{get.mcache.reffun}
\alias{get.mcache.store.name}
\alias{get.new.file.numbers}
\alias{get.path.list}
\alias{get.Rdata.path}
\alias{get.ref.info}
\alias{group}
\alias{hack.help}
\alias{hack.library}
\alias{install.proged}
\alias{is.nonzero}
\alias{lazify}
\alias{least.mutual.dependency}
\alias{load.mvb}
\alias{make.arguments.section}
\alias{make.internal.doc}
\alias{make.new.cd.task}
\alias{make.usage.section}
\alias{maybe.save.after.move}
\alias{move.backup.file}
\alias{move.fix.list}
\alias{move.to.mcache}
\alias{mupdate.mcache}
\alias{organize.web.display}
\alias{plot.cdtree}
\alias{prepare.for.move}
\alias{print.cdtree}
\alias{print.docattr}
\alias{print.pagertemp}
\alias{promote.2.to.1}
\alias{read.bkind}
\alias{reattach.placeholder}
\alias{save.mchanged}
\alias{save.mvb.db}
\alias{save.refdb}
\alias{set.path.attr}
\alias{set.pos.and.path}
\alias{set.test}
\alias{upper.case}
\alias{what.is.open}
\alias{write.mvb.tasks}
\title{Internal functions for mvbutils}
\description{Internal functions for \code{mvbutils}, not meant to be called directly.}
\usage{
**\%(x, y)
.First.lib(libname, pkgname)
.onLoad(libname, pkgname)
as.docattr(x)
assign.to.base(x, what = lapply(named(x), function(x, where) get("replacement." \%&\% x, pos = where), where = where), where = -1)
cd..(pos, nlocal = sys.parent())
cd.change.all.paths.guts(found, task.dir, task.name = "??", env, old.path, new.path, case)
cd.load(taskname, pos, attach.new, nlocal = sys.parent())
cd.write.mvb.tasks.guts(found, task.dir, task.name, env)
cdfind.guts(found, task.dir, task.name, pattern, env)
cdtree.guts(found, task.dir, task.name, env)
char.unlist(x)
close.selfdeleting.file(con, \dots)
create.bkind.if.needed(dir)
deal.with.backups(name, where)
discard.mouldering.backups(nlocal = sys.parent())
dont.save()
dos.or.windows()
expand.match(short, long, any.case = FALSE)
find.derefs(envir)
find.path(rel.path, char.rel.path, return.all = FALSE)
find.prefix(j, nodes, parents)
find.web(nlocal = sys.parent())
fixr.guts(name, new = FALSE, proged, fixing = TRUE)
get.bkfile(name, bkdir, create = FALSE)
get.cd.from.menu()
get.image.filename(full.path)
get.mcache.reffun(whati, envir)
get.mcache.store.name(envir)
get.new.file.numbers(derefs, file.numbers)
get.path.list()
get.Rdata.path(path)
get.ref.info(envo, nlocal = sys.parent())
group(m, \dots)
hack.help()
hack.library()
install.proged(option.name = "program.editor")
is.nonzero(x)
lazify(path, package, pkgpath)
least.mutual.dependency(funmat, funs, level)
load.mvb(filename, name, pos, attach.new = pos != 1, path, \dots)
make.arguments.section(funs = find.funs() \%except\% find.documented(doctype = "Rd"), file = stdout())
make.internal.doc(funs, package)
make.new.cd.task(task.name, nlocal = sys.parent(), answer, dir.name)
make.usage.section(funs = find.funs() \%except\% find.documented(doctype = "Rd"), file = stdout())
maybe.save.after.move(to.from)
move.backup.file(name, old.dir, new.dir, copy = FALSE)
move.fix.list(nlocal = sys.parent())
move.to.mcache(what, envir, getfrom)
mupdate.mcache(what, mcache)
organize.web.display(resequence = TRUE, merge01 = FALSE, plotmath = FALSE, nlocal = sys.parent())
plot.cdtree(x, \dots)
prepare.for.move(path)
print.cdtree(x, \dots)
print.docattr(x, \dots)
print.pagertemp(x, \dots)
promote.2.to.1()
read.bkind(dir)
reattach.placeholder(sn, nlocal = sys.parent())
save.mchanged(objs, envir)
save.mvb.db(env, path, dbtype = attr(path, "mvb.db.type"), objs, \dots)
save.refdb(file, envir, \dots)
set.path.attr(env, the.path, task.name = character(0))
set.pos.and.path(nlocal = sys.parent())
set.test(a, b)
upper.case(s)
what.is.open()
write.mvb.tasks(tasks = get("tasks", env = env), env = .GlobalEnv, dir = attr(env, "path"))
}
\keyword{internal}
