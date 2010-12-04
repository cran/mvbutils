# This is package mvbutils 

"%!in%" <-
function (a, b) 
!(a %in% b)


"%&%" <-
function (a, b) 
  paste(a, b, sep = "")


"%**%" <-
function (x, y) 
{
    dimnames(x) <- NULL
    dimnames(y) <- NULL
    if (length(dim(x)) == 2 && length(dim(y)) == 2 && dim(x)[2] == 
        1 && dim(y)[1] == 1) 
        return(c(x) %o% c(y))
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
        dim(x) <- NULL
    if ((!is.null(dim(y)) && any(dim(y) == 1))) 
        dim(y) <- NULL
    if (is.null(dim(x)) && is.null(dim(y))) {
        if (length(x) == length(y)) 
            x <- x %*% y
        else {
            if ((length(x) != 1) && (length(y) != 1)) 
                stop("lengths of x (" %&% length(x) %&% ") and y (" %&% 
                  length(y) %&% ") are incompatible")
            else x <- x * y
        }
    }
    else x <- x %*% y
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
        dim(x) <- NULL
    x
}


"%downto%" <-
function( from, to) if( from >= to) from:to else numeric( 0)


"%except%" <-
function (vector, condition) 
vector[match(vector, condition, 0) == 0]


"%in.range%" <-
function (a, b) 
(a >= min(b)) & (a <= max(b))


"%is.a%" <-
function (x, what) 
inherits(x, what, FALSE)


"%is.an%" <-
function (x, what) 
inherits(x, what, FALSE)


"%is.not.a%" <-
function (x, what) 
!inherits(x, what, FALSE)


"%is.not.an%" <-
function (x, what) 
!inherits(x, what, FALSE)


"%matching%" <-
function( x, patt) 
  unique( unlist( lapply( patt, grep, x=as.character( x), value=TRUE)))


"%not.in%" <-
function (a, b) 
!(a %in% b)


"%such.that%" <-
function( a, b)
  a[ eval( do.call( 'substitute', list( substitute( b), list( '.'=quote( a)))),  list( a=a), enclos=sys.frame( mvb.sys.parent()) )]


"%SUCH.THAT%" <-
function( a, b) {
  if( !length( a))
return( a)
  fun <- function( .) .
  body( fun) <- substitute( b)
  environment( fun) <- sys.frame( sys.parent())
  ind <- logical( length( a))
  for( i in seq( along=a))
    ind[ i] <- fun( a[[ i]])
  a[ ind]
}


"%that.are.in%" <-
function( a, b) 
  a[ a %in% b]


"%that.dont.match%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  x[ seq_along( x) %except% unlist( lapply( patt, grep, x=as.character( x)))]
}


"%that.match%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  unique( unlist( lapply( patt, grep, x=as.character( x), value=TRUE)))
}


"%upto%" <-
function (from, to) 
if (from <= to) from:to else numeric(0)


"%where%" <-
function( x, cond) {
  # x is data.frame; cond is expression to evaluate, subbing first in x then in caller
  # Example: if x has a column 'stuff'
  # x %where% (stuff < 3)
  # is the same as x[ x$stuff<3,]
  # Note the brackets, required by operator precedence rules
  
  mum <- mvb.sys.parent()
  if( mum==0)
    mum <- .GlobalEnv
  else
    mum <- sys.frames()[[ mum]]
    
  cond <- eval( substitute( cond), x, enclos=mum)
  cond[ is.na( cond)] <- FALSE
  x[ cond,]
}


"%where.warn%" <-
function( x, cond) {
  # x is data.frame; cond is expression to evaluate, subbing first in x then in caller
  # Example: if x has a column 'stuff'
  # x %where.warn% (stuff < 3)
  # is the same as x[ x$stuff<3,]
  # but if any of the conditions is NA or FALSE, a warning is given for those rows
  # Note the brackets, required by operator precedence rules
  
  sub.cond <- deparse( substitute( cond), nlines=1, width=50)
  sub.x <- deparse( substitute( x), nlines=1, width=20)
  rx <- row.names( x)

  mum <- mvb.sys.parent()
  if( mum==0)
    mum <- .GlobalEnv
  else
    mum <- sys.frames()[[ mum]]
    
  cond <- eval( substitute( cond), x, enclos=mum)
  cond[ is.na( cond)] <- FALSE
  if( !all( cond))
    warning( sprintf( 'Check of %s fails on row(s) [%s] of %s', sub.cond, 
        paste( rx[ !cond], collapse=','), sub.x))
  x[ cond,]
}


"%without.name%" <-
function( x, what) {
  new.names <- names( x) %except% what
  x[ new.names]
}


".Last.lib" <-
function( libpath){
  s <- index( search()=='mvb.session.info')[1]
  if( is.na( s))
return()

  s <- pos.to.env( s)
  for( i in ls( s, patt='^base\\.'))
    assign.to.base( sub( '^base\\.', '', i), s[[i]])
    
  autoedit( FALSE)
}


".onLoad" <-
function( libname, pkgname) {
  if( 'mvb.session.info' %in% search()) 
return() # create only once per session

  wd <- getwd()
  attach( pos = 2, name = "mvb.session.info", list( .First.top.search = wd,
     .Path = c( ROOT=wd), session.start.time = Sys.time(), partial.namespaces=character(0)))
  nsenv <- environment( sys.function())
  evalq( {
      maintained.packages <- originals.mp <- dont.lock.envs <- presave.hooks <- list()
      fix.list<- empty.data.frame( name= , file= , where=, where.type=, dataclass='',
          file.time=0)
      dont.lock.envnames <- character(0) },
    env=nsenv)

  # Bindings are only locked *after* .onLoad-- so can't unlock them here...
  dont.lockBindings( 'dont.lock.envs', pkgname)
  dont.lockBindings( 'dont.lock.envnames', pkgname)

#   Now putting fix.list & maintained.packages into mvb.session.info, instead of package:mvbutils
#    copy.ns.objects( cq( fix.list, maintained.packages), pkgname)
  f <- function( val) blah-blah-blah
  for( x in cq( fix.list, maintained.packages, presave.hooks)) {
    body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
    environment( f) <- asNamespace( 'mvbutils')
    makeActiveBinding( x, f, as.environment( 'mvb.session.info'))
    dont.lockBindings( x, pkgname)
  }    

  set.path.attr( pos.to.env( 1), .Path)
  setup.mcache( .GlobalEnv) # in case of cached objects in ROOT, which 'load' won't understand

  assign.to.base( 'lockEnvironment', hack.lockEnvironment(), override.env=FALSE)
  assign.to.base( 'importIntoEnv', hack.importIntoEnv(), override.env=FALSE)  
  assign.to.base( 'loadNamespace', hack( 'loadNamespace', 
      partial={ pn <- get( 'partial.namespaces',  'mvb.session.info')
          (length( pn)>0) && ((pn == "EVERY PACKAGE") || (package %in% pn)) }),
      override.env=FALSE)
  if( exists( 'tasks', .GlobalEnv, mode='character', inherits=FALSE)
      && !is.na( tasks[ 'mvbutils']))
    load.maintained.package( 'mvbutils', full.path( tasks[ 'mvbutils'], wd), cq( ROOT, mvbutils),
        autopatch=!is.null( getOption( 'autopatch.mvbutils')))

  assign.to.base( 'rbind.data.frame', mvb.rbind.data.frame) # no choice on this one

  if( exists( 'Sys.setenv', mode='function', envir=baseenv()))
    assign( 'Sys.putenv', Sys.setenv, envir=nsenv)

  my.reps <- option.or.default( 'mvbutils.replacements', TRUE)
  # Circumvent user stuff-ups...
  my.reps <- switch( typeof( my.reps),
    'logical' = !identical( FALSE, my.reps[1]),
    'character' = my.reps,
    TRUE)

  my.reps.opts <- named( cq( difftime, '+.POSIXt', '-.POSIXt', loadhistory, savehistory, 
      save.image, library, help))

  my.reps <- my.reps.opts[ my.reps] %except% NA # storm the last bastion of user stuff-ups

  # Only do nominated replacements
  assign.to.base.opt <- function( what, ...) if( what %in% my.reps) assign.to.base( what, ...)

  assign.to.base.opt( 'difftime', hack( difftime, 
      units=c( 'secs', 'mins', 'hours', 'days', 'weeks', 'auto')))
  assign.to.base.opt( '+.POSIXt', mvb.plus.POSIXt)
  assign.to.base.opt( '-.POSIXt', mvb.minus.POSIXt)

  for( i in cq( load, save) %&% 'history')
    assign.to.base.opt( i, hack( i,
        file=if( nzchar( histfile <- Sys.getenv( 'R_HISTFILE'))) histfile else '.Rhistory'))

  # the mvbutils::: next is in case mvbutils gets loaded invisibly but not put on search()
  assign.to.base.opt( "library", hack( library, pos=1+rev( mvbutils:::search.task.trees())[1]))

  hack.save.image <- function( ...) {
      # formals will be replaced by those of 'save.image'
      # Evaluate args and check if they match defaults
      mc <- match.call( base.save.image)
      mc <- as.list( mc)[-1]
      mc[] <- mget( names( mc), sys.frame( sys.nframe()))
      form <- formals( base.save.image)
      if( length( mc) && !identical( form[ names( mc)], mc)) {
        mc <- c( quote( base.save.image), mc)
        eval( as.call( mc), sys.parent())
      } else # length(mc)==0 => default params anyway
        Save()
    }
  formals( hack.save.image) <- formals( save.image)
  assign.to.base.opt( "save.image", hack.save.image)

  hack.help <- function ( ...) {
    # help <- get("base.help", pos = "mvb.session.info")
    mc <- as.list(match.call(expand.dots = TRUE))
    mc[[1]] <- quote( base.help)
    if (!is.null(mc$topic) && !is.call(mc$topic) && is.null(mc$type) &&
        is.null(mc$lib.loc) && is.null(mc$try.all.packages)) {
        h1 <- try(eval(as.call(mc), sys.frame( sys.parent())), silent = TRUE)
        if (((h1 %is.not.a% "try-error") && length(unclass(h1)) >
            0) || ((h1 <- dochelp(as.character(mc$topic))) %is.a%
            "pagertemp"))
            return(h1)
    }

    eval(as.call(mc), sys.frame( sys.parent()))
  }
  formals( hack.help) <- formals( help)
  assign.to.base.opt( 'help', hack.help)
  
  cat( 'MVBUTILS loaded\n')
}


"add.flatdoc.to" <-
function( x=NULL, char.x=NULL, pkg=NULL, env=NULL) {
  if( is.null( env))
    env <- if( !is.null( pkg)) maintained.packages[[ pkg]] else parent.frame()
    
  if( is.null( char.x))
    char.x <- as.character( substitute( x))
  
  text <- docskel( x=x, char.x=char.x, env=env)
  class( text) <- 'docattr'
  if( is.null( x))
    x <- env[[ char.x]]
  attr( x, 'doc') <- text
  x
}


"as.cat" <-
function( x) { stopifnot( is.character( x)); oldClass( x) <- 'cat'; x}


"as.data.frame.I" <-
function( x, row.names=NULL, optional=FALSE, ...) {
  protect <- !sapply( x, is.factor) & !sapply( x, is.numeric)
  x[ protect] <- lapply( x[ protect], I)
  as.data.frame( x, row.names, optional, ...)
}


"as.docattr" <-
function( x) {
  stopifnot( is.character( x))
  class( x) <- 'docattr'
  x
}


"assign.to.base" <-
function( x, what=lapply( named( x),
    function( x, where) get( 'replacement.' %&% x, pos=where), where=where),
  where=-1, in.imports=exists( '.__NAMESPACE__.', environment( sys.function( sys.parent()))),
  override.env=TRUE) {
  if( !is.list( what))
    what <- list( what)

  if( is.null( names( what)))
    names( what) <- x

  reassign <- function( obj, value, env) {
      if( locked <- bindingIsLocked( obj, env))
        unlockBinding( obj, env)
      if( override.env)
        environment( value) <- environment( get( obj, env))
      assign( obj, value, env)
      if( locked) {
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding( obj, env)
      }
    }

  penv <- if( in.imports)
      parent.env( environment( sys.function( sys.parent())))
    else
      NULL

  for( xi in x) {
    this <- what[[ xi]]
    if( !is.null( penv) && exists( xi, penv, inherits=FALSE))
      reassign( xi, this, penv)

    where.xi <- find( xi, mode='function', numeric=TRUE)
    if( length( where.xi)>1) {
      warning( xi %&% ' appears more than once in search(); overwriting top copy only')
      where.xi <- where.xi[1] }

    if( !length( where.xi))
  next

    system.xi <- get( xi, where.xi)
    reassign( xi, this, pos.to.env( where.xi))

    # Also assign to the hidden namespace version, if it exists
    ns <- try( asNamespace( sub( 'package:', '', search()[ where.xi])), silent=TRUE)
    if( ns %is.not.a% 'try.error') {
      if( exists( xi, ns, inherits=FALSE))
        reassign( xi, this, ns)
      S3 <- ns$.__S3MethodsTable__.
      if( !is.null( S3) && exists( xi, S3, inherits=FALSE))
        reassign( xi, this, S3)
    }

    # Keep original
    if( !exists( 'base.' %&% xi, where='mvb.session.info', inherits=FALSE))
      assign( 'base.' %&% xi, system.xi, 'mvb.session.info')
  }

  invisible( NULL)
}


"attach.mlazy" <-
function( dir, pos=2,
    name='data:' %&% attr( .GlobalEnv, 'name') %&% ':' %&% basename( dir)) {
  attach( list(), pos=pos, name=name)
  e <- pos.to.env( pos)
  attr( e, 'path') <- dir <- task.home( dir)
  load.refdb( envir=e) # does nothing if no file
}


"autoedit" <-
function( do=TRUE){
  s <- as.environment( 'mvb.session.info')
  
  if( do) {
    if( !exists( 'autoedit.callback', env=s, inherits=FALSE) ||
        is.null( s$autoedit.callback))
      assign( 'autoedit.callback', addTaskCallback( 
          function( ...) { 
            try( FF())
            TRUE
          }), 
          env=s)
  } else if( !is.null( s$autoedit.callback)) {
    removeTaskCallback( s$autoedit.callback)
    s$autoedit.callback <- NULL
  }
}


"cachefile.path" <-
function (..., fsep = .Platform$file.sep) 
{
    if (any(sapply(list(...), length) == 0)) 
        return(character())
    paste(..., sep = fsep)
}


"called.by" <-
function( fname, can.match, where) {
  where <- if( is.environment( where)) list( where) else as.list( where)
  which <- unlist( lapply( where, exists, x=fname), use=FALSE)
  if( !any( which)) {
    f <- if( exists( fname)) get( fname) else list() }
  else
    f <- get( fname, pos=where[[ index( which)[ 1] ]])

#  flist_ as.character( unlist( f[length(f)], use=FALSE))
  flist <- char.unlist( f)

  if( !length( flist))
return( numeric( 0))

# Check for functions occurring in default parameters!
# R version does this automatically
#  everything_ unique( c( flist, as.character( unlist( as.list( f)[-length(f)], use=FALSE))))
  everything <- flist

# Main task:
  everything <- match( everything, can.match, nomatch=0)
  everything <- everything[ everything>0]

# Check for generic functions:
# Ignore for now in R
#  if( mode(f[[length(f)]])=='internal' | flist[1]=='UseMethod')
#    everything_ c( everything, index(substring( can.match, 1, nchar( fname)+1) == fname %&% '.'))

  everything
}


"callees.of" <-
function( funs, fw=foodweb( plotting=FALSE)) {
  if( fw %is.a% 'foodweb')
    fw <- fw[[1]]
  all <- dimnames( fw)[[1]]
  
  vec <- all %in% funs
  all[ vec %*% fw > 0]
}


"callers.of" <-
function( funs, fw=foodweb( plotting=FALSE)) {
  if( fw %is.a% 'foodweb')
    fw <- fw[[1]]
  all <- dimnames( fw)[[1]]
  
  vec <- all %in% funs
  all[ fw %*% vec > 0]
}


"cd" <-
function (to, execute.First = TRUE, execute.Last = TRUE) {
  # This to allow cd(..) from task "mvbutils" itself or a subtask...
  penv <- environment( sys.function())
  if( identical( penv, .GlobalEnv) || identical( penv, pos.to.env( 2))) {
    mc <- match.call( expand.dots=TRUE)
    mc[[1]] <- quote( mvbutils:::cd)
return( eval( mc, sys.frame( sys.parent())))
  }

  need.to.promote.on.failure <- FALSE
  on.exit({
    if (need.to.promote.on.failure) promote.2.to.1()
    if (!is.null(wd <- attr(.GlobalEnv, "path"))) setwd(wd)
    if (.Path[length(.Path)] != wd) {
      .Path <<- if (any(.Path == wd))
          .Path[1:max(index(.Path == wd))]
        else
          c("??" = character(0), "??" = wd)
    }
    cdprompt()
  })
  orig.path <- attr(.GlobalEnv, "path")
  if (is.null(orig.path) || !my.all.equal(orig.path, .Path[length(.Path)]))
stop("problem with taskly status of .GlobalEnv!")

  if (missing(to))
    to <- get.cd.from.menu()
  else to <- substitute(to)
  to <- strsplit(deparse(to), "/", fixed=TRUE)[[1]]
  if (to[1] == "0")
    to <- c(rep("..", length(.Path) - 1), to[-1])
  to <- to %except% "."
  if (!length(to))
return(invisible())

  ii <- to[-length(to)] != ".." & to[-1] == ".."
  ii <- c(ii, FALSE) | c(FALSE, ii)
  to <- to[!ii]
  if (!length(to))
return(invisible())

  if (to[1] == ".." && length(.Path) == 1)
stop("Can't move backwards from ROOT!")

  # Tedious temporaries...
  if( option.or.default( 'mvbutils.quick.cd', FALSE))
    suppressWarnings(
        mlazy( what=cq( .Random.seed, last.warning, .Traceback, .Saved.plots)
            %SUCH.THAT% exists( ., where=1, inherits=FALSE)))

  #save.image() # replaced by...
  Save.pos( 1) # 12/04, to work with all.rda & lazy-Load

  if( !nchar( Sys.getenv( 'R_HISTFILE')))
    Sys.putenv( R_HISTFILE=file.path( getwd(), '.Rhistory'))

  if( option.or.default( 'mvbutils.update.history.on.cd', TRUE))
    try( savehistory())

  need.to.promote.on.failure <- TRUE
  if (to[1] == "..") {
    cd..(1)
    for (i in 1 %upto% sum(to == ".."))
      cd..(2)
  } else
    load.mvb( file.path( orig.path, '.RData'), names(orig.path),
        pos = 2, attach = TRUE, path = orig.path)
  remove(list = lsall(pos = 1), pos = 1)
  attributes(.GlobalEnv) <- list()
  if (length(to)) {
    for (i in 2 %upto% length( to)) {
      cd.load(to[1], pos = 2, attach.new = TRUE)
      to <- to[-1]
    }
    cd.load(to[1], pos = 1, attach.new = FALSE)

    if( option.or.default( 'mvbutils.update.history.on.cd', TRUE))
      try( loadhistory())
    need.to.promote.on.failure <- FALSE
  }
}


"cd.." <-
function( pos, nlocal=sys.parent()) mlocal({
  # Do .Last before checking move, because this might detach rubbish
  if( execute.Last) {
    .Last.task <- if( exists( '.Last.task', where=pos, inherits=FALSE))
        get( '.Last.task', pos=pos)
      else
        function( pos) {}
    try( .Last.task( 1))
  }

  # For MPs with loaded namespaces:
  if( regexpr( '^temp.nsobj:', search()[pos+1])>0)
    detach( pos=pos+1)
    
  can.go.up <- !is.null( names( attr( as.environment( pos+1), 'path')))
  if( can.go.up)
    update.maintained.package( names( .Path)[ length( .Path)])
  else {
    need.to.promote.on.failure <- pos>1
stop( "Can't cd up; there's a non-task in position 2", call.=FALSE)
  }

  if( pos>1) {
    need.to.promote.on.failure <- TRUE
    detach( pos=pos)
  }

  to <- to[-1]
  orig.cd.path <- paste( names( .Path), collapse='/')
  .Path <<- .Path[ -length( .Path)]
  setwd( .Path[ length( .Path)])
  
  # All good; change fix.list if cd'ing up from a maintained package
  if( (names( orig.path) %in% names( maintained.packages)) &&
      (attr( maintained.packages[[ names( orig.path)]], 'path') == orig.path)) {
    fixing.in.pkg <- fix.list$where==orig.cd.path
    fix.list$where.type[ fixing.in.pkg] <<- 'package'
  }
})


"cd.change.all.paths" <-
function( from.text='0', old.path, new.path) {
  case <- if( .Platform$OS=='windows') 
      upper.case 
    else 
      function( x) x # case-sensitive

  cditerate( from.text, cd.change.all.paths.guts, '', old.path=case( old.path), 
      new.path=case( new.path), case=case)
}


"cd.change.all.paths.guts" <-
function( found, task.dir, task.name='??', env, old.path, new.path, case) {
  cat( task.name, '\n')
  if( exists( 'tasks', envir=env, inherits=FALSE) && is.character( tasks)) {
    tasks <- get( 'tasks', envir=env)
    tasks[] <- otasks <- gsub( '\\\\', '/', tasks) # [] to keep names
    tasks[] <- gsub( old.path, new.path, case( tasks))
    if( any( tasks != otasks)) {
      assign( 'tasks', tasks, envir=env)
      save.refdb( env, file=file.path( task.dir, '.RData'))

      if( option.or.default( 'write.mvb.tasks', FALSE))
        write.mvb.tasks( env=env, dir=task.dir)
    }
  }

  found
}


"cd.load" <-
function (taskname, pos, attach.new, nlocal = sys.parent()) mlocal({
  if (!exists("tasks", where = 2, inherits = FALSE))
    tasks <- character(0)
  full.path <- tasks[taskname]
  if (is.na(full.path)) {
    if (yes.no("Task " %&% taskname %&% " does not exist yet. Create it? "))
      full.path <- make.new.cd.task(taskname)
    else {
      cat("No ")
      stop("Just exiting cd")
    }
  }
  if( regexpr( '^[.]{1,2}/', full.path)>0) # rel paths OK; 24/6/2005
    full.path <- file.path( getwd(), full.path)

  # Strip out .. and .
  full.path <- gsub( '/\\./', '/', full.path)
  full.path <-  gsub( '[^/]*/\\.\\./', '', full.path)
  names( full.path) <- taskname

#  tasks.on.search <- sapply(seq(along = search()),
#      function(x)
#        if (is.null(x <- names(attr(pos.to.env(x), "path"))))
#          ""
#        else
#          x
#    )
#  is.attached <- 1 + index(tasks.on.search[-1] == taskname)
#  if (length(is.attached)) {
#    if (length(is.attached) > 1)
#      sapply(rev(is.attached)[-1], function(x) detach(pos = x))
#    Save.pos(is.attached[1])
#    # ...changed 12/04 from 'save.pos' to cope with all.rda & lazy-loads
#  }

  filename <- file.path( full.path, '.RData')
  if( is.na( filename) || !file.exists( full.path)) # || added 1/7/2005
stop( "Can't find an image file to load for '" %&% taskname %&% "'!")

  # Will *assume* there is just one possible package
  # Make sure saved image is up-to-date
  # save.refdb OK because can't mtrace in m.p. itself
  if( any( names( maintained.packages)==taskname)) {
    save.refdb( file=filename, env=maintained.packages[[ taskname]])
    fixing.in.pkg <- index( fix.list$where == paste( c( names(.Path), taskname), collapse='/'))
    fix.list$where.type[ fixing.in.pkg] <<- 'task'
  }

  load.mvb( filename, name = taskname,
    pos = pos, attach.new = attach.new, path = full.path)

  .Path <<- c(.Path, full.path)
  setwd( full.path) # new 24/6/2005, to allow rel paths

  epos <- as.environment( pos)
  if( any( names( maintained.packages)==taskname)) {
    if( packageHasNamespace( taskname, full.path)
        && (taskname %not.in% loadedNamespaces()))
      warning( "Package version of '" %&% taskname %&% "' not loaded yet-- may behave slightly differently")
    else if( taskname %in% loadedNamespaces()) {
      # Make "copies" of all extra stuff that's in namespace, using active bindings to ensure namespace
      # is synchronized. Copies go into a new search environment just below task
      ns <- asNamespace( taskname)
      etemp <- attach( NULL, pos=pos+1, name='temp.nsobj:' %&% taskname)
      # Don't copy weird stuff
      extroids <- lsall( ns) %except% lsall( epos)
      extroids <- extroids %such.that% (regexpr( '^\\.__.*__\\.$', .)<0)
      for( x in extroids) {
#        f <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
#        makeActiveBinding( x, as.function( alist( val=, f), envir=ns), etemp)
        f <- function( val) 0
        body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
        environment( f) <- ns
        makeActiveBinding( x, f, etemp)
      }
    } # if loaded namespace

    # Change fix list to point to here rather than m.p.
    repfl <- index( fix.list$where==search.task.trees()[1])
    if( length( repfl))
      fix.list[ repfl, 'where.type'] <<- 'task'
  } # if maintained

  if (execute.First && exists(".First.task", where = pos, inherits = FALSE)) {
    .First.task <- epos$.First.task # reassign for clarity of any error msg
    try(.First.task(pos))
  }
})


"cd.write.mvb.tasks" <-
function( from=., from.text=substitute( from)) 
  invisible( cditerate( from.text, cd.write.mvb.tasks.guts, vector( 'list', 0)))


"cd.write.mvb.tasks.guts" <-
function( found, task.dir, task.name, env) {
#  cat( task.name, task.dir); print( env)
  if( exists( 'tasks', env=env, inherits=FALSE))
    write.mvb.tasks( env=env, dir=task.dir)
  found
}


"cdfind" <-
function( pattern, from=., from.text, show.task.name=FALSE) {
  if( missing( from.text))
    from.text <- substitute( from) 
  answer <- cditerate( from.text, cdfind.guts, vector( 'list', 0), pattern, show.task.name=show.task.name)
  attributes( answer) <- list( names=names( answer))
  answer
}


"cdfind.guts" <-
function (found, task.dir, task.name, pattern, env) {
  if (length( o <- lsall(envir = env))) {
    o <- o %that.match% pattern
    if (length(o)) {
      a <- match(o, names(found), 0)
      if (sum(a)) 
        found[names(found)[a]] <- lapply(found[names(found)[a]], 
          c, task.name)
      if (sum(a == 0)) 
        found <- c(found, structure(.Data = rep(task.name, 
          sum(a == 0)), names = o[a == 0], mode = "list"))
    }
  }
  found
}


"cditerate" <-
function( from.text, what.to.do, so.far=vector('NULL',0), ..., show.task.name=FALSE) {
  assign( '[[', my.index, env=sys.frame( sys.nframe()))
  assign( '[[<-', my.index.assign, env=sys.frame( sys.nframe()))

  nodes <- find.path( char.rel.path=from.text)
  if( dos.or.windows())
    nodes <- upper.case( nodes)
  node.list <- list(1)
  names( node.list) <- names( nodes)
  parents <- 0

  is.task <- function( x) {
      if( !is.null( x <- attr( pos.to.env( x), 'path')) &&  !is.null( x <- names( x)[1]))
        x
      else
        ''
    }

  attached.tasks <- sapply( 1:length( search()), is.task)

  orig.env <- env <- new.env()
  i <- 1
  while( my.index.exists( i, node.list)) { # length( node.list[[i]])) {
    # Look first to see if task is attached
    this.name <- names( nodes[ node.list[[ i]]]) 
    if( show.task.name)
      cat( '\n' %&% names( unlist( node.list))[ match( node.list[[ i]], unlist( node.list))])
    m <- match( this.name, attached.tasks, 0)
    if( m)
      env <- as.environment( m)
    else if( file.access( this.file <- file.path( nodes[ node.list[[i]] ], '.RData'))==0) {
      # was: this.file <- file.path( nodes[ node.list[[i]] ], '.RData')
      # Clear last batch of objects
      env <- orig.env
      remove( list=lsall( env), envir=env)
      attr( env, 'path')  <- dirname( this.file)
      checko <- try( load.mvb( this.file, envir=env, name=this.name))
      if( checko %is.a% 'try-error') # hopefully things will just work anyway...
        warning( "Problem loading " %&% this.file)
    }

    so.far <- what.to.do( found=so.far, task.dir=nodes[ node.list[[i]]],
            task.name=find.prefix( node.list[[i]], nodes, parents), env=env, ...)

    deeper <- exists.mvb( 'tasks', env=env)
    if( deeper) {
      new.nodes <- get( 'tasks', env=env)
      deeper <- length( new.nodes) > 0 }

    if( deeper) {
      new.nodes <- sapply( new.nodes, full.path, start=nodes[[ node.list[[i]]]])
      if( dos.or.windows())
        new.nodes <- upper.case( new.nodes)

#       Eliminate self-referential subtasks!
      if( any( drop <- !is.na( sr <- match( new.nodes, nodes)))) {
        prefix <- find.prefix( node.list[[i]], nodes, parents)
        other.prefix <- character( sum( drop))
        for( j in 1:sum( drop))
          cat( 'Loop or self-reference in task hierarchy: ',
              prefix %&% '/' %&% names(new.nodes)[drop][ j], '=',
              find.prefix( sr[ drop][j], nodes, parents), '\n')
        new.nodes <- new.nodes[ !drop]
      } #self-reference

      nodes <- c( nodes, new.nodes)
      parents <- c( parents, rep( node.list[[i]], length( new.nodes)))
      new.nodes[] <- seq( to=length(nodes), by=1, length=length(new.nodes))
      mode( new.nodes) <- 'numeric'
      mode( new.nodes) <- 'list'
      node.list[[i]] <- c( node.list[[i]], new.nodes)
      i <- c( i, 2)
    } else { # !deeper
#     Move up while no more sibs.
      while( length( i)>1 && i[ length(i)] == length( node.list[[ i[-length(i)] ]]))
        i <- i[ -length(i)]

#     Move to next sib, if any.
      i[ length(i)] <- i[ length(i)] + 1
    } # deeper or not
  } # of master loop

  attr( so.far, 'nodes') <- nodes
  attr( so.far, 'node.list') <- node.list
  so.far
}


"cdprompt" <-
function() {
  opened <- what.is.open()
  if( length( opened)) 
    opened <- paste( c( '', opened), collapse='<')
    
  prompt <- names( .Path)[-1]
  if( length( prompt)>1 && (abbr.char <- option.or.default( 'abbreviate.cdprompt', 0)) > 0)
    prompt[ -length( prompt)] <- substring( prompt[ -length( prompt)], 1, abbr.char)

  invisible( options( prompt = paste( prompt, collapse = "/") %&% opened %&% "> ")) 
}


"cdregexpr" <-
function( regexp, from=., from.text, show.task.name=FALSE) {
  if( missing( from.text))
    from.text <- substitute( from) 
  answer <- cditerate( from.text, cdregexpr.guts, vector( 'list', 0), regexp,
      show.task.name=show.task.name)
  attributes( answer) <- list( names=names( answer))
  answer
}


"cdregexpr.guts" <-
function (found, task.dir, task.name, regexp, env) 
{
    if (length(o <- search.for.regexpr(regexp, where = env))) {
        found <- c(found, structure(.Data = rep(task.name, length(o)), 
            names = o, mode = "list"))
    }
    found
}


"cdtree" <-
function( from=., from.text=substitute( from), charlim=90) {
  indices <- cditerate( from.text, cdtree.guts, empty.data.frame( full.name=, own.name='', parent=0))

# Now produce function matrix etc.
  funs <- indices$own.name
  n <- length( funs)

# Avoid problems with duplicated names
  pre.X <- rep( 1, n)
  while( !is.na( d <- index( duplicated( funs))[1])) {
    pre.X[ d] <- pre.X[ d]+1
    funs[ d] <- 'X' %&% funs[ d] }

  funmat <- matrix( 0, n, n, dimnames=list( funs, funs))
  funmat[ cbind( indices$parent[-1], 2 %upto% n)] <- 1
  organize.web.display()
  funs <- substring( funs, pre.X, nchar( funs))
  dimnames( funmat) <- list( funs, funs)
  names( level) <- funs

  answer <- list( funmat=funmat, level=level, x=x, nodes=attr( indices, 'nodes'), 
    node.list=attr( indices, 'node.list'))
  class( answer) <- cq( cdtree, foodweb)
  answer
}


"cdtree.guts" <-
function (found, task.dir, task.name, env) 
{
    task.info <- strsplit(task.name, "/")[[1]]
    this.task.name <- task.info[length(task.info)]
    parent <- paste(task.info[-length(task.info)], collapse = "/")
    i <- match(parent, found$full.name, 0)
    rbind(found, list(full.name = task.name, parent = i, own.name = this.task.name))
}


"char.unlist" <-
function (x) {
  if (!(listable <- is.list(x))) {
    if( isS4( x) && ('.Data' %in% names( getSlots( class( x)))))
      x <- x@.Data
    if (listable <- (!is.atomic(x) && !is.symbol(x))) 
      x <- as.list(x)
  }
  
  if (listable) 
    unlist(lapply(x, char.unlist), use.names = FALSE)
  else 
    paste(deparse(x), collapse = "\n")
}


"clip" <-
function( x, n=1) x[ 1 %upto% ( length( x) - n)]


"close.selfdeleting.file" <-
function( con, ...) {
  fname <- summary( con)$description
  NextMethod( 'close')
  unlink( fname)
}


"copy.ns.objects" <-
function( objects, pkgname) {
  objects <- objects # force
  icns <- function( pkgname, pkgpath){
      senv <- as.environment( 'package:' %&% pkgname)
      cat( 'Locked?\n')
      print( environmentIsLocked( senv))
      print( objects)
      ns <- asNamespace( pkgname)
      f <- function( val) blah-blah-blah
      for( x in objects) {
        body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
        environment( f) <- ns
        makeActiveBinding( x, f, senv)
      }
    }
  setHook( packageEvent( pkgname, 'attach'), icns)
}


"cq" <-
function( ...) {
# Saves putting in quotes!
# E.G.: quoted( first, second, third) is the same as c( 'first', 'second', 'third')
# wrapping by as.character means cq() returns character(0) not list()
  as.character( sapply( as.list( match.call( expand.dots=TRUE))[-1], as.character))
}


"create.backups" <-
function( pos=1) {
  pos <- as.environment( pos)
  if( is.null( t <- attr( pos, 'path')))
stop( "Don't know what path to use for search environment:" %&% pos)

  mkdir( file.path( t, '.Backup.mvb'))
  fob <- read.bkind( t)

  # changed 5/4/2005 for speed with mcache
  cand <- lsall( pos) %SUCH.THAT% !bindingIsActive( ., env=pos)
  cand <- cand %SUCH.THAT% (mode(.)=='function')
  sapply( cand %except% fob$object.names, deal.with.backups, where=pos)
  invisible( NULL)
}


"create.bkind.if.needed" <-
function( dir) {
  dir <- file.path( dir, '.Backup.mvb')
  if( !is.dir( dir ))
    try( mkdir( dir))
  if( !is.dir( dir))
return('') # mucho problemo

  index.file <- file.path( dir, "index")
  if(!file.exists(index.file))
    file.create(index.file)
  index.file
}


"current.source" <-
function () 
{
    if (exists("source.list", "mvb.session.info")) {
        sl <- get("source.list", "mvb.session.info")
        sl[[length(sl)]]
    }
    else stdin()
}


"deal.with.backups" <-
function( name, where) {
  infeasible.R.line <- "'\"@\"@'@ START OF BACKUP @'@\"@\"'"
  backup.fix <- option.or.default( "backup.fix", c( 0, 0))
  if( backup.fix[1] == 0)
return()

  where <- as.environment( where)
  bdd <- attr( where, "path")
  if( !nchar( create.bkind.if.needed( bdd))) {
    warn( "Can't create backup directory!")
return() }

  filename <- get.bkfile( name, bdd, create = TRUE)
  ow <- options( warn = -1)
  previous.backups <- readLines( filename)
  options( ow)
  fun.type <- has.source( where[[name]])
  
  if( length( previous.backups)) {
    line.breaks <- index( previous.backups == infeasible.R.line)
    if( !fun.type) {
      # Line after infeasible is number of lines until next infeasible 
      next.break <- line.breaks <- line.breaks[ 1]
      repeat{ 
        next.break <- next.break + 3 + 
            as.numeric( previous.backups[ next.break+2])
        if( next.break > length( previous.backups))
      break
      
        if( previous.backups[ next.break] != infeasible.R.line) {
warning( "Stuffed backup for " %&% name %&% "; keeping extra stuff")
      break
        }
        
        line.breaks <- c( line.breaks, next.break)
      }
    }
      
    if( !length( line.breaks))
      previous.backups <- character( 0)
    else
      discard.mouldering.backups()
  }
  cat( c( previous.backups, infeasible.R.line, "SESSION=" %&% unclass( session.start.time)),
      file = filename, sep = "\n")
  if( fun.type)
    write.sourceable.function( where[[ name]], filename, append = TRUE,
        print.name = TRUE, xn=name)
  else 
    cat( length( where[[name]]), where[[name]], file=filename, sep='\n', append=TRUE)
}


"demlazy" <-
function( ..., what, envir=.GlobalEnv) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)

  envir <- as.environment( envir)

  mcache <- attr( envir, 'mcache')
  what <- what %such.that% (. %in% names( mcache))
  if( !length( what))
return()

  for( i in what) {
    temp <- envir[[ i]]
    remove( list=i, envir=envir)
    envir[[ i]] <- temp
  }

  fp <- attr( envir, 'path')
  if( option.or.default( 'mlazy.subdir', TRUE)) 
    fp <- file.path( fp, 'mlazy')
    
  file.remove( file.path( fp, 'obj' %&% mcache[ what] %&% '.rda'))
  attr( envir, 'mcache') <- mcache %without.name% what
  invisible( NULL)
}


"deparse.names.parsably" <-
function( x) {
  if( typeof( x)=='symbol')
    'as.name("' %&% as.character( x) %&% '")'
  else
    deparse( x)
}


"discard.mouldering.backups" <-
function (nlocal = sys.parent()) 
mlocal({
    if (line.breaks[1] > 1) {
        previous.backups <- previous.backups[line.breaks[1]:length(previous.backups)]
        line.breaks <- line.breaks - line.breaks[1] + 1
    }
    keepo <- rep(TRUE, length(line.breaks))
    prev.times <- sapply(strsplit(previous.backups[line.breaks + 
        1], "=", fixed=TRUE), function(x) as.numeric(paste(x[-1], 
        collapse = "")))
    old.sessions <- unique(prev.times) %except% session.start.time
    if (length(old.sessions) > backup.fix[2]) 
        old.sessions <- rev(sort(old.sessions))[1 %upto% backup.fix[2]]
    keepo <- keepo & (prev.times %in% c(old.sessions, session.start.time))
    is.this.session <- prev.times == session.start.time
    if (sum(is.this.session) >= backup.fix[1]) 
        keepo <- keepo & (!is.this.session | (cumsum(is.this.session) > 
            sum(is.this.session) + 1 - backup.fix[1]))
    copy.lengths <- diff(c(line.breaks, length(previous.backups) + 
        1))
    keepo <- rep(keepo, copy.lengths)
    previous.backups <- previous.backups[keepo]
})


"do.in.envir" <-
function( fbody, envir=parent.frame(2)) {
  ff <- sys.function( sys.parent())
  body( ff) <- substitute( fbody)
  environment( ff) <- envir
  cc <- sys.call( sys.parent())
  cc[[1]] <- ff
  eval.parent( cc, 2)
}


"doc2Rd" <-
function( text, file=NULL, append=formals(cat)$append, warnings.on=TRUE, Rd.version=NULL,
    def.valids=NULL, check.legality=TRUE) {
  if( is.function( text)) {
    forig <- text
    text <- attr( text, 'doc')
  stopifnot( is.character( text))
  } else
    forig <- NULL

  # Enforce PERL syntax in regexes
  for( regexo in cq( grep, sub, gsub, regexpr, gregexpr)) {
    ff <- get( regexo)
    formals( ff)$perl <- quote( !fixed)
    assign( regexo, ff, envir=sys.frame( sys.nframe()))
  }

  if( is.null( Rd.version))
    Rd.version <- if( getRversion() >= '2.10.0') '2' else '1'
  is.Rd2 <- numeric_version( Rd.version) >= '2'
  
  # ... and for 'subco' (which uses Rd.version, for example)
  subco <- subco
  environment( subco) <- sys.frame( sys.nframe())
 
  # Pre-empt backslash and brace woes-- Rdoc 1 is very buggy about this
  notcom <- grep( '^[^%]', text)  
  # if( !is.Rd2) ??
  text[notcom] <- gsub( '\\', '\016', text[notcom], fixed=TRUE) # now leave til end
  text[notcom] <- gsub( '{', '\020', text[notcom], fixed=TRUE) 
  text[notcom] <- gsub( '}', '\021', text[notcom], fixed=TRUE) 

  # Check for completely informal doco...
  if( !match( 'DESCRIPTION', text, 0) && !match( 'Description:', text, 0)) {
    if( warnings.on)
      cat( "Looks like informal doco to 'doc2Rd', in " %&% text[1] %&% '\n')
    first.blank <- index( !nzchar( text))[1]
    if( is.na( first.blank)) {
      if( warnings.on)
        warning( "No blank lines-- so no aliasses will be set")
      text <- c( text[ 1], '', text[-1])
      first.blank <- 2
    }
    
    # Prepare to ignore other section-like lines-- just bold them
    seclines <- grep( '^([A-Z][a-z0-9 ]*[a-zA-Z0-9])(\\([Ss]\\))?:$', text)
    text[ seclines] <- '*' %&% seclines %&% '*'
    seclines <- grep( '^[A-Z][A-Z0-9.]+(\\(S\\))?$', text)    
    text[ seclines] <- '*' %&% seclines %&% '*'

    # Add DESCRIPTION field, containing everthing:
    text <- multinsert( text, first.blank, list( c( 'Documentation for ' %&% text[1], '', 
        'DESCRIPTION', '')))
        
    if( !is.null( forig)) {
      text <- c( text, '', 'USAGE', '', '# This section is machine-generated...', 
          sub( '^ *([^ ]+) .*', '\\1', text[1]) %&% sub( '^NULL$', '()', 
              sub( 'list', '', deparse( formals( forig)))))
      if( length( formals( forig)))
        text <- c( text, '', 'ARGUMENTS', '', 'This section is machine-generated...', 
            paste( ' ', names( formals( forig)), ': ???', sep=''))
    }
  }
  
  # Global sub of colonized section titles to caps
  seclines <- grep( '^([A-Z][a-z0-9]*[a-zA-Z0-9])(\\([Ss]\\))?:$', text)
  text[ seclines] <- sub( ' ', '.', substring( text[ seclines], 1, 1) %&% 
      toupper( substring( text[ seclines], 2, nchar( text[ seclines])-1)), fixed=TRUE)

  # Global sub of refs to section titles-- AUTHOR(S) as well as AUTHOR
  sectitles <- grep( '^[A-Z][A-Z0-9.]+(\\(S\\))?$', text, value=TRUE)
  for( secti in sectitles) {
    # tricky in case secti ends with .
    text <- gsub( '\\b([Ss])ee ' %&% to.regexpr( secti) %&% '($|\\W)', 
        '\\1ee *' %&% substring( secti, 1, 1) 
        %&% tolower( gsub( '.', ' ', gsub( '\\.$', '', 
        substring( secti, 2)), fixed=TRUE)) %&% '*\\2', 
        text)
    text <- gsub( '\\b' %&% to.regexpr( secti) %&% ' \\(qv\\)', '*' %&% substring( secti, 1, 1)
        %&% tolower( gsub( '.', ' ', gsub( '\\.$', '', 
        substring( secti, 2)), fixed=TRUE)) %&% '*', text)
  }

  tcon <- textConnection( text)
  on.exit( close( tcon))
  Rd <- character( 0)
  EOF <- FALSE

# Definitions:
  verbatim <- function( string) {
    string <- gsub( '\\', '\001', string, fixed=TRUE)
    string <- gsub( '{', '\\{', string, fixed=TRUE)
    string <- gsub( '}', '\\}', string, fixed=TRUE)
    string <- gsub( '%', '\\%', string, fixed=TRUE)
    string <- gsub( '\001', '\\\\', string, fixed=TRUE)
    string
  }
  
  out <- function( string, string2, strip.spaces.at.start=FALSE) {
      if( !missing( string2)) {
        if( strip.spaces.at.start)
          string2 <- sub( '^ +', '', string2)

        string2 <- 
          if( length( string2)==1)
            string <- paste( '\\', string, '{', string2, '}', sep='')
          else
            string <- c( '\\' %&% string %&% '{', string2, '}')
      }
      Rd <<- c( Rd, string)
    } # out function

  line <- function( skip.blanks=TRUE, do.subs=TRUE, auto.link=FALSE, uncomment=TRUE, valid.links=NULL) {
      repeat{
        line <- readLines( tcon, 1)
        if( !length( line)) {
          EOF <<- TRUE
      return( line) }
        if( uncomment && substring( line, 1, 1)=='%')
      return( substring( line, 2)) # unmodified apart from removing %
      
        line <- sub( ' +$', '', line) # strip spaces at the end
        if( uncomment)
          line <- gsub( '%', '\\%', line, fixed=TRUE)

        if( !skip.blanks || nzchar( line))
      break
      }

      if( do.subs)
        line <- subco( line, auto.link=auto.link, valid.links=def.valids)
      line
    }

  block <- function( do.subs=TRUE, bs17=FALSE, blank.stop=FALSE, auto.link=FALSE, Rd2.Rlike=FALSE) {
      block <- character( 0)
      repeat{
        new.line <- line( do.subs=do.subs, skip.blanks=!blank.stop, auto.link=auto.link, 
            valid.links=def.valids)
        if( EOF)
      break
        if( blank.stop && !nzchar( new.line))
      break
        # Check for field names
        if( length( grep( '^[A-Z][A-Z0-9.]+(\\(S\\))?$', new.line))) {
          # replace AUTHOR(S) by AUTHOR
          pushBack(  sub( '(S)', '', new.line, fixed=TRUE), tcon)
      break
        }
        
        # Pre-formatted?
        if( !bs17 && substring( new.line, 1, 2)=='%#') {
          pref.block <- block( do.subs=FALSE, bs17=TRUE, blank.stop=TRUE)
          # All into one line for now...
          block <- c( block, paste( c( '\\preformatted{', pref.block, '}'), collapse='\n'))
        } else 
          block <- c( block, new.line)
      }
      
      if( bs17) { 
        # Flag backslashes and braces for different treatment in verbatim-style bits
        # Same thing happens in 'line' inside code blocks
        block <- gsub( '\016', '\017', block, fixed=TRUE)
        block <- gsub( '\020', '\022', block, fixed=TRUE)
        block <- gsub( '\021', '\023', block, fixed=TRUE)
      }
      
      if( Rd2.Rlike) {
        block <- gsub( '\016', '\\', block, fixed=TRUE) # now leave til end
        block <- gsub( '\020', '{', block, fixed=TRUE) 
        block <- gsub( '\021', '}', block, fixed=TRUE) 
      
        block <- make.Rd2( block)
      }
      
      block
    }

  insert.para.breaks <- function( block) {
      if( length( block)>1) {
        n <- length( block)
        block <- rep( block, each=2)
        block[ 2*(1:n)] <- ''
        block <- block[ -2*n]
      }
      block
    }

  itemize <- function( block) {
      # Unlabelled (bulleted) lists 
      while( length( block) && length( items <- index( regexpr( '^ +[*-] ', block)>0))) {
        n.items <- min( index( diff( c( items, length(block)+5)) %!in% 1:2))

        # Start \itemize{
        block <- multinsert( block, items[1]-1, '\\itemize{')
        items <- items + 1 # to allow for the new \\itemize{ line
        if( n.items>1) # zap any blank lines between items
          block <- block[ -( items[1]:items[n.items] %except% items[1:n.items])]

        # Add \item
        items <- items[1]+(1:n.items)-1
        block[ items] <- '\\item ' %&% sub( '^ +[*-] ', '', block[ items])

        # End with back-brace for \itemize
        block <- multinsert( block, items[ n.items], '}')
      }
      
      # Labelled lists, e.g. value: result
      while( length( block) && length( items <- index( regexpr( '^ +[^:]*: ', block)>0))) {
        n.items <- min( index( diff( c( items, length(block)+5)) %!in% 1:2))

        # Start \describe{
        block <- multinsert( block, items[1]-1, '\\describe{')
        items <- items + 1 # to allow for the new \\describe{ line
        if( n.items>1) # zap any blank lines between items
          block <- block[ -( items[1]:items[n.items] %except% items[1:n.items])]

        # Add \item{label}{body}
        items <- items[1]+(1:n.items)-1
        block[ items] <- '\\item{' %&% sub( '^ +([^:]*): +', '\\1}{', block[ items]) %&% '}'

        # End with back-brace for \describe
        block <- multinsert( block, items[ n.items], '}')
      }

      block
    }

  list.block <- function( sub.item.names=FALSE, auto.link=FALSE) {
      block <- character( 0)
      repeat{
        new.line <- line( do.subs=FALSE) # subs done later
        if( EOF)
      break
        # Check for field names
        if( length( grep( '^[A-Z][A-Z0-9.]+$', new.line))) {
          pushBack(  new.line, tcon)
      break
        }
        # Check for list item: line starts with space, then comma-separated words ending with a colon
        if( regexpr( '^ ', new.line)>0) {
          # NB: whole item text is assumed to be on one line
          item <- strsplit( new.line, ': ')[[1]]
          item[1] <- if( sub.item.names) subco( item[1]) else gsub( "'", '', item[1]) 
          new.line <- paste( '\\item{', item[1], '}{',
              subco( paste( item[ -1], collapse=':')), '}', sep='')
        } else
          new.line <- subco( new.line, auto.link=auto.link, valid.links=def.valids)
        block <- c( block, new.line)
      }
      block
    }

  seealso.block <- function() {
      block <- ' ' %&% block() %&% ','
      block <- block[ regexpr( '^%', block) <0] # comment lines
      # Strip out anything already in \code{}...
      block <- gsub( '\\\\code\\{([^}]*)\\}', "'\\1'", block)
      # ...and put single words ended by comma or semicolon into \code{\link{}}
      block <- gsub( " ([a-zA-Z.][---a-zA-Z.0-9]*)('*)[,;]",
          ' \\\\code\\{\\\\link\\{\\1\\}\\}\\2,', block)
      # ...and strip quotes around these
      block <- gsub( "'(\\\\code\\{\\\\link\\{[^}]*\\}\\})'", '\\1', block)
      # ... and any remaining quotes back into \code{}
      block <- gsub( " '([^']+)'", " \\\\code\\{\\1\\}", block)
      block <- substring( block, 1, nchar( block)-1)
      block
    }


  keyword.block <- function() {
      block <- block()
      block <- grep( '^[^%]', block, value=TRUE) # drop comment lines
      block <- paste( block, collapse=' ')
      block <- gsub( '[,;]', ' ', block)
      block <- gsub( ' +', ' ', block)
      block <- strsplit( block, ' ')[[ 1]]
      block[ nchar( block) > 0]
    }

  nice.title <- function( section.title) {
      section.title <- gsub( '\\.', ' ', section.title)
      substring( section.title, 1, 1) <- upper.case( substring( section.title, 1, 1))
      section.title
    }

#  fields <- cq( description, usage, synopsis, arguments, arguments., value, details, examples,
#      author, references, note, see.also, keywords)
#  fields <- c( fields, 'author(s)')

# Code starts here
  name <- strsplit( line(), ' ')[[1]][1]
  out( 'name', name)
  overall.name <- name

  if( is.package <- regexpr( '\\-package', name)>0)
    out( 'alias', sub( '\\-package.*', '', name))

  while( nchar( name)){
    if( !is.null( def.valids))
      def.valids <- def.valids %except% name # don't link to myself
    out( 'alias', verbatim( name), strip.spaces.at.start=TRUE)
    name <- line( FALSE, FALSE, uncomment=FALSE)
  }

  if( is.package)
    out( 'docType', 'package')
    
  if( is.data <- match( 'FORMAT', text, 0)>0)
    out( 'docType', 'data')

  out( 'title', line( do.subs=FALSE)) # no special stuff allowed in title

  #  Itemizing rules are:
  #   - don't use \code subs in item names in VALUE or ARGUMENTS
  #   - optional to use it in other fields
  #   - don't use \itemize except for unnamed bullet-point lists (like this para)

  while( !EOF) {
    next.field <- tolower( line())
    if( EOF)
  break
    switch( next.field,
      description=,
      details=,
      author=,
      "author(s)"=,
      references=,
      format=,
      source=,
      note= out( next.field, itemize( insert.para.breaks( block( auto.link=!is.null( def.valids))))),       examples=,
      synopsis=,
      usage= out( next.field, block( do.subs=FALSE, Rd2.Rlike=is.Rd2, bs17=!is.Rd2)),
      see.also= out( 'seealso', insert.para.breaks( 
          block( auto.link=!is.null( def.valids)))), #seealso.block())),
      value=,
      arguments= out( next.field, list.block(FALSE, auto.link=!is.null( def.valids))),
      keywords= out( '\\keyword{' %&% keyword.block() %&% '}'),
      out( 'section{' %&% nice.title( next.field) %&% '}',
                  itemize( insert.para.breaks( block( auto.link=!is.null( def.valids)))))
    )
    
    # For user's own sections, used to have 
    #        if( regexpr( '\\.$', next.field)<0)
    #          itemize( insert.para.breaks( block()))
    #        else
    #          list.block(TRUE))
    # but it didn't work with funny characters anyway
  } # while new field

#  Rd <- Rd[ nchar( Rd)>0]

# Post-process to set /dontrun examples:
  dontrun <- grep( "^## +(DON'T|NOT) +RUN\\b", Rd, ignore.case=TRUE)
  if( length( dontrun)) {
    end.dontrun <- grep( "^## +END( +| *\\( *)(DON'T|NOT) +RUN\\b", Rd, ignore.case=TRUE)
    if( (length( end.dontrun) != length( dontrun)) || !all( diff( c( t( 
        matrix( c( dontrun, end.dontrun), ncol=2)))) > 0))
      warning( "Unmatched DON'T RUN block in doc with header:\n" %&% text[1]) # chaos likely later...
    Rd[ dontrun] <- '\\dontrun{'
    Rd[ end.dontrun] <- '}'
  }

  methodize.USAGE() # sigh

  # \keywords{} is mandatory...
  if( !length( grep( '^\\\\keyword\\{', Rd)))
    Rd <- c( Rd, '\\keyword{' %&% (if( is.data) 'data' else 'misc') %&% '}')
  
  # Split \preformatted; don't zap blanks
  preflines <- grep( '\n', Rd, fixed=TRUE)
  Rd <- multirep( Rd, preflines, strsplit( Rd[ preflines], '\n'))

  if( is.Rd2) {
    Rd <- gsub( '\016', '\\\\', Rd, fixed=TRUE) 
    Rd <- gsub( '\020', '\\{', Rd, fixed=TRUE)
    Rd <- gsub( '\021', '\\}', Rd, fixed=TRUE)

    # ... and in verbatim bits:  
    Rd <- gsub( '\017', '\\\\', Rd, fixed=TRUE) 
    Rd <- gsub( '\022', '{', Rd, fixed=TRUE)
    Rd <- gsub( '\023', '}', Rd, fixed=TRUE)
  } else {
    # Old format Rd had problems with some weird-but-legal sequences...
    # Restore backslashes & braces in normal text -- get round buggy Rd
    Rd <- gsub( '\016', '\\\\\\enc{}{}', Rd, fixed=TRUE) 
    Rd <- gsub( '\020', '\\{\\enc{}{}', Rd, fixed=TRUE)
    Rd <- gsub( '\021', '\\}\\enc{}{}', Rd, fixed=TRUE)

    # ... and in verbatim bits:  
    Rd <- gsub( '\017', '\\\\\\link{}', Rd, fixed=TRUE) 
    Rd <- gsub( '\022', '\\{\\link{}', Rd, fixed=TRUE)
    Rd <- gsub( '\023', '\\}\\link{}', Rd, fixed=TRUE)

    reduce.empty.links() # minimize offence to Rcmd check...
  }
  
  if( !is.null( file)) 
    cat( Rd, sep='\n', file=file, append=append)
    
  Rd <- as.cat( Rd)
  if( is.Rd2 && check.legality && getRversion() >= '2.10.0') {
    # parse_Rd unreliable in 2.9.x so only do this in 2.10 onwards, regardless of Rd.version
    ow <- options( warn=2)
    check.file <- tempfile( overall.name)
    on.exit( { options( ow); unlink( check.file)}, add=TRUE)
    cat( Rd, sep='\n', file=check.file)
    p1 <- try( parse_Rd( check.file))  # warning => error
    if( p1 %is.a% 'try-error') 
      class( Rd) <- c( 'try-error', class( Rd))
  } 
  
return( Rd)
}


"dochelp" <-
function( topic, doc) {
  # "doc" might point to another object. Start by looping til we have a character "doc".
  current.topic <- topic
  if( missing( doc)) { # TRUE unless this is being used as a pager
    doc <- 0 
    while( !is.character( doc) && exists( current.topic) && 
        length( doc <- attr( get( current.topic), 'doc'))) 
      if( is.list( doc))
        current.topic <- doc[[1]] # unwrap list 
  }

  fff <- FALSE # default
  if( has.doc <- is.character( doc)) {
    fff <- tempfile() 
    # on.exit( rm( fff)) not in 2.x
    
    doc <- doc[ regexpr( '^%', doc) < 0] # drop "%" lines
    doc <- strsplit( doc, '\n')
    doc[ !sapply( doc, length)] <- ''
    doc <- strwrap( unlist( doc), simplify=FALSE)
    doc[ !sapply( doc, length)] <- ''
    #   writeLines( paste( unlist( doc), collapse='\n'), con=fff) # writelines seems to zap empty lines
    cat( paste( unlist( doc), collapse='\n'), file=fff)
    # file.show( fff) 
    names( fff) <- topic
    class( fff) <- 'pagertemp'
  } 
  
#  invisible( has.doc) changed for 2.x
   invisible( fff)
}


"docskel" <-
structure( function( x=NULL, char.x=NULL, env=.GlobalEnv, assign.=FALSE){
  if( !identical( env, .GlobalEnv)) {
    env <- as.environment( env)
    pkg <- sub( 'package:', '', attr( env, 'name'))
  } else
    pkg <- 'not-yet-a-package'

  if( is.null( char.x)) {
    sx <- substitute( x)
    if( is.call( sx)) {
      x <- as.character( sx)
      if( x[1] %not.in% c( '::', ':::') || length( x)<3)
stop( "Don't know how to fixr '" %&% deparse( sx) %&% "'")
      pkg <- x[2]
      char.x <- x[3]
      if( any( search()==pkg))
        env <- as.environment( pkg)
      else if( is.null( env <- maintained.packages[[ pkg]])) {
        if( any( search()=='package:' %&% pkg))
          env <- as.environment( 'package:' %&% pkg)
        else if( pkg %in% loadedNamespaces())
          env <- asNamespace( pkg)
        else
stop( "Package '" %&% pkg %&% "' not available")
      }
    } else
      char.x <- deparse( sx)[1]
  }

  if( is.null( x))
    x <- env[[ char.x]]

  text <- c( paste( char.x, "    package:", pkg, "\n", sep=''), attr( sys.function(), 'att1'),
      make.usage.section( char.x, NULL, env))

  if( length( formals( x)))
    text <- c( text, attr( sys.function(), 'att2'),  make.arguments.section( char.x, NULL, env))

  text <- c( text, attr( sys.function(), 'att3'))

  #text <- unlist( strsplit( text, '\n'))
  if( assign.) {
    class( text) <- 'docattr'
    attr( x, 'doc') <- text
    env[[ char.x]] <- x # will do the assign for real...
  }

  class( text) <- 'cat'
  text
}
, att1 = structure(c("", "Do something-or-other", "", "DESCRIPTION", "",  "A splendid function that does something jolly useful", "", "",  "USAGE", "", "# This section is a formal requirement, and as such often isn't useful...",  "# ...in showing how to use the function(s). You can show more realistic usages...",  "# ...in comment lines, and/or refer to the EXAMPLES section.",  ""), class = "docattr") 
, att2 = structure(c("", "ARGUMENTS", "", "You can put normal text in ARGUMENTS, too, like this. Remember to indent all arguments, as below.",  ""), class = "docattr") 
, att3 = structure(c("", "VALUE", "", "Immense. NB this section isn't compulsory.",  "", "", "DETAILS", "", "Not compulsory. Other section headings, e.g. AUTHOR, should also go here. Use *single* quotes around object names and code fragments, e.g. 'bit.of.code()'. Use *double* quotes for \"text\" or \"file.name\". See 'doc2Rd' for full details of format.",  "", "", "SEE.ALSO", "", "'doc2Rd', 'flatdoc'", "", "", "EXAMPLES ",  "", "# Not compulsory to have an EXAMPLES -- you can put examples into other sections.",  "# Here's how to make a \"don't run\" example:", "", "## Don't run",  "reformat.my.hard.drive()", "## End don't run", "", "", "KEYWORDS",  "", "%% You can delete the KEYWORDS section-- it will be auto-added by 'doc2Rd'",  "%% These lines starting with \"%%\" won't appear in user-visible help.",  "", "misc"), class = "docattr") 
)


"dont.lock.me" <-
function( env=environment( sys.function( -1))){
  assign.to.base( 'lockEnvironment', hack.lockEnvironment())
  attr( env, 'dont.lock.me') <- TRUE
}


"dont.lockBindings" <-
function( what, pkgname, namespace.=TRUE) {
  # cat( 'dlb on ', what, 'in', pkgname, 'with namespace=', namespace., '\n')   
  what <- what # force ??
  f <- function( pkgname, pkgpath) 99 
  if( namespace.)
    body( f) <- substitute( sapply( what, unlockBinding, env=asNamespace( pkgname)), list( what=what))
  else
    body( f) <- substitute( sapply( what, unlockBinding, 
        env=as.environment( paste( 'package:', pkgname, sep=''))), list( what=what))
  environment( f) <- baseenv()
  setHook.once( pkgname, if( namespace.) "onLoad" else "attach", f, 'append')
}


"dont.save" <-
function() 
  option.or.default("dont.Save", cq( .packageName, .SavedPlots, last.warning, .Last.value, .Traceback))


"dos.or.windows" <-
function () 
.Platform$OS.type == "windows"


"empty.data.frame" <-
function (...) 
{
    mc <- as.list(match.call()[-1])
    m <- sapply(mc, mode)
    is.a.name <- m == "name"
    is.a.name[is.a.name] <- nchar(as.character(mc[is.a.name])) == 
        0
    mc[is.a.name] <- mc[!is.a.name][(cumsum(!is.a.name) + 1)[is.a.name]]
    df <- do.call("list", mc)
    df <- as.data.frame.I(df)
    df <- df[-(1:nrow(df)), , drop = FALSE]
    df
}


"exists.mvb" <-
function (x, pos = -1, envir = pos.to.env(pos), frame, mode = "any", inherits = FALSE) {
  if (!missing(frame)) 
      envir <- sys.frame(frame)
  else
    if( is.character( pos)) {
      sl <- search()
      pos <- structure( .Data=seq( sl), names=sl)[ pos] }
  .Internal(exists(x, envir, mode, inherits))
}


"expand.match" <-
function( short, long, any.case=FALSE) {
# Expands unique partial matches of 'short' in 'long'. 
# Leaves non-matches or non-uniques alone
  if( any.case)
    i <- pmatch( toupper( short), toupper( long), dup=TRUE)
  else
    i <- pmatch( short, long, dup=TRUE)
  short[ !is.na( i)] <- long[ i[ !is.na(i)]]
#  short[ is.na( i)] <- NA
  short
}


"expanded.call" <-
function( nlocal=sys.parent()) mlocal(
  lapply( named( names( formals( sys.function( mvb.sys.nframe())))), function( x) eval( as.name( x)))
)


"extract.named" <-
function( l, to=parent.frame()) {
  n <- names( l)
  for( i in n[ nchar( n)>0])
    assign( i, l[[ i]], envir=to)
}


"FF" <-
function() {
  # Check list of filenames to see if they've been updated
  new.file.times <- unclass( file.info( fix.list$file)[,'mtime'])
  modified <- is.na( new.file.times) | new.file.times!= fix.list$file.time
  if( !any( modified))
return( structure( character( 0), for.info='No modifications'))

  FF.find.homes() # check that the homes are attached...

  if( !any( modified))
return( structure( character( 0), for.info='No modifications (but some updated files)'))

  old.warn <- options( 'warn')[[1]]
  on.exit( options( warn=old.warn))

  for( mod in index( modified)) {
    name <- unclass( fix.list$name)[ mod]
    cat( name, ': ')
    stuffed <- FALSE
    if( length( grep( '\\<character\\>', fix.list$dataclass[ mod]))) {
      ff <- readLines( fix.list$file[ mod])
      the.class <- strsplit( fix.list$dataclass[ mod], ',', fixed=TRUE)[[1]] %except% 'character'
      if( !length( the.class))
        the.class <- 'cat' # print.as.cat
      class( ff) <- the.class
      cat( 'OK\n')
    } else {
      # Could be anything... 
      source.code <- readLines( fix.list$file[ mod]) # everything incl. any errors
      mt <- new.env( parent=asNamespace( 'mvbutils')) # limit damage
      code <- try( list( value=source.mvb( fix.list$file[ mod], max.n.expr=1, envir=mt)))

      if( code %is.a% 'try-error') {
        stuffed <- TRUE
        ff <- eval( substitute( function( ...) stop( my.name %&% 'failed to parse'),
            list( my.name=name)))
        attr( ff, 'source') <- source.code
      } else {
        ff <- code$value
        if( !is.function( ff))
          attr( ff, 'source') <- source.code
        cat( 'OK\n')
      }
    }
    
    if( is.function( ff)) {
      # Next bit modified 26/7/2005 to use old environment if available
      if( exists( name, mode='function', w[[ mod]], inherits=FALSE))
        environment( ff) <- environment( w[[ mod]][[ name]])
      else
        environment( ff) <- .GlobalEnv # why not?
    }
    assign(name, ff, w[[ mod]])
    if( has.source( ff) || is.character( ff)) # should now work with charvecs too
      try( deal.with.backups( name, w[[ mod]])) # ought not to crash, but...

    if( !stuffed && mods.in.packages[ mod])
      update.loaded.pkg( attr( w[[mod]], 'name'), name, ff)
  } # loop over modifiees

  autosave <- option.or.default( 'FF.autosave', FALSE)
  for( i in unique( w[ mods.in.tasks | mods.in.packages]))
    if( !identical( i, .GlobalEnv) &&
        ( autosave || yes.no( "Save task '" %&% attr( i, 'name') %&% "'? ")))
      Save.pos( i)

  answer <- unclass( fix.list$name[ modified])
  if( 'package:debug' %in% search() && any( is.traced <- (answer %in% names( tracees)))) {
    cat( 'Reapplying trace(s)...')
    lapply( answer[ is.traced], mtrace, fname=NULL, tracing=TRUE) # fname=NULL forces char.fname
    cat( 'done\n')
  }

  # fix.list <<- fix.list[ !modified,]
  fix.list$file.time <<- new.file.times # doesn't seem to work in one step
  answer
}


"FF.find.homes" <-
function( nlocal=sys.parent()) mlocal({
  w <- vector( 'list', nrow( fix.list))
  mods.in.tasks <- modified & fix.list$where.type=='task'
  if( any( mods.in.tasks)) {
    stt <- search.task.trees()
    where.tasks <- match( fix.list$where, names( stt))
    not.here <- mods.in.tasks & is.na( where.tasks) # nowhere to go. Warn?
    modified[ not.here] <- FALSE
    where.tasks[ not.here] <- NA
    use <- modified & !is.na( where.tasks)
    w[ use] <- lapply( stt[ where.tasks[ use]], as.environment)
  }

  mods.in.packages <- modified & fix.list$where.type=='package'
  if( any( mods.in.packages)) {
    task.trees <- sapply( lapply( maintained.packages, attr, which='task.tree'), paste, collapse='/')
    where.packs <- match( fix.list$where, task.trees, NA)
    not.here <- mods.in.packages & is.na( where.packs)
    modified[ not.here] <- FALSE
    where.packs[ not.here] <- NA
    use <- modified & !is.na( where.packs)
    w[ use] <- maintained.packages[ where.packs[ use]]
  }

  mods.in.attached <- modified & fix.list$where.type=='attached'
  if( any( mods.in.attached)) {
    where.att <- match( fix.list$where, search(), NA)
    not.here <- mods.in.attached & is.na( where.att)
    modified[ not.here] <- FALSE
    where.att[ not.here] <- NA
    use <- modified & !is.na( where.att)
    w[ use] <- lapply( where.att[ use], pos.to.env)
  }
})


"find.and.get" <-
function( nlocal=sys.parent()) mlocal({
  if( is.null( pkg)) {
    if( new)
      num.load.from <-  1
    else {
      num.load.from <- find( name, numeric=TRUE)[1]
      if( is.na( num.load.from)) {
        if( length( maintained.packages)) {
          mpls <- lapply( maintained.packages, lsall)
          m <- sapply( mpls, match, x=name, nomatch=0)
          if( sum( m>0)==1)
            pkg <- names( maintained.packages)[ m>0] # handle below
          else if( sum( m>0)>1)
  stop( "'" %&% name %&% "' found in more than one live package ('" %&%
          paste( names( maintained.packages)[ m>0], collapse="', '") %&% "'): must specify which one")
        }
        if( is.null( pkg)) { # can't find anywhere
          new <- TRUE
          num.load.from <- 1
        }
      } else if( regexpr( '^package:', search()[ num.load.from])>0) {
        # check for maintained version
        pkg <- substring( search()[ num.load.from], nchar( 'package:')+1) # handle below
        if( is.null( maintained.packages[[ pkg]])) {
          if( fixing)
            warning( "Reluctantly fixing '" %&% name %&% "' directly in 'package:" %&% pkg %&%
                "'-- won't affect any namespace copies and won't be saved",
                immediate.=TRUE)
          pkg <- NULL
        }
      }
    }
  }

  if( !is.null( pkg)) { # could be set during last
    num.load.from <- NA
    load.from <- maintained.packages[[ pkg]]
    if( is.null( load.from))
stop( "Package '" %&% pkg %&% "' not set up for editing-- see 'maintain.packages'")

    name.load.from <- paste( attr( load.from, 'task.tree'), collapse='/')
    type.load.from <- 'package'
    new <- new || !exists( name, load.from, inherits=FALSE)
  } else { # num.load.from cannot be NA
    load.from <- pos.to.env( num.load.from)
    if( !is.null( names( attr( pos.to.env( num.load.from), 'path')))) {
      name.load.from <- rev( unlist( lapply( num.load.from:length( search()),
          function( x) names( attr( pos.to.env( x), 'path')))))
      type.load.from <- 'task'
    } else {
      name.load.from <- search()[ num.load.from]
      type.load.from <- 'attached'
    }
  }

  name.load.from <- paste( name.load.from, collapse='/')

  trace.was.on <- FALSE
  if(!new) {
    x <- get( name, load.from)
    trace.was.on <- exists( 'tracees', 'mvb.session.info') && (name %in% names( tracees)) }
  else
    x <- what
})


"find.derefs" <-
function( envir) {
  if( is.null( mcache <- attr( envir, 'mcache')))
    attr( envir, 'mcache') <- mcache <- named( integer( 0))
  names( mcache) %SUCH.THAT% ( envir[[.]] %is.not.a% 'promise')
}


"find.docholder" <-
function( what, pos=find( funs[1])) {
  pos <- as.environment( pos)
  o <- lsall( pos) %except% mcachees( pos)
  searchfun.Rd <- function( x) {
    if( is.function( xo <- pos[[x]])) 
      doco <- attr( xo, 'doc')
    else if( length( grep( '\\.doc$', x)) && is.character( xo))
      doco <- xo
    else
      doco <- character(0)
    what %in% named.in.doc( doco)
  } # searchfun.Rd

  searchfun.casual <- function( x) if( !is.null( doc <- attr( pos[[x]], 'doc')) &&
      is.list( doc)) doc[[1]] else character(0)
  searchfun.own <- function( x) if( !is.null( doc <- attr( pos[[x]], 'doc')) && 
      is.character( doc)) x else character(0)

  keepo1 <- list( length( what))
  Rds <- sapply( o, searchfun.Rd)
  dim( Rds) <- c( length( what), length( o))
  # apply over-simplifies, so...
  keepo <- lapply( split( Rds, row( Rds)), function( ins) o[ins])
  keepo2 <- lapply( named( what), searchfun.own) # what not o
  keepo <- mapply( c, keepo, keepo2, SIMPLIFY=FALSE) 
  keepo3 <- lapply( named( what), searchfun.casual) # what not o
  keepo <- mapply( c, keepo, keepo3, SIMPLIFY=FALSE)
  names( keepo) <- what
  lapply( keepo, unique)
}


"find.documented" <-
function( pos=1, doctype=c( 'Rd', 'casual', 'own', 'any'), 
    only.real.objects=TRUE) {
# 'pos' can have length > 1-- so guts live inside function

  findo <- function( pos) {
    pos <- as.environment( pos)
    oallall <- lsall( pos) 
    oall <- oallall %except% mcachees( pos)
    ofuns <- oall %SUCH.THAT% exists( ., mode='function', envir=pos)
    odoc <- (oall %except% ofuns) %that.match% '\\.doc$'
    searchfun.docobj.Rd <- function( x) named.in.doc( pos[[x]])
    searchfun.Rd <- function( x) named.in.doc( attr( pos[[x]], 'doc'))
    searchfun.casual <- function( x) x[ !is.null( attr( pos[[x]], 'doc')) ]
    searchfun.own <- function( x) x[ !is.null( doc <- attr( pos[[x]], 'doc')) && 
        is.character( doc) ]

    keepo <- character( 0)
    for( dt in doctype)
      keepo <- c( keepo, unlist( lapply( ofuns, FUN='searchfun.' %&% dt)))
    if( 'Rd' %in% doctype)
      keepo <- c( keepo, unlist( lapply( odoc, searchfun.docobj.Rd)))
  returnList( keepo=unique( keepo), oall=oallall)
  } # findo

  doctype <- match.arg( doctype)
  if( doctype=='any')
    doctype <- c( 'Rd', 'casual')

  if( is.environment( pos))
    pos <- list( pos)
  res <- lapply( pos, findo)
  keepo <- unique( unlist( lapply( res, '[[', x='keepo')))
  obs <- unlist( lapply( res, '[[', x='oall'))
  if( only.real.objects)
    keepo <- keepo %that.are.in% obs

return( keepo)
}


"find.funs" <-
function( pos=1, ..., exclude.mcache=TRUE, mode='function') {
# In this version, "pos" can have length > 1
  findo <- function( pos2) {
      o <- named( lsall( pos=pos2, ...)) 
      if( exclude.mcache)
        o <- o %except% mcachees( pos2)
      if( !length( o))
    return( character( 0))
      old.warn <- options( warn=-1)$warn
      on.exit( options( warn=old.warn))
      keep <- sapply( o, exists, where=pos2, mode=mode, inherits=FALSE)
      if( !any( keep))
    return( character( 0))

      names( o) <- NULL
      o[keep]
    }

  if( is.environment( pos))
    pos <- list( pos)
  else
    pos <- lapply( pos, as.environment)
  unlist( lapply( pos, findo), use=FALSE)
}


"find.lurking.envs" <-
function( obj, trace=FALSE){
  listo <- list( quote( obj))
  out.str <- character(0)
  out.size <- numeric( 0)
  
  while( length( listo)) {
    if( trace)
      print( listo[[1]])
    thing <- eval( listo[[1]])
    out.str <- c( out.str, deparse( listo[[1]])[1])
    if( is.environment( thing)) {
      out.str[ length( out.str)] <- paste( out.str[ length( out.str)], 
        sub( 'environment', '', format( thing)))
      out.size <- c( out.size, Inf)
      # do not add environments to this list...
    } else {
      # process it, and add to list...
      out.size <- c( out.size, object.size( thing))
      
      if( is.recursive( thing)) {
        if( is.function( thing)) {
          nonmis <- sapply( formals( thing), function( x) !is.name( x) || nzchar( as.character( x)))
          listo <- c( listo, substitute( environment( x), list( x=listo[[1]])) ) #, 
              #lapply( names( formals( thing))[ nonmis], function( y) substitute( formals( x)$y, 
              #    list( x=listo[[1]], y=as.name( y)))), 
              #substitute( body( x), list( x=listo[[1]])))
        } else if( !is.call( thing) && !is.null( names( thing)))
          listo <- c( listo, lapply( names( thing), 
              function( x) substitute( a$b, list( a=listo[[1]], b=as.name( x)))))
        else
          listo <- c( listo, lapply( seq_along( thing), 
              function( x) substitute( a[[b]], list( a=listo[[1]], b=x))))
      }

      attro <- names( attributes( thing)) %except% 
          cq( dim, dimnames, class, levels, names, comment, row.names, tsp)

      if( length( attro))
        listo <- c( listo, lapply( attro, 
            function( x) substitute( attr( a, b), list( a=listo[[1]], b=x))))
    }
    
    listo <- listo[-1]
  }
  
  o <- order( out.size)
  data.frame( what=out.str[o], size=out.size[o])
}


"find.mp" <-
function( x, mode='any'){
  sx <- find( x, mode=mode, numeric=TRUE)[1]
  if( is.na( sx)) {
    sx <- index( sapply( maintained.packages,
        function( env) exists( x, env, mode=mode, inherits=FALSE))[1])
    if( is.na( sx))
stop( "Can't find '" %&% x %&% "' in search path or maintained packages")
    sx <- maintained.packages[[ sx]]
  } else
    sx <- as.environment( sx)

  sx
}


"find.path" <-
function( rel.path, char.rel.path, return.all=FALSE) {
  if( !missing( char.rel.path))
    rel.path <- as.character( char.rel.path) # in case of the number 0
  else
    rel.path <- deparse( substitute( rel.path))

  if( substring( rel.path, 1, 2)=='..' &&
      exists( rel.path, as.environment( 'mvb.session.info'), mode='environment', inherits=FALSE)) 
return( as.environment( 'mvb.session.info')[[ rel.path]])

  # Parse input string: NB that R interprets a/b/c as function calls!
  rel.path <- strsplit( rel.path, '/', fixed=TRUE)[[1]]
  rel.path <- as.character( unlist( rel.path))
  rel.path <- rel.path[ rel.path!="/"]

  search.list <- sapply( seq( search()),
      function( x) {
        x <- names( attr( pos.to.env( x), 'path'))
        if( is.null( x))
          x <- ''
        x }
      )

  get.tasks.if.present <- function( env.or.pos) {
      if( is.character( env.or.pos))
        env.or.pos <- pos.to.env( index( search.list==env.or.pos))

      if( exists.mvb( 'tasks', env=env.or.pos))
        get( 'tasks', env=env.or.pos)
      else
        character( 0)
    }

  env <- new.env()
  wp <- get( '.Path', pos='mvb.session.info')
  for( igo in seq( rel.path)) {
    go <- rel.path[ igo]
    if( go=='..') {
      if( length( wp))
        wp <- wp[-length(wp)] }
    else if( go=='0')
      wp <- .Path['ROOT']
    else if( go!='.') {
      if( identical( wp, .Path[ 1:length( wp)]))
        ctasks <- get.tasks.if.present( names( .Path)[ length( wp)])
      else {
#        cat( 'loading tasks from', wp[ length( wp)], '\n')
        load( file.path( wp[ length( wp)], '.RData'), envir=env)
        ctasks <- get.tasks.if.present( env)
        remove( list=objects( env=env, all=TRUE), envir=env)
      }

      if( !any( go==names( ctasks)))
stop( 'can\'t find task named ' %&% go %&% ' in ' %&% wp[length(wp)])
      else {
        old.wd <- getwd()
        actual.ctask <- try( {
            setwd( wp[ length( wp)])
            setwd( ctasks[ go])
            getwd()
          })
        setwd( old.wd)
        if( actual.ctask %is.a% 'try-error')
stop( "can't find dir of task named '" %&% go %&% "' in '" %&% wp[ length( wp)])

        names( actual.ctask) <- go
        wp <- c( wp, actual.ctask)
      }
    } # if: different types of 'go'
  } # for

  if( !return.all)
    wp <- wp[ length( wp)]

  wp
}


"find.prefix" <-
function (j, nodes, parents) 
{
    s <- names(nodes[j])
    while ((j <- parents[j]) > 0) s <- names(nodes[j]) %&% "/" %&% 
        s
    s
}


"find.web" <-
function( nlocal=sys.parent()) mlocal({
  funs <- unique( c( funs, generics))
  n <- length( funs)
  if( !n)
stop( 'Nothing there!')

  funmat <- matrix( 0, n, n, dimnames=list( MASTER=funs, SLAVE=funs))
  master.of <- lapply( funs, called.by, can.match=funs, where=where)
  n.master <- unlist( lapply( master.of, length))
  if( !sum( n.master))
stop( 'Bo-RING! No food chain here!')

  setup <- c( rep( 1:length(funs), n.master), unlist( master.of))
  dim( setup) <- c( sum( n.master), 2)
  funmat[ setup] <- 1
  diag( funmat) <- 0 # to drop self-references

# Not interested in calls TO generic functions:
  funmat[ ,generics] <- 0

# check whether any methods of generic functions:
  drop.generics <- funmat[ generics, ] %**% rep( 1, n) == 0
  if( any( drop.generics)) {
    funs <- funs[ -match( generics[drop.generics], funs)]
    funmat <- funmat[ funs, funs]
    n <- n-sum( drop.generics) }

  color <- rep( textcolor, n)

  if( length( prune)) {
    prunio <- matrix( 0, length( prune), n)
    prunio <- sapply( prune, regexpr, text=funs)
    prunio <- as.logical( (prunio != -1) %**% rep( 1, length( prune)))
    color[ prunio] <- highlight

  # Everything descended from a prune
    if( descendents) {
      old.descendents <- rep( FALSE, n)
      descendents <- prunio
      while( sum( descendents)!=sum( old.descendents)) {
        old.descendents <- descendents
        descendents <- descendents | (descendents %**% funmat > 0) } }
    else
      descendents <- prunio

  # All ancestors of a prune
    if( ancestors) {
      old.ancestors <- rep( FALSE, n)
      ancestors <- prunio
      while( sum( ancestors) != sum( old.ancestors)) {
        old.ancestors <- ancestors
        ancestors <- ancestors | (funmat %**% ancestors > 0) } }
    else
      ancestors <- prunio

    color <- color[ ancestors | descendents]
    funs <- funs[ ancestors | descendents]
    funmat <- funmat[ funs, funs, drop=FALSE]
    n <- length( funs)
  }

# Now we have to figure out what level in the hierarchy each fn. belongs at.
# Simple-minded approach: anything NOT called by any other function is top-
# level; anything called only by top-levels is second-level; etc.

  if( !n)
stop( 'Nothing there!')
  level <- rep(0, n); names( level) <- funs
  current.level <- 1
  while( any( level==0)) {
    tops <- rep( 1, sum( level==0)) %**% funmat[level==0, level==0] == 0
    if( !any( tops))  # we have to sort out functions that call each other
      tops <- least.mutual.dependency( funmat, funs, level)

    level[ dimnames( funmat)[[1]] [ level==0] [tops] ] <- current.level
    current.level <- current.level+1
  }
})


"fix.order" <-
function( env=1) {
  oenv <- env
  env <- as.environment( env)
  if( is.null( path <- attr( env, 'path')) || is.null( names( path))) 
stop( 'Not a task')

  fob <- read.bkind( path)
  if( !length( fob[[1]]))
stop( 'Can\'t deduce fix.order')

  fdates <- file.info( file.path( path, '.Backup.mvb', fob$files))$mtime
  o <- order( fdates)
  fob <- fob$object.names[ o]
  fob <- fob[ fob %in% find.funs( oenv) ] # remove deleted functions still with backups
  fob
}


"fixr" <-
function( x, new=FALSE, install=FALSE, what=list( function(){}, '')[[1]], fixing=TRUE, pkg=NULL, character.only=FALSE, new.doc=FALSE) {

    if( missing( x))
return( 'Nothing to edit!')

  prog <- 'program.' %&% ifelse( fixing, 'editor', 'reader')
  proged <- getOption( prog)
  if( is.null( proged) || install)
    proged <- install.proged( option.name=prog)

  if( !character.only) {
    sx <- substitute( x)
    if( is.call( sx)) {
      x <- as.character( sx)
      if( x[1] %not.in% c( '::', ':::', '$') || length( x)<3)
stop( "Don't know how to fixr '" %&% deparse( sx) %&% "'")
      pkg <- x[2]
      if( substring( pkg, 1, 2)=='..') { # ..mypack$fun
        pkg <- substring( pkg, 3)
        if( pkg %not.in% names( maintained.packages))
stop( "Package '" %&% pkg %&% "' is not a 'maintained package'!")
      }
      name <- x[3]
    } else
      name <- deparse( substitute( x), width=30, nlines=1)
  } else
    name <- x

  find.and.get()

  if( is.function( x))
    environment( x) <- .GlobalEnv # to prevent the environment string being printed after the definition.
    # ...mostly for new functions; bad practice to set environments otherwise.

  if( new.doc)
    x <- add.flatdoc.to( x, char.x=name, env=load.from)

  dir <- c( getOption( 'edit.scratchdir'), Sys.getenv( 'TMP'), Sys.getenv( 'TEMP'))
  dir <- dir[ nchar( dir)>0][1]
  if( is.na( dir))
stop( "Don't know where to put scratch files:" %&%
    " none of options( 'edit.scratchdir') or TMP or TEMP are set!")

  # Filename including "version" number if required
  exact.same <- index( name==fix.list$name & name.load.from==fix.list$where)[1]
  if( !is.na( exact.same))
    filename <- fix.list$file[ exact.same]
  else {
    if( !length( partial <- index( name==fix.list$name)))
      version.suffix <- ''
    else {
      ofnames <- fix.list$file[ partial]
      versions <- suppressWarnings( as.integer( sub( '(.*)#([0-9]+)\\.R$', '\\2', ofnames)))
      versions[ is.na( versions)] <- 0
      new.version <- min( (1:max( versions)) %except% versions)
      version.suffix <- '#' %&% new.version
    }

    filename <- file.path( dir, legal.filename( name %&% version.suffix %&% (
        if( has.source( x)) '.R' else '.txt')))
  }

  old.warn <- options(warn = -1, width = 180)[1:2] # wide to avoid line breaks
  failed.to.edit <- TRUE # usual pessimism
  on.exit({
    if( failed.to.edit && file.exists( filename))
      unlink(filename)
    if( trace.was.on)
      mtrace( char.fname=name)
    options(old.warn) })

  # Do backup only if task
  if( fixing && !new && type.load.from %in% cq( package, task) && has.source( x))
    deal.with.backups( name, load.from) # takes env or number

  if( has.source( x))
    write.sourceable.function( x, filename)
  else
    cat( x, file=filename, sep='\n')

#  OK <- shell( proged(name, filename), translate=TRUE, wait = FALSE) # shell doesn't work on Linux
  cmd <- proged( name, filename)
  if( dos.or.windows())
    cmd <- gsub( '([^ ])/', '\\1\\\\', cmd)

  OK <- system( cmd, wait=FALSE) # before 12/2005 'wait' was only set FALSE on Windows; dunno why

  if(OK != 0)
stop("Couldn't launch editor")

# Avoid returning focus to console
  put.in.session( just.created.window=TRUE)

# Zap duplicates
  if( fixing) {
    fix.list <<- fix.list[ fix.list$name != name | fix.list$where != name.load.from,]
    fix.list <<- rbind(fix.list,
        list( name = name, file = filename, where = name.load.from, where.type= type.load.from,
        dataclass = paste( c( class( x), if( is.character( x)) 'character'), collapse=','),
        file.time=unclass( file.info( filename)[1,'mtime'])))
  }

  failed.to.edit <- FALSE
  invisible(NULL)
}


"fixr.guts" <-
function( name, new=FALSE, proged, fixing=TRUE, what=list( function(){}, '')[[1]], obj) {
# Just like
  if( missing( name))
return( "Nothing to edit!")

  trace.was.on <- FALSE

# Function to edit, and its name (may be different from 'name' if method)
  if( !missing( obj)) {
    load.from <- 1
    x <- obj
    is.new <- trace.was.on <- FALSE
  } else {
    load.from <- if( new) NA else find( name, numeric=TRUE)[1] # mode check removed 28/7/2005
    is.new <- is.na( load.from)
    if(!is.new) {
      x <- if( missing( obj)) get( name, pos=load.from) else obj
      trace.was.on <- exists( 'tracees', 'mvb.session.info') && (name %in% names( tracees)) }
    else {
      x <- what
      load.from <- 1 }
  }

  if( is.function( x))
    environment( x) <- .GlobalEnv # to prevent the environment string being printed after the definition.
    # ...mostly for new functions; bad practice to set environments otherwise.

  try.load.from <- NULL
  num.load.from <- load.from
  if( load.from>1) {
    try.load.from <- names( attr( pos.to.env( load.from), 'path'))
    if( is.null( try.load.from))
      load.from <- search()[ load.from]
    else
      load.from <- try.load.from
  } else {
    load.from <- try.load.from <- names( attr( pos.to.env( 1), 'path')) # else ".GlobalEnv" will cause problems if there's a "cd"
    if( is.null( load.from)) {
      warning( search()[ load.from] %&% ' doesn\'t seem to be a task: object will be saved into .GlobalEnv')
      load.from <- '.GlobalEnv' }
  }

  dir <- options('edit.scratchdir')[1]
  if( is.null( dir)) {
    dir <- Sys.getenv( 'TMP')
    if( !nchar( dir))
      dir <- Sys.getenv( 'TEMP')
    if( !nchar( dir))
stop( "Don't know where to put scratch files: none of options( 'edit.scratchdir') or TEMP or TMP are set!")
  }

  filename <- file.path( dir, legal.filename( name)) # used to append.R to avoid...
  #... editors loading e.g. .First.lib as a binary file! Now assuming this is done outside

  old.warn <- options(warn = -1, width = 180)[1:2] # wide to avoid line breaks
  failed.to.edit <- TRUE # usual pessimism
  on.exit({
    if( failed.to.edit && file.exists( filename))
      unlink(filename)
    if( trace.was.on)
      mtrace( char.fname=name)
    options(old.warn) })

  if( fixing && !is.new && !is.null( try.load.from) && is.function( x)) # only do backup if task
    deal.with.backups( name, num.load.from)

  if( is.function( x))
    write.sourceable.function( x, filename)
  else
    cat( x, file=filename, sep='\n')

#  OK <- shell( proged(name, filename), translate=TRUE, wait = FALSE) # shell doesn't work on Linux
  cmd <- proged( name, filename)
  callo <- quote( system( cmd))
  if( 'wait' %in% names( formals( system)))
    callo$wait <- FALSE
  OK <- eval( callo)

  if(OK != 0)
stop("Couldn't launch editor")

# Avoid returning focus to console
  put.in.session( just.created.window=TRUE)

# Zap duplicates
  if( fixing) {
    fix.list <<- fix.list[ fix.list$name != name,]
    fix.list <<- rbind(fix.list,
        list( name = name, file = filename, where = load.from,
        dataclass = paste( class( x), collapse=','), file.time=unclass( file.info( filename)[1,'mtime'])))
  }

  failed.to.edit <- FALSE
  invisible(NULL)
}


"fixtext" <-
function( x, ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$what <- ''
  mc[[1]] <- quote( fixr)
  eval( mc, parent.frame())
}


"fixup.DLLs" <-
function( in.memory, ipath, rpath, pkg, use.newest=FALSE, nlocal=sys.parent()) mlocal({
  suffix <- '[.]' %&% (if( .Platform$OS.type=='windows') '(dll|DLL)' else 'so') %&% '$'

  dlls1 <- sort( dir( rpath, patt=suffix))
  dlls2 <- suppressWarnings( sort( dir( file.path( rpath, 'libs'), patt=suffix)))
  dlls <- c( dlls1, dlls2)
  dll.paths <- c( file.path( rpath, dlls1), file.path( rpath, 'libs', dlls2))
  names( dll.paths) <- dlls
  
  if( !is.null( ipath)) {
    if( is.dir( ipath.libs <- file.path( ipath, 'libs')))
      idlls <- sort( dir( ipath.libs, patt=suffix))
    else
      idlls <- character(0)
    inst.dll.paths <- file.path( ipath, 'libs', idlls)
    names( inst.dll.paths) <- idlls
    
    # New DLLs in source get copied; new DLLs in inst get deleted
    use.source <- dlls %except% idlls # provisionally, ones to replace

    if( length( both <- intersect( idlls, dlls))) {
      # dir results are sorted, so will be in same order for inst & source
      
      # Used to use MD5 but timestamp seems better
      #md5source <- sapply( dll.paths[ both], md5sum)
      #md5inst <- sapply( inst.dll.paths[ both], md5sum)
      # diff <- both[ md5inst != md5source]
      
      time.source <- file.info( dll.paths[ both])$mtime
      time.inst <- file.info( inst.dll.paths[ both])$mtime
      names( time.source) <- names( time.inst) <- both

      if( use.newest) { 
        if( length( newer.inst <- both[ time.source < time.inst])) {
          mvb.file.copy( inst.dll.paths[ newer.inst], dll.paths[ newer.inst], overwrite=TRUE)
          mvb.file.copy( inst.dll.paths[ newer.inst], 
              file.path( rpath, pkg, 'inst', 'libs', newer.inst), overwrite=TRUE)
        }
      }

      # If source is newer, handle below via 'use.source'  
      use.source <- c( use.source, both[ time.source > time.inst])
    } # if duplicated

    # in.memory=TRUE for reloading DLLs as appropriate-- always TRUE in current code
    # Unload / unlink
    if( length( use.source)) {
      # unload, replace, reload
      if( in.memory)
        lapply( getLoadedDLLs()[ sub( '.dll', '', use.source, ignore.case=TRUE)], 
            function( x) try( if( !is.null( x)) 
            dyn.unload( x[['path']]), silent=TRUE))
      mvb.file.copy( dll.paths[ use.source], file.path( ipath, 'libs', use.source), 
          overwrite=TRUE)
      if( in.memory)
        lapply( inst.dll.paths[ use.source], dyn.load)
    }

    # DLLs in inst that now shouldn't be (because not in source):
    inxs.dlls <- idlls %except% dlls 
    
    # Ones with source shouldn't be zapped either
    if( is.dir( file.path( rpath, 'src'))) {
      src.files <- dir( file.path( rpath, 'src'), patt='[.](c|cc|cpp|C|f|f90|f95|m|mm|M)$')
      if( length( src.files)) {
        # Include mypack.c-- though I recommend naming your 
        src.files <- c( src.files, pkg %&% '.c')
        src.dlls <- to.regexpr( sub( '[.][^.]*$', '', src.files)) # strip ext 
        src.dlls <- src.dlls %&% suffix # prepare for match
        inxs.dlls <- inxs.dlls %that.dont.match% src.dlls
      }
    }
    
    lapply( getLoadedDLLs()[ sub( '.dll', '', use.source, ignore.case=TRUE)],
    function( x) try( if( !is.null( x))
        dyn.unload( x[['path']]), silent=TRUE))
    file.remove( file.path( ipath, 'libs', inxs.dlls))
  }
})


"fixup.exports" <-
function( pkg) {
  # Make sure exported functions are visible
  
  ns <- asNamespace( pkg)
  # Export list must be read directly from the NAMESPACE file, ugggh
  things.to.export <- parseNamespaceFile( pkg, 
      package.lib=dirname( getNamespaceInfo( ns, 'path')))$exports
  
  expenv <- ns$.__NAMESPACE__.$exports
  unexportees <- lsall( expenv) %except% things.to.export
  rm( list=unexportees, envir=expenv)
  for( new.exportee in things.to.export %except% lsall( expenv)) 
    assign( new.exportee, structure( new.exportee, names=new.exportee), envir=expenv)
  
  visible <- list()
  if( ('package:' %&% pkg) %in% search())
    visible <- list( list( env=as.environment( 'package:' %&% pkg), 
        can.reset=FALSE)) # see below for can.reset

  loaded.users <- getNamespaceUsers( pkg) 
  loaded.users <- loaded.users %SUCH.THAT% (("package:" %&% .) %in% search())
  loaded.users <- loaded.users %SUCH.THAT% (try( asNamespace( .)) %is.not.a% 'try-error')
  for( lu in loaded.users)
    visible <- c( visible, list( list( env=parent.env( asNamespace( lu)), can.reset=FALSE)))
    
  # Space for code to look thru importing environments of other packages that might import this one;
  #... should add the parent.env of the namespace of the importing package to the list 'visible'
  
  # These might be locked, in which case it would be
  #... possible to fudge by resetting their parent.env to a new environment to contain these exports, 
  #... and whose own parent is the original parent.env. That is a bit RISKY so if I do code this, be sure 
  #... to make it optional. The 'can.reset' field is used to indicate whether this is OK; it isn't OK for
  #... the search path copy, but that shouldn't be locked in the first place.
  
  for( vis in visible) {    
    things.to.make.vis <- things.to.export %except% lsall( vis$env)
    things.to.zap <- unexportees %that.are.in% lsall( vis$env)
    if( length( c( things.to.make.vis, things.to.zap))) {
      assenv <- NULL # default: can't do it
      if( environmentIsLocked( vis$env)) {
        if( vis$can.reset && identical( parent.env( vis$env), asNamespace( 'base'))) {
          assenv <- new.env( parent=parent.env( vis$env))
          parent.env( vis$env) <- assenv # scary...
        } else if( environmentIsLocked( parent.env( vis$env)))
          warning( "Can't or daren't add to imports for " %&% format( vis$env))
        else
          assenv <- parent.env( vis$env) # not locked; probably fudged like this already
      } else # not locked
        assenv <- vis$env
      
      if( !is.null( assenv)) {
        rm( list=things.to.zap, envir=assenv)
        for( thing in things.to.make.vis)
          assign( thing, ns[[ thing]], assenv)
      }
    } # if anything to assign
  } # loop over places where the funcs should be visible
}


"fixup.help" <-
function( nlocal=sys.parent()) mlocal({
  # Work out which Rd files are new
  # md5sum is incredibly fast for this!
  
  manpath <- file.path( spath, subdir, 'man')
  Rd.files <- dir( manpath, patt='[.]Rd$')
  new.Rd.info <- md5sum( file.path( manpath, Rd.files))
  names( new.Rd.info) <- Rd.files
  
  alias <- matrix( scan( file.path( ipath, 'help', 'AnIndex'), what='', sep='\t', quiet=TRUE), 
      ncol=2, byrow=TRUE)
  uaf <- unique( alias[,2])
  alias.files <- alias[,2] %&% '.Rd'
  alias <- alias[,1]

  # In theory, could check against the man/pkg.Rd.gz file in the installation
  # ...but no guaranteed unzip method available. Instead, use a from-last-time file. 
  
  Rd.info.file <- file.path( ipath, 'Meta', 'Rd.info.rda')      
  if( !force.all.docs && file.exists( Rd.info.file))
    load( Rd.info.file) # creates old.Rd.info
  else {
    old.Rd.info <- rep( -1, length( uaf))
    names( old.Rd.info) <- uaf %&% '.Rd'
  }
  
  zipped <- file.exists( file.path( ipath, 'help', 'Rhelp.zip'))

  new.files <- names( new.Rd.info) %except% names( old.Rd.info)
  changed.files <- names( new.Rd.info) %that.are.in% names( old.Rd.info)
  changed.files <- changed.files[ old.Rd.info[ changed.files] != new.Rd.info[ changed.files]]
  gone.files <- names( old.Rd.info) %except% names( new.Rd.info)

  # Fudge for R2.10 to get round parse_Rd/lazyLoad bugs: force re-parsing
  if( dynamic.help && !file.exists( file.path( ipath, 'help', 'patched'))) {
    new.files <- names( new.Rd.info)
    changed.files <- character( 0)
  }
  
  # Function to prepare shell & then run commands in it
  log <- character( 0)
  system2 <- function( commands, intern=TRUE, ...) {
    bf <- tempfile()
    on.exit( unlink( bf))
    if( .Platform$OS.type=='windows') {
      commands <- sub( '\\bR CMD\\b', 'RCMD', commands, perl=T)
      bf <- bf %&% '.bat'
    } else
      commands <- sub( '\\bRCMD\\b', 'R CMD', commands, perl=T)
    
    cat( option.or.default( 'rcmd.shell.setup', character( 0)), # 'CALL SET-R-BUILD-PATH.BAT'
        commands, sep='\n', file=bf) 
    log <<- c( log, system( bf, intern=intern, ...))
  }
  
  if( length( gone.files)) {
    fzap <- sub( '[.]Rd$', '', gone.files)
    if( !zipped)
      try( suppressWarnings( file.remove( file.path( ipath, 'help', fzap))), silent=TRUE)
    else 
      system2( 'zip -d ' %&% file.path( ipath, 'help', 'Rhelp.zip') %&% fzap)
    try( suppressWarnings( file.remove( file.path( ipath, 'html', fzap %&% '.html'))), silent=TRUE)
  }
  
  dealias.files <- c( changed.files, gone.files)
  if( length( dealias.files)) {
    alias <- alias[ alias.files %not.in% dealias.files]
    alias.files <- alias.files %except% dealias.files
    uaf <- unique( alias.files)
  }

  files.to.update <- c( new.files, changed.files)

  if( length( files.to.update)) {
    fnew <- sub( '[.]Rd$', '', files.to.update)
    full.fnew <- file.path( rpath, 'man', fnew %&% '.Rd')

    # from .build_Rd_db:    
    enco <- try( tools:::.get_package_metadata( ipath, FALSE)["Encoding"])
    if( (enco %is.a% 'try-error') || is.na(enco)) 
      enco <- "unknown"

    if( dynamic.help) {
      # Magic to avoid lazyLoad trouble:
      .Call("R_lazyLoadDBflush", file.path( ipath, 'help', pkg %&% '.rdb'), PACKAGE = "base")
      if( force.all.docs)
        file.remove( file.path( ipath, 'help', pkg %&% c( '.rdb', '.rdx')))
        
      # Next routine is smart about updating
      testo <- try( tools:::.install_package_Rd_objects( rpath, ipath, enco=enco)) 
      # ...try() should only be to trap any errors with user's own Rd files..?

      # Fastest to get aliases from parsed Rd
      .Call("R_lazyLoadDBflush", file.path( ipath, 'help', pkg %&% '.rdb'), PACKAGE = "base")
      Rdlist <- tools:::fetchRdDB( file.path( ipath, 'help', pkg))
      aliases <- lapply( Rdlist, tools:::.Rd_get_metadata, kind='alias') 
      alias <- unlist( aliases)
      alias.files <- rep( names( aliases), sapply( aliases, length))
      names( alias.files) <- alias
      .saveRDS( alias.files, file.path( ipath, 'help', 'aliases.rds'))
      
      tools:::.install_package_Rd_indices( rpath, ipath)
      tools:::.writePkgIndices( rpath, ipath)
    } else {
      text.fnew <- file.path( ipath, 'help', fnew)
      html.fnew <- file.path( ipath, 'html', fnew %&% '.html')
      Rd.fnew <- file.path( ipath, 'man', fnew %&% '.Rd')

      if( is.Rd2) { # if( exists( 'Rd2txt', mode='function'))  # assume Rd2HTML does, too
        for( i in seq_along( fnew)) {
          p1 <- try( tools:::prepare_Rd( full.fnew[i], encoding=enco, defines = .Platform$OS.type, 
                    stages = "install", warningCalls = FALSE))

          # p1 <- try( parse_Rd( full.fnew[i])) doesn't do macro subs
          if( p1 %is.a% 'try-error')
            warning( "Can't parse_Rd " %&% fnew[ i] %&% "; no help for this one")
          else {
            attr( p1, 'prepared') <- 3L # from .build_rd_db
            Rd2txt( p1, out=text.fnew[i], package=pkg)
            Rd2HTML( p1, out=html.fnew[i], package=pkg)
          } # parse_Rd OK
        } # for i
      } else { # Rdoc 1
        cat( 'Rdconv-ing ', length( fnew), ' Rd files, twice...\n')
        system2( 'RCMD Rdconv ' %&% c( 
            paste( '--package=', pkg, '-t=txt', '-o=' %&% text.fnew, full.fnew),
            paste( '-t=html', '-o=' %&% html.fnew, full.fnew)))

        if( .Platform$OS.type=='windows') { # ...then hack links
          # Endless work needed to do this right for pre-2.10, so just make all point here
          for( i.html in html.fnew) {
            reado <- readLines( i.html)
            reado <- gsub( '"[.][.]/[.][.]/[.][.]/doc/html/search/SearchObject.html[?]([^"]+)"', 
                '"\\1.html"', reado)
            cat( reado, sep='\n', file=i.html)
          }
        } # windows

        cat( '...done\n')
      } # which Rd version

      # ?Need to do something about 00index.html? (both versions)

      if( zipped) {
        system2( 'zip -j ' %&% file.path( ipath, 'help', 'Rhelp.zip') %&% ' ' %&% text.fnew)
        try( suppressWarnings( file.remove( text.fnew)), silent=TRUE)
      }

      # Alias info
      for( ifnew in fnew) {
        filio <- readLines( file.path( rpath, 'man', ifnew %&% '.Rd'))
        sections <- grep( '^[\\][A-Za-z0-9]+\\{', filio) # }
        namas <- sections[ grep( '^[\\](name|alias)[{]', filio[ sections])]
        not.namas <- c( sections %except% namas, Inf)
        namas <- namas[ namas < min( not.namas)]
        this.alias <- unique( sub( '.*[{]([^}]+)[}].*', '\\1', filio[ namas]))
        alias <- c( alias, this.alias)
        alias.files <- c( alias.files, rep( ifnew, length( this.alias)))
      }
    } # if not dynamic help

    # Index
    alias.files <- sub( '[.]Rd$', '', alias.files)
    cat( paste( alias, alias.files, sep='\t'), 
        sep='\n', file=file.path( ipath, 'help', 'AnIndex'))
  } # if length( files.to.update)
  
  
  # old.Rd.info
  old.Rd.info <- new.Rd.info
  save( old.Rd.info, file=Rd.info.file)
  
  # help.search index... TO DO I guess
})


"fixup.package.info" <-
function( nlocal=sys.parent()) mlocal({
#  # DESCRIPTION
#  inst.DESC <- readLines( file.path( ipath, 'DESCRIPTION'))
#  new.DESC <- readLines( file.path( rpath, 'DESCRIPTION'))
#  new.headers <- sub( ':.*', '', grep( '#[# ]+:', new.DESC, value=TRUE))
#  inst.headers <- sub( ':.*', '', grep( '#[# ]+:', inst.DESC, value=TRUE))
#  extra.inst.headers <- inst.headers %except% new.headers
#  inst.colons <- grep( '#[# ]+:', inst.DESC)
#  extra.inst.colons <- grep( '#(' %&% paste( extra.inst.headers, collapse='|') %&% '):', inst.DESC)
#  keep.extra <- inst.colons[ findInterval( seq_along( inst.DESC), inst.colons)] %in% extra.inst.colons
#  
#  # Stuff for Meta description below
#  condense.extra <- sub( '#([# ])', '\n', inst.DESC[ keep.extra])
#  condense.extra <- sub( '# +', ' ', condense.extra)
#  condense.extra <- splitstr( paste( condense.extra, collapse=''), '\n')[[1]]
#  full.DESC <- c( new.DESC, condense.extra)
#  
#  # Actual installed DESCRIPTION file
#  new.DESC <- c( strwrap( new.DESC, 72, exdent=8), inst.DESC[ keep.extra])
#  cat( new.DESC, file=file.path( ipath, 'DESCRIPTION'), sep='\n')
#
#  # Stuff in Meta directory-- for now, only description and package-dependency info
#  meta <- .readRDS( file.path( ipath, 'Meta', 'package.rds'))
#  meta$DESCRIPTION <- full.DESC
#  
#  for( field in cq( Depends, Imports, Suggests)) {
#    packs <- splitstr( sub( '#[# ]+: *', '', grep( '#' %&% field %&% ':', full.DESC, value=TRUE)), 
#        ',')[[1]]
#    ver.packs <- grep( '(', packs, fixed=TRUE, value=TRUE)
#    packs <- packs %except% ver.packs
#  }
#    
    
  
  
  

})


"flatdoc" <-
function( EOF="<<end of doc>>") { 
  doctext <- readLines.mvb( current.source(), EOF=EOF)
  class( doctext) <- 'docattr'
  doctext
}


"foodweb" <-
function( funs, where=1, charlim=80, prune=character(0), rprune, ancestors=TRUE, descendents=TRUE,
    plotting=TRUE, plotmath=FALSE,
    generics=c( 'c','print','plot', '['), lwd=0.5, xblank=0.18,
    border='transparent', boxcolor='white', textcolor='black', color.lines=TRUE, highlight='red', ...) {
  oldpar <- par( ..., no.readonly=TRUE)
  on.exit( par( oldpar))

  charlim <- charlim/par('cex')
  par( lwd=lwd) # lwd included as a parameter, in case this screws up

  skip.computations <- FALSE
  if( missing( funs)) {
    if( is.environment( where))
      where <- list( where)
    funs <- unique( unlist( lapply( where, find.funs)))
  } else if( funs %is.a% 'foodweb') { # basically redisplay
    skip.computations <- TRUE
    extract.named( funs)
    funs <- names( level)
    n <- length(level) }

  if( !skip.computations) {
    if( !missing( rprune))
      prune <- funs %matching% rprune
    funs <- unique( c( funs, prune))

    if( !length( funs))
return( structure( list( funmat=matrix( 0,0,0), x=numeric( 0), level=numeric( 0)),
    class='foodweb'))

    find.web()
    organize.web.display( plotmath=plotmath) }

  answer <- list( funmat=funmat, x=x, level=level)
  class( answer) <- 'foodweb'

  if( plotting)
    plot( answer, border=border, boxcolor=boxcolor, xblank=xblank, textcolor=textcolor,
        color.lines=color.lines, plotmath=plotmath, ...)
  invisible( answer)
}


"force.assign" <-
function( x, value, envir) {
  envir <- as.environment( envir)
  if( bl <- exists( x, envir, inherits=FALSE) && bindingIsLocked( x, envir))
    unlockBinding( x, envir)
  assign( x, value, envir=envir)
  if( bl)
    lockBinding( x, envir)
}


"from.here" <-
function( EOF=as.character( NA)) {
  f1 <- tempfile()
#  cat( 'FILENAME: ', f1, '\n')
  cat( readLines.mvb( current.source(), EOF=EOF), file=f1, sep='\n')
  c1 <- file( f1)
  class( c1) <- c( 'selfdeleting.file', class( c1))
  c1
}


"full.path" <-
function( path, start='.'){
  spath <- strsplit( path, '/', fixed=TRUE)[[1]]
  if( spath[1] %in% c( '.', '..'))
    path <- file.path( start, path)

  # Eliminate . and ..

  spath <- strsplit( path, '/')[[1]]
  spath <- spath %except% '.'

  while( !is.na( first.parent <- index( spath == '..')[1]))
    spath <- spath[ -( first.parent + -1:0)]

  paste( spath, collapse='/')
}


"get.backup" <-
function( name, where=1, rev=TRUE, zap.name=TRUE, unlength=TRUE) {
  bdd <- get.path.from.where( where)
    
  if( !is.dir( bdd)) {
    warning( "Can't find backup directory")
return() }

  filename <- get.bkfile( name, bdd, create = FALSE)
  if( !nchar( filename)) {
    warning( "Can't find backup file")
return() }

  # Zap warnings about unterminated lines
  ow <- options(warn = -1); on.exit( options( ow))
  bu <- readLines(filename); options(ow); on.exit()

  if( !length( bu)) {
    warning( "Nothing in the backup file")
return()
  }

  nonblanks <- regexpr( '[^ ]', c( bu, 'x'))>0
  bu <- bu[ min( index( nonblanks)) %upto% length( bu)]

  # Next line must match 'get.bkfile'
  infeasible.R.line <- "'\"@\"@'@ START OF BACKUP @'@\"@\"'"

  line.breaks <- bu == infeasible.R.line
  if( !sum(line.breaks)) {
    warning( "No marker lines in the backup file")
return()
  }
    
  bu <- split( bu, cumsum( line.breaks))
  bu <- lapply( bu, '[', -(1:2))
  if( zap.name) {
    zap.name.function <- function( x) {
        x[ 1] <- sub( '"[^"]*" <- *', '', x[ 1])
        x
      }
    bu <- lapply( bu, zap.name.function)
  }
  
  # Character object backups are preceded by one line giving the length of the object. Remove.
  if( unlength) 
    bu <- lapply( bu, function( x) {
      l <- suppressWarnings( as.numeric( x[1]))
      if( !is.na( l) && length( x)==l+1)
        x <- x[-1]
      return( x)
    })
  
  if( rev)
    bu <- rev( bu)
  
  bu  
}


"get.bkfile" <-
function (name, bkdir, create = FALSE) 
{
    fob <- read.bkind(bkdir)
    i <- match(name, fob$object.names)
    if (is.na(i)) {
        if (!create) 
            return("")
        file.nums <- as.integer(unlist(strsplit(fob$files, "BU", fixed=TRUE)))
        n <- min(1:(length(file.nums) + 1) %except% file.nums)
        filename <- "BU" %&% n
        fob$files <- c(fob$files, filename)
        fob$object.names <- c(fob$object.names, name)
        cat(paste(fob$files, fob$object.names, sep = "="), sep = "\n", 
            file = file.path(bkdir, ".Backup.mvb", "index"))
    }
    else filename <- fob$files[i]
    filename <- file.path(bkdir, ".Backup.mvb", filename)
    if (!file.exists(filename)) 
        file.create(filename)
    filename
}


"get.cd.from.menu" <-
function() {
  if(!exists( "tasks", where=1, inherits=FALSE))
    tasks <- structure( character(0), names=character(0)) # avoid sort complaining about names

  catstop <- function() {
      cat( 'No ')
stop( 'merely quitting cd', call.=FALSE) 
    }    
    
  line.end <- ifelse( option.or.default( 'cd.extra.CR', FALSE), '\n', '')
  
  can.go.up <- ifelse( length( .Path) > 1, 1, 0)
  to <- menu( c( sort(names(tasks)), if( can.go.up) '..' else NULL, 
      "CREATE NEW TASK"), graphics = !is.null(getOption('gui')), title = "Task menu")
  if(to == 0) 
catstop()

  if(to == 1 + can.go.up +length(tasks)) {
    cat( "Name of new task (ENTER to quit): " %&% line.end)
    to <- readline()
    if(to=="")
catstop() } 
  else if( to > length( tasks))
    to <- '..'
  else
    to <- sort( names(tasks))[to]

return( parse( text=to)[[1]])
}


"get.info.for.mcache" <-
function( x, envir, name=TRUE) {
  if( name)
    x <- envir[[ x]]
  lapply( named( cq( mode, class, dim, length, object.size)),
      function( f) get(f)(x))
}


"get.mcache.reffun" <-
function( whati, envir) {
  # Must avoid name clash between 'whati' and internal vars of fx
  fx <- function( x) NULL
  body( fx) <- substitute(
      if( missing( x))
        qwhati
      else {
        mc <- attr( envir, 'mcache')
        mci <- as.list( attr( mc, 'info'))
        mc[ whati] <- -abs( mc[ whati]) # signal a change
        mci[[ whati]] <- get.info.for.mcache( x, name=FALSE)
        attr( mc, 'info') <- mci
        oldClass( mc) <- 'nullprint'
        attr( envir, 'mcache') <- mc
        qwhati <<- x
    }, list( whati=whati, qwhati=as.name( whati), x=as.name( 'x' %&% whati),
        mc=as.name( 'mc' %&% whati)))
  names( formals( fx)) <- 'x' %&% whati

  e <- new.env( parent=asNamespace( 'mvbutils'))
  e$envir <- envir # doesn't work if I sub envir directly into body( fx)
  environment( fx) <- e
  fx
}


"get.mcache.store.name" <-
function( envir) {
  lsnc <- lsall( envir=envir, patt='^\\.mcache[0-9]+')
  if( !length( lsnc))
    cache.name <- '.mcache0'
  else
    cache.name <- lsnc[ order( nchar( lsnc), decreasing=TRUE)[1]]
  cache.name
}


"get.new.file.numbers" <-
function( derefs, file.numbers) {
  had.numbers <- derefs %such.that% (. %in% names( file.numbers))
  file.numbers <- file.numbers %without.name% derefs
  derefs <- derefs %except% had.numbers
  new.file.numbers <- (1 %upto% max( file.numbers)) %except% file.numbers
  new.file.numbers <- c( new.file.numbers, max( c( 0, file.numbers)) +
      1 %upto% (length( derefs)-length(new.file.numbers)))[ 1:length(derefs)]
  names( new.file.numbers) <- derefs
  new.file.numbers
}


"get.path.from.where" <-
function( where){
  if( is.character( where) && is.dir( where))
return( where)

  if( !is.environment( where)) {
    if( length( where) != 1)
stop( "'where' should be length 1")

    where <- named( search())[ where] # to character
    where <- index( search()==where) # to numeric
    if( !is.numeric( where) || is.na( where))
  stop( "'where'?")

    pfw <- file.path(attr(pos.to.env(where), "path"))
  } else 
    pfw <- attr( where, 'path')
return( pfw)
}


"get.path.list" <-
function () 
{
    path.list <- search()
    apfun <- function(x) {
        x <- attr(pos.to.env(x), "path")
        if (!is.null(x)) 
            x <- names(x)[1]
        if (is.null(x)) 
            x <- ""
        x
    }
    ap <- sapply(seq(path.list), apfun)
    path.list[nchar(ap) > 0] <- ap[nchar(ap) > 0]
    path.list
}


"get.ref.info" <-
function( envo, nlocal=sys.parent()) mlocal({
  if( is.null( cache <- attr( envo, 'cache')))
    attr( envo, 'cache') <- cache <- new.env( hash=TRUE, envo)
  lscache <- lsall( cache)
  refs <- derefs <- promises <- character(0)
  file.numbers <- numeric( 0)
  if( length( lscache)) {
    refs <- names( which( unlist( eapply( envo, inherits, 'mref'))))
    derefs <- lscache %that.are.in% (lsall( envo) %except% refs)
    prom.func <- function( x) {cache[[x]] %is.a% 'promise'}
    promises <- names( which( sapply( lscache, prom.func)))
    fnum.func <- function( x) unclass( envo[[ x]])$nfile
    if( length( refs))
      file.numbers <- sapply( refs, fnum.func)
  }
})


"get.S3.generics" <-
function( pack, ns=TRUE){
  if( ns) {
    packname <- pack
    pack <- asNamespace( pack)
    meths <- lsall( pack$.__S3MethodsTable__.) 
  } else {
    packname <- '' # nameless
    meths <- find.funs( pack)
  }
    
  prefixes <- character( 0)
  for( imeth in meths) {
    spl <- clip( strsplit( imeth, '.', fixed=TRUE)[[1]])
    prefixes <- c( prefixes, sapply( 1 %upto% length( spl), 
        function( x) paste( spl[ 1:x], collapse='.')))
  }

  packgens <- unique( prefixes %that.are.in% find.funs( pack))
  packgens <- packgens[ unlist( lapply( packgens, 
      function( f) 'UseMethod' %in% all.names( body( pack[[f]]))))]
  structure( rep( packname, length( packgens)), names=packgens)
}


"group" <-
function( m, ...) {
  l <- list( ...)
  if( length( l)==1 && is.list( l))
    l <- l[[ 1]]
  rep( names( l), sapply( l, length))[ match( m, unlist( l), NA)]
}


"hack" <-
function( fun, ...){
  if( is.character( fun))
    fun <- get( fun)
  mc <- match.call( expand.dots=FALSE)$...
  for( i in names( mc))
    formals( fun)[[ i]] <- mc[[ i]]
  fun
}


"hack.importIntoEnv" <-
function () { # impenv, impnames, expenv, expnames) {
  le <- if( exists( 'base.importIntoEnv', where='mvb.session.info', inherits=FALSE))
        get( 'base.importIntoEnv', 'mvb.session.info')
      else
        base:::importIntoEnv
        
  subbo <- substitute(
      {
        if( environmentName( expenv) %in% names( maintained.packages)) {
          for( i in seq_along( impnames))
            do.call( 'delayedAssign', list( x=impnames[ i], value=call( 'get', expnames[i]), 
                eval.env=expenv, assign.env=impenv))
      
         # Version that uses active binding into private env that inherits from expenv: 
         # ...equivalent, but more complicated
#     
#          for( i in seq_along( impnames)) {
#            # eval needed, else it's a call to 'function'
#            get.fun <- eval( substitute( 
#                function( v) 
#                  if( !missing( v)) # put copy into my private environment
#                    assign( char.fname, v, envir=environment( sys.function()))
#                  else fname, # from private env if previously put there, else from export
#                list( fname=as.name( expnames[i]), char.fname=expnames[i]))) 
#            e <- new.env( parent=expenv)
#            environment( get.fun) <- e
#            if( locko <- (impnames[i] %in% lsall( impenv)) && bindingIsLocked( impnames[i], where))
#                unlockBinding( impnames[i], impenv)
#            makeActiveBinding( impnames[i], get.fun, impenv)
#            if( locko)
#              lockBinding( impnames[i], impenv)
#          }
        } else
          default
      }, list( default=body( le)))
  body( le) <- subbo
  environment( le) <- asNamespace( 'mvbutils')
  le
}


"hack.lockEnvironment" <-
function(){
  le <- if( exists( 'base.lockEnvironment', where='mvb.session.info', inherits=FALSE))
        get( 'base.lockEnvironment', 'mvb.session.info')
      else
        base:::lockEnvironment
  subbo <- substitute(
      { 
      #cat( 'Checking '); print( env)
      #cat( '  '); print( exists( '.__NAMESPACE__.', env, mode='environment', inherits=FALSE))
      #cat( '  '); print( exists( '.packageName', env, mode='character', inherits=FALSE))
      #cat( '  '); print( sum( match( env$.packageName, names( maintained.packages), 0)))
      is.mp.ns <- exists( '.__NAMESPACE__.', env, mode='environment', inherits=FALSE) &&
            exists( '.packageName', env, mode='character', inherits=FALSE) &&
            sum( match( env$.packageName, names( maintained.packages), 0))
        if( is.mp.ns || any( sapply( dont.lock.envs, identical, y=env)) ||
            !is.null( attr( env, 'dont.lock.me')) ||
            sum( match( attr( env, 'name'), dont.lock.envnames, 0))) {
          # cat( "Not locking\n") 
          if( is.mp.ns) {
            dont.lock.envnames <<- c( dont.lock.envnames, 'package:' %&% env$.packageName)
            dont.lock.envs <<- c( dont.lock.envs, structure( list( parent.env( env)),
                names='imports:' %&% env$.packageName))
          }
        } else
          default
      }, list( default=body( le)))
  body( le) <- subbo
  environment( le) <- asNamespace( 'mvbutils')
  le
}


"has.source" <-
function( x) is.function( x) || !is.null( attr( x, 'source'))


"help2flatdoc" <-
function( fun.name, pkg=NULL, text=NULL){
  if( is.null( text)) {
    libpath <- dirname( .find.package( pkg))
    
    if( getRversion() >= "2.10") {
      al <- .readRDS( file.path( libpath, pkg, 'help', 'aliases.rds'))
      hfilename <- al[ fun.name]
      p1 <- tools:::fetchRdDB( file.path( libpath, pkg, 'help', pkg), hfilename)
      t1 <- tempfile()
      Rd2txt( p1, t1)
      text <- readLines( t1)
      unlink( t1)    
    } else {
      # Get 'help' to create text, via fake 'pager' function
      text <- character( 0)
      repager <- function( file, header, title, delete.file) {
        text <<- readLines( file)
        if( delete.file)
          unlink( file)
      }
    # Now 'print' will invoke 'repager'       

    # Non-standard treatment of 'package' arg in 'help' requires the following hack:
      eval( substitute( 
          print( help( fun.name, package=pkg, htmlhelp=FALSE, chmhelp=FALSE, pager=repager, 
              lib.loc=libpath)),
          list( pkg=pkg)))
    }
    if( !length( text))
stop( "No help found for " %&% fun.name)
  }

  # cat( length( text), 'lines of help read; class=', class( text), '\n')
  otext <- text
  text <- c( text, '')
  text <- gsub( '[' %&% sQuote( '') %&% ']', "'", text)
  text <- gsub( '[' %&% dQuote( '') %&% ']', '"', text)

  is.heading <- regexpr( '^_\b', text) > 0 & regexpr( ':$', text) > 0
  text <- gsub( '_\b', '', text)
  text[ is.heading] <- upper.case( substring( text[ is.heading], 1, nchar( text[ is.heading])-1))

  # Trim leading spaces, but only as far as the indent in DESCRIPTION
  is.descrip <- index( text=='DESCRIPTION')[1]
  is.normal.line <- grep( '^ *[^ ]', text) 
  descrip.text.1 <- min( is.normal.line %such.that% (. > is.descrip))
  def.indent <- sub( '[^ ].*', '', text[ descrip.text.1])
  text <- gsub( '^' %&% def.indent, '', text)
  # old brutal version: text <- gsub( '^ +', '', text)

  text[ is.heading] <- gsub( ' +', '.', text[ is.heading])
  expando <- rep( seq( along=text), 1+is.heading)
  text <- text[ expando]
  is.heading <- is.heading[ expando]
  zappo <- 1+index( diff( is.heading)==1)
  is.heading[ zappo] <- FALSE
  text[ zappo] <- ''

  nc <- nchar( text)
  nc.next <- c( nc[-1], 0)
  nc.prev <- c( 0, clip( nc))
  is.heading <- is.heading & nc>0

  myhead <- c( '', text[ is.heading])[ 1+cumsum( is.heading)]

  is.argdef <- myhead=='ARGUMENTS' & nc>0 & nc.prev==0 & regexpr( '^[^ ]+: ', text)>0
  text[ is.argdef] <- ' ' %&% text[ is.argdef]
  start.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc.prev==0 & nc>0 & nc.next>0
  mid.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc>0 & nc.prev>0
  end.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc.prev >0 & nc>0 & nc.next==0
  splitto <- split( text[ start.cont | mid.cont], cumsum( start.cont)[ start.cont | mid.cont])
  text[ start.cont] <- sapply( splitto, paste, collapse=' ')

  # Aliasses: all functions named in USAGE
  alias.lines <- text[ myhead=='USAGE' & !mid.cont]
  alias.lines1 <- grep( "^[a-zA-Z0-9._]+\\(", alias.lines, value=TRUE)
  alias.funs <- sub( '^ *([a-zA-Z0-9._]+)\\(.*', '\\1', alias.lines1)
  alias.lines2 <- grep( "^[^(]+%[a-zA-Z0-9_.]+%", alias.lines, value=TRUE)
  alias.ops <- sub( "^[^(]+(%[a-zA-Z0-9_.]+%).*", '\\1', alias.lines2)
  aliasses <- unique( c( alias.funs, alias.ops)) %except% sub( ' .*', '', text[1])

  text <- c( sub( ' +R Documentation *$', '', text[1]), aliasses, text[ !mid.cont][-1])

  class( text) <- 'cat'
  text
}


"hook.set.already" <-
function( pkg, hook.type, f, action=cq( append, prepend, replace)){
  identical.to.f <- function( x) {
    y <- x
    attr( y, '.Environment') <- NULL
    identical( y, f) }
  mangle <- packageEvent( pkg, hook.type)
  hooks <- getHook( mangle)
  if( !any( sapply( hooks, identical.to.f))) {
    action <- match.arg( action)
    setHook( mangle, f, action)
  }
}


"Hours" <-
function( x){
  Minutes( 60*x)
}


"index" <-
function (lvector) 
seq_along( lvector)[lvector]


"install.proged" <-
function( option.name='program.editor') {
  readonly <- ifelse( option.name=='program.reader', 'in read-only mode', '')
  cat( 'Must set up program editor information before "fixr" works.')
  repeat {
    cat( '\nType whatever you\'d type in a command window to',
      'invoke your editor', readonly, 'on a file called "myfun.r".',
      '  For example, on Unix-like systems: myedit myfun.r &',
      '  In Windows, use double quotes around a path if it contains spaces,',
      '  and use \\ not \\\\ or / as the separator;',
      'to find the path, look at Properties/Shortcut/Target of the icon or shortcut.',
      '  Otherwise, type <ENTER> to quit: ', sep='\n')
    pe.path <- readline()
    if( !nchar( pe.path))
return()
    if( length( grep( 'myfun\\.r', pe.path))==1)
  break
  }
  
  pe.path <- strsplit( pe.path, 'myfun.r', fixed=TRUE)[[1]]
  if( length( pe.path)==1)
    pe.path <- c( pe.path, '')
  
  pe <- substitute( function( name, fname) paste( path1, fname, path2, sep=''), 
      list( path1=pe.path[1], path2=pe.path[2]))
      
  edit.scratchdir <- Sys.getenv( 'TEMP')
  if( !nchar( edit.scratchdir))
    edit.scratchdir <- Sys.getenv( 'TMP')
  repeat{ 
    cat( 'Enter directory for scratch files (single backslashes only in Windows)')
    if( nchar( edit.scratchdir))
      cat( 'or <ENTER> for', edit.scratchdir)
    cat( ': ')
    check <- readline()
    if( nchar( check))
      edit.scratchdir <- check
      
    if( !is.dir( edit.scratchdir))
      mkdir( edit.scratchdir)
    if( is.dir( edit.scratchdir))
  break
    cat( "Can't create directory", edit.scratchdir, "!")
  }

  edit.scratchdir <- as.vector( edit.scratchdir)[1] 
  
  if( option.name=='program.editor') {
    backup.fix <- NULL # don't backup by default
    repeat{
      cat( 'Automatic backups #1: how many backups per session (0 for no backups)? ')
      n.per.session <- as.integer( readline())
      if( is.na( n.per.session) || n.per.session < 0)
    next
      if( n.per.session==0)
    break
      cat( 'Automatic backups #2: how many sessions to keep last version from? ')
      n.sessions <- as.integer( readline())
      if( is.na( n.sessions) || n.sessions<0)
    next
      backup.fix <- c( n.sessions, n.per.session)
    break
    }

    o <- substitute( options( program.editor=pe, edit.scratchdir=edit.scratchdir, backup.fix=backup.fix))
  } else
    o <- substitute( options( program.reader=pe))
  eval( o)
  
  cat( 'You should use "fixr" to make sure that the following appears in your .First:',
    deparse( o), 'autoedit( TRUE)', '', sep='\n')
    
  options()[[ option.name]]
}


"internal.copy.ns.objects" <-
function( pkgname, pkgpath){
  senv <- as.environment( 'package:' %&% pkgname)
  ns <- asNamespace( pkgname)
  f <- function( val) blah-blah-blah
  environment( f) <- ns
  print( objects)
  for( x in objects) {
    body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
    makeActiveBinding( x, f, senv)
  }
}


"is.dir" <-
function (dir) 
{
    ok <- file.exists(dir)
    ok[ok] <- file.info(dir[ok])$isdir
    ok
}


"is.nonzero" <-
function (x) 
{
    val <- FALSE
    if (length(x) == 1) {
        if (is.character(x) || is.factor(x)) 
            val <- pmatch(x, "FALSE", 0) == 0
        else if (is.logical(x)) 
            val <- x
        else if (is.numeric(x)) 
            val <- x != 0
    }
    val
}


"lazify" <-
function( path, package, pkgpath) {
# Taken from tools:::makeLazyLoading
  e <- new.env( hash=TRUE)
  load( path, e)
  file.remove( path)
  tools:::makeLazyLoadDB( e, file.path( dirname( path), package), compress=TRUE)

  # Next line to avoid use of bad cache if reloaded:
  .Call("R_lazyLoadDBflush", file.path( dirname( path), package %&% '.rdb'), PACKAGE = "base")

  loaderFile <- file.path( R.home(), "share", "R",
      ( if( packageHasNamespace( package, dirname( pkgpath))) 'ns') %&% "packloader.R")
  file.copy( loaderFile, file.path( dirname( path), package), TRUE)
}


"least.mutual.dependency" <-
function (funmat, funs, level) 
{
    group <- funmat[level == 0, level == 0, drop = FALSE]
    mode(group) <- "logical"
    old.group <- group & FALSE
    while (any(group != old.group)) {
        old.group <- group
        for (i in funs[level == 0]) {
            newbies <- group[, group[, i], drop = FALSE] %*% 
                rep(1, sum(group[, i]))
            group[, i] <- group[, i] | (newbies > 0)
        }
    }
    nn <- sum(level == 0)
    keep <- c(TRUE, rep(FALSE, nn - 1))
    for (i in 2:nn) {
        old.group <- matrix(as.vector(group[, i]) == as.vector(group[, 
            keep]), nrow = nn)
        keep[i] <- !any(rep(1, nn) %*% old.group == nn)
    }
    group <- group[, keep, drop = FALSE]
    if (ncol(group) > 1) {
        nn <- ncol(group)
        old.group <- matrix(0, nn, nn)
        for (i in 1:nn) for (j in (1:nn)[1:nn != i]) {
            old.group[i, j] <- set.test(group[, i], group[, j])
            old.group[j, i] <- -old.group[i, j]
        }
        old.group[old.group < 0] <- 0
        not.keep <- old.group %*% rep(1, nn) > 0
        group <- group[, !not.keep, drop = FALSE]
    }
    group <- dimnames(group)[[1]][apply(group, 1, any)]
    match(group, funs[level == 0])
}


"legal.filename" <-
function (name) 
{
    length.limit <- 250
    filenames <- strsplit(substr(name, 1, length.limit), "")[[1]]
    filenames[filenames %in% c(":", "*", "?", "'", "/", "\\", 
        "\"", ">", "<", '+', ' ')] <- "."
    if (!(upper.case(filenames[1]) %in% LETTERS)) 
        filenames <- c("X", filenames)
    paste(filenames, collapse = "")
}


"lib.pos" <-
function() as.vector( 1+rev( search.task.trees())[1]) # not needed now


"load.live.package" <-
function( pkgname, path){
  e <- new.env( parent=if( exists( 'emptyenv', mode='function')) emptyenv() else baseenv())
      attr( e, 'path') <- structure( path, names=pkgname)
      attr( e, 'name') <- pkgname
      load.refdb( envir=e)
  live.packages[[ pkgname]] <<- e # into 'mvb.session.info' I hope
}


"load.maintained.package" <-
function( name, path, task.tree, autopatch=TRUE){
  e <- new.env( parent=if( exists( 'emptyenv', mode='function')) emptyenv() else baseenv())
  attr( e, 'path') <- structure( path, names=name)
  attr( e, 'name') <- name
  attr( e, 'task.tree') <- task.tree

  # Am putting this before the load, in case the latter triggers namespacing...
  # ... dunno why that should happen, but it does
  maintained.packages[[ name]] <<- e 
  assign( '..' %&% name, e, as.environment( 'mvb.session.info')) # alias for ease of access
  tryo <- try( load.refdb( envir=e))
  if( (tryo %is.a% 'try-error') || !length( lsall( e))) {
    warning( "No package '" %&% name %&% "' found in 'maintain.packages'")
    rm( e)
    rm( '..' %&% name, as.environment( mvb.session.info))
    maintained.packages <<- maintained.packages %without.name% name
  }
  
  if( exists( '.onLoad', e, mode='function')) {
    setHook( packageEvent( name, 'onLoad'), no.lazyLoad.hook, 'prepend')
    setHook( packageEvent( name, 'attach'), no.lazyLoad.attach.hook, 'prepend')
  }
}


"load.mvb" <-
function (filename, name, pos, attach.new=is.null( envir) && pos != 1, 
    path=attr( envir, 'path'), envir=NULL, ...) {
  if (attach.new)
    envir <- attach(NULL, pos = pos, name = name)
  else {
    if( is.null( envir))
      envir <- as.environment(pos)
    attr( envir, "name") <- name
  }

# This stuff used to be after the load, but load.refdb needs the path attr set

  if( tail( splitto <- strsplit( filename, '.', fixed=TRUE)[[1]], 1)=='rdb') {
    .Call("R_lazyLoadDBflush", paste( clip( splitto), collapse='.') %&% '.rdb', PACKAGE = "base")
    lazyLoad( paste( clip( splitto), collapse='.'), envir=envir)
  } else
    load.refdb(filename, env = envir, fpath=path)

  attr( envir, 'path') <- path
  ll <- list(...)
  if (length(ll))
    for (attro in names(ll)) attr(envir, attro) <- ll[[attro]]
}


"load.refdb" <-
function( file=file.path( fpath, '.RData'), envir, fpath=attr( envir, 'path')) {
  envir <- as.environment( envir)
  if( !file.exists( file))
return()

  load( file, envir)
  setup.mcache( envir, fpath)

  invisible( lsall( envir))
}


"local.on.exit" <-
function( expr, add=FALSE) { 
# Assigns expr to on.exit.code in mlocal manager. See local.return for explanation of 'where'
# Don't know what should "really" happen if expr is missing but add is TRUE

  subex <- if( missing( expr)) NULL else substitute( expr)
  where <- get( 'enclos', envir=parent.frame(2))
  if( add) {
    oldex <- get( 'on.exit.code', where)
    subex <- substitute( { oldex; subex }, returnList( oldex, subex))
  }
    
  assign( 'on.exit.code', subex, envir=where)
}


"local.return" <-
function( ...) { # Returns its arguments; unnamed arguments are named using deparse & substitute
  orig.mc <- mc <- as.list( match.call())[ -1]

  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=parent.frame())
    else { # multiple arguments, so return as named list
      if( is.null( names( mc)))
        which <- 1:length( mc)
      else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=parent.frame())
    }
  }

# R version. This uses a trick: the call to "eval" that invokes the mlocalized routine 
# containing this call to "local.return", sets up a frame with 3 args including "enclos" 
# which is actually ignored. However I deliberately set this argument in the final call 
# to "eval" inside "mlocal", so that "local.return" knows where to put the answer. This
# is probably dependent on a quirk of implementation.
# The need to do this at all, is that loops terminated with a "break" in R _don't_ have
# the value of the last expression before the break. They do in S.

  assign( 'override.answer', mc, envir=get( 'enclos', envir=parent.frame(2)))
}


"localfuncs" <-
function( funcs) {
  pf <- parent.frame()
  funcs <- lapply( named( funcs), get, envir=pf, inherits=TRUE)
  for( i in names( funcs)) {
    f <- funcs[[ i]]
    environment( f) <- pf
    assign( i, f, envir=pf)
  }
  invisible( NULL)
}


"lsall" <-
function( ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$all.names <- TRUE
  mc[[1]] <- as.name( 'ls')
  eval( mc, parent.frame())
}


"lsize" <-
function( envir=.GlobalEnv){
  envir <- as.environment( envir)
  mcache <- attr( envir, 'mcache')
  mcs <- names( mcache)

  if( length( mcache)) {
    mcfiles <- file.path( attr( envir, 'path'), 'mlazy', 'obj' %&% abs( mcache) %&% '.rda')
    mcsize <- file.info( mcfiles)$size
    names( mcsize) <- mcs
  } else
    mcsize <- numeric( 0)

  obs <- lsall( envir=envir) %except% mcs
  if( length( obs))
    obsize <- sapply( named( obs), function( x) object.size( envir[[x]]))
  else
    obsize <- numeric( 0)
return( sort( c( obsize, mcsize)))
}


"maintain.packages" <-
function( ..., character.only=FALSE){
  if( character.only)
    packs <- unlist( list(...))
  else {
    mc <- as.list( match.call( expand.dots=FALSE)$...)
    packs <- sapply( mc, as.character)
  }

  # Don't reload
  # packs <- packs %such.that% (. %not.in% names( maintained.packages))
  packs <- packs %except% names( maintained.packages)

  # Can't be retrospective
  if( length( packs) && (
      any( already <- packs %in% loadedNamespaces() || 
      !is.na( match( 'package:' %&% packs, search()))))) {
    cat( "Can't maintain package(s) {", paste( packs[ already], collapse=','),
        "}: already loaded!\n")
    packs <- packs[ !already]
  }
  
  # Can't be cd'ed into or below the 
  if( length( packs) && (
      any( already <- packs %in% names( .Path)))) {
    cat( "Can't maintain package(s) {", paste( packs[ already], collapse=','),
        "}: already cd'ed into\n")
    packs <- packs[ !already]
  }

  if( length( packs)) {
    snames <- lapply( seq( along=search()), function( x) names( attr( pos.to.env( x), 'path'))[1])
    snames[ sapply( snames, is.null)] <- ''
    snames <- unlist( snames)
    snames <- match( rev( names( .Path)), snames)
    tasks <- lapply( snames, function( x)
        if( exists( 'tasks', pos.to.env( x), mode='character')) pos.to.env( x)$tasks else character( 0))

    owner <- match( packs, names( unlist( tasks)), 0)
    packs <- packs[ owner>0]
    owner <- rep( 1:length( tasks), sapply( tasks, length))[ owner[ owner>0]]

    # Need fully qualified path name, using path attr + task thing which might be relative
    for( ipkg in seq( along=packs)) {
      pe <- pos.to.env( snames[ owner[ ipkg]])
      task.tree <- c( names( .Path)[ 1:(length( snames) + 1 - owner[ ipkg])], packs[ ipkg])
      load.maintained.package( packs[ ipkg], full.path( pe$tasks[ packs[ ipkg]], attr( pe, 'path')),
        task.tree)
    } # for ipkg
  } # if length( packs)

return( names( maintained.packages))
}


"make.arguments.section" <-
function( funs=find.funs( env) %except% find.documented( env, doctype='Rd'), file=stdout(),
    env=.GlobalEnv) {
  arguments <- function( x) {
      ax <- names( formals( env[[ x]]))
      if( length( ax))
        ' ' %&% ax %&% ': (' %&% x %&% ')'
      else
        character( 0)
    }
 funs <- unlist( lapply( funs, arguments))
 if( !is.null( file))
   cat( funs, sep='\n', file=file)
 invisible( funs)
}


"make.internal.doc" <-
function( funs, package) {
  if( !length( funs))
return( character( 0))

  # xfuns is to cope with operators,
  # whose names start with %. This is interpreted as a "don't-show-rest-of-line"
  # by the standard flatdoc system, and is removed by 'doc2Rd'.
  # So we need to add an extra % symbol.
  xfuns <- ifelse( regexpr( '^%', funs)>0, '%', '') %&% funs
  text <- c( "PACKAGE-internal package:PACKAGE\n",
      xfuns, "
Internal functions for PACKAGE

DESCRIPTION

Internal functions for 'PACKAGE', not meant to be called directly.


USAGE

",	make.usage.section( funs, NULL), "

KEYWORDS

internal
")
	text <- gsub( 'PACKAGE', to.regexpr( package), text)
	unlist( strsplit( text, '\n'))
}


"make.NAMESPACE" <-
function( env=1, path=attr( env, 'path'),
    description=read.dcf( file.path( path, 'DESCRIPTION'))[1,], more.exports=character( 0)) {
  env <- as.environment( env)
  import <- paste( description[ 'Depends'], description[ 'Imports'], sep=',')
  import <- gsub( '\\([^)]*\\)', '', import)
  import <- gsub( ' *', '', import)
  import <- strsplit( import, ',')[[1]]
  import <- sub( '[<>].*', '', import)
  import <- unique( import %except% c( 'R', 'NA'))
  
  # Eliminate non-NAMESPACE packages
  has.NAMESPACE <- rep( NA, length( import))
  names( has.NAMESPACE) <- import
  for( lp in .libPaths()) {
    packs <- .packages( T, lp)
    new.imps.here <- import[ is.na( has.NAMESPACE) & (import %in% packs)]
    if( length( new.imps.here))
      has.NAMESPACE[ new.imps.here] <- packageHasNamespace( new.imps.here, lp)
    if( !any( is.na( has.NAMESPACE)))
  break
  }
  if( any( is.na( has.NAMESPACE)))
stop( "Can't find depended package " %&% import[ is.na( has.NAMESPACE)])

  import <- import[ has.NAMESPACE]
  
  owndoc <- find.documented( env, doctype='own')
#  internals <- character(0)
#  for( internaldoc in owndoc[ sapply( owndoc, function( x) regexpr( '-internal', attr( get( x), 'doc')[1])>0)]) {
#    tc <- unclass( attr( get( internaldoc), 'doc'))[-1]
#    gap <- index( regexpr( '[^ ]', tc)<0)[1]
#    internals <- c( internals, gsub( ' +', '', tc[ 1 %upto% (gap-1)]))
#  }
  force.exports <- possible.methods <- ffe <- find.funs( env)
  force.exports <- force.exports[ sapply( force.exports,
      function( x) !is.null( attr( get( x, env=env), 'export.me')))]
  possible.methods <- possible.methods %except% force.exports
  export <- unique( c( ffe %that.are.in% find.documented( env),
      force.exports, more.exports)) %that.are.in% lsall( env)

  methods <- list()
  S3.generics <- .knownS3Generics
  for( ipack in import) 
    S3.generics <- c( S3.generics, get.S3.generics( ipack, ns=TRUE))
  S3.generics <- c( S3.generics, get.S3.generics( env, ns=FALSE))
  
  for( gen in names( S3.generics))
    methods[[ gen]] <- possible.methods %that.match% ('^' %&% to.regexpr( gen) %&% '\\.')
  methods <- methods %SUCH.THAT% (length(.)>0)
  generics <- rep( names( methods), sapply( methods, length)) 
  
  if( length( methods)) {
    # Weed out apparent non-methods
    # Default env in arg1 is namespace of package where generic 'x' lives
    pseudo.ns <- function( pack) if( nzchar( pack)) asNamespace( pack) else env
    arg1 <- function( x, env=pseudo.ns( S3.generics[ x])) {
        x <- names( formals( get(x, envir=env)))[1]
        if( is.null( x))
          x <- ''
        x
    }
    genarg1 <- sapply( named( names( methods)), arg1)
    genarg1 <- rep( genarg1, sapply( methods, length))
    methods <- unlist( methods, use.names=FALSE)
    metharg1 <- sapply( named( methods), arg1, env=env)
    is.meth <- genarg1=='' | metharg1==genarg1
    methods <- methods[ is.meth]
    generics <- generics[ is.meth]

    if( length( methods)) {
      # Check doco to see if possible methods really are methods
      # ...USAGE section should not refer to specific method but to generic
      # No doc => no evidence against being a method, but otherwise...
      methdoc <- find.docholder( methods, env)
      is.meth <- rep( TRUE, length( methods))
      has.doc <- index( sapply( methdoc, length) != 0)
      for( i in has.doc) {
        docobj <- get( methdoc[[ i]][1], envir=env)
        docobj <- if( is.function( docobj)) attr( docobj, 'doc') else docobj
        USAGE.line <- grep( '^%?USAGE$', docobj)[1]
        ARGUMENTS.line <- grep( '^%?ARGUMENTS$', docobj)[1]
        # Is it called by its own name?
        if( !is.na( USAGE.line + ARGUMENTS.line))
          is.meth[ i] <- !length( grep( '\\<' %&% methods[ i] %&% ' *\\(',  #)
              docobj[ (USAGE.line+1) %upto% (ARGUMENTS.line-1)]))
      }
    
      methods <- methods[ is.meth]
      generics <- generics[ is.meth]
    } # if any possible methods with OK args
  } # if any possible methods at all
  
  classes <- substring( methods, nchar( generics)+2)
  export <- export %except% methods
  S3 <- matrix( c( generics, classes), ncol=2)

  returnList( import, export, S3)
}


"make.new.cd.task" <-
function( task.name, nlocal=sys.parent(), answer, dir.name) mlocal({
  # dir.name <- file.path( task.home(), legal.filename( task.name))
  dir.name <- './' %&% legal.filename( task.name) # syntax for rel paths: 26/6/2005
  line.end <- ifelse( option.or.default( 'cd.extra.CR', FALSE), '\n', '')

  repeat {
    cat("Default directory = ", dir.name, "\n(names will be expanded relative to ", task.home(),
        ")\nDirectory: " %&% line.end)
    answer <- readline()
    if(answer == "")
      answer <- dir.name
    else {
      answer <- gsub( '\\\\', '/', answer)
    if( (.Platform$OS.type=='windows' && (substring( answer, 1, 1) != '/') && 
          (substring( answer, 2, 2) != ':')) ||
        (.Platform$OS.type=='unix' && (substring( answer, 1, 1) %not.in% c('~','/')))  ) {
      # want relative path
      if( substring( answer, 1, 2) != './')
        answer <- './' %&% answer
      }
    }

    if( file.exists( answer)) {
      if( !is.dir( answer))
        cat("Directory already exists, as a file!\n")
      else
  break }
    else # if !file.exists
      if( mkdir( answer))
  break
      else
        cat( 'Failed to create directory ', answer,
           '\nWarning: unwanted directories may have been created!\n')
  }

  dir.name <- answer

  if( !exists( 'tasks', where=2, inherits=FALSE))
    tasks <- character( 0)
  tasks <- c( tasks, dir.name)
  names( tasks)[length( tasks)] <- task.name
  assign( 'tasks', tasks, pos=2)
  pe2 <- pos.to.env( 2)
  if( option.or.default( 'write.mvb.tasks', FALSE))
    write.mvb.tasks( tasks, pe2)
  Save.pos( 2) #  save( list=objects( pos=2, all=TRUE), envir=pe2, file=file.path( attr( pe2, 'path'), '.RData'))
  rdata.path <- file.path( dir.name, '.RData')
  if( !file.exists( rdata.path))
    save( list=character(0), file=rdata.path)
  names( dir.name) <- task.name
  dir.name
})


"make.Rd2" <-
function( strings){
  badatt <- FALSE
  strings <- gsub( '\\\\%', '%', strings) # because line() will pre-insert backslash before percent
  
  if( try( parse( text=strings), silent=TRUE) %is.a% 'try-error') {
    # Make everything a comment, and do escapes accordingly
    strings <- '#' %&% strings # what about %-starters?
    badatt <- TRUE
  }
  
  # Hide escaped quotes
  search.string <- "(^|[^\\])([\\\\])+\\"
  strings <- gsub( search.string %&% "'", '\\1\\2\001', strings)
  strings <- gsub( search.string %&% '"', '\\1\\2\002', strings)
  strings <- gsub( search.string %&% "`", '\\1\\2\003', strings)
#  strings <- gsub( search.string %&% "n", '\004', strings) # also special
  strings <- strings %&% '\n'

  sq <- charToRaw( "'")
  dq <- charToRaw( '"')
  bq <- charToRaw( '`')
  hash <- charToRaw( '#')
  eol <- charToRaw( '\n') # all hash-modes end at EOL, which is always found
  
  brace <- charToRaw( '{')
  backbrace <- charToRaw( '}')
  backslash <- charToRaw( '\\')
  percent <- charToRaw( '%')
  rep.brace <- '\005'
  rep.backbrace <- '\006'
  rep.backslash <- '\007'
  rep.percent <- '\008'
  
  specials <- end.specials <- c( sq, dq, bq, hash)
  end.specials[ end.specials==hash] <- eol

  state <- 0  # string states are carried across lines  
  for( istr in seq_along( strings)) {
    rch <- charToRaw( strings[ istr])
    l <- length( rch)
    states <- rep( state, length( rch)) # default
    to.do <- rep( TRUE, length( rch))
    done <- 0
    while( done < length( rch)) {
      if( state==0) {
        # Match any special
        matcho <- sapply( specials, match, table=rch[ to.do], nomatch=l+1-done) + done
        next.state <- which.min( matcho)
        matcho <- matcho[ next.state]
      } else {
        # Match only the end-special for this state
        matcho <- match( end.specials[ state], rch[ to.do], nomatch=l+1-done) + done
        next.state <- 0
      }
      
      states[ (done+1) %upto% (matcho-1)] <- state
      done <- matcho
      to.do <- (done+1) %upto% l
      if( matcho <= l)
        state <- next.state
    }
    
    # Flag characters in rch that need escaping
    rch[ rch==backslash] <- charToRaw( rep.backslash)
    rch[ rch==percent] <- charToRaw( rep.percent)
    escape.braces <- states %in% c( 0, 4)
    rch[ escape.braces & rch==brace] <- charToRaw( rep.brace)
    rch[ escape.braces & rch==backbrace] <- charToRaw( rep.backbrace)
    
    strings[ istr] <- rawToChar( clip( rch))
  }

  strings <- gsub( '\001', "\\'", strings, fixed=TRUE)
  strings <- gsub( '\002', '\\"', strings, fixed=TRUE)
  strings <- gsub( '\003', '\\`', strings, fixed=TRUE)
  strings <- gsub( '\004', '\\n', strings, fixed=TRUE)
  strings <- gsub( rep.percent, '\\%', strings, fixed=TRUE)
  strings <- gsub( rep.brace, '\\{', strings, fixed=TRUE)
  strings <- gsub( rep.backbrace, '\\}', strings, fixed=TRUE)
  strings <- gsub( rep.backslash, '\\\\', strings, fixed=TRUE) # ?is this correct #backslashes?
  
  attr( strings, 'badatt') <- badatt # NULL if parsed OK
return( strings)
}


"make.usage.section" <-
function( funs=find.funs( env) %except% find.documented( env, doctype='Rd'), file=stdout(),
    env=.GlobalEnv) {
  usage <- function( x) {
      if( regexpr( '^%.*%', x)>0) {
        # Assumes binary op with no defaults
        y <- names( formals( env[[ x]]))
        y <- paste( y[1], x, y[2], sep=' ')
      } else {
				y <- clip( deparse( args( env[[x]])))
				y <- sub( '^ +', ' ', y)
				y[1] <- sub( '^function ', to.regexpr( x), y[1])
				y <- paste( y, collapse='')
			}
      y
    }
 funs <- sapply( funs, usage)
 if( !is.null( file))
   cat( funs, sep='\n', file=file)
 invisible( funs)
}


"masked" <-
function (pos) {
  if( is.character( pos))
    pos <- match( pos, search())

  if (any(pos < 2)) 
return(structure(.Data = character(0), info = "Nothing in .Global.env can be masked!"))

  o <- unique(unlist(lapply(pos, objects, all = TRUE)))
  all.objects <- unlist(lapply(1:(min(pos) - 1), objects, 
      all = TRUE), use.names = FALSE)
  mm <- match(all.objects, o, 0)
  tabu <- tabulate(mm, nbins = length(o))
  o[tabu > 0]
}


"masking" <-
function (pos = 1) {
  if( is.character( pos))
    pos <- match( pos, search())
  if (any(pos >= (sl <- length(search())))) 
return(structure(.Data = character(0), info = "Objects at the bottom can't mask anything!"))

  o <- unique(unlist(lapply(pos, objects, all = TRUE)))
  all.objects <- unlist(lapply((max(pos) + 1):sl, objects, 
      all = TRUE), use.names = FALSE)
  mm <- match(all.objects, o, 0)
  tabu <- tabulate(mm, nbins = length(o))
  o[tabu > 0]
}


"massrep" <-
function( orig, atlist, replist, sorted.at=TRUE){
  if( !length( atlist))
return( orig)

  repextend <-  function( a, r)
      if( length( a)==1)
        r
      else
        c( r, rep( list( character(0)), length(a)-1) )
  la <- sapply( atlist, length)
  rl <- rep( list( orig[0]), sum( la)) # preserves type of 'orig'
  rl[ 1 + c( 0, cumsum( clip( la)))] <- replist
return( multirep( orig, unlist( atlist), rl, sorted.at))
}


"maybe.save.after.move" <-
function (to.from) {
  if( is.na( to.from$saving)) {
    thing.for.message <- if( !is.null( names( to.from$path)))
        '"' %&% names( to.from$path) %&% '" [' %&% to.from$path %&% ']'
      else
        to.from$path
    to.from$saving <- yes.no( 'Save workspace of ' %&% thing.for.message %&% '? ') 
  }
  
  if( to.from$saving) 
    Save.pos( to.from$env, to.from$path)
}


"mcachees" <-
function( envir=.GlobalEnv)
  if( is.null( mcache <- attr( as.environment( envir), 'mcache'))) character(0) else names( mcache)


"mcut" <-
function( x, breaks, pre.lab='', mid.lab='', post.lab='', digits=getOption( 'digits')){
  lbreaks <- format( round( breaks, digits=digits)) %&% mid.lab
  labs <- pre.lab %&% '[' %&% c( '<' %&% lbreaks[1],  
      clip( lbreaks) %&% ',' %&% lbreaks[ -1], '>=' %&% rev( lbreaks)[1]) %&% ']' %&% post.lab
  if( length( breaks)==1)
    labs <- labs[-2]
  else if( length( breaks)==0)
    labs <- labs[2]
  xc <- 1+findInterval( x, breaks)
  factor( labs[ xc], levels=labs)
}


"methodize.USAGE" <-
function( nlocal=sys.parent()) mlocal({
# Post-process to set "\method" pedantry in USAGE
# Check for aliases that don't appear in USAGE-- if these appear to be methods of S3 generics, then
# ... tag the USAGE calls with \method

  USAGE.start <- grep( '^\\\\usage\\{', Rd)[1]
  if( !is.na( USAGE.start)) { # Not all docos have USAGE or ARGUMENTS, e.g. package doco  
    # All on one line? If so, split
    if( length( grep( '\\}', Rd[ USAGE.start]))) {
      bits <- sub( ' *\\} *$', '', sub( '^\\\\usage\\{ *', '', Rd[ USAGE.start]))
      Rd <- multirep( Rd, USAGE.start, list( c( '\\usage{', bits, '}')))
    }
  
    USAGE.end <- match( '}', Rd[ -(1:USAGE.start)], NA)+USAGE.start

    ulines <- (USAGE.start+1) %upto% (USAGE.end-1)
    aliases <- unique( c( overall.name, sub( '\\\\alias\\{ *', '', sub( ' *\\}.*', '', 
        grep( '\\alias{', Rd, fixed=TRUE, value=TRUE)))))
    ucall.lines <- ulines[ grep( '^ *([^#,= (]+)\\(.*', Rd[ ulines])]
    usage.calls <- sub( '^ *([^# (]+)\\(.*', '\\1', Rd[ ucall.lines])
    gen.lines <- ucall.lines[ match( usage.calls, names( .knownS3Generics), 0) > 0]
    if( length( gen.lines)) {
      gen.calls <- sub( '^ *([^#,= (]+)\\(.*', '\\1', Rd[ gen.lines])
      poss.meths <- aliases %except% (aliases %that.are.in% gen.calls)
      poss.meths <- poss.meths %that.match% paste( '^' %&% gen.calls %&% '\\.', collapse='|')
      if( length( gen.lines) == length( poss.meths)) { 
        # one line per call
        meth.for.gen <- poss.meths
      } else {
        # Expect a comment at the end of the call saying which method is being called
        call.end.lines <- ulines[ grepl( '^[^#]*\\) *#', Rd[ ulines])]
        first.after <- call.end.lines[ findInterval( gen.lines, call.end.lines)]
        meth.for.gen <- gsub( ' ', '', sub( '.*# *', '', Rd[ first.after]))
      }
      for( i in seq_along( gen.lines))
        Rd[ gen.lines[i]] <- sub( '([^ (]+)\\(', '\\\\method{\\1}{' %&% 
          sub( gen.calls[i] %&% '.', '', meth.for.gen[i], fixed=TRUE) %&% '}(', Rd[ gen.lines[i]])
    }
  }  

})


"mintcut" <-
function( x, breaks, prefix='', all.levels=!is.null( attr( breaks, 'all.levels'))) {
  # Labels of the form 2-7 or 3, or 8+ (for last in range)
  # x<breaks[1] := NA
  all.levels <- force( all.levels)
  x <- as.integer( x)
  breaks <- as.integer( breaks)
  
  xc <- findInterval( x, breaks)
  xc[ xc==0] <- NA
  xlabs <- breaks %&% '-' %&% c( breaks[ -1]-1, Inf)
  gap1 <- c( clip( breaks)==breaks[-1]-1, FALSE)
  xlabs[ gap1] <- breaks[ gap1]
  xlabs[ length( breaks)] <- breaks[ length( breaks)] %&% '+'
  xlabs <- prefix %&% xlabs
  factor( xlabs[ xc], levels=if( all.levels) xlabs else xlabs[ 1 %upto% max( xc, na.rm=TRUE)])
}


"Minutes" <-
function( x){
  Seconds( 60*x)
}


"mkdir" <-
function( dirlist) {
  outcome <- logical(length(dirlist))
  for (dir in 1:length(dirlist)) {
    answer <- strsplit(strsplit(dirlist[dir], "/", fixed=TRUE)[[1]], "\\", fixed=TRUE)
    # Deal with absolute strings starting with '/'
    if( !length( answer[[1]])) {
      answer <- answer[-1]
      answer[[1]] <- '/' %&% answer[[1]]
    }
    next.dir <- character(0)
    for (i in answer) 
      if( !is.dir( next.dir <- paste( c( next.dir, i), collapse = "/")) &&
          !( substring( next.dir, nchar( next.dir), nchar( next.dir))==':')) 
        dir.create(next.dir)
    outcome[dir] <- is.dir(next.dir)
  }
  outcome
}


"mlazy" <-
function( ..., what, envir=.GlobalEnv, save.now=TRUE) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)
  if( !length( what))
return()

  envir <- as.environment( envir)

  what <- what %such.that% (. %in% lsall( envir))
  if( !length( what)) {
    warning( 'nothing exists to be mlazyed')
return()
  }

  # Next call used to have a getfrom arg, set to sys.frame( mvb.sys.parent()) ..?
  move.to.mcache( what, envir, save.now=save.now) 
  if( !identical( envir, .GlobalEnv))
    save.refdb( envir=envir) # not until asked
}


"mlibrary2" <-
function( task, character.only=FALSE,
    fpath=tasks[ task], pos=lib.pos(), execute.First=TRUE){
  if( !character.only)
    task <- as.character( substitute( task))
  file <- file.path( fpath, '.RData')
  stopifnot( file.exists( file))
  if( missing( task) && missing( task))
    task <- basename( dirname( fpath))
  env <- attach( list(), name='package:' %&% task, pos=pos)
  attr( env, 'path') <- structure( fpath, names=task)
  load.refdb( file, envir=env)

  if (execute.First && exists( ".First.lib", envir = env, inherits = FALSE)) {
    .First.lib <- get( ".First.lib", envir = env, inherits = FALSE)
    tt <- try( .First.lib( basename( task), task))
    if( tt %is.a% "try-error") {
      warning(".First.lib failed in" %&% task)
      try( detach( pos))
return( FALSE)
    }
  }

  invisible( TRUE)
}


"mlocal" <-
function( expr) {
  sp <- sys.parent()
  sp.env <- sys.frame(sp)
  # nlocal_ eval( as.name( 'nlocal'), envir=sp.env) # used to work in S but not in R
  nlocal <- get( 'nlocal', envir=sp.env)
  nlocal.env <- sys.frame( nlocal)

# on.exit stuff changed 7/2/2005; looks like old version was for Splus
  on.exit( {
#    eval( sys.on.exit()[[nlocal]], envir=nlocal.env) # zapped

#   Get rid of temporaries
    remove( list=names( params) %that.are.in%
        (lsall( env=nlocal.env) %except% names( savers)), envir=nlocal.env)

#   Restore things hidden by params
    for( i in names( savers))
      assign( i, savers[[ i]], envir=nlocal.env)

#    eval( old.on.exit, envir=nlocal.env) # so old code will execute on return to 'nlocal' # zapped
  })

  eval( expression( on.exit())[[1]], envir=nlocal.env)

  params <- formals( sys.function( sp))
  params <- params[ names(params)!='nlocal']
  savers <- names( params)

  if( length( params)) {
    names( savers) <- savers
    savers <- sapply( savers, exists.mvb, envir=nlocal.env)
    savers <- names( savers)[ savers]
    if( length( savers)) {
      names( savers) <- savers
      savers <- lapply( savers, function( x) mget( x, envir=nlocal.env)[[1]])
    }
#   Parameters and temporary working variables:

    for( i in names( params)) {
      if( eval( call( 'missing', i), envir=sp.env)) {
        if( is.symbol( params[[ i]]) && !nzchar( as.character( params[[ i]])) &&
            exists.mvb( i, env=nlocal.env))
          remove( list=i, envir=nlocal.env)
        else
          assign( i, params[[i]], envir=nlocal.env) }
          #delayedAssign( i, params[[i]], eval.env=nlocal.env, assign.env=nlocal.env) }
      else # CHANGED from: bugs here? doesn't force... should do so or use delayedAssign?
        assign( i, sp.env[[i]], envir=nlocal.env)
        #assign( i, eval( call( 'get', i), envir=sp.env), envir=nlocal.env)
        #delayedAssign( i, call( 'eval', i, envir=sp.env), assign.env=nlocal.env)
        
    } # else NORMAL case
  } # parameter loop

# Embed "expr" in an artificial loop, so that calls to 'break' at top-level will quit the function. This feature
# is only for S-compatibility. Preferred syntax in R is return( local.return( ...)) which works inside any depth of
# loops

  expr <- substitute( repeat{ assign( 'answer', expr, envir=env); break },
      list( expr=substitute( expr), env=sys.frame(sys.nframe())))

# The business end!
  on.exit.code <- quote( NULL)
  eval( expr, envir=nlocal.env, enclos=sys.frame( sys.nframe()))

# New bug fix, 7/2/2005
  eval( on.exit.code, envir=nlocal.env, enclos=sys.frame( sys.nframe()))

  if( exists.mvb( 'override.answer', envir=sys.frame( sys.nframe()))) # set by a call to "local.return"
    answer <- override.answer
  if( exists.mvb( 'answer', envir=sys.frame( sys.nframe())))
    answer # else return NULL. Will only happen if user has a "return" call
           # without "local.return"-- bad practice.
}


"move" <-
function( x='.', from='.', to='.', what, overwrite.by.default=FALSE, copy=FALSE) {
  if( !missing( what)) {
    to <- substitute( from)
    from <- substitute( x) }
  else {
    what <- as.character( substitute( x))
    from <- substitute( from)
    to <- substitute( to) }
  
  if( (to %is.a% 'call') && (to[[1]]==quote( `$`))) { # maintained package
    to <- eval( to, parent.frame())
  } else { # normal
    if( !is.character( to))
      to <- deparse( to)
    to <- find.path( char=to)
  }
  
  if( (from %is.a% 'call') && (from[[1]]==quote( `$`))) { # maintained package
    from <- eval( from, parent.frame())
  } else { # normal
    if( !is.character( from))
      from <- deparse( from)
    from <- find.path( char=from)
  }  

  from <- prepare.for.move( from)
  to <- prepare.for.move( to)

  if( identical( from$env, to$env) || from$path==to$path)
stop( '"from" and "to" point to the same place!')

  found <- !is.na( match( what, from$obj))
  if( !all( found))
    warning( 'Can\'t find ' %&% paste( what[!found], collapse=','))
  what <- what[ found]
  if( !length( what)) {
    cat( 'Nothing to move!')
return( invisible( character(0))) }

  overwrite <- is.na( match( what, to$obj)) | overwrite.by.default
  names( overwrite) <- what
  for( i in what[!overwrite]) {
    all.over <- FALSE
    repeat{
      cat( 'Overwrite ', i, ' [Y(es)/N(o)/A(ll)]? ')
      answer <- upper.case( substring( readline(), 1,1))
      overwrite[ i] <- NA
      if( answer=='Y')
        overwrite[ i] <- TRUE
      else if( answer=='N')
        overwrite[ i] <- FALSE
      else if( answer=='A') {
        overwrite[ index( i==what):length( overwrite)] <- TRUE
        all.over <- TRUE }
      if( !is.na( overwrite[i]))
    break }

    if( all.over)
  break }

  what <- what[ overwrite]
  if( !length( what)) {
    cat( 'Nothing to move!')
return( invisible( character(0))) }

  # Changed 14/3/2004 to cope with mrefs
  to.mcache <- attr( to$env, 'mcache')
  from.mcache <- attr( from$env, 'mcache') # replaces info from to$env

  whatrefs <- what %such.that% (. %in% names( from.mcache))
  mtidy( what=whatrefs, from$env)

  for( i in what %except% whatrefs) {
    obj <- get( i, env=from$env)
    assign( i, obj, envir=to$env)
    update.loaded.pkg( names( attr( to$env, 'path')), i, obj) # live pkgs: 6/7/2006
    move.backup.file( i, old.dir=from$path, new.dir=to$path)
  }

  if( length( whatrefs)) {
    # mcache not applicable to loaded packages, phew
    new.to.mcache <- mupdate.mcache( whatrefs, to.mcache, from$env)

    from.obj.files <- file.path( from$path, 'mlazy', 
        'obj' %&% from.mcache[ whatrefs] %&% '.rda')
    to.obj.files <- file.path( to$path, 'mlazy',
        'obj' %&% new.to.mcache[ whatrefs] %&% '.rda')

    suppressWarnings( file.remove( to.obj.files))
    renamed <- logical( length( from.obj.files))
    for( i in seq_along( from.obj.files))
      renamed[i] <- file.rename( from.obj.files[i], to.obj.files[i])
    if( any( !renamed))
      file.copy( from.obj.files[ !renamed], to.obj.files[ !renamed] )

    attr( to$env, 'mcache') <- new.to.mcache
    setup.mcache( to$env, refs=whatrefs) # change nfile & env
  }

  move.fix.list()

  maybe.save.after.move( to)

  if( !copy) {
    remove( list=what, envir=from$env)
    if( length( maintained.packages) && 
        !is.na( mp <- index( sapply( maintained.packages, identical, from$env))[1]))
      rm.pkg( names( maintained.packages)[ mp], list=what)
    if( length( whatrefs)) {
      suppressWarnings( file.remove( from.obj.files))
      attr( from$env, 'mcache') <- from.mcache %without.name% whatrefs
    }
    maybe.save.after.move( from)
  }

  invisible( what)
}


"move.backup.file" <-
function( name, old.dir, new.dir, copy=FALSE) {
  if( !nchar( old.file <- get.bkfile( name, old.dir, create=FALSE)))
return()

  new.index <- create.bkind.if.needed( new.dir)
  new.file <- get.bkfile( name, new.dir, create=TRUE)
  file.copy( from=old.file, to=new.file, overwrite=TRUE)
  
  unlink( old.file)
  if( !copy) {
    old.index <- create.bkind.if.needed( old.dir) # sure to exist
    old.index.contents <- read.bkind( old.dir)
    which <- match( name, old.index.contents$object.names)
    cat( paste( old.index.contents$files[ -which], old.index.contents$object.names[ -which], sep='='), sep='\n', 
      file=old.index)
  }
}


"move.fix.list" <-
function( nlocal=sys.parent()) mlocal({
  fixing <- match( fix.list$name, what, 0) > 0
  if( any( fixing)) { # must all be moving to the same place
    stt <- search.task.trees()
    path.list <- sapply( stt, function( x) attr( pos.to.env( x), 'path'))
    if( !is.na( to.match <- match( attr( to$env, 'path'), path.list)[1])) {
      fix.list$where[ fixing] <<- names( stt)[ to.match]
      fix.list$where.type[ fixing] <<- 'task'
    } else {
      if( !is.null( attr( to$env, 'name')) && (attr( to$env, 'name') %in% names( maintained.packages))) {
        fix.list$where[ fixing] <<- paste( attr( to$env, 'task.tree'), collapse='/')
        fix.list$where.type[ fixing] <<- 'package'
      } else {
        cat( 'Warning: the following have moved out of memory and further fixes will not be committed: ',
            paste( fix.list$name[ fixing], collapse=','), '\n')
        fix.list <<- fix.list[ !fixing,]
      }
    }
  }
})


"move.to.mcache" <-
function( what, envir, save.now) { # used to have a getfrom arg
  mcache <- attr( envir, 'mcache')
  if( is.null( mcache))
    mcache <- numeric(0)

  if( !length( what))
return( mcache)

  what <- (what %SUCH.THAT% exists( ., envir=envir)) %SUCH.THAT% !bindingIsActive( ., env=envir)
#  what <- what %SUCH.THAT% exists( ., envir=getfrom, inherits=TRUE)

  ow <- options( warn=-1)
  on.exit( options( ow))
  attr( envir, 'mcache') <- mcache <- mupdate.mcache( what, mcache, envir)

  path <- attr( envir, 'path')
  if( option.or.default( 'mlazy.subdir', TRUE)) {
    dir.create( file.path( path, 'mlazy'), showWarnings=FALSE)
    objpath <-  file.path( 'mlazy', 'obj')
  } else
    objpath <- 'obj'

  for( i in what) {
    # Anything moved to the cache must be saved
    this.file <- file.path( path, objpath %&% mcache[ i] %&% '.rda')
    save( list=i, file=this.file, envir=envir, compress=TRUE) # used to have envir=getfrom ???

    fx <- get.mcache.reffun( i, envir)
    environment( fx)[[ i]] <- envir[[ i]]
    remove( list=i, envir=envir)
    suppressWarnings( makeActiveBinding( i, fx, envir))
  }

return( mcache)
}


"mp.synch" <-
function( pkg){
  nspos <- try( asNamespace( pkg), silent=TRUE)
  if( nspos %is.not.a% 'try-error') {
    # use identical to check whether *really* changed

    if( exports.have.changed)
    users <- getNamespaceUsers( pkg)

  }

  pkpos <- match( 'package:' %&% pkg, search(), 0)
  if( pkpos>0) {

  }

}


"mtidy" <-
function( ..., what, envir=.GlobalEnv) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)
  if( !length( what))
return()

  envir <- as.environment( envir)
  mcache <- attr( envir, 'mcache')

  if( !missing( what)) {
    what <- what %such.that% ( . %in% lsall( envir))
    mlazy( what=what %except% names( mcache), envir=envir)
  } else
    what <- names( mcache)

  if( !length( what))
return( invisible( what))

  path <- attr( envir, 'path')
  if( is.null( path))
stop( 'environment has no path attribute')

  save.mchanged( what, envir)

  # Replace cachees by new active bindings
  remove( list=what, envir=envir)
  setup.mcache( refs=what, envir=envir)

  invisible( what)
}


"multinsert" <-
function( orig, at, ins, sorted.at=TRUE){
  if( !length( at))
return( orig)

  if( !is.list( ins))
    ins <- if( length( at)==1) list( ins) else as.list( ins) # assumes each ins elt is length-1
  if( length( ins) < length( at))
    ins <- rep( ins, length( at) / length( ins))

  if( !sorted.at) {
    o <- order( at)
    at <- at[ o]
    ins <- ins[ o]
  }

  inslen <- sapply( ins, length)
  # NB replace call in next line: in case at[1]==0
  new <- orig[ rep( seq_along( orig), c( 1, 1+inslen)[ 1+match( seq_along( orig),
      replace( at, at==0, 1), nomatch=0)])]
  new[ rep( at, inslen) + 1:sum( inslen)] <- unlist( ins)
  new
}


"multirep" <-
function( orig, at, repl, sorted.at=TRUE){
  if( !length( at))
return( orig)

if( !sorted.at) {
  o <- order( at)
  at <- at[ o]
  repl <- repl[ o]
}

  replen <- sapply( repl, length)
  new <- orig[ rep( seq_along( orig), c( 1, replen)[ 1+match( 
      seq_along( orig), at, nomatch=0)])]
  new[ rep( at, replen) + 1:sum( replen) - rep( 1:length(at), replen)] <- unlist( repl)
  new
}


"mupdate.mcache" <-
function( what, mcache, envir) {
  had.num <- what %such.that% (. %in% names( mcache))
  need.num <- what %except% had.num
  if( !length( need.num))
return( mcache)
  mci <- attr( mcache, 'info')

  if( !length( mcache))
    new.mcache <- seq( along=need.num)
  else {
    new.mcache <- (1 %upto% (max( c( 0, mcache), na.rm=TRUE) + length( need.num))) %except% abs( mcache)
    new.mcache <- new.mcache[ 1:length( need.num)]
  }

  names( new.mcache) <- need.num
  new.mci <- lapply( need.num, get.info.for.mcache, envir=envir, name=TRUE)

  mcache <- c( mcache, new.mcache)
  attr( mcache, 'info') <- c( mci, new.mci)
  mcache
}


"mvb.file.copy" <-
function( file1, file2, overwrite=TRUE) {
  # file.copy stuffs up 'mtime' so...
  if( .Platform$OS.type=='windows') {
    syscopy <- Sys.getenv( 'COMSPEC') %&% ' /c copy /y'
    file1 <- '"' %&% gsub( '/', '\\\\', file1) %&% '"'
    file2 <- '"' %&% gsub( '/', '\\\\', file2) %&% '"'
    copy.same.mtime <- function( f1, f2)
      system( paste( syscopy, f1, f2), show.output.on.console=FALSE)
  } else {
    syscopy <- 'cp'
    # Escape spaces and backslashes... and probably all sorts of other crap NFN
    subbo <- function( f) {
      f <- gsub( '\\', '\001', f, fixed=TRUE)
      f <- gsub( ' ', '\\ ', f, fixed=TRUE)
      f <- gsub( '\001', '\\\\', f, fixed=TRUE)
    }
    
    copy.same.mtime <- function( f1, f2) {
      result <- system( paste( syscopy, subbo( f1), subbo( f2)))
      if( result==0) {
        f1.mtime <- format( file.info( f1)$mtime, '%Y%m%d%H%M.%S')
        system( paste( 'touch -m -t ', f1.mtime, subbo( f2)))
      }
      result
    }
  }

  ok <- rep( FALSE, length( file1))
  for( i in seq_along( file1))
    if( overwrite || !file.exists( file2[i]))
      ok[ i] <- copy.same.mtime( file1[i], file2[i])
  return( ok)
}


"mvb.formalize.package.hook" <-
function( default.list) {
  default.list$exclude.funs <- character( 0)
  default.list
}


"mvb.match.call" <-
function (definition = sys.function( mvb.sys.parent()), 
    call = sys.call(mvb.sys.parent()), expand.dots = TRUE) {
  # This has to be tricky to get it to work in 'debug'
  # eg f <- function( ...) g(...), g <- function( alpha=1) match.call(), f(1)
  # It's not clear to me that ... is consistently handled when call is non-default
  # ... because it still depends on the calling context
  
  callo <- quote( base:::match.call())
  callo[[2]] <- definition
  callo[[3]] <- base:::call( 'quote', call)
  callo$expand.dots <- expand.dots
  eval( callo, mvb.parent.frame(2))
}


"mvb.minus.POSIXt" <-
function (e1, e2) {
  # Hacked by MVB in 1/2008 to avoid difftime
  if(e1 %is.not.a% "POSIXt") 
stop("Can only subtract from POSIXt objects")

  if (nargs() == 1) 
stop("unary - is not defined for \"POSIXt\" objects")

  pure.tdiff <- e2 %is.a% "POSIXt"
  if( pure.tdiff)
    e2 <- as.POSIXct( e2) # guaranteed seconds
  
  e1 <- as.POSIXct( e1)
  value <- c( unclass(e1) - Seconds( unclass( e2)))

  if( pure.tdiff)
return( value)
  else
return( structure( value, class = c("POSIXt", "POSIXct"), tzone=attr( e1, 'tzone')))
}


"mvb.nargs" <-
function() 
  length( sys.calls()[[ mvb.sys.parent()]])-1


"mvb.parent.frame" <-
function (n = 1) 
  sys.frame( mvb.sys.parent( n+1)) # +1 added Oct 09


"mvb.plus.POSIXt" <-
function (e1, e2) {
  # Hacked by MVB in 1/2008 to avoid difftime
  if (nargs() == 1) 
return(e1)

  if( (e1 %is.a% "POSIXt") && (e2 %is.a% "POSIXt")) 
stop("binary + is not defined for \"POSIXt\" objects")

  if( e2 %is.a% 'POSIXt') {
    temp <- e1
    e1 <- e2
    e2 <- temp
  }

  if( e1 %is.a% "POSIXlt") 
    e1 <- as.POSIXct(e1) # get tzone OK
  
  structure( unclass( e1) + unclass( Seconds( e2)), class = c("POSIXt", 
      "POSIXct"), tzone = attr( e1, 'tzone'))
}


"mvb.rbind.data.frame" <-
function( ..., deparse.level=1) {
  allargs <- list( ...)
  fake.first <- length( allargs)>1 && is.data.frame( allargs[[1]]) && nrow( allargs[[1]])==0
  if( fake.first)
    allargs[[1]][1,] <- allargs[[1]][1,] # sets to NAs
  rbindo <- do.call( 'base.rbind.data.frame', c( allargs, list( deparse.level=deparse.level)))
  if( fake.first)
    rbindo <- rbindo[-1,]
  rbindo
}


"mvb.sys.call" <-
function( which=0) {
  if( which>0)
    .Internal( sys.call( which))
  else {
    which <- try( mvb.sys.parent( 1-which), silent=TRUE)
    if( which %is.a% 'try-error')
stop( 'not that many enclosing functions')
    else if( which==0)
      NULL # that's what R 1.8.1 does
    else
      .Internal( sys.call( which))
  }
}


"mvb.sys.function" <-
function( n) {
  if( missing( n))
    n <- mvb.sys.parent()
  sys.function( n)
}


"mvb.sys.nframe" <-
function() mvb.sys.parent(1)


"mvb.sys.on.exit" <-
function() {
  p <- mvb.sys.parent()
  f <- find.debug.HQ()
  f <- get( '.frames.', envir=f)
  dbg <- f$debug[ match( p, f$actual, 0)]
  if( !length( dbg))
stop( "sys.on.exit won't work reliably here when the debugger is being used; must be 'unusual' code!")

  get( 'expr', envir=sys.frame(dbg))[[2]][[2]]
}


"mvb.sys.parent" <-
function(n=1) {
  p <- sys.nframe()
  frames <- lapply( sys.frames(), list) # this wrapper seems to be necessary to get it to work. R "feature"
  parents <- sys.parents()
  for( gen in 0 %upto% n)
    p <- parents[ which( sapply( frames, identical, frames[[p]]) )[ 1] ] # parent of FIRST pointer to this env in frame list

  p
}


"my.all.equal" <-
function (x, y) 
{
    stupid <- all.equal(x, y)
    if (!is.logical(stupid)) 
        stupid <- FALSE
    stupid
}


"my.fixup.package.URLs" <-
function (pkg, force = FALSE) 
{
    top <- paste("file:///", chartr("\\", "/", R.home()), sep = "")
    fixedfile <- file.path(pkg, "fixedHTMLlinks")
    if (file.exists(fixedfile)) {
        oldtop <- readLines(fixedfile)
        if (!force && (length(oldtop) == 1) && top == oldtop) 
            return(TRUE)
        olddoc <- paste(oldtop, "/doc", sep = "")
        oldbase <- paste(oldtop, "/library/base", sep = "")
        oldutils <- paste(oldtop, "/library/utils", sep = "")
        oldgraphics <- paste(oldtop, "/library/graphics", sep = "")
        oldstats <- paste(oldtop, "/library/stats", sep = "")
        olddata <- paste(oldtop, "/library/datasets", sep = "")
        oldgrD <- paste(oldtop, "/library/grDevices", sep = "")
        oldmeth <- paste(oldtop, "/library/methods", sep = "")
    }
    else {
        olddoc <- "../../../doc"
        oldbase <- "../../base"
        oldutils <- "../../utils"
        oldgraphics <- "../../graphics"
        oldgrDevices <- "../../grDevices"
        oldstats <- "../../stats"
        olddata <- "../../datasets"
        oldgrD <- "../../grDevices"
        oldmeth <- "../../methods"
    }
    if (!file.create(fixedfile)) 
        return(FALSE)
    cat(top, "\n", sep = "", file = fixedfile)
    htmldir <- file.path(pkg, "html")
    if (!file.exists(htmldir)) 
        return(FALSE)
    files <- list.files(htmldir, pattern = "\\.html$", full.names = TRUE)
    doc <- paste(top, "/doc", sep = "")
    base <- paste(top, "/library/base", sep = "")
    utils <- paste(top, "/library/utils", sep = "")
    graphics <- paste(top, "/library/graphics", sep = "")
    stats <- paste(top, "/library/stats", sep = "")
    datasets <- paste(top, "/library/datasets", sep = "")
    grD <- paste(top, "/library/grDevices", sep = "")
    meth <- paste(top, "/library/methods", sep = "")
    
    # altered by MVB 3/2009, to avoid changing unmodified files
    for (f in files) {
        page <- readLines(f)
        old.page <- page # MVB
        page <- gsub(olddoc, doc, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldbase, base, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldutils, utils, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldgraphics, graphics, page, fixed = TRUE, 
            useBytes = TRUE)
        page <- gsub(oldstats, stats, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(olddata, datasets, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldgrD, grD, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldmeth, meth, page, fixed = TRUE, useBytes = TRUE)
        if( identical( page, old.page))
    next
        out <- try(file(f, open = "w"), silent = TRUE)
        if (inherits(out, "try-error")) {
            warning(gettextf("cannot update '%s'", f), domain = NA)
            next
        }
        writeLines(page, out)
        close(out)
    }
    return(TRUE)
}


"my.index" <-
function( var, ...) {
#  pg <- .Primitive( '[[') # doesn't cope with pairlists
#  pg <- function( x, i) .Primitive( '[[')( as.list( x), i) # screws up e.g. on factors
  pg <- function( x, i) .Primitive( '[[')( if( is.pairlist( x)) as.list( x) else x, i)
  vv <- as.name( 'var')
  for( i in c(...))
    vv <- call( 'pg', vv, i)
  eval( vv)
}


"my.index.assign" <-
function (var, ..., value) 
{
    i <- c(...)
    if (length(i) < 2) 
        return(.Primitive("[[<-")(var, i, value))
    pa <- .Primitive("[[<-")
    pg <- .Primitive("[[")
    vario <- as.name("var")
    for (ii in i[-length(i)]) vario <- call("pg", vario, ii)
    callio <- substitute(value, env = parent.frame())
    for (ii in rev(i)) {
        callio <- call("pa", vario, ii, callio)
        if (length(vario) > 1) 
            vario <- vario[[2]]
    }
    return(eval(callio))
}


"my.index.exists" <-
function( i, var) {
  for( ii in 1 %upto% length( i))
    if( missing( var) || !is.recursive( var) || i[ ii] > length( var))
return( FALSE)
    else
      var <- as.list( var)[[ i[ ii] ]]
return( TRUE) }


"named" <-
function (x) {
  if( !length( x))
return( x)

  names(x) <- as.character(x)
  x
}


"named.in.doc" <-
function( doc) {
  if( is.null( doc) || !is.character( doc))
return( character( 0))

  doc <- c( doc, ' ') # guarantees blank
  blank <- seq( along=doc) %except% grep( '[^ ]', doc)
  namelines <- doc[ 1 %upto% (min(blank)-1)] # 2: to ignore first line
  namelines <- sub( '^ +', '', namelines) # leading spaces
  namelines <- gsub( ' +[^ ]+', '', namelines) # keep first word only
  namelines <- gsub( ' *', '', namelines) # trailing spaces
  namelines
}


"no.lazyLoad.attach.hook" <-
function( pkgname, pkglib) {
  # Identical to no.lazyLoad.hook, but for search-path version
  # Hook to force immediate loading, and to avoid trouble with lazyLoad being out-of-synch later  
  # Don't force loading of mlazies

  ns <- as.environment( 'package:' %&% pkgname) 
      
  for( obj in lsall( ns)) {
    get.promise <- call( 'substitute', as.name( obj))
    c1 <- eval( get.promise, ns) 
    if( (c1 %is.a% 'call') && (c1[[1]]==as.name( 'lazyLoadDBfetch')))
      ns[[ obj]] # force
  }
}


"no.lazyLoad.hook" <-
function( pkgname, pkglib) {
  # Hook to force immediate loading, and to avoid trouble with lazyLoad being out-of-synch later  
  # Don't force loading of mlazies

  ns <- asNamespace( pkgname) 
      
  for( obj in lsall( ns)) {
    get.promise <- call( 'substitute', as.name( obj))
    c1 <- eval( get.promise, ns) 
    if( (c1 %is.a% 'call') && (c1[[1]]==as.name( 'lazyLoadDBfetch')))
      ns[[ obj]] # force
  }
}


"not.for.packaging" <-
function( env){
  nfp <- cq( tasks, .Traceback, .packageName, last.warning, .Random.seed, .SavedPlots)
  if( !is.null( pkgname <- attr( env, 'name')))
    nfp <- c( nfp, pkgname %&% '.package.doc')
  if( exists( 'exclude.from.package', mode='character', env))
    nfp <- c( nfp, env$exclude.from.package, 'exclude.from.package')
  nfp
}


"option.or.default" <-
function (opt.name, default=NULL) {
  value <- getOption(opt.name)
  if (!is.null(value)) 
    value
  else default
}


"organize.web.display" <-
function( resequence=TRUE, merge01=FALSE, plotmath=FALSE, nlocal=sys.parent()) mlocal({
# Now we have to figure out what level in the hierarchy each fn. belongs at.
# Simple-minded approach: anything NOT called by any other function is top-
# level; anything called only by top-levels is second-level; etc.

  level <- rep(0, n); names( level) <- funs
  current.level <- 1
  if( n>1)
  while( any( level==0)) {
    tops <- rep( 1, sum( level==0)) %**% funmat[level==0, level==0] == 0
    if( !any( tops))  # we have to sort out functions that call each other
      tops <- least.mutual.dependency( funmat, funs, level)

    level[ (1:n)[ level==0] [tops] ] <- current.level # dimnames( funmat)[[1]]
    current.level <- current.level+1 }
  else
    level[] <- 1

# Super. Now we need to organize things on each level, placing slaves below
# their masters. This OUGHT to be a 'forwards-and-backwards' algorithm,
# because the appropriate placement of masters may depend on which slaves
# they call. EG if you have masters A, B, C, calling slaves (a,c), (b), (c)
# respectively, then  ACB is better than ABC, to avoid crossings.
# Bugger that for now! I am going to fix each layer in concrete, and let the
# underlings sort themselves out.

  x <- numeric( n)
  n.masters <- sum( level==1)

# Now sift out 'level 0 functions'; that is, top-level functions that don't
# call any others. No logical reason for this, but may improve clarity.
  if( !merge01) {
    level[ level==1 & ((funmat %*% rep(1,n))==0)] <- 0
    if( !sum( level==1)) # then we have 'taken the top biscuit'!
      level[level==0] <- 1
  }
  
  for( current.level in min(level):max(level)) {
    if( resequence) {
      if( current.level>1) {
  #     Position of slave 's' is based on mean position of s's callers
        slave.of <- funmat[ funs[level<current.level], funs[level==current.level],
            drop=FALSE]
        pos.order <- (x[ level<current.level] %*% slave.of) /
            (rep( 1, sum( level<current.level)) %*% slave.of)
        pos.order <- jitter( c( 0, 1, pos.order))[ -(1:2)] }
      else if( current.level==1) {
  #     Rough ordering algorithm for the top layer. The aim is to put heavy
  #     callers in the middle, light ones at either end.
        pos.order <- rank( jitter( c( -2, -1, funmat[ level==1,] %*% rep( 1, n)))[-(1:2)])
        pos.order[ pos.order %% 2==0] <-
            2*length( pos.order)-pos.order[ pos.order %% 2==0] }
      else # level 0 order is arbitrary
        pos.order <- 1:sum( level==0)

      pos.order <- order( pos.order)
    } else # if not resequence
      pos.order <- 1:sum(level==current.level)

#   Space out function names ppnl to # of letters
    if( plotmath) {
      fn <- lapply( funs[ level==current.level], function( x) parse( text=x)[[1]])
      nch <- sapply( fn, strwidth)
      charlim <- strwidth( paste( rep( 'x', charlim), collapse='')) }
    else 
      nch <- nchar( funs[ level==current.level])
    if( exists( 'minstrl', frame=sys.nframe()))
      nch <- pmax( nch, minstrl)
    nch <- cumsum( nch[ pos.order])
    x[ level==current.level][pos.order] <-
       (c(0,nch[-length(nch)]) + nch)/ (2*nch[length(nch)])
    layers <- nch[length(nch)] %/% charlim
    if( layers)
      layers <- rep( 0.1*seq( from=-layers, to=layers, by=2),
          sum( level==current.level) / (1+layers) + 1)[
          1:sum(level==current.level)]
    level[level==current.level][pos.order] <-
        level[level==current.level][pos.order] + layers
  }

  level <- 1+max(round(level))-level
})


"patch.install" <-
function(...){
  # Synonym for patch.installed
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- as.name( 'patch.installed')
  eval( mc, parent.frame())
}


"patch.installed" <-
function( pkg, character.only=FALSE, force.all.docs=FALSE, help.patch=TRUE, DLLs.only=FALSE,
    update.installed.cache=option.or.default( 'mvb.update.installed.cache', TRUE),
    pre.inst=TRUE, Rd.version=NULL, subdir=pkg){
########################  
  if( !character.only)
    pkg <- as.character(substitute( pkg))
    
  if( is.null( Rd.version))
    Rd.version <- if( getRversion() >= '2.10.0') '2' else '1'
  is.Rd2 <- numeric_version( Rd.version) >= '2'        

  if( pre.inst)
    pre.install( pkg, character.only=TRUE, force.all.docs=force.all.docs, Rd.version=Rd.version, 
        subdir=subdir)

  find.pkg <- index( search()=='package:' %&% pkg)[1]
  ipath <- if( !is.na( find.pkg))
        attr( as.environment( find.pkg), 'path')
      else if( pkg %in% loadedNamespaces())
        asNamespace( pkg)$.__NAMESPACE__.$path
      else {
        lr <- library()$results
        file.path( lr[ lr[,'Package']==pkg, 'LibPath'], pkg)
      }

  if( is.null( ipath))
stop( "Can't find path of installed package '" %&% pkg %&% "'")

  ipath <- ipath[1] # if multiple installations, then fix only topmost
  dynamic.help <- is.Rd2 && file.exists( file.path( ipath, 'help', 'paths.rds'))

  rpath <- if( pkg %in% search())
        attr( as.environment( pkg), 'path')
      else
        attr( maintained.packages[[ pkg]], 'path')

  if( is.null( rpath))
stop( "Can't find path of raw package '" %&% pkg %&% "'")

  # DLLs
  spath <- rpath
  fixup.DLLs( TRUE, ipath, rpath, pkg, use.newest=TRUE)
  
  if( DLLs.only)
return( invisible( NULL))  

  rpath <- file.path( rpath, subdir)
  
  # inst and demo files
  localfuncs( 'update.installed.dir')
  update.installed.dir( 'demo')
  if( is.dir( file.path( rpath, 'inst'))) {
    update.installed.dir( 'inst', '.', FALSE)
    inst.dirs <- unique( sub( '/[^/]+$', '', dir( file.path( rpath, 'inst'), recursive=TRUE)))
    for( id in inst.dirs)
      update.installed.dir( file.path( 'inst', id), sub( '^[^/]+/', '', id), FALSE)
  }
  
  # R functions
  if( !file.exists( from <- file.path( rpath, 'R', 'funs.rda')))
stop( "No 'funs.rda' file available for quick reinstall")

  if( file.exists( nsfile <- file.path( rpath, 'NAMESPACE'))) {
    file.copy( nsfile, file.path( ipath, 'NAMESPACE'), TRUE)
  } else
    suppressWarnings( unlink( file.path( ipath, 'NAMESPACE'))) # changed to non-NAMESPACE package!
  # Force direct use of NAMESPACE, if any
  suppressWarnings( unlink( file.path( ipath, 'Meta', 'nsInfo.rds'))) 
  
  is.rda <- file.exists( to <- file.path( ipath, 'R', 'all.rda'))
  is.rdb <- !is.rda && file.exists( file.path( ipath, 'R', pkg %&% '.rdb'))

  lazy.loading <- tools:::.read_description( file.path( ipath, 'DESCRIPTION'))[ 'LazyLoad']
  lazy.loading <- is.na( lazy.loading) | (toupper( lazy.loading) %in% c( 'Y', 'YES'))

  if( packageHasNamespace( pkg, dirname( ipath))) {
    ns <- try( asNamespace( pkg)) # will force load
    if( ns %is.a% 'try-error')
stop( "Before 'patch.install'ing a namespaced package, it should either be loaded by library, " %&% 
      "or freshly installed by R itself")
    loader.file <- 'nspackloader.R'
  } else{ 
    ns <- .GlobalEnv
    loader.file <- 'packloader.R'
  } # loader files may not be used

  if( !lazy.loading) { # Raw source
    src <- readLines( file.path( rpath, 'R', pkg %&% '.R'))
    src <- c( ".packageName <- '" %&% pkg %&% "'", src)
    cat( src, file=file.path( ipath, 'R', pkg), sep='\n')
  } else
    file.copy( file.path( R.home(), "share", "R", loader.file), file.path( ipath, 'R', pkg))

  # For some reason, R2.10 makes a pkg.rdb even if not lazy-loading
  if( is.rda | is.rdb) {
    e <- new.env()
    load( from, env=e)
    f <- find.funs( e)
    for( i.f in f) {
      g <- e[[i.f]]
      environment( g) <- ns
      e[[ i.f]] <- g
    }

    if( is.rda) # Saved image
      save( list=lsall( e), envir=e, file=to)
    else { # Lazy load
      # NB all in-package promises are forced at load-time for maintained packages, so should be no risk
      # of loading the wrong bit of the file. 
      # Should apply to importees, too, thanks to hack of importIntoEnv
      tools:::makeLazyLoadDB( e, file.path( ipath, 'R', pkg), compress=TRUE)
      .Call("R_lazyLoadDBflush", file.path( ipath, 'R', pkg %&% '.rdb'), PACKAGE = "base")
    }
    rm( e)
  }
  
  # Non-functions
  if( file.exists( from.nonfuns <- file.path( rpath, 'R', 'sysdata.rda'))) {
    # As of ~R2.9, extra data *must* be lazy-loaded
    to.nonfuns <- file.path( ipath, 'R', 'sysdata')
    e <- new.env()
    load( from.nonfuns, env=e)
    tools:::makeLazyLoadDB( e, to.nonfuns, compress=TRUE)
    .Call("R_lazyLoadDBflush", to.nonfuns %&% '.rdb', PACKAGE = "base")
    rm( e)
    # if( file.exists( to.nonfuns %&% '.rdb')) {
    # ...code above
    # } else
    #   file.copy( from.nonfuns, to.nonfuns %&% '.rda', TRUE)
  }

#  # mlazy: should be done automatically from the inst subdirectory
#  if( is.dir( rml <- file.path( rpath, 'mlazy'))) {
#    if( is.dir( iml <- file.path( ipath, 'mlazy'))) {
#      # synch files
#      olds <- dir( iml, patt='#obj[0-9]+[.]rda$')
#      news <- dir( rml, patt='#obj[0-9]+[.]rda$')
#      if( length( legacy <- (olds %except% news)))
#        file.remove( file.path( iml, olds))
#      if( length( newbies <- (news %except% olds)))
#        file.copy( file.path( rml, news %except% olds), iml)
#      
#      # Check versions
#      boths <- intersect( news, olds)
#      newv <- md5sums( file.path( rml, boths))
#      oldv <- md5sums( file.path( iml, boths))
#      if( any( newv != oldv))
#        file.copy( file.path( rml, boths[ newv != oldv]), iml)
#    } else { # new in source: some mlazy files
#      mkdir( iml)
#      file.copy( dir( rml, patt='#obj[0-9]+[.]rda$', full=TRUE), iml)
#    }
#  } else { # no mlazy in source
#    if( is.dir( iml <- file.path( ipath, 'mlazy'))) # legacy
#      unlink( iml, recursive=TRUE)
#  }

  #fixup.package.info()-- luckily done by:
  #tools:::.vinstall_package_descriptions_as_RDS( sub( '/[^/]+$', '', ipath), pkg)
  owidth <- options( width=72)
  on.exit( options( owidth))
  tools:::.install_package_description( rpath, ipath)
  if( file.exists( file.path( ipath, 'NAMESPACE')))
    tools:::.install_package_namespace_info( ipath, ipath) 

  rindex <- file.path( rpath, 'INDEX')
  iindex <- file.path( ipath, 'INDEX')
  if( !identical( md5sum( rindex), md5sum( iindex))) # OK with non-existent files
    mvb.file.copy( rindex, iindex)

  if( help.patch)
    fixup.help() # doesn't yet link properly into search system
    
  if( pkg %in% loadedNamespaces())
    fixup.exports( pkg)

  if( update.installed.cache)
    installed.packages( noCache=TRUE) # reset info
invisible( NULL)
}


"patch.package" <-
function( pkg, things, all.things=FALSE){
stopifnot( pkg %in% names( maintained.packages))

  pkenv <- maintained.packages[[ pkg]]
  attacho <- index( search()=='package:' %&% pkg)[1]
  lns <- loadedNamespaces()

  if( all.things) {
    things <- lsall( pkenv)
    unthings <- things %SUCH.THAT% (mode(pkenv[[.]]) != 'function')
    unthings <- unthings %SUCH.THAT% is.null( attr( pkenv[[.]], 'export.me'))
    things <- things %except% unthings

    things.to.kill <- originals.mp[[ pkg]] %except% things
    rm.from.pkg( pkg, what=things.to.kill)
  }

  if( pkg %in% lns) {
    nspkg <- asNamespace( pkg)
    export.envs <- lapply( getNamespaceUsers( pkg), function( x) parent.env( asNamespace( x)))
  }

  # Put into search path if:
  # (i) package is attached, and
  # (ii) obj is exportable or pkg is not namespaced
  if( !is.na( attacho)) {
    if( pkg %not.in% lns)
      attached.things <- things
    else
      attached.things <- things %SUCH.THAT% any( names( attributes( .)) %in% cq( doc, export.me))

    for( name in attached.things) {
      obj <- pkenv[[ name]]
      attributes( obj) <- attributes( obj)[ 'source'] # only
      assign( name, obj, attacho)
    } # for attached.things
  } # if attached

  # Put into namespace & importers thereof
  if( pkg %in% lns)
    for( name in things) {
      obj <- pkenv[[ name]]
      attributes( obj) <- list()

      # Environment will usually be namespace but...
      environment( obj) <- if( exists( name, nspkg, inherits=FALSE))
          environment( nspkg[[ name]])
        else if( !exists( '.__default.env__.', nspkg, inherits=FALSE))
          nspkg$.__default.env__. # undocumented
        else
          nspkg

      force.assign( name, obj, nspkg)
      for( j in export.envs)
        if( exists( name, j, inherits=FALSE))
          force.assign( name, obj, j)

      is.S3method<- !is.na( pmatch( names( .knownS3Generics) %&% '.', name))
      if( any( is.S3method))
        force.assign( name, obj, asNamespace( .knownS3Generics[ is.S3method])$.__S3MethodsTable__.)
    } # for namespaced things
}


"plot.cdtree" <-
function( x, ...) {
  foodweb( x, ...)
  invisible( x)
}


"plot.foodweb" <-
function( x, textcolor, boxcolor, xblank, border, textargs=list(), use.centres=TRUE, color.lines=TRUE, 
    poly.args=list(), expand.xbox=1.05, expand.ybox=expand.xbox*1.2, plotmath=FALSE, cex=par( 'cex'), 
    ...) {
  for( ipar in cq( boxcolor, xblank, border, textcolor))
    if( do.call( 'missing', list( ipar)))
      assign( ipar, formals( foodweb)[[ ipar]])

  oldwarn <- options( warn=-1)$warn
  oldpar <- par( mar=c(1,2,1,2), no.readonly=TRUE) # , new=FALSE)
  options( warn=oldwarn)
  on.exit( par( oldpar))
  do.call( 'par', list( ...))

  web <- x # called 'x' in arglist only to match generic 'plot'
  level <- web$level; funmat <- web$funmat; x <- web$x; funs <- names(level)
  n <- length( level)

#  if( names(dev.cur()[1])=='graphsheet') {
#    gs <- guiGetCurrMetaDoc( 'GraphSheet')
#    colortab <- guiGetPropertyValue( 'GraphSheet', Name=gs, 'ColorTable')
#    colortab <- unlist( unpaste( colortab, '|'), use=FALSE)
#    boxcolor <- background <- length( colortab)
##   Can't get background color directly as a number. Make it the negative of the first colour!
#    background.color <- 255 - as.numeric( unlist( unpaste( colortab[1], ','), FALSE))
#    colortab[ background] <- paste( background.color, collapse=',') # '255,255,255'
#    colortab <- paste( colortab, collapse='|')
#    guiModify( 'GraphSheet', Name=gs, ColorTable=colortab)
#  }

  plot( 0:1, c(min(level)-0.5, max( level)+0.5), axes=FALSE, type='n',
      xlab='', ylab='', main='')
  from <- rep( 1:n, n)[ funmat>0]
  to <- rep( 1:n, rep(n,n))[ funmat>0]
  same <- round(level[from])== round(level[to])
  if( any( same)) {
    segments( (x[from[same]]+x[to[same]])/2, level[from[same]]+0.5,
        x[ to[same]], level[ to[same]], col=if( color.lines) level[from[same]] else 1 )
    arrows( x[from[same]], level[from[same]], (x[from[same]]+x[to[same]])/2,
        level[from[same]]+0.5, #size=par('cin'), open=TRUE, works in Splus
        col=if( color.lines) level[from[same]] else 1)
    from <- from[!same]; to <- to[!same] }

# Now just the different-level calls (the vast majority). Used to have arrows
# here too, but can make for too much clutter!

  if( identical( version$language, 'R')) {
    if( plotmath)
      funs <- lapply( funs, function( x) parse( text=x)[[1]])
    sw <- sapply( funs, strwidth); sh <- sapply( funs, strheight) # works for plotmath expressions as well as text
  } else
    sw <- sh <- 0

  if( length( from)) {
    if( use.centres)
      segments( x[from], level[from], x[to], level[to], col=if( color.lines) level[from] else 1 )
    else
      segments( x[from], level[from]-sh[from]/2, x[to], level[to]+sh[to]/2, col=if( color.lines) level[from] else 1)
  }

#  arrows( x[from], level[from], (x[to]+x[from])/2,
#      (level[from]+level[to])/2, size=par('cin'), open=TRUE)

# Empty boxes for text. Doesn't work in Splus 4.0.
#  charscale <- par('1em')
#  if( is.null( charscale))
  charscale <- par( 'cxy')
  if( is.null( xblank))
    xblank <- 1
  if( identical( version$language, 'R'))
    do.call( 'rect', c( list( x-expand.xbox*sw/2, level-expand.ybox*sh/2,
        x+expand.xbox*sw/2, level+expand.ybox*sh/2, border=border, col=boxcolor), poly.args))
  else
    do.call( 'polygon', c( list( rep( x, rep( 5, n))+xblank*charscale[1]*rep( nchar( funs), rep( 5, n))*c(-1,-1,1,1,NA),
        rep( level, rep( 5, n))+0.5*charscale[2]*c(-1,0.5,0.5,-1,NA), col=boxcolor), poly.args))
  retlist <- returnList( x, level, funs)
  for( i in seq( along=x))
    text( x[i], level[i], funs[[i]], col=textcolor, cex=cex)
#  do.call( 'text', c( unname( retlist), list( col=textcolor), textargs))
  mc <- as.list( match.call( expand.dots=TRUE))
  ac <- formals( sys.function())
  not.named <- names( ac) %except% c( names( mc), '...')
  for( i in not.named)
    mc[[ i]] <- get( i)
  mode( mc) <- 'call'
  attr( retlist, 'call') <- mc
  invisible( retlist)
}


"pos" <-
function(substrs, mainstrs, any.case = FALSE, names.for.output) {
  ls <- length(substrs)
  lm <- length(mainstrs)
  .pos <- function(substr, mainstr)
  {
    ns <- nchar(substr)
    nm <- nchar(mainstr)
    if(ns > nm)
      return(0)
    mainstr <- substring(mainstr, 1:(nm - ns + 1), ns:nm)
    t <- (1:length(mainstr))[mainstr == substr]
    if(length(t) == 0)
      0
    else t
  }
  if(any.case) {
    substrs <- upper.case(substrs)
    mainstrs <- upper.case(mainstrs)
  }
  if((ls == 1) && (lm == 1))
    return(matrix(.pos(substrs, mainstrs), 1))
  if((ls %% lm) * (lm %% ls))
    warning( "Length of longer not a multiple of length of shorter")
  if(ls < lm) {
    if(missing(names.for.output))
      names.for.output <- names(mainstrs)
    substrs <- rep(substrs, (lm %/% ls) + 1)
  }
  else if(ls > lm) {
    if(missing(names.for.output))
      names.for.output <- names(substrs)
    mainstrs <- rep(mainstrs, (ls %/% lm) + 1)
  }
  else if(missing(names.for.output))
    names.for.output <- names(mainstrs)
  ls <- max(ls, lm)
  j <- vector("list", ls)
  for(i in (1:ls))
    j[[i]] <- .pos(substrs[i], mainstrs[i])
  max.n.pos <- max(sapply(j, length))
  if(max.n.pos == 1)
    jj <- matrix(unlist(j), 1)
  else {
    jj <- sapply(j, function(x, w)
    c(x, rep(0, w - length(x))), w = max.n.pos)
  }
  dimnames(jj) <- list(character(0), names.for.output)
  t(jj)
}


"pre.install" <-
function( pkg, character.only=FALSE, force.all.docs=FALSE, Rd.version=NULL, 
    subdir=pkg, ...) {
  if( !character.only)
    pkg <- as.character(substitute( pkg))

  loaded.as.task <- regexpr( '/' %&% pkg %&% '$', names( search.task.trees()) %&% '$',
      fixed=TRUE)>0
  where <- index( loaded.as.task)[1]
  if( !is.na( where))
    ewhere <- as.environment( where)
  else {
    ewhere <- maintained.packages[[ pkg]]
    if( is.null( ewhere))
stop( "Can't find raw package '" %&% pkg %&% "'")
  }

  dir. <- attr( ewhere, 'path')
  # Herewith a fudge to avoid unnecessary file-copies of mlazy objects later on
  # ... move all existing inst/mlazy/obj**.rda files into a tempdir
  # ... and 
  mlazy.temp.dir <- NULL
  if( is.dir( mlazy.inst.dir <- file.path( dir., subdir, 'inst', 'mlazy'))) {
    # Some fairly paranoid programming here
    tdctr <- 0
    while( file.exists( mlazy.temp.dir <- file.path( dir., 'temp-inst-mlazy' %&% tdctr)))
      tdctr <- tdctr + 1
    mkdir( mlazy.temp.dir)
    mlazy.OK <- FALSE # reset later if all goes well
    on.exit({
      if( !mlazy.OK) {
        suppressWarnings( mkdir( mlazy.inst.dir))
        mlazy.inst.files <- dir( mlazy.temp.dir, patt='^obj[0-9]+.rda$')
        for( fi in mlazy.inst.files)
          file.rename( file.path( mlazy.temp.dir, fi), 
              file.path( mlazy.inst.dir, fi)) # won't overwrite newer versions
      }
      unlink( mlazy.temp.dir, TRUE)
    }) # on.exit
        
    old.mlazy.files <- dir( mlazy.inst.dir, patt='^obj[0-9]+.rda$')
    for( fi in old.mlazy.files)
      file.rename( file.path( mlazy.inst.dir, fi), file.path( mlazy.temp.dir, fi))
  }
  
  unlink( file.path( dir., subdir), recursive=TRUE)
  if( !all( mkdir( file.path( dir., subdir, cq( R, man, inst)))))
stop( "couldn't make directories")

  if( file.exists( description.file <- file.path( dir., 'DESCRIPTION'))) {
    # Can't do in one step as gsub strips names
    description <- read.dcf( description.file)[1,]
    description[] <- gsub( '\n', ' ', description) 
  } else
    description <- c( Package=pkg, Title='What the package does',
        Version='1.0', Author='R.A. Fisher', Description='More about what it does',
        Maintainer='Who to complain to <yourfault@somewhere.net>',
        License='???') # adapted from 'package.skeleton'

  description <- description %without.name% 'Built'
#  description[ 'SaveImage'] <- 'yes'
#  description[ cq( LazyLoad, LazyData)] <- 'no'

  changes.file <- file.path( dir., 'changes.txt')
  changes.exists <- exists( 'changes.txt', mode='character', ewhere, inherits=FALSE)
  has.changelog <- changes.exists || file.exists( changes.file)
  if( has.changelog) {
    description[ 'ChangeLog'] <- 'inst/changes.txt'
    if( changes.exists)
      cat( ewhere$changes.txt, file=file.path( dir., subdir, 'inst', 'changes.txt'), 
          sep='\n')
    else
      mvb.file.copy( changes.file, file.path( dir., subdir, 'inst', 'changes.txt'), TRUE)
  }

  # Sometimes makefiles live in the main dir, instead of / as well as in src:
  if( length( makes.in.top <- dir( dir., patt='^Makefile')))
    mvb.file.copy( file.path( dir., makes.in.top), file.path( dir., subdir, makes.in.top))

  fixup.DLLs( TRUE, NULL, dir., subdir)

  get.nondirs <- function( x, recursive=FALSE) {
      if( is.dir( cdir <- file.path( dir., x))) {
        f <- file.path( x, dir( cdir, all.files=TRUE, recursive=recursive))
    f[ !is.dir( file.path( dir., f))] }
      else
    character( 0)
    }
  copies <- lapply( named( cq( src, data, demo, exec, tests)), get.nondirs)

  # For "inst", need to do subdirs too
  inst.copies <- get.nondirs( 'inst', TRUE)
  inst.copies <- split( inst.copies, sub( '/[^/]+$', '', inst.copies))
  copies <- c( copies, inst.copies)
  
  # Next bits from 'find.documented': should be separated into its own function
  named.in.doc <- function( doc) {
      if( is.null( doc) || !is.character( doc))
    return( character( 0))

      doc <- c( doc, ' ') # guarantees blank
      blank <- seq( along=doc) %except% grep( '[^ ]', doc)
      namelines <- doc[ 1 %upto% (min( blank)-1)] # 2: to ignore first line
      namelines <- sub( '^ +', '', namelines) # leading spaces
      namelines <- gsub( ' +[^ ]+', '', namelines) # keep first word only
      namelines <- gsub( ' *', '', namelines) # trailing spaces
      namelines
    }

  # Documented functions
  extra.docs <- lsall( ewhere, patt='\\.doc$') %SUCH.THAT% exists( ., ewhere, 
      mode='character')
  named.in.extra.docs <- unlist( lapply( extra.docs, 
      function( x) named.in.doc( ewhere[[x]])))
  # avoid mvbutils-utils
  named.in.extra.docs <- named.in.extra.docs %that.are.in% lsall( ewhere)

  # Namespace
  use.existing.NAMESPACE <- FALSE
  if( NAMESPACE.exists <- file.exists( file.path( dir., 'NAMESPACE'))) {
    nscontents <- readLines( file.path( dir., 'NAMESPACE'))
    use.existing.NAMESPACE <- any( regexpr( '^ *export(Pattern)? *\\(', nscontents) > 0)
  }
  has.namespace <- NAMESPACE.exists || exists( '.onLoad', ewhere, inherits=FALSE) ||
      !is.na( description[ 'Imports'])
  # Next line is default namespace stuff-- may not use
  forced.exports <- if( exists( 'forced!exports', ewhere, mode='character', 
        inherits=FALSE))
      ewhere$'forced!exports'
    else
      character( 0)
  nsinfo <- make.NAMESPACE( ewhere, description=description,
      more.exports=c( named.in.extra.docs, forced.exports))
  
  # *** HOOK CALLED HERE ***
  default.list <- c( copies, dll.paths, returnList(
      env=ewhere,
      extra.docs,
      description,
      has.namespace,
      use.existing.NAMESPACE,
      nsinfo,
      exclude.funs= c( 'pre.install.hook.' %&% pkg, '.First.task'),
      exclude.data=  c( lsall( ewhere, patt='\\.doc$'),
          cq( 'forced!exports', .required, tasks, .Traceback, .packageName, last.warning, 
              .Random.seed, .SavedPlots, .Last.value)),
      task.path=pkg))
  if( is.function( fphook <- ewhere[[ 'pre.install.hook.' %&% pkg]]))
    default.list <- fphook( default.list, ...)
  extract.named( default.list %without.name% cq( task.path, env))

  cat( paste( names( description), description, sep=': '),
      file = file.path( dir., subdir, 'DESCRIPTION'), sep = '\n')

  # Straight file copies:
  for( cdir in names( copies))
    if( length( cfiles <- default.list[[cdir]])) {
      mkdir( file.path( dir., subdir, cdir))
      mvb.file.copy( file.path( dir., cfiles), file.path( dir., subdir, cdir, 
          basename( cfiles)), TRUE)
    }
    
  # Demo index
  if( is.dir( demo.dir <- file.path( dir., subdir, 'demo')) && 
      !file.exists( file.path( demo.dir, '00Index'))) {
    # make one!
    demos <- dir( demo.dir, patt='\\.(r|R)$')
    first.comment <- function( f) {
      txt <- readLines( file.path( demo.dir, f))
      hash <- grep( '^#', txt, value=TRUE)[1]
      if( !is.na( hash))
        stuff <- sub( '# +', '', hash)
      else
        stuff <- 'Demo of ' %&% sub( '\\.(r|R)$', '', basename( f))
    return( stuff)
    }
    demo.lines <- sapply( demos, first.comment)
    cat( paste( sub( '\\.(r|R)$', '', demos), demo.lines, sep='\t'), 
        file=file.path( demo.dir, '00Index'), sep='\n')
  }
  
  # Zap inst if empty, otherwise R 2.10 complains...
  if( !length( dir( file.path( dir., subdir, 'inst'), all.files=TRUE) %except% c( '.', '..')))
    unlink( file.path( dir., subdir, 'inst'), recursive=TRUE)
    
  # DLLs
  if( length( dll.paths)) {
    mkdir( file.path( dir., subdir, 'inst', 'libs'))
    mvb.file.copy( dll.paths, file.path( dir., subdir, 'inst', 'libs', names( dll.paths)))
  }

  # Augment functions to include all that are named in each others aliasses
  funs <- find.funs( ewhere) %except% exclude.funs
  # Search env for functions: son of ewhere so on-the-fly changes can go there
  ewhereson <- new.env( parent=ewhere) 
  
  # mlazy objects, and code to auto-load them (involves hacking .onLoad or .First.lib
  mlazies <- mcachees( ewhere) %except% c( '.Random.seed', exclude.data)
  if( length( mlazies)) {
    mkdir( file.path( dir., subdir, 'inst', 'mlazy'))
    objfiles <- 'obj' %&% attr( ewhere, 'mcache')[ mlazies] %&% '.rda'
    md5new <- sapply( file.path( dir., 'mlazy', objfiles), md5sum)
    if( !is.null( mlazy.temp.dir)) { 
      md5old <- sapply( file.path( mlazy.temp.dir, objfiles), md5sum)
  #    fsnew <- file.info( file.path( dir., 'mlazy', objfiles))
  #    fsold <- file.info( file.path( dir., subdir, 'inst', 'mlazy', objfiles)) # some may not exist
  #    different.file <- (fsnew$size != fsold$size) | (fsnew$mtime != fsold$mtime)
      different.file <- md5old != md5new
      different.file[ is.na( different.file)] <- TRUE
    } else
      different.file <- rep( TRUE, length( objfiles))
    if( any( different.file))
      mvb.file.copy( file.path( dir., 'mlazy', objfiles[ different.file]), 
          file.path( dir., subdir, 'inst', 'mlazy', objfiles[ different.file]))
    for( fi in objfiles[ !different.file])
      file.rename( file.path( mlazy.temp.dir, fi), file.path( dir., subdir, 'inst', 'mlazy', fi))
    mlazy.OK <- TRUE # files sorted out
    
    if( has.namespace) {
      wot.env <- 'environment( sys.function())'
      wot.fun <- '.onLoad'
    } else {
      wot.env <- 'as.environment( "package:" %&% pkgname))'
      wot.fun <- '.First.lib'
    }

    plb <- substitute( nsenv <- wot.env, list( wot.env=parse( text=wot.env)[[1]]))
    for( i in mlazies)
      plb <- c( plb, substitute( delayedAssign( x=i, {
          load( file.path( libname, pkgname, 'mlazy', objfile), nsenv)
          nsenv[[ i]] 
        }, assign.env=nsenv, eval.env=environment()), 
        returnList( i, objfile='obj' %&% attr( ewhere, 'mcache')[i] %&% '.rda')))
    
    # Ensure these data are unlocked, so that they can be loaded.
    # ...use 'dont.lockBindings' mechanism but ensure package is indept of mvbutils
    dlb <- dont.lockBindings
    sho <- setHook.once
    environment( dlb) <- environment( sho) <- .GlobalEnv
    plb <- c( plb, substitute( {
        dont.lockBindings <- dlb
        setHook.once <- sho}, returnList( dlb, sho)))
    plb <- c( plb, substitute( dont.lockBindings( mlazies, pkgname), 
        list( mlazies=mlazies)))

    loader <- get( wot.fun, ewhere)
    if( is.null( loader))
      loader <- function( libname, pkgname) NULL
      
    # Now have to prepend plb to body of loader
    # ...not easy
    thing <- quote( {a})
    for( i in seq_along( plb))
      thing[[ i+1]] <- plb[[i]]
    body( loader) <- call( '{', thing, body( loader))
    assign( wot.fun, loader, envir=ewhereson)
    mlazy.OK <- TRUE
  }

  # Source code:
  ff <- function( x) {
    cat( '\n"', x, '" <-\n', sep='', file=rfile, append=TRUE)
    fx <- get( x, ewhereson, inherits=TRUE) # ewhere[[ x]] is broken
    if( is.function( fx)) {
      attributes( fx) <- attributes( fx) %without.name% 'doc'
      write.sourceable.function( fx, rfile, append=TRUE, doc.special=FALSE)
#      if( has.namespace)
#        environment( fx) <- asNamespace(
#      e[[ x]] <- fx
    } else
      print( fx)
  }

  suppressWarnings( 
    file.path( dir., subdir, 'R',
        dir( file.path( dir., subdir, 'R'), all.files=TRUE))) # clean out oldies
  rfile <- file.path( dir., subdir, 'R', pkg %&% '.R')
  # cat( '.packagename <- "', pkg, '"\n', sep='', file=rfile)
  cat( '# This is package', pkg, '\n', file=rfile)
  sapply( funs, ff)

  # Non-functions:
  extra.data <- lsall( ewhere) %except% c( find.funs( ewhere), exclude.data, mlazies)
  if( length( extra.data))
    save( list=extra.data, file=file.path( dir., subdir, 'R', 'sysdata.rda'), envir=ewhere,
        compress=TRUE)

  # Save file ready for patch.installed
  e <- new.env() # will hold stuff to save for patch.installed
  # ... did have 'parent=ewhere' but parent( ewhere)==EmptyEnv for maintained package so..?
  source.mvb( rfile, env=e)
  # e$'original!object!list' <- c( funs, extra.data, mlazies)
  e$.packageName <- pkg # ready for quick install
  save( list=lsall( e), file=file.path( dir., subdir, 'R', 'funs.rda'), envir=e, 
      compress=TRUE)
  rm( e)
  
  # Try to tell RCMD not to build "funs.rda" into package... doesn't seem to work
  if( file.exists( RBI <- file.path( dir., '.Rbuildignore')))
    mvb.file.copy( RBI, file.path( dir., subdir, '.Rbuildignore'))
  cat( c( '[.]/R/funs[.]rda', ''), file=file.path( dir., subdir, '.Rbuildignore'), append=TRUE)

  # Obsolete: Object list, used by maintain.packages-- funny name to avoid clashing
  # cat( '\n`original!object!list` <-', deparse( c( funs, extra.data)), '', sep='\n',
  #    file=rfile, append=TRUE)

  # Documentation:
  # Code is set to only update files if they've changed. However, they're *all* deleted anyway
  # ...by the unlink( recruvsive=TRUE) above
  
  doc2Rd.info.file <- file.path( dir., 'doc2Rd.info.rda')      
  if( !force.all.docs && file.exists( doc2Rd.info.file))
    load( doc2Rd.info.file) # creates doc2Rd.info
  else
    doc2Rd.info <- list()

  # Check for handwritten Rd files-- must live in a subdir called Rd
  Rd.files.to.keep <- character(0)
  if( is.dir( Rd.dir <- file.path( dir., 'Rd'))) {
    existing.Rd.files <- dir( Rd.dir, patt='\\.Rd$', all.files=TRUE)
    Rd.files.to.keep <- existing.Rd.files
    Rd.already <- character( 0)
    mvb.file.copy( file.path( Rd.dir, existing.Rd.files),
        file.path( dir., subdir, 'man', existing.Rd.files), TRUE)
    for( i in existing.Rd.files) {
      rl <- readLines( file.path( Rd.dir, i))
      docced <- rl %that.match% c( '^\\name\\{', '^\\alias\\{')
      Rd.already <- c( Rd.already, sub( '.*\\{([^}])\\}.*', '\\1', rl))
    }
  } else
    Rd.already <- character(0)

  # The point here is to avoid calling the Rdconv function if the requisite output already exists
  # Rdconv will basically be doc2Rd but can vary slightly depending on how get.updated.Rd is called
  get.updated.Rd <- function( docname, new.docco, Rdconv, ...) {
      if( !identical( new.docco, doc2Rd.info[[ docname]]$docattr)) {
        Rd <- Rdconv( ...)
        doc2Rd.info[[ i]] <<- if( Rd %is.not.a% 'try-error')
            list( docattr=new.docco, Rd=Rd)
          else 
            NULL
      } else # no change
        Rd <- doc2Rd.info[[ docname]]$Rd
      Rd
    }

  provisionally.add.man.file <- function( docname, text, fname) {
    new.md5 <- doc2Rd.info[[ docname]]$md5
    already <- file.path( dir., subdir, 'man', fname)

    if( !force.all.docs && !is.null( new.md5) && !is.na( new.md5) && 
        file.exists( already)) {
      do.write <- md5sum( already) != new.md5
    } else
      do.write <- TRUE
    
    if( do.write) {
      cat( text, file=already, sep='\n')
      doc2Rd.info[[ docname]]$md5 <<- md5sum( already)
    }
    
    Rd.files.to.keep <<- c( Rd.files.to.keep, fname)
  }  

  alldoc <- find.documented( ewhere, doctype='Rd', only.real.objects=FALSE)

  docfuns <- (funs %except% Rd.already) %that.are.in% 
      find.documented( ewhere, doctype='own')
  for( i in docfuns) {
    geti <- ewhere[[i]]
    Rd <- get.updated.Rd( i, attr( geti, 'doc'), doc2Rd, geti, Rd.version=Rd.version, 
        def.valids=alldoc)
    if( Rd %is.not.a% 'try-error') {
      fname <- sub( '\\}', '', sub( '\\\\name\\{', '', Rd[1])) %&% '.Rd'
      if( length( grep( '^\\.', fname)))
        fname <- '01' %&% fname
      provisionally.add.man.file( i, Rd, fname)
    }
  }

  if( !has.namespace) {
    Rdconv.internals <- function() doc2Rd( make.internal.doc( undoc.funs, pkg), Rd.version=Rd.version)
    undoc.funs <- funs %except% c( find.documented( ewhere, doctype='any'),
        cq( .First.lib, .Last.lib, .onLoad, .onAttach))
    if( length( undoc.funs)) {
      raw.undocco <- unlist( lapply( cq( mlazy, cd), 
          function( x) clip( deparse( args( get( x))))))
      Rd.undoc <- get.updated.Rd( pkg %&% '-internal', raw.undocco, Rdconv.internals)
      provisionally.add.man.file( pkg %&% '-internal', Rd.undoc, pkg %&% '-internal.Rd')
    }
  }

  # Could possibly check for clash with Rd.already, but will assume user's brain is working
  for( i in extra.docs) {
    geti <- ewhere[[ i]]
    Rd.extra <- get.updated.Rd( i, geti, doc2Rd, geti, Rd.version=Rd.version, def.valids=alldoc)
    # For package doc, put 00 first to get indexing right...
    # ...and change . into -
    docname <- if( length( grep( '\\.package\\.doc$', i)))
        '00' %&% sub( '\\.', '-', sub( '\\.doc$', '', i))
      else
        sub( '\\.doc$', '', i)
    provisionally.add.man.file( i, Rd.extra, docname %&% '.Rd')        
    # cat( file=file.path( dir., subdir, 'man', docname %&% '.Rd'), Rd.extra, sep='\n')
  }
  suppressWarnings(   file.remove( file.path( dir., subdir, 'man',
        dir( file.path( dir., subdir, 'man'), all.files=TRUE) %except% Rd.files.to.keep)))

  save( doc2Rd.info, file=doc2Rd.info.file)

  if( has.namespace) {
    if( use.existing.NAMESPACE)
      mvb.file.copy( file.path( dir., 'NAMESPACE'), file.path( dir., subdir, 'NAMESPACE'), TRUE)
    else
      write.NAMESPACE( nsinfo, file.path( dir., subdir, 'NAMESPACE'))
  }

  # Index last, so it looks up-to-date for RCMD BUILD  
  index.file <- file.path( dir., subdir, 'INDEX')
  Rdindex( file.path( dir., subdir, 'man'), index.file)
  # Put the ***-package file first, if it exists
  index.stuff <- scan( index.file, what='', sep='\n', quiet=TRUE)
  if( !is.na( i <- grep( '^' %&% pkg %&% '\\-package', index.stuff)[1]))
    cat( index.stuff[ c( i, (1:length( index.stuff)) %except% i)], sep='\n',
        file=index.file)

  invisible( NULL)
}


"pre.install.hook.mvbutils" <-
function( default.list) {
  # Just for demo purposes really; its only role is to include itself in the package!
  default.list$exclude.funs <- default.list$exclude.funs %except% 'pre.install.hook.mvbutils'
  default.list
}


"prepare.for.move" <-
function( path) {
  if( is.environment( path)) { # maintained.packages$packagename
    saving <- NA # used to have TRUE, which forced auto-save; bit bossy
    env <- path
    path <- attr( env, 'path')
  } else {
    found.me <- function( x) (!is.null( spath <- attr( as.environment( x), 'path')) && spath==path)

    env <- index( sapply( 1:length( search()), found.me))[1]
    if( found <- !is.na( env)) 
      env <- as.environment( env)
    else if( length( maintained.packages)) {
      env <- index( sapply( maintained.packages, found.me))[1]
      if( found <- !is.na( env))
        env <- maintained.packages[[ env]]
    }

    if( !found) {
      env <- new.env()
      attr( env, 'path') <- path
      load.refdb( file=file.path( path, '.RData'), envir=env)
      saving <- TRUE
    } else
      saving <- if( path != .Path[ length( .Path)]) NA else FALSE # don't explicitly save globalenv
  }
  
  obj <- lsall( envir=env)
  list( env=env, saving=saving, obj=obj, path=path)
}


"print.cat" <-
function( x, ...) { cat( x, sep='\n'); invisible( x) }


"print.cdtree" <-
function( x, ...) {
  levs <- round( x$level)
  max.lev <- max( levs)
  indents <- sapply( split( names( levs), levs), function( nn) max( nchar( nn)))
  indents <- cumsum( c( 0, rev( indents[-1])+1))
  indents <- sapply( indents, function( x) paste( rep( ' ', x), collapse=''))
  indents <- rev( indents)[ levs] %&% names( levs)
  cat( indents, sep='\n')
  invisible( x)
}


"print.docattr" <-
function (x, ...) 
  cat("# FLAT-FORMAT DOCUMENTATION\n")


"print.nullprint" <-
function( x, ...) 
  NULL


"print.pagertemp" <-
function( x, ...) {
  file.show( x, title="mvbutils-style informal help on '" %&% names( x) %&% "'", delete.file=TRUE)
  put.in.session( just.created.window=TRUE)
  invisible( x)
}


"promote.2.to.1" <-
function () {
    full.path <- attr(pos.to.env(2), "path")
    detach(2)
    load.mvb( file = file.path( full.path, '.RData'), name=names( full.path), pos=1, path=full.path)
    env <- .GlobalEnv
    attr(env, "path") <- full.path
}


"put.in.session" <-
function (...) 
{
    orig.mc <- mc <- as.list(match.call())[-1]
    if (length(mc)) {
        if (is.null(names(mc))) 
            which <- 1:length(mc)
        else which <- names(mc) == ""
        for (i in index(which)) if (is.symbol(orig.mc[[i]])) 
            names(mc)[i] <- as.character(orig.mc[[i]])
        mc <- lapply(mc, eval, envir = parent.frame())
        for (i in 1:length(mc)) assign(names(mc)[i], mc[[i]], 
            pos = "mvb.session.info")
    }
    invisible(NULL)
}


"read.bkind" <-
function( where=1) {
  dir <- get.path.from.where( where)
  files <- object.names <- character( 0) # in case can't find
  
  index.file <- file.path( dir, '.Backup.mvb/index')
  if( file.exists( index.file)) {
    ow <- options( warn=-1)
    files <- readLines( index.file)
    options( ow)
    files <- files[ substr( files, 1, 2)=='BU']
  }
  
  if( length(files)) {
    object.names <- strsplit( files, '=', fixed=TRUE)
    files <- sapply( object.names, '[', 1)
    object.names <- lapply( object.names, '[', -1)
    object.names <- sapply( object.names, paste, collapse='=') # e.g. for something called 'per=verse'
 }

  returnList( files, object.names)
}


"readLines.mvb" <-
function( con = stdin(), n = -1, ok = TRUE, EOF=as.character( NA)) {
  if( is.character( con))
    con <- file( con)
    
  if( !isOpen( con, 'r')) {
    open( con, open='r')
    on.exit( close( con))
  }
    
  n[ n < 0] <- Inf 
  answer <- character( 0)  
  lines.read <- 0
  
  while( lines.read < n) { 
    new.answer <- readLines( con=con, n=1, ok=TRUE)
    if( !length( new.answer) || match( new.answer, EOF, 0))
  break
    lines.read <- lines.read + 1
    answer <- c( answer, new.answer)
  }
  
  answer
}


"readr" <-
function( x, ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$fixing <- FALSE
  mc$new <- FALSE
  mc[[1]] <- quote( fixr)
  eval( mc, parent.frame())
}


"reattach.placeholder" <-
function (sn, nlocal = sys.parent()) mlocal({
  was.attached <- index(search() == "PLACEHOLDER:" %&% sn)[1]
  if (!is.na(was.attached)) {
    filename <- get.image.filename( attr( .GlobalEnv, 'path'))
    detach(pos = was.attached)
    load.mvb( filename, name = "package:" %&% sn,
        pos = was.attached, attach.new = TRUE, path = .Path[length(.Path)])
  }
})


"reduce.empty.links" <-
function( nlocal=sys.parent()) mlocal({
  # Rather pointless since empty links are harmless
  
  # Shouldn't be any occurrences of \link{} except to avoid Rd bugs...
  # ... because \link{} itself would appear as something different!
  
  # I *think* that...
  # ... if character after \link{} is not special, we can delete the \link{} 
  # ... do it sequentially, deleting last one in each line in turn
  
  # Note that \code is always a risky thing, so "c" is treated as special here
  
  mtlinx <- seq_along( Rd)
  repeat{ 
    mtlinx <- mtlinx[ grep( '\\\\link\\{\\}([^c{\\\\%]|$|\\\\\\})', Rd[ mtlinx])]
    if( !length( mtlinx))
  break
    Rd[ mtlinx] <- sub( '\\\\link\\{\\}([^c{\\\\%]|$|\\\\\\})', '\\1', Rd[ mtlinx])
  }
})


"remove.from.package" <-
function( ...) { # identical to rm.pkg
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( rm.pkg)
  eval( mc, sys.parent())
}


"restore.all.backups" <-
function( where=2) {
  for( f in find.funs( where)) { 
    cat( f, '\n')
    bk <- get.backup( f, where)[[1]]
    if( length( bk)) {
      bk <- replace.underscores( bk)
      tc <- textConnection( bk)
      fun <- try( list( source.mvb( tc)))
      if( fun %is.not.a% 'try.error') {
        fun <- fun[[1]]
        environment( fun) <- .GlobalEnv
        assign( f, fun, envir=.GlobalEnv)
      }
      close( tc)
    }
  }
}


"returnList" <-
function( ...) { 
# Returns its arguments; unnamed arguments are named using deparse & substitute
# Does what the deprecated version of 'return' used to do before R 1.8
  orig.mc <- mc <- as.list( match.call())[ -1]

  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=parent.frame())
    else { # multiple arguments, so return as named list
      if( is.null( names( mc)))
        which <- 1:length( mc)
      else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=parent.frame())
    }
  }
  
  mc
}


"rm.pkg" <-
function( pkg, ..., list=NULL) {
  if( is.null( list))
    list <- sapply( match.call( expand.dots=FALSE)$..., as.character)

  if( is.null( list)) # nothing to do-- can happen in patch.package
return()

  if( is.environment( pkg)) {
    pkenv <-  pkg
    pkg <- attr( pkenv, 'name')
  } else {
    pkenv <- maintained.packages[[ pkg]]
  }
  suppressWarnings( rm( list=list, envir=pkenv)) # that bit was easy
  maybe.save.after.move( list( env=pkenv, path=attr( pkenv, 'path'), saving=NA))

  attacho <- index( search()=='package:' %&% pkg)[1]
  if( !is.na( attacho))
    suppressWarnings( rm( list=list, envir=pos.to.env( attacho)))

  lns <- loadedNamespaces()
  if( pkg %in% lns) {
    nspkg <- asNamespace( pkg)
    suppressWarnings( rm( list=list, envir=nspkg))

    exlist <- list %that.are.in% lsall( nspkg$.__NAMESPACE__.$exports)
    if( length( exlist)) {
      # Import envs are locked, so can't remove
      # Could possibly hack round that with 'hack.lockEnvironment' but hard & ?dangerous?
      # Can't use active binding instead of existing binding either
      # Best is to use delayedAssign to try to fetch the object from baseenv
      gnu <- getNamespaceUsers( pkg)
      impenvs <- lapply( named( gnu), function( x) parent.env( asNamespace( x)))
      impls <- lapply( impenvs, ls)
      impacks <- rep( gnu, sapply( impls, length))
      impls <- unlist( impls, use.names=FALSE)
      for( x in exlist %that.are.in% impls) {
        for( impenv in impenvs[ impacks[ impls==x]]) {
          if( bl <- bindingIsLocked( x, impenv)) # should be locked
            unlockBinding( x, impenv)
          do.call( 'delayedAssign', list( x=x, value=substitute( get( x, baseenv()), list( x=x)),
              eval.env=baseenv(), assign.env=impenv))
          if( bl)
            lockBinding( x, impenv)
        } # for ihas in has
      } # for x in exlist...
    } # if length exlist

    # meths <- pmatch( names( .knownS3Generics) %&% '.', list, dup=TRUE)
    # For now, just zap methods known to base

    suppressWarnings( rm( list=list, envir=baseenv()$.__S3MethodsTable__.))
  }
}


"safe.rbind" <-
function( df1, df2) {
  # In R, can hit problems when vars take all-NA or "numeric" values in one df, but character values in the other
  if( is.null( df1))
return( df2)
  if( is.null( df2))
return( df1)

  fac1 <- sapply( df1, is.factor)
  fac2 <- sapply( df2, is.factor)
  if( any( fac2 & !fac1))
    df1[ fac2 & !fac1] <- lapply( df1[ fac2 & !fac1], factor)
  if( any( fac1 & !fac2))
    df2[ fac1 & !fac2] <- lapply( df2[ fac1 & !fac2], factor)  
  rbind( df1, df2)
}


"Save" <-
function() {
  Save.pos( 1)
}


"save.mchanged" <-
function( objs, envir) {
  path <- attr( envir, 'path')
  mcache <- omcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names(.) %in% lsall( envir))

  changed.objs <- objs %such.that% (mcache[.]<0)
  if( length( changed.objs) || length( mcache)<length(omcache)) {
    if( option.or.default( 'mlazy.subdir', TRUE)) {
      dir.create( file.path( path, 'mlazy'), showWarnings=FALSE)
      objpath <-  file.path( 'mlazy', 'obj') }
    else 
      objpath <- 'obj'
      
#   e <- new.env() # looks as if 'e' is unnecessary-- acbins get saved as normal objects
    for( i in changed.objs) 
      save( list=i, file=file.path( path, objpath %&% -mcache[ i] %&% '.rda'),
          envir=envir, compress=TRUE)

    mcache[ changed.objs] <- -mcache[ changed.objs]
    if( option.or.default( 'mlazy.index', FALSE))
      cat( names( mcache) %&% '\t' %&% mcache, sep='\n', file= objpath %&% '.ind')
  }

  attr( envir, 'mcache') <- mcache
}


"Save.pos" <-
function (pos, path, ascii = FALSE) {
  set.pos.and.path()
#    on.exit(save.pos(pos)) # in R2.0, can't safely default to this

  if ("mvb.session.info" %!in% search()) {
    warn("Can't find session info")
return(invisible(NULL))
  }

  if( ('package:debug' %in% search()) && exists( 'tracees', 'package:debug')
      && length( pos.tracees <- check.for.tracees( pos))) {
    retracees <- pos.tracees %that.are.in% names( tracees)
    restoro <- sapply( named( retracees), get, envir=pos)
    temp.unmtraced <- tracees[ retracees]
    on.exit( {
      for( fname in retracees)
        lapply( retrace.envs[[ fname]], assign, x=fname, value=restoro[[ fname]])
      tp <- asNamespace( 'debug')$tracees # debug:::tracees annoys RCMD CHECK...
      tp[ retracees] <- temp.unmtraced
      assign( 'tracees', tp, 'package:debug') # does namespace version as well!
    })
    
    # Now untrace them, and store the environment(s) containing (un)traced functions...
    # ... this may not be 'pos' if there is namespacing
    retrace.envs <- lapply( named( retracees), mtrace, fname=NULL, tracing=FALSE, 
        return.envs=TRUE) # fname=NULL forces char.fname
        
    # Now functions that are in a debugged state, but that debug has forgotten...    
    lapply( pos.tracees %except% retracees, mtrace, fname=NULL, tracing=FALSE, 
        return.envs=FALSE) # fname=NULL forces char.fname
        
  }

  save.refdb( file=file.path( path, '.RData'), pos)
  if( !is.null( option.or.default( 'backup.fix', NULL)))
    create.backups( pos)

  return(invisible(NULL))
}


"save.refdb" <-
function( file, envir, ...) {
  envir <- as.environment( envir)

  if( missing( file)) {
    path <- attr( envir, 'path')
    if( !is.dir( path))
      mkdir( path)
    file <- file.path( path, '.RData')
  } else
    path <- dirname( file)

  mcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names( .) %in% lsall( envir))
  attr( envir, 'mcache') <- mcache

  # Housekeep dead files

  mpath <- attr( envir, 'path')
  if( option.or.default( 'mlazy.subdir', TRUE))
    mpath <- file.path( mpath, 'mlazy')
  if( is.dir( mpath)) {
    objfiles <- list.files( mpath, '^obj[0-9]+\\.rda$')
    file.remove( file.path( mpath, objfiles %except% ('obj' %&% mcache %&% '.rda')))
  }

  # Save into temporary file and keep old one, in case of stuff-up
  # Changed Sept 07 so safety-check works even without mcached objects
  new.file <- file
  if( file.exists( file)) {
    while( file.exists( new.file))
      new.file <- file.path( dirname( new.file), 'n' %&% basename( new.file))
  }

  # Check for ..mypackage accidentally stored here...
  badness <- lsall( envir) %that.match% '^[.][.][^.]'
  badness <- badness %SUCH.THAT% is.environment( envir[[.]])
  badness <- badness %SUCH.THAT% (all( cq( path, name, task.tree) %in% 
      names( attributes( envir[[.]]))))
  if( length( badness))
    warning( "Not saving '" %&% paste( badness, collapse="', '") %&% "' which didn't ought to be here...")

  if( length( mcache)) {
    cache.name <- get.mcache.store.name( envir) %&% '0' # guaranteed not to exist & to be findable
    e <- new.env( parent=envir)
    assign( cache.name, abs( mcache), e) # avoid assigning into envir
    # was.there etc check if any changes have been made. If not, leave original file...
    # strictly unchanged datewise.
    
    ans <- save( list = c( cache.name, lsall( envir=envir) %except% c( names( mcache), badness,
        dont.save())), file=new.file, envir=e, ...)
    rm( e) # ?not necessary?

    save.mchanged( names( mcache), envir)
  } else
    ans <- save( list=lsall( envir=envir) %except% c( badness, dont.save()), file=new.file,
        envir=envir, ...)

  if( new.file != file) {
    checksums <- md5sum( c( file, new.file))
    if( checksums[1]==checksums[2])
      file.remove( new.file)
    else {
      file.remove( file)
      file.rename( from=new.file, to=file)
    }
  }

  ans
}


"search.for.regexpr" <-
function( pattern, where=1, lines=FALSE, doc=FALSE, ...) {
  get.source <- if( doc) {
    function( f) 
      as.character( if( is.function( f)) attr( f, 'doc') else if( is.character( f)) f else NULL)
  } else {
    function( f) {
      if( !is.null( source <- attr( f, 'source')))
        source
      else
        deparse( f)
    }
  } # if doc

  found <- function( f, pattern, where) {
    f <- get.source( get( f, envir=where))
    any( regexpr( pattern, f, ...) != -1) 
  }

  search.one <- function( where) {
    ff <- find.funs( where)
    if( doc)
      ff <- c( ff, (lsall( where) %except% ff) %that.match% "\\.doc$")
    if( length( ff)) {
      successful <- sapply( ff, found, pattern=pattern, where=as.environment( where))
      ff <- ff[ successful] 
    }
    
    ff
  }
  
  if( is.environment( where))
    where <- list( where)
  answer <- lapply( where, search.one)
  if( is.numeric( where) || is.character( where))
    names( answer) <- search()[ where]
    
  has.some <- sapply( answer, length)>0
  
  if( lines) {
    for( e in index( has.some))
      answer[[ e]] <- lapply( named( answer[[ e]]), 
        function( x) grep( pattern, get.source( get( x, envir=where[[e]])), value=TRUE, ...))
  }
  
  answer[ has.some]
}


"search.task.trees" <-
function(){
  tasks <- lapply( seq( along=search()), function( x) names( attr( pos.to.env( x), 'path')[1]))
  taski <- index( sapply( tasks, is.character))
  tasks <- unlist( tasks)
  task.trees <- sapply( 1:length( tasks), function( x) paste( tasks[length( tasks):x], collapse='/'))
  names( taski) <- task.trees
  taski
}


"Seconds" <-
function( x){
  if( !is.null( attr( x, 'units')))
    x <- switch( attr(x, "units"), secs = x, mins = 60 * x, 
        hours = 60 * 60 * x, days = 60 * 60 * 24 * x, 
        weeks = 60 * 60 * 24 * 7 * x)
  attr( x, 'units') <- 'secs'
  x
}


"set.path.attr" <-
function (env, the.path, task.name = character(0)) 
{
    if (length(task.name)) 
        names(the.path) <- task.name
    attr(env, "path") <- the.path
}


"set.pos.and.path" <-
function (nlocal = sys.parent()) mlocal({
  pos <- as.environment( pos)
  if (missing(path)) {
    path <- attr( pos, "path")
    if (is.null(path))
    {
      cat("No obvious place to save it. What is the filename (single forward slashes only please)? ")
      path <- readline()
    }
  }

  path
})


"set.presave.hook.mvb" <-
function( hook, set=TRUE){
  if( set)
    presave.hooks <<- c( presave.hooks, list( hook))
  else {
    which <- lapply( presave.hooks, hook, identical)
    presave.hooks <<- presave.hooks[ !which]
  }
NULL
}


"set.test" <-
function (a, b) 
{
    r <- range(a - b)
    if (all(r == c(-1, 0))) 
        -1
    else if (all(r == c(0, 1))) 
        1
    else 0
}


"setHook.once" <-
function( pkg, hook.type, f, action=c( 'append', 'prepend', 'replace')){
  # the WEIRD thing here is that if I use cq instead of c in the args, R can't find it...
  identical.to.f <- function( x) {
    y <- x
    attr( y, '.Environment') <- NULL
    identical( y, f) }
  mangle <- packageEvent( pkg, hook.type)
  hooks <- getHook( mangle)
  if( !any( sapply( hooks, identical.to.f))) {
    action <- match.arg( action)
    setHook( mangle, f, action)
  }
}


"setup.mcache" <-
function( envir, fpath=attr( envir, 'path'), refs) {
  envir <- as.environment( envir)

  mcache <- attr( envir, 'mcache') # usually NULL & overwritten by next bit
  if( missing( refs)) {
    cache.name <- get.mcache.store.name( envir)
    if( !exists( cache.name, envir=envir, inherits=FALSE))
return() # nothing to do; pre-mcache DB
    mcache <- get( cache.name, envir)
    refs <- names( mcache)
    remove( list=cache.name, envir=envir)
  }

  if( !length( refs)) # post-mcache nothing to do
return()

  objpath <- 'obj'
  if( option.or.default( 'mlazy.subdir', TRUE)) {
    # Back-compatibility tedious here; move files if in wrong place
    files.to.move <- (objpath %&% mcache[ refs] %&% '.rda') %such.that% (
        file.exists( file.path( fpath, .)) & !file.exists( file.path( fpath, 'mlazy', .)))
    # Normally, next if won't happen    
    if( length( files.to.move)) {
      dir.create( file.path( fpath, 'mlazy'), showWarnings=FALSE) # harmless fail if exists
      file.rename( file.path( fpath, files.to.move), file.path( fpath, 'mlazy', files.to.move))
    }
    objpath <- file.path( 'mlazy', 'obj')
  }
  
  # Create promises to load
  remove( list=refs %such.that% (. %in% lsall( envir=envir)), envir=envir) # only needed with 'move'
  for( i in refs) {
    objfile <- file.path( fpath, objpath %&% mcache[ i] %&% '.rda')
    if( !file.exists( objfile)) {
      warning( 'Can\'t find file "' %&% objfile %&% '"; deleting object "' %&% i %&% '"')
      mcache <- mcache %without.name% i
    } else {
      fx <- get.mcache.reffun( i, envir)
      efx <- environment( fx)
      # For efficiency (?), do this via promise, rather than directly coding 'if(!loaded)' in fx
      subbo <- substitute( { load( file, e); e[[i]]}, list( e=efx, file=objfile, i=i))
      do.call( 'delayedAssign', list( x=i, value=subbo, eval.env=efx, assign.env=efx))
      suppressWarnings( makeActiveBinding( i, fx, envir))
    }
  }

  attr( envir, 'mcache') <- mcache
}


"source.mvb" <-
function( con, envir=parent.frame(), max.n.expr=Inf, echo=option.or.default( 'verbose', FALSE),
    prompt.echo=getOption( 'prompt'), evaluate=TRUE) {
  if( !exists( 'source.list', 'mvb.session.info'))
    source.list <- list()
  else
    source.list <- get( 'source.list', 'mvb.session.info')
  if( is.character( con))
    con <- file( con)

  source.list[[ length( source.list)+1]] <- con
  put.in.session( source.list=source.list)
  if( !isOpen( con)) {
    open( con, 'r') # if you want fancy options on e.g. blocking, you need to pre-open 'con'
    on.exit( try( close( con)))
  }

  on.exit( { put.in.session( source.list=clip( source.list)) },
      add=TRUE)

  orig.line <- 0

  ow <- options( warn=-1)
  on.exit( options( ow), add=TRUE)

  expr.count <- 1
  while( expr.count <= max.n.expr) {
    # Loop until EOF or a non-blank line
    repeat{
      check.EOF <- readLines( con, n=1, ok=TRUE)
      if( !length( check.EOF) || nchar( check.EOF))
    break
    }

    if( !length( check.EOF))
  break
    pushBack( check.EOF, con)

#    cat( 'Con =', seek( con)); print( con)
#    cat( 'Inc=', isIncomplete( con), '\n')

    tryo <- try( list( parse( file=con, n=1)), silent=TRUE)
    if( tryo %is.a% 'try-error') {
#      print( readLines( con))
      if( echo)
        cat( "parse error; not echoing expression\n")
      errline <- as.numeric( rev( strsplit( geterrmessage(), ' ')[[1]])[1])
      if( !is.na( errline))
stop( "parse error in line " %&% errline, call.=FALSE)
      else
stop( geterrmessage(), call.=FALSE)
    }

    if( echo) {
      dp <- unlist( lapply( tryo[[1]], deparse), use.names=FALSE)
      dp[ 1] <- prompt.echo %&% dp[1]
      dp[ 2 %upto% length( dp)] <- getOption( 'continue') %&% dp[ 2 %upto% length( dp)]
      cat( '', dp, sep='\n')
    }


#    Experimental code to only evaluate if it seems "useful"-- probably not a good idea
#    do.eval <- !is.na( evaluate) && evaluate
#    if( is.na( evaluate)) {
#      do.eval <- is.call( tryo[[1]][[1]]) && (tryo[[1]][[1]][[1]]=='structure') && 
#          is.call( lt <- tryo[[1]][[1]][[ length( tryo[[1]][[1]]) ]] ) && 
#          is.name( lt[[1]]) && (as.character(lt[[1]]) %in% cq( readLines.mvb, flatdoc))
#    }
    
#    if( do.eval)
    if( evaluate)
      last <- eval( tryo[[ 1]], env=envir)
    else 
      last <- tryo[[1]][[1]] # get through the 'expression'
      
    if( echo)
      try( print( last))

    expr.count <- expr.count + 1
  }

  last
}


"strip.missing" <-
function( obs) {
  sp <- sys.frame( mvb.sys.parent())
  for( i in obs) {
    get.i <- mget( i, sp)[[1]]
    if( try( mode( get.i), silent=TRUE) %is.a% 'try-error')
      obs <- obs %except% i
  }
  obs
}


"subco" <-
function( line, auto.link=!is.null( valid.links), valid.links=NULL){
  # PERL syntax in regexes
  
  line <- ' ' %&% line %&% ' '  
  chsubs <- raw(0)
  fullsubs <- character(0)
  rawl <- function( i, n.clip=0) rawToChar( rl[clip( i, n.clip)])

  # This must come before code & auto-link tests
  pkg1.frags <- gregexpr( "\\b[Pp]ackage '[a-zA-Z.][a-zA-Z.0-9]*'", line)[[1]]
  pkg2.frags <- gregexpr( "[Tt]he '[a-zA-Z.][a-zA-Z.0-9]*' package\\b", line)[[1]]
  pkg.frags <- c( pkg1.frags, pkg2.frags)
  o <- order( pkg.frags)
  o <- o[ pkg.frags[o]>0]
  pkg.frags <- pkg.frags[o]
  if( any( pkg.frags>0)) { # then they all will be
    pkg.len <- c( attr( pkg1.frags, 'match.length'), attr( pkg2.frags, 'match.length'))[o]
    pkg.seq <- mapply( seq, from=pkg.frags, length=pkg.len, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    pkg.sub <- sapply( pkg.seq, rawl)
    pkg.sub <- sub( "'([^']+)'", "\\\\pkg\\{\\1\\}", pkg.sub)
    rl[ pkg.frags] <- charToRaw( '\003')
    chsubs <- c( chsubs, rep( '\003', length( pkg.sub)))
    fullsubs <- c( fullsubs, pkg.sub)
    line <- rawToChar( rl[ -unlist( lapply( pkg.seq, '[', -1))])
  }

  if( (link.qv.frags <- gregexpr( "[( -]'[a-zA-Z.][a-zA-Z.0-9]*' +\\(qv\\)", line)[[1]])[1] > 0) {
    link.qv.frags[] <- link.qv.frags + 1
    link.qv.len <- attr( link.qv.frags, 'match.length') - 1
    link.qv.seq <- mapply( seq, from=link.qv.frags+1, length=link.qv.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.qv.sub <- sapply( link.qv.seq, rawl) # start AFTER sQuote
    link.qv.sub <- sub( "([^']+)'.*", "\\\\code\\{\\\\link\\{\\1\\}\\}", link.qv.sub)
    rl[ link.qv.frags] <- charToRaw( '\001')
    chsubs <- c( chsubs, rep( '\001', length( link.qv.sub)))
    fullsubs <- c( fullsubs, link.qv.sub)
    line <- rawToChar( rl[ -unlist( link.qv.seq)])
  }
  
  if( (link.see.frags <- gregexpr( "\\b[Ss]ee '[a-zA-Z.][a-zA-Z.0-9]*'", line)[[1]])[1] > 0) {
    link.see.len <- attr( link.see.frags, 'match.length')
    link.see.seq <- mapply( seq, from=link.see.frags, length=link.see.len, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.see.sub <- sapply( link.see.seq, rawl)
    link.see.sub <- sub( "'([^']+)'", "\\\\code\\{\\\\link\\{\\1\\}\\}", link.see.sub)
    rl[ link.see.frags] <- charToRaw( '\002')
    chsubs <- c( chsubs, rep( '\002', length( link.see.sub)))
    fullsubs <- c( fullsubs, link.see.sub)
    line <- rawToChar( rl[ -unlist( lapply( link.see.seq, '[', -1))])
  }

  if( auto.link && ((
      link.auto.frags <- gregexpr( "[( -]'([a-zA-Z.][a-zA-Z0-9._]*)'", line)[[1]])[1] > 0)) {
    link.auto.frags[] <- link.auto.frags
    link.auto.len <- attr( link.auto.frags, 'match.length') 
    link.auto.seq <- mapply( seq, from=link.auto.frags+2, length=link.auto.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.auto.sub <- sapply( link.auto.seq, rawl, n.clip=1) # between quotes
    if( !is.null( valid.links)) {
      ok <- link.auto.sub %in% valid.links
      link.auto.sub <- link.auto.sub[ok]
      link.auto.frags <- link.auto.frags[ok]
      link.auto.seq <- link.auto.seq[ok]
    }
      
    if( length( link.auto.sub)) {
      rl[ link.auto.frags+1] <- charToRaw( '\007')
      chsubs <- c( chsubs, rep( '\007', length( link.auto.sub)))
      fullsubs <- c( fullsubs, '\\code{\\link{' %&% link.auto.sub %&% '}}')
      line <- rawToChar( rl[ -unlist( link.auto.seq)])
    }
  }

  if( (code.frags <- gregexpr( "([ (])'([^']+)'", line)[[1]])[1] > 0) {
    code.len <- attr( code.frags, 'match.length')
    code.seq <- mapply( seq, from=code.frags+2, length=code.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    code.sub <- sapply( code.seq, rawl)
    code.sub <- substring( code.sub, 1, nchar( code.sub)-1)
    # Some characters need to be escaped:
    # code.sub <- gsub( '{', '\\{', gsub( '%', '\\%', code.sub, fixed=TRUE), fixed=TRUE)
    code.sub <- '\\code{' %&% code.sub %&% '}'
    rl[ code.frags+1] <- charToRaw( '\006')
    # Backslash and braces in \code will be treated differently...
    code.sub <- gsub( '\016', '\017', code.sub, fixed=TRUE)
    code.sub <- gsub( '\020', '\022', code.sub, fixed=TRUE)
    code.sub <- gsub( '\021', '\023', code.sub, fixed=TRUE)    
    chsubs <- c( chsubs, rep( '\006', length( code.sub)))
    fullsubs <- c( fullsubs, code.sub)
    line <- rawToChar( rl[ -unlist( code.seq)])
  }
  
  emph.frags <- gregexpr( '[ (]_[^"_]+_', line)[[1]]
  if( emph.frags[1]>0) {
    emph.len <- attr( emph.frags, 'match.length')
    emph.seq <- mapply( seq, from=emph.frags+2, length=emph.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    emph.sub <- sapply( emph.seq, rawl)
    emph.sub <- '\\emph{' %&% substring( emph.sub, 1, nchar( emph.sub)-1) %&% '}'
    rl[ emph.frags+1] <- charToRaw( '\004')
    chsubs <- c( chsubs, rep( '\004', length( emph.sub)))
    fullsubs <- c( fullsubs, emph.sub)
    line <- rawToChar( rl[ -unlist( emph.seq)])
  }

  bold.frags <- gregexpr( '[ (]\\*[^"*]+\\*', line)[[1]]
  if( bold.frags[1]>0) {
    bold.len <- attr( bold.frags, 'match.length')
    bold.seq <- mapply( seq, from=bold.frags+2, length=bold.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    bold.sub <- sapply( bold.seq, rawl)
    bold.sub <- '\\bold{' %&% substring( bold.sub, 1, nchar( bold.sub)-1) %&% '}'
    rl[ bold.frags+1] <- charToRaw( '\005')
    chsubs <- c( chsubs, rep( '\005', length( bold.sub)))
    fullsubs <- c( fullsubs, bold.sub)
    line <- rawToChar( rl[ -unlist( bold.seq)])
  }

  url.frags <- gregexpr( "<[a-z]+://[0-9a-zA-Z%$_.+!*'()/-]+>", line)[[1]]
  if( url.frags[1]>0) {
    url.len <- attr( url.frags, 'match.length')
    url.seq <- mapply( seq, from=url.frags+1, length=url.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    url.sub <- sapply( url.seq, rawl)
    url.sub <- '\\url{' %&% substring( url.sub, 1, nchar( url.sub)-1) %&% '}'
    rl[ url.frags] <- charToRaw( '\010')
    chsubs <- c( chsubs, rep( '\010', length( url.sub)))
    fullsubs <- c( fullsubs, url.sub)
    line <- rawToChar( rl[ -unlist( url.seq)])
  } 

  email.frags <- gregexpr( '<[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]+>', line)[[1]]
  if( email.frags[1]>0) {
    email.len <- attr( email.frags, 'match.length')
    email.seq <- mapply( seq, from=email.frags+1, length=email.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    email.sub <- sapply( email.seq, rawl)
    email.sub <- '\\email{' %&% substring( email.sub, 1, nchar( email.sub)-1) %&% '}'
    rl[ email.frags] <- charToRaw( '\013')
    chsubs <- c( chsubs, rep( '\013', length( email.sub)))
    fullsubs <- c( fullsubs, email.sub)
    line <- rawToChar( rl[ -unlist( email.seq)])
  } 


  # Escape other special characters - {}\ already done
  # line <- gsub( '([#\\_${}])', '\\\\\\1', line)
  if( numeric_version( Rd.version) < '2')
    line <- gsub( '([#_$])', '\\\\\\1', line)

  # R and ellipsis
  line <- gsub( "( |\\()R([ .,;:'])", "\\1\\R{}\\2", line)
  line <- gsub( '...', '\\dots{}', line, fixed=TRUE)

  # Multiple tabs & spaces go down to 1 space, except keep a double space at the start
  line <- gsub( '(.)[ \t]+', '\\1 ', line)

  # Put things back
  for( isub in seq_along( chsubs))
    line <- sub( chsubs[ isub], fullsubs[ isub], line, fixed=TRUE)
    
  substring( line, 2, nchar( line)-1) # remove first and last spaces
}


"task.home" <-
function(fname) {
  if(!missing(fname)) {
    if(fname == "" || substr( fname, 1, 1) %in% c( '/', '\\') || pos(":", fname)[1])
return(fname)
    else
return( file.path( .Path[ length( .Path)], fname))    
  } else
return( as.vector( .Path[ length( .Path)]))
}


"to.regexpr" <-
function (x) 
{
    x <- strsplit(x, "")
    repfun <- function(xx) {
        m <- match(xx, c("&", ".", "%", "\\", "[", "]", "(", 
            ")", "^", "{", "}"), 0)
        xx[m > 0] <- "\\" %&% xx[m > 0]
        paste(xx, collapse = "")
    }
    sapply(x, repfun)
}


"unmaintain.packages" <-
function( ..., character.only=FALSE, autosave=NA){
  if( character.only)
    packs <- list(...)[[1]]
  else {
    mc <- as.list( match.call( expand.dots=FALSE)$...)
    packs <- sapply( mc, as.character)
  }

  if( is.na( autosave) || save)
    for( i in packs %that.are.in% names( maintained.packages))
      if( !is.na( autosave) || yes.no( "Save maintained package '" %&% i %&% "'? "))
        Save.pos( maintained.packages[[ i]])
  maintained.packages <<- maintained.packages %without.name% packs

  # Clear load hooks
  for( i in packs) {
    setHook( packageEvent( i, 'onLoad'), NULL, 'replace')
    setHook( packageEvent( i, 'attach'), NULL, 'replace')
  }

  dont.lock.envs <<- dont.lock.envs %without.name% 'imports:' %&% packs
  dont.lock.envnames <<- dont.lock.envnames %except% 'package:' %&% packs

return( names( maintained.packages))
}


"unpackage" <-
function( spath, force=FALSE) {
  if( getRversion() < '2.10')
stop( "help2flatdoc only works with R 2.10 & up")

  oppo <- options( useFancyQuotes=FALSE, keep.source=TRUE)
  on.exit( options( oppo))
  if( !is.dir( spath) || is.na( 
      desc <- grep( '^DESCRIPTION([.]in)$', dir( spath), perl=TRUE, value=TRUE)[1]))
stop( dQuote( spath) %&% " doesn't seem to be a source package")

  x <- tools:::.read_description( file.path( spath, desc))[ 'Package']
  
  if( !force && is.dir( x) && length( dir( x, all=TRUE) %except% c( '.', '..'))>2) {
    force <- yes.no( 'Directory "' %&% x %&% 
    '" already exists and has stuff in it, which will be deleted if you proceed. OK? ')
  } else if( !force && file.exists( x) && !is.dir( x)) {
    cat( '"x" already exists, as a file\n')
    force <- FALSE
  }  else
    force <- TRUE
    
  if( force) {
    tpath <- file.path( getwd(), x)
    mkdir( tpath)
    unlink( file.path( tpath, dir( tpath) %except% c( '.', '..')), recursive=TRUE)
  } else
stop( "Not overwriting")    

  # one file goes straight to task: DESCRIPTION, Makefile.*  
  # some files disappear: CONTENTS, INDEX, MD5, NAMESPACE (
  # other files go to "inst" (this will include CITATION)
  # some dirs disappear: chtml, html, latex, meta, R-ex
  # some dirs get used (R, help, data)
  # some dirs go straight to task (src) (??)
  # all other dirs (incl. libs, demos) go to "inst"
  
  # Recursive file- or directory-copy
  filecop <- function( fname, ...) {
      if( !any( file.exists( file.path( spath, fname))))
    return()
    
      tpath... <- do.call( 'file.path', c( list( tpath), unlist( list( ...))))
      mkdir( tpath...)
      if( is.dir( file.path( spath, fname[1]))) {
        mkdir( file.path( tpath..., fname))
        mcf <- mcd <- as.list( match.call( expand.dots=TRUE))
        
        fd <- dir( file.path( spath, fname), all=TRUE) %except% c( '.', '..')
        
        # Copy all *files* at once...
        mcf$fname <- file.path( fname, fd[ !is.dir( fd)])
        eval( as.call( mcf), sys.parent())  
        
        # ... and subdirs one at a time...
        for( this.dir in fd[ is.dir( fd)]) {
          mcd$fname <- file.path( fname, this.dir)
          eval( as.call( mcd), sys.parent())
        }
      } else 
        file.copy( file.path( spath, fname), 
            file.path( tpath..., fname))
    }
  
  # not filecop( 'DESCRIPTION') in case DESCRIPTION.in
  mvb.file.copy( file.path( spath, desc), file.path( tpath, 'DESCRIPTION'))
  
  nondirs <- dir( spath, all=TRUE) %such.that% !is.dir( file.path( spath, .))
  nondirs <- nondirs %except% c( desc, cq( CONTENTS, INDEX, MD5, NAMESPACE))
  filecop( nondirs, '.')
  
  filecop( 'src')
  filecop( 'demo')
  filecop( 'tests')
  filecop( 'exec')
  filecop( 'inst')
  
  instdirs <- (dir( spath, all=TRUE) %such.that% is.dir( .)) 
  instdirs <- instdirs %except% 
      cq( ., .., src, chtml, html, latex, meta, 'R-ex', R, help, data, libs)
  filecop( instdirs, 'inst')
  filecop( 'libs', file.path( 'inst', 'libs'))
  
  # Deal wih R code, data, and help
  # R code:
  e <- new.env()
  sapply( dir( file.path( spath, 'R'), patt='[.][RrSsq]$', full=TRUE),
      function( f) eval( substitute( source( f, local=TRUE)), e))

  if( file.exists( file.path( spath, 'NAMESPACE')) && is.null( e$.onLoad))
    e$.onLoad <- function( libname, pkgname){} # so pre.install will NAMESPACE

  funs <- find.funs(e)
  droppo <- lsall( e) %except% funs
  droppo <- droppo %SUCH.THAT% (e[[.]] %is.an% 'environment')
  if( length( droppo)) {
    cat( "Warning: dropping some environments:", droppo, sep='\n')
    rm( list=droppo, envir=e) 
  }    

  # Unexported data:
  if( file.exists( sysdat <- file.path( spath, 'R', 'sysdata.rda')))
    load( sysdat, envir=e)
    
  # Next: exported data
  if( is.dir( datdir <- file.path( spath, 'data'))) {
    exdata <- dir( datdir, patt='[.](R|r|rda|Rdata|txt|TXT|tab|csv|CSV)$')
    lapply( sub( '[.][^.]+$', exdata), data, package=x, lib.loc=dirname( spath), verbose=FALSE, envir=e)
  }

  # Help:
  t1 <- tempfile()
  on.exit( unlink( t1), add=TRUE)
  Rd.files <- dir( file.path( spath, 'man'), patt='Rd$', full=TRUE)
  for( rdf in Rd.files) {
    p1 <- parse_Rd( rdf)
    rdt <- tools:::RdTags( p1)
    name <- unlist( p1[ rdt=='\\name'])
    aliases <- unlist( p1[ rdt=='\\alias'])
    namal <- unique( c( name, aliases))
    tools:::Rd2txt( p1, t1)
    helpo <- help2flatdoc( text=readLines( t1))
    class( helpo) <- 'docattr'
    matcho <- min( match( namal, funs, nomatch=length( funs)+1))
    if( matcho <= length( funs))
      attr( e[[ funs[ matcho]]], 'doc') <- helpo
    else
      e[[ name %&% '.doc']] <- helpo
  }
  
  # And save...
  save( list=lsall( e), file=file.path( tpath, '.RData'), envir=e)
}


"unpackage.installed" <-
function (x, character.only = FALSE, force = FALSE, env.with.source = NULL) 
{
    if (!character.only) 
        x <- as.character(substitute(x))[1]
    libs <- unclass(library()[[2]])
    if (!(which.lib <- match(x, libs[, "Package"], 0))) 
        stop("Package '" %&% x %&% "' not installed-- can't unpackage")
    ipath <- file.path(libs[which.lib, "LibPath"], x)
    if (!force && is.dir(x) && length(dir(x, all = TRUE)) > 2) {
        force <- yes.no("Directory \"" %&% x %&% "\" already exists and has stuff in it, which will be deleted if you proceed. OK? ")
    }
    else if (!force && file.exists(x) && !is.dir(x)) {
        cat("\"x\" already exists, as a file\n")
        force <- FALSE
    }
    else force <- TRUE
    if (force) {
        spath <- file.path(getwd(), x)
        mkdir(spath)
        unlink(dir(spath) %except% c(".", ".."), recursive = TRUE)
    }
    else stop("Not overwriting")
    filecop <- function(fname, ...) {
        if (!length(fname)) 
            return()
        spath... <- do.call("file.path", c(list(spath), unlist(list(...))))
        mkdir(spath...)
        if (is.dir(fname)) {
            mkdir(file.path(spath..., fname))
            mcf <- mcd <- as.list(match.call(expand.dots = TRUE))
            fd <- dir(file.path(ipath, fname), all = TRUE) %except% 
                c(".", "..")
            mcf$fname <- file.path(fname, fd[!is.dir(fd)])
            eval(as.call(mcf), sys.parent())
            for (this.dir in fd[is.dir(fd)]) {
                mcd$fname <- file.path(fname, this.dir)
                eval(as.call(mcd), sys.parent())
            }
        }
        else file.copy(file.path(ipath, fname), file.path(spath..., 
            fname))
    }
    filecop("DESCRIPTION")
    nondirs <- dir(ipath, all = TRUE) %such.that% !is.dir(file.path(ipath, 
        .))
    instcopies <- nondirs %except% cq(DESCRIPTION, CONTENTS, 
        INDEX, MD5)
    filecop(instcopies, "inst")
    filecop("src")
    instdirs <- (dir(ipath, all = TRUE) %such.that% is.dir(.))
    instdirs <- instdirs %except% cq(., .., src, chtml, html, 
        latex, meta, "R-ex", R, help, data, libs)
    filecop(instdirs, "inst")
    filecop("libs", file.path("inst", "libs"))
    if (TRUE) {
        e <- new.env()
        if (all(file.exists(file.path(ipath, "R", x %&% c(".rdb", 
            ".rdx"))))) {
            .Call("R_lazyLoadDBflush", file.path(ipath, "R", 
                x %&% ".rdb"), PACKAGE = "base")
            lazyLoad(file.path(ipath, "R", x), e)
        }
        else eval(substitute(source(f, local = TRUE, verbose = FALSE, 
            keep.source = TRUE), list(f = file.path(ipath, "R", 
            x))), envir = e)
        if (!is.null(file.with.source)) 
            eval(substitute(source(file.with.source, local = TRUE, 
                verbose = FALSE, keep.source = TRUE), returnList(file.with.source)), 
                envir = env.with.source)
        if (!is.null(env.with.source)) 
            for (f in (lsall(e) %that.are.in% lsall(env.with.source))) e[[f]] <- env.with.source[[f]]
        sourced <- any(eapply(e, function(z) !is.null(attr(z, 
            "source"))))
        if (!sourced) 
            warning("No source attributes found-- see ?unpackage")
        if (file.exists(file.path(ipath, "NAMESPACE")) && is.null(e$.onLoad)) 
            e$.onLoad <- function(libname, pkgname) {
            }
        funs <- lsall(e)
        if (file.exists(sysdat <- file.path(ipath, "R", "sysdata.rda"))) 
            load(sysdat, envir = e)
    }
    if (is.dir(datdir <- file.path(ipath, "data"))) {
        exdata <- dir(datdir, patt = "[.](R|r|rda|Rdata|txt|TXT|tab|csv|CSV)$")
        lapply(sub("[.][^.]+$", exdata), data, package = x, verbose = FALSE, 
            envir = e)
    }
    if (TRUE) {
        helpindex <- matrix(scan(file.path(ipath, "help", "AnIndex"), 
            what = "", sep = "\t"), ncol = 2, byrow = T)
        helpindex <- structure(helpindex[, 2], names = helpindex[, 
            1])
        funs.with.own.help <- unique(helpindex) %that.are.in% 
            funs
        for (ff in funs.with.own.help) {
            this.ff <- e[[ff]]
            attr(this.ff, "doc") <- as.docattr(help2flatdoc(ff, 
                ipath))
            e[[ff]] <- this.ff
        }
        for (txthelp in unique(helpindex) %except% funs.with.own.help) e[[gsub("-", 
            ".", txthelp, fixed = TRUE) %&% ".doc"]] <- as.cat(help2flatdoc(txthelp, 
            ipath))
    }
    save(list = lsall(e), file = file.path(spath, ".RData"), 
        envir = e)
}


"update.installed.dir" <-
function( source, installed=source, delete.obsolete=TRUE) {
	if( is.dir( file.path( rpath, source))) {
		mkdir( file.path( ipath, installed))
		if( delete.obsolete)
			suppressWarnings( file.remove( file.path( ipath, installed, 
					dir( ipath, installed) %except% dir( rpath, source))))
	  sources <- dir( file.path( rpath, source), full=TRUE)
	  installeds <- dir( file.path( ipath, installed), full=TRUE)
	  installeds <- installeds[ !is.dir( installeds)] 
	  source.dirs <- basename( sources)[ is.dir( sources)] %except% c( '.', '..')
	  if( length( source.dirs))
	    mkdir( file.path( ipath, installed, source.dirs))
	  sources <- sources[ !is.dir( sources)]
	  
	  if( length( sources)) {
			# Really should check whether files have turned into dirs or vice versa...
			new.files <- basename( sources) %except% basename( installeds)
			old.md5 <- md5sum( installeds)
			names( old.md5) <- basename( names( old.md5))
			new.md5 <- md5sum( sources)
			names( new.md5) <- basename( names( new.md5))
			changed.files <- names( new.md5) %that.are.in% names( old.md5)
			changed.files <- changed.files[ new.md5[ changed.files] != old.md5[ changed.files]]
			to.copy <- c( new.files, changed.files)
			if( length( to.copy)) # keep file times
				mvb.file.copy( file.path( rpath, source, to.copy), file.path( ipath, installed, to.copy),
						overwrite=TRUE)
		} # if anything potentially to copy
	} else
		unlink( file.path( ipath, installed))
}


"update.loaded.pkg" <-
function( pkg, name, ff, disatt= ff %is.a% 'function') {
  ffatt <- attributes( ff)
  if( disatt) {
    if( ff %is.a% 'function') {
      keepo <- names( ffatt) %except% cq( doc, export.me)
      if( !option.or.default( 'keep.source.pkgs', TRUE))
        keepo <- keepo %except% 'source'
      attributes( ff) <- ffatt[ keepo]
    } # else attributes( ff) <- list() # too brutal? what about 'source' attr for call-types etc?
  } # if disatt

  if( !is.na( j <- index( search()=='package:' %&% pkg)[1])
      && (pkg %not.in% loadedNamespaces()))
    assign( name, ff, j) # shouldn't be locked
  

  # Put into namespace & importers thereof
  if( pkg %in% loadedNamespaces()) {
    nspkg <- asNamespace( pkg)

    if( is.function( ff)) {
      # respect existing environment, in case of weird stuff like my soap hacks
      if( exists( name, envir=nspkg, mode='function', inherits=FALSE))
        environment( ff) <- environment( nspkg[[ name]])
      else
        environment( ff) <- nspkg # don't muck about
    }

    if( name %in% lsall( nspkg$.__NAMESPACE__.$exports)
        || any( names( ffatt) %in% cq( doc, export.me)) 
        || exists( name %&% '.doc', envir=nspkg, mode='character', inherits=FALSE) ) {
      if( match( 'package:' %&% pkg, search(), 0)) # still with source
        try( force.assign( name, ff, as.environment( 'package:' %&% pkg)))
      
      # Check importers that are loaded:
      gnu <- getNamespaceUsers( pkg) %that.are.in% loadedNamespaces()
      
      for( j in lapply( gnu, function( x) parent.env( asNamespace( x))))
        if( exists( name, j, inherits=FALSE))
          force.assign( name, ff, j)
    }
    
    # If a doc object or function, be sure to export the thing(s) being docced
    exclude.me <- character(0)
    if( length( grep( '\\.doc$', name)) && is.character( ff) 
        && exists( sub( '\\.doc$', '', name), envir=nspkg, inherits=FALSE))
      doc.to.check <- ff 
    else if( is.function( ff) && ('doc' %in% names( ffatt))) {
      doc.to.check <- ffatt$doc
      exclude.me <- name # have already arranged my own export
    } else
      doc.to.check <- NULL
    
    for( doccee in named.in.doc( doc.to.check) %except% exclude.me) {
      if( exists( doccee, envir=nspkg, inherits=FALSE)) {
        if( match( 'package:' %&% pkg, search(), 0)) # ?on search path?
          force.assign( doccee, get( doccee, envir=nspkg), as.environment( 'package:' %&% pkg))
        nspkg$.__NAMESPACE__.$exports[[ doccee]] <- doccee

        for( j in lapply( getNamespaceUsers( pkg), function( x) parent.env( asNamespace( x))))
          if( !environmentIsLocked( j) || exists( name, j, inherits=FALSE))
            force.assign( name, ff, j)
      } # if docced object exists yet
    } # loop over docced objects

    force.assign( name, ff, nspkg)
    if( ('package:' %&% pkg) %in% search())
      force.assign( name, ff, as.environment( 'package:' %&% pkg))

    is.S3method<- !is.na( pmatch( names( .knownS3Generics) %&% '.', name))
    if( any( is.S3method))
      force.assign( name, ff, asNamespace( .knownS3Generics[ is.S3method])$.__S3MethodsTable__.)
  } # if namespaced
}


"update.maintained.package" <-
function( pkg, nlocal = sys.parent()) mlocal({
  if( !is.null( mp <- maintained.packages[[ pkg]])) {
    rm( list=lsall( mp), envir=mp)
    load.refdb( env=mp) # filename is deduced
    try( pre.install( pkg, character.only=TRUE))
    try( patch.installed( pkg, character.only=TRUE))
  }
})


"upper.case" <-
function (s) 
{
    a <- attributes(s)
    if (exists("casefold", mode = "function")) 
        s <- casefold(s, upper = TRUE)
    else {
        s <- strsplit(s, "")
        lets <- LETTERS
        names(lets) <- letters
        transfer <- function(x) {
            change <- x %in% letters
            x[change] <- lets[x[change]]
            paste(x, collapse = "")
        }
        s <- sapply(s, transfer)
    }
    do.call("structure", c(list(.Data = s), a))
}


"what.is.open" <-
function () 
{
    if (!exists(".Open", "mvb.session.info")) 
        character(0)
    else get(".Open", "mvb.session.info")
}


"write.mvb.tasks" <-
function( tasks=get( 'tasks', env=env), env=.GlobalEnv, dir=attr( env, 'path'))  
  cat( '\ntasks <- ', deparse( as.call( c( as.name( 'c'), tasks))), 
    file=file.path( dir, 'tasks.R'), append=TRUE)


"write.NAMESPACE" <-
function( ns, file){
  sink( file)
  on.exit( sink())
  if( !is.null( ns$useDynLib))
    cat( 'useDynLib\n')
  cat( 'export( ')
  # I used to have dQuote here but it just leads to trouble AAAAARGH
  cat( '"' %&% ns$export %&% '"', sep=',\n')
  cat( ')\n')
    if( length( ns$import))
    cat( 'import( ' %&%  ns$import %&% ')\n', sep='')
  if( length( ns$importFrom))
    cat( 'importFrom( ' %&% ns$importFrom[,1] %&% ', ' %&% ns$importFrom[,2]%&% ')\n', sep='')
  if( length( ns$S3))
      cat( 'S3method( ' %&% ns$S3[,1] %&% ', ' %&% ns$S3[,2] %&% ')\n', sep='')
  cat( '\n')
}


"write.sourceable.function" <-
function( x, con, append=FALSE, print.name=FALSE, doc.special=TRUE, xn=NULL) {
  if( is.character( con))
    con <- file( con)
  if( need.to.close <- !isOpen( con))
    open( con, open=ifelse( append, 'a', 'w'))

  if( !identical( con, stdout())) {
    sink( con)
    on.exit( sink())
  }

  on.exit( if( need.to.close) try( close( con)), add=TRUE)

  if( print.name && is.null( xn)) {
    xn <- x
    if( !is.character( x)) {
      if( is.name( substitute( x)))
        xn <- deparse( substitute( x))
      else
stop( "Can't figure out what name to print")
    }
    cat( '"', xn, '" <- ', sep='')
  }

  if( is.character( x))
    x <- get( x)

  natts <- names( attributes( x)) %except% cq( source, bug.position)
  if( is.function( x) && length( natts)) {
    # Prepare to have other attributes
    cat( 'structure( ')
    atts <- attributes( x)[ natts]
    attributes( x)[ natts] <- NULL
  }

  if( is.function( x))
    environment( x) <- .GlobalEnv # avoid <environment: x> after definition
  else
    x <- as.cat( attr( x, 'source'))
  print(x)

  if( is.function( x) && length( natts)) {
    # Treat class "docattr" attributes specially. Non-character doc's (references) are OK.
    freeforms <- if( doc.special)
        natts[ sapply( atts, 'inherits', 'docattr') ]
      else
        character( 0)

    for( iatt in natts %except% freeforms)
      cat( ',', iatt, '=',
          paste( deparse.names.parsably( atts[[ iatt]]), collapse=' '), '\n')

    if( length( freeforms)) { # bizarre syntax of next line is to avoid misleading a syntax-highlighting editor
      if( any( freeforms=='doc'))
        freeforms <- c( freeforms %except% 'doc', 'doc') # move doc to last
      eof.markers <- '<<end of ' %&% freeforms %&% '>>'
      names( eof.markers) <- freeforms
      for( iatt in freeforms)
        while( any( atts[[ iatt]] == eof.markers[ iatt]))
          eof.markers[ iatt] <- paste( eof.markers[ iatt], '<', iatt, '>', sep='')
#      eof.markers[ length( eof.markers)] <- '' # default for doc; help syntax highlighters
      cat( ',', paste( freeforms %&% '=flatdoc( EOF="' %&% eof.markers %&% '")',
          collapse=',\n'), ')\n', sep='')
      for( iatt in freeforms)
        cat( atts[[iatt]], eof.markers[ iatt], sep='\n') } # last one will be end of doc
    else
      cat( ')')
    cat( '\n')
  }

  cat("\n")
}


"yes.no" <-
function (prompt, default) 
repeat {
    cat(prompt)
    answer <- upper.case(readline())
    if (answer == "" && !missing(default)) 
        answer <- default
    if (!is.na(answer <- pmatch(answer, c("YES", "NO")))) 
        return(answer == 1)
}


"zip.file.extract2" <-
function (file, zipname = "R.zip", unzip = getOption("unzip"), zipdir=tempdir()) {
    if (!is.character(unzip) || length(unzip) != 1) 
        stop("'unzip' must be a single character string")
    if (!nzchar(unzip)) 
        unzip <- "internal"
    path <- dirname(file)
    topic <- basename(file)
    tmpd <- zipdir # would just make it an arg except for .Internal?
    if (file.exists(file.path(path, zipname))) {
        # tmpd <- tempdir()
        if (unzip != "internal") {
            cmd <- paste(unzip, "-oq", shQuote(file.path(path, 
                zipname)), topic, " -d ", tmpd)
            res <- if (.Platform$OS.type == "windows") 
                system(cmd, invisible = TRUE)
            else system(paste(cmd, "> /dev/null"))
            if (!res) 
                file <- file.path(tmpd, topic)
        }
        else {
            rc <- .Internal(int.unzip(file.path(path, zipname), 
                topic, tmpd))
            if (rc == 0) 
                file <- file.path(tmpd, topic)
        }
    }
    file
}

