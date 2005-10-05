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
    ind[ i] <- fun( a[ i])
  a[ ind]
}


"%that.are.in%" <-
function( a, b) 
  a[ a %in% b]


"%that.match%" <-
function( x, patt) 
  unique( unlist( lapply( patt, grep, x=as.character( x), value=TRUE)))


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


"%without.name%" <-
function( x, what) {
  new.names <- names( x) %except% what
  x[ new.names]
}


".First.lib" <-
function( libname, pkgname) {
  if( 'mvb.session.info' %!in% search()) { # create only once per session
    attach( pos = 2, name = "mvb.session.info", list( .First.top.search = getwd( ), 
       .Path = 0, session.start.time = Sys.time( )))
    .Path <<- c( ROOT = .First.top.search)
    set.path.attr( pos.to.env( 1), .Path)
    if( !exists( "tasks", pos.to.env( 1)) || 
        !is.character( tasks <- get( "tasks", pos.to.env( 1)) )) {
      cat( "MVBUTILS: no \"tasks\" vector found in ROOT\n")
      tasks <- character( 0)
    }

    if( 'mvbutils' %in% names( tasks))
      set.path.attr( pos.to.env( index( search( ) == "package:" %&% 
          pkgname)[1]), tasks["mvbutils"])

    setup.mcache( .GlobalEnv) # in case of cached objects in ROOT, which 'load' won't understand

    if( option.or.default( 'mvbutils.replacements', TRUE)) {
      assign.to.base( "savehistory", hack.history( 'save'))
      assign.to.base( 'loadhistory', hack.history( 'load'))
      assign.to.base( "help", hack.help())
      assign.to.base( "library", hack.library( ))
    }
  }
}


".onLoad" <-
function( libname, pkgname) .First.lib( libname, pkgname)


"as.data.frame.I" <-
function( x, row.names=NULL, optional=FALSE) {
  protect <- !sapply( x, is.factor) & !sapply( x, is.numeric)
  x[ protect] <- lapply( x[ protect], I)
  as.data.frame( x, row.names, optional)
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
  where=-1) {
  if( !is.list( what))
    what <- list( what)
    
  if( is.null( names( what)))
    names( what) <- x
    
  reassign <- function( obj, value, env) {
      locked <- bindingIsLocked( obj, env)
      if( locked)
        unlockBinding( obj, env)
      assign( obj, value, env)
      if( locked) {
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding( obj, env)
      }
    }
    
  for( xi in x) {
    where.xi <- find( xi, mode='function', numeric=TRUE)
    if( length( where.xi)>1) {
      warning( xi %&% ' appears more than once in search(); overwriting top copy only')
      where.xi <- where.xi[1] }
    system.xi <- get( xi, where.xi)
    this <- what[[ xi]]
    environment( this) <- environment( system.xi)
    reassign( xi, this, pos.to.env( where.xi))
    
    # Also assign to the hidden namespace version, if it exists
    ns <- try( asNamespace( sub( 'package:', '', search()[ where.xi])), silent=TRUE)
    if( ns %is.not.a% 'try.error')
      reassign( xi, this, ns)
      
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


"autoFF" <-
function( ...) { 
  if( exists( 'fix.list', where='mvb.session.info')) 
    try( FF())
  TRUE 
}


"called.by" <-
function( fname, can.match, where) {
  which <- unlist( lapply( where, exists, x=fname), use=FALSE)
  if( !any( which)) {
    f <- if( exists( fname)) get( fname) else list() }
  else
    f <- get( fname, pos=where[ index( which)[ 1]])

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
  to <- strsplit(deparse(to), "/", FALSE)[[1]]
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

  #save.image() # replaced by...
  Save.pos( 1) # 12/04, to work with all.rda & lazyLoad

	if( !nchar( Sys.getenv( 'R_HISTFILE')))
		Sys.putenv( R_HISTFILE=file.path( getwd(), '.Rhistory'))

  if( option.or.default( 'mvbutils.update.history.on.cd', TRUE))
    savehistory()
    
  need.to.promote.on.failure <- TRUE
  if (to[1] == "..") {
    cd..(1)
    for (i in 1 %upto% sum(to == "..")) 
      cd..(2)
  }
  else 
    load.mvb( get.image.filename( orig.path), names(orig.path),
      pos = 2, attach = TRUE, path = orig.path)
  remove(list = objects(pos = 1, all = TRUE), pos = 1)
  attributes(.GlobalEnv) <- list()
  if (length(to)) {
    n.attaches <- length(to)
    if (n.attaches > 1)
      for (i in 2:n.attaches) {
        cd.load(to[1], pos = 2, attach.new = TRUE)
        to <- to[-1]
      }
    cd.load(to[1], pos = 1, attach.new = FALSE)

    if( option.or.default( 'mvbutils.update.history.on.cd', TRUE))
      loadhistory()
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

  can.go.up <- !is.null( names( attr( pos.to.env( pos+1), 'path')))
  if( can.go.up)
    reattach.placeholder( names( .Path)[ length( .Path)])
  else {
    need.to.promote.on.failure <- pos>1
stop( "Can't cd up; there's a non-task in position 2", call.=FALSE)
  }
    
  if( pos>1) {
    need.to.promote.on.failure <- TRUE
    detach( pos=pos)
  }
  
  to <- to[-1]
  .Path <<- .Path[ -length( .Path)]
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
      save.mvb.db( env, get.Rdata.path( task.dir))

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
  tasks.on.search <- sapply(seq(along = search()), 
      function(x) 
        if (is.null(x <- names(attr(pos.to.env(x), "path"))))
          ""
        else 
          x
    )
  is.attached <- 1 + index(tasks.on.search[-1] == taskname)
  if (length(is.attached)) {
    if (length(is.attached) > 1)
      sapply(rev(is.attached)[-1], function(x) detach(pos = x))
    Save.pos(is.attached[1])
    # ...changed 12/04 from 'save.pos' to cope with all.rda & lazyloads
  }

  filename <- get.image.filename( full.path)
  if( is.na( filename))
stop( "Can't find an image file to load for '" %&% taskname %&% "'!")

  load.mvb( filename, name = taskname,
    pos = pos, attach.new = attach.new, path = full.path)
  if (length(is.attached)) {
    ow <- options(warn = -1)
    local.on.exit( options(ow) )
    detach(pos = is.attached[1] + attach.new)
    options(ow)
    attach(NULL, pos = is.attached[1] + attach.new, 
        name = "PLACEHOLDER:" %&% taskname)
  }
  
  .Path <<- c(.Path, full.path)
  if (execute.First && exists(".First.task", where = pos, inherits = FALSE)) {
    .First.task <- get(".First.task", pos = pos, inherits = FALSE)
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
function( pattern, from=., from.text) {
  if( missing( from.text))
    from.text <- substitute( from) 
  answer <- cditerate( from.text, cdfind.guts, vector( 'list', 0), pattern)
  attributes( answer) <- list( names=names( answer))
  answer
}


"cdfind.guts" <-
function (found, task.dir, task.name, pattern, env) 
{
    o <- objects(env = env, all.names = TRUE)
    if (length(o)) {
        o2 <- regexpr(pattern, o) > 0
        o <- o[o2]
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
function( from.text, what.to.do, so.far=vector('NULL',0), ...) {
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
    m <- match( names( nodes[ node.list[[ i]]]), attached.tasks, 0)
    if( m)
      env <- pos.to.env( m)
    else if( file.access( this.file <- file.path( nodes[ node.list[[i]] ], '.RData'))==0) {
      # Clear last batch of objects
      env <- orig.env
      remove( list=objects( envir=env, all.names=TRUE), envir=env)
      load( this.file, envir=env)
    }

    so.far <- what.to.do( found=so.far, task.dir=nodes[ node.list[[i]]],
            task.name=find.prefix( node.list[[i]], nodes, parents), env=env, ...)

    deeper <- exists.mvb( 'tasks', env=env)
    if( deeper) {
      new.nodes <- get( 'tasks', env=env)
      deeper <- length( new.nodes) > 0 }

    if( deeper) {
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
function (x) 
{
    if (!(listable <- is.list(x))) {
        if (listable <- (!is.atomic(x) && !is.symbol(x))) 
            x <- as.list(x)
    }
    if (listable) 
        unlist(lapply(x, char.unlist), use.names = FALSE)
    else paste(deparse(x), collapse = "\n")
}


"clip" <-
function( x, n=1) x[ 1 %upto% ( length( x) - n)]


"close.selfdeleting.file" <-
function( con, ...) {
  fname <- summary( con)$description
  NextMethod( 'close')
  unlink( fname)
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
  if( is.null( t <- attr( pos.to.env( pos), 'path')))
stop( "Don't know what path to use for search environment:" %&% pos)

  mkdir( file.path( t, '.Backup.mvb'))
  fob <- read.bkind( t)

  # changed 5/4/2005 for speed with mcache
  epos <- pos.to.env( pos)
  cand <- lsall( epos) %SUCH.THAT% !bindingIsActive( ., env=epos)
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

  bdd <- file.path( attr( pos.to.env( where), "path"))
  if( !nchar( create.bkind.if.needed( bdd))) {
    warn( "Can't create backup directory!")
return() }

  filename <- get.bkfile( name, bdd, create = TRUE)
  ow <- options( warn = -1)
  previous.backups <- readLines( filename)
  options( ow)
  if( length( previous.backups)) {
    line.breaks <- index( previous.backups == infeasible.R.line)
    if( !length( line.breaks)) 
      previous.backups <- character( 0)
    else 
      discard.mouldering.backups()
  }
  cat( previous.backups, infeasible.R.line, "SESSION=" %&% unclass( session.start.time), 
      file = filename, sep = "\n")
  write.sourceable.function( name, filename, append = TRUE, 
      print.name = TRUE)
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
  
  file.remove( file.path( attr( envir, 'path'), 'obj' %&% mcache[ what] %&% '.rda'))
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
        1], "=", FALSE), function(x) as.numeric(paste(x[-1], 
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
function( text) {
  if( is.function( text))
    text <- attr( text, 'doc')
  stopifnot( is.character( text))
  
  tcon <- textConnection( text)
  on.exit( close( tcon))
  Rd <- character( 0)
  EOF <- FALSE

# Definitions:  
  out <- function( string, string2, strip.spaces.at.start=FALSE) {
      if( !missing( string2)) {
        if( strip.spaces.at.start)
          string2 <- sub( '^ +', '', string2)
        if( length( string2)==1)
          string <- paste( '\\', string, '{', string2, '}', sep='')
        else
          string <- c( '\\' %&% string %&% '{', string2, '}')
      }
      Rd <<- c( Rd, string)
    }
    
  line <- function( skip.blanks=TRUE, do.subs=TRUE) {
      repeat{ 
        line <- readLines( tcon, 1)
        if( !length( line)) {
          EOF <<- TRUE
      return( line) }
        line <- sub( ' +$', '', line) # strip spaces at the end
        line <- sub( '^%', '', line) # strip initial invisible-making "%"
        line <- gsub( '(%|{|}|\\\\|\\|)', '\\\\\\1', line) 
        
        if( !skip.blanks || nchar( line))
      break
      }
      
      if( !do.subs)
    return( line)
      
      # Things inside single quotes go to \code fragments
      line <- " " %&% line %&% " "
      line <- gsub( "([a-zA-z])'(a-zA-Z)", "\\1\3\\2", line) # protect e.g. "can't" with a ASCII 03 character

      # Multiple tabs & spaces go down to 1 space, except keep a double space at the start
      line <- gsub( '(.)[ \t]+', '\\1 ', line)
      
      # \pkg{...} for "[Pp]ackage 'x'" or "'x' package"
      line <- gsub( "([Pp]ackage )'([a-zA-Z.][a-zA-Z.0-9]*)'", "\\1\\\\pkg\\{\\2\\}", line)
      line <- gsub( " '([a-zA-Z.][a-zA-Z.0-9]*)' package", " \\\\pkg\\{\\1\\} package", line)
      
      # \code{\link{...}} for e.g. "see 'single.word'" or "'single.word'" (qv)
      line <- gsub( "([Ss])ee '([a-zA-Z.][a-zA-Z.0-9]*)'",
          "\\1ee \\\\code\\{\\\\link\\{\\2\\}\\}", line)
      line <- gsub( "'([a-zA-Z.][a-zA-Z.0-9]*)' \\(qv\\)",
          "\\\\code\\{\\\\link\\{\\1\\}\\}", line)
      
      # " *emphasize*[;:,. ]" to \emph{emphasize}
      line <- gsub( ' \\*([^*]+)\\*([ ,.;:])', ' \\\\emph{\\1}\\2', line)
      
      # Just \code{} for other things in squotes
      line <- gsub( "([ (])'([^']+)'", "\\1\\\\code\\{\\2\\}", line)
      line <- gsub( "\\.\\.\\.", "\\\\dots", line) # \dots substitution
      
      # Now substitute \R for R
      line <- gsub( " R ", " \\\\R ", line)
      line <- gsub( " R([.,;:]) ", " \\\\R\\1 ", line) # could use 0 or 1 count op
      
      line <- gsub( "\3", "'", line) # put "can't" back as it was
      line <- substring( line, 2, nchar( line)-1) # remove initial and final space
      
      line
    }
    
  block <- function() {
      block <- character( 0)
      repeat{ 
        new.line <- line()
        if( EOF)
      break
        # Check for field names or single words in all-uppercase
        first.word <- strsplit( new.line, ' ')[[1]]
        if( ( toupper( new.line) %in% fields) || 
            ( length( first.word)==1 && all( strsplit( first.word, '')[[1]] %in% c( '.', LETTERS)))) {
          pushBack(  new.line, tcon)
      break
        }
        block <- c( block, new.line)
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
      while( length( block) && length( items <- index( regexpr( '^ +- ', block)>0))) {
        n.items <- min( index( diff( c( items, length(block)+5)) %!in% 1:2))
        
        # Start \itemize{
        block <- c( block[ 1 %upto% (items[1]-1)], '\\itemize{', 
            block[ items[1] %upto% length( block)])
        items <- items + 1 # to allow for the new \\itemize{ line
        if( n.items>1) # zap any blank lines between items
          block <- block[ -( items[1]:items[n.items] %except% items[1:n.items])]

        # Add \item
        items <- items[1]+(1:n.items)-1
        block[ items] <- '\\item ' %&% sub( '^ +- ', '', block[ items])

        # End with back-brace for \itemize
        block <- c( block[ 1:items[ n.items]], '}', block[ -(1:items[ n.items])])
      }
      
      # Protect #
      if( length( block))
        block <- gsub( '\\#', '\\\\#', block)
      
      block
    }
      
  list.block <- function() {
      block <- character( 0)
      repeat{ 
        new.line <- line()
        if( EOF)
      break
        # Check for field names or single words in all-uppercase
        first.word <- strsplit( new.line, ' ')[[1]]
        if( toupper( new.line) %in% fields || 
            ( length( first.word)==1 && all( strsplit( first.word, '')[[1]] %in% c( LETTERS, '.')))) {
          pushBack(  new.line, tcon)
      break
        }
        # Check for list item: line starts with space, then comma-separated words ending with a colon
        if( regexpr( '^ ', new.line)>0) { 
          # NB: whole item text is assumed to be on one line
          item <- strsplit( new.line, ': ')[[1]]
          new.line <- paste( '\\item{', item[1], '}{', 
              paste( item[ -1], collapse=':'), '}', sep='')
        }
        block <- c( block, new.line)
      }
      block
    }

  seealso.block <- function() {
      block <- ' ' %&% block() %&% ','
      block <- block[ regexpr( '^%', block) <0] # comment lines
      # Strip out anything already in \code{}...
      block <- gsub( '\\\\code{([^}]*)}', "'\\1'", block)
      # ...and put single words ended by comma or semicolon into \code{\link{}}
      block <- gsub( " ([a-zA-Z.][---a-zA-Z.0-9]*)('*)[,;]", 
          ' \\\\code{\\\\link{\\1}}\\2,', block)
      # ...and strip quotes around these
      block <- gsub( "'(\\\\code{\\\\link{[^}]*}})'", '\\1', block)
      # ... and any remaining quotes back into \code{}
      block <- gsub( " '([^']+)'", " \\\\code\\{\\1\\}", block)
      block <- substring( block, 1, nchar( block)-1)
      block 
    }


  keyword.block <- function() {
      block <- block()
      block <- block[ regexpr( '^%', block) <0] # comment lines
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
    
  fields <- cq( description, usage, synopsis, arguments, arguments., value, details, examples, 
      author, references, note, see.also, keywords)
  
# Code starts here  
  name <- strsplit( line(), ' ')[[1]][1]
  out( 'name', name)
  
  while( nchar( name)){ 
    out( 'alias', name, strip.spaces.at.start=TRUE)
    name <- line( FALSE)
  }
  
  out( 'title', line( do.subs=FALSE)) # no special stuff allowed in title

  while( !EOF) {
    next.field <- tolower( line())
    if( EOF)
  break
    switch( next.field,
      description=, 
      details=,
      author=,
      references=,
      note= out( next.field, itemize( insert.para.breaks( block()))),
      examples=,
      synopsis=,
      usage= out( next.field, block()),
      see.also= out( 'seealso', seealso.block()),
      value=,
      arguments= out( next.field, list.block()),
      keywords= out( '\\keyword{' %&% keyword.block() %&% '}'),
      out( 'section{' %&% nice.title( next.field) %&% '}', 
          if( regexpr( '\\.$', next.field)<0) 
            itemize( insert.para.breaks( block()))
          else
            list.block())
    )
  }
  
#  Rd <- Rd[ nchar( Rd)>0]

# Post-process to set /dontrun examples:
  dontrun <- index( pmatch( upper.case( Rd), "## DON'T RUN:", 0)>0)
  if( length( dontrun)) {
    end.dontrun <- index( pmatch( upper.case( Rd), "## END DON'T RUN", 0)>0)
    if( length( end.dontrun) != length( dontrun))
stop( "Unmatched DON'T RUN block")
    Rd[ dontrun] <- '\\dontrun{'
    Rd[ end.dontrun] <- '}'
  }  

  Rd
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


"dont.save" <-
function() 
  option.or.default("dont.Save", cq( .packageName, .SavedPlots, last.warning, .Traceback))


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

  path.list <- get.path.list()

  pl1 <- names( attr( pos.to.env( 1), 'path'))
  if( is.null( pl1)) {
    warning( '.GlobalEnv doesn\'t seem to be a task!')
    pl1 <- '.GlobalEnv' }
  path.list[ 1] <- pl1

  search.saves <- numeric(0)
  old.warn <- options( 'warn')[[1]]
  on.exit( options( warn=old.warn))
  for( mod in index( modified)) {
    name <- unclass( fix.list$name)[ mod]
    cat( name, ': ')
    code <- try( list( value=source.mvb( fix.list$file[ mod], max.n.expr=1)))
    if( inherits( code, 'try-error')) {
      ff <- eval( substitute( function( ...) stop( my.name %&% ' failed to parse'),
          list( my.name=name)))
      attr( ff, 'source') <- readLines( fix.list$file[ mod])
      bugline <- strsplit( code, ' ')[[1]] # unpaste( code, ' ')[[1]]
      bugline <- bugline[ length( bugline)]
      bugline <- as.numeric( substr( bugline, 1, nchar( bugline)-1))
      attr( ff, 'bug.position') <- c( line=bugline, char=1) }
    else {
      ff <- code$value
      if( option.or.default( 'replace.underscores', is.loaded( 'repund', PACKAGE='REPLACE_UNDERSCORES')))
        attr( ff, 'source') <- replace.underscores( attr( ff, 'source'))
#      if( !is.null( doc <- attr( ff, 'doc'))) class( attr( ff, 'doc')) <- 'docattr'
#     above now handled by flatdoc; this allows list-mode doc attrs without docattr class
      cat( 'OK\n')
    }

    w <- match( fix.list$where[ mod], path.list, 0)
    if( !w) {
      warning( 'Can\'t find right place for ' %&% name)
      w <- 1 }

    search.saves <- c( search.saves, w)
    environment( ff) <- pos.to.env( 1)
    assign(name, ff, pos = w)
    try( deal.with.backups( name, w)) # ought not to crash, but...
  }

  sl <- search()
  for( i in unique( search.saves))
    if( (i>1)
        && (((.Path[ sl[ i]])!='NA') ||   # ancestor of current task
             !is.null( attr( pos.to.env( i), 'path')))        # library code
        && yes.no( 'Save workspace of "' %&% sl[ i] %&% '"? ')) { # polite to ask
      Save.pos( i)
    }

  answer <- unclass( fix.list$name[ modified])
  if( exists( 'tracees', where='mvb.session.info') && any( answer %in% names( tracees))) {
    cat( 'Reapplying trace(s)...')
    lapply( answer[ answer %in% names( tracees)], function( n) mtrace( char.fname=n))
    cat( 'done\n')
  }

  # fix.list <<- fix.list[ !modified,]
  fix.list$file.time <- new.file.times # doesn't seem to work in one step
  fix.list <<- fix.list
  answer
}


"find.derefs" <-
function( envir) {
  if( is.null( mcache <- attr( envir, 'mcache')))
    attr( envir, 'mcache') <- mcache <- named( integer( 0))
  names( mcache) %SUCH.THAT% ( envir[[.]] %is.not.a% 'promise')
}


"find.docholder" <-
function( funs, pos= unlist( lapply( funs, find, mode='function', numeric.=TRUE))[1]) {
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

  o <- named( lsall( pos=pos))
  doclisto <- lapply( o, function( x) named.in.doc( attr( get( x, pos=pos, inherits=FALSE), 'doc')))
  
  anso <- lapply( doclisto, '%that.are.in%', funs)
  anso <- anso[ sapply( anso, length)>0]
  if( !length( anso))
    docholders <- list()
  else {
    docholders <- split( rep( names( anso), sapply( anso, length)), unlist( anso, use.names=FALSE))
    docholders[ funs %except% names( docholders)] <- list( character( 0))
  }
  docholders
}


"find.documented" <-
function( pos=1, doctype=c( 'Rd', 'casual', 'own', 'any')) {
# In this version, 'pos' can have length > 1
  named.in.doc <- function( doc) {
    if( is.null( doc) || !is.character( doc))
  return( character( 0))
  
    doc <- c( doc, ' ') # guarantees blank
    blank <- seq( along=doc) %except% grep( '[^ ]', doc)
    namelines <- doc[ 1 %upto% (min( blank)-1)] # 2: to ignore first line
    namelines <- sub( '^ +', '', namelines) # leading spaces
    namelines <- gsub( ' +[^ ]+', '', namelines) # keep first word only
    namelines <- gsub( ' *', '', namelines) # trailing spaces
    namelines }

  findo <- function( pos) {
    o <- named( objects(pos=pos, all.names=TRUE))
    searchfun.Rd <- function( x) named.in.doc( attr( get( x, pos=pos, inherits=FALSE), 'doc'))
    searchfun.casual <- function( x) x[ !is.null( attr( get( x, pos=pos, inherits=FALSE), 'doc')) ]
    searchfun.own <- function( x) x[ !is.null( doc <- attr( get( x, pos=pos, inherits=FALSE), 'doc')) && is.character( doc) ]
    searchfun.any <- function( x) searchfun
    
    keepo <- character( 0)
    for( dt in doctype)
      keepo <- c( keepo, unlist( lapply( o, FUN='searchfun.' %&% dt)))
    unique( keepo)
  }
  
  doctype <- match.arg( doctype)
  if( doctype=='any')
    doctype <- c( 'Rd', 'casual')

  unlist( lapply( pos, findo), use=FALSE)
}


"find.funs" <-
function( pos=1, ...) {
# In this version, "pos" can have length > 1
  findo <- function( pos2) {
      o <- named( lsall( pos=pos2, ...))
      if( !length( o))
    return( character( 0))
      old.warn <- options( warn=-1)$warn
      on.exit( options( warn=old.warn))
      keep <- sapply( o, exists, where=pos2, mode="function", inherits=FALSE)
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


"find.path" <-
function( rel.path, char.rel.path, return.all=FALSE) {
  if( !missing( char.rel.path))
    rel.path <- as.character( char.rel.path) # in case of the number 0
  else
    rel.path <- deparse( substitute( rel.path))

  # Parse input string: NB that R interprets a/b/c as function calls!
  rel.path <- strsplit( rel.path, '/', FALSE)[[1]]
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
      else
        wp <- c( wp, ctasks[ go])
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
function( x, new=FALSE, install=FALSE) {
  proged <- options()$program.editor
  if( is.null( proged) || install)
    proged <- install.proged()
  if( missing( x))
return( 'Nothing to edit!')

  fixr.guts( as.character( substitute( x)), new=new, proged=proged, fixing=TRUE)
}


"fixr.guts" <-
function( name, new=FALSE, proged, fixing=TRUE) {
  if( missing( name))
return( "Nothing to edit!")

  trace.was.on <- FALSE

# Function to edit, and its name (may be different from 'name' if method)
  load.from <- if( new) NA else find( name, mode = "function", numeric=TRUE)[1]
  is.new <- is.na( load.from)
  if(!is.new) {
    x <- get( name, pos=load.from)
    trace.was.on <- exists( 'tracees', 'mvb.session.info') && (name %in% names( tracees)) }
  else {
    x <- function() {}
    load.from <- 1 }

  environment( x) <- .GlobalEnv # to prevent the environment string being printed after the definition. Mostly for new functions; bad practice to set environments otherwise.

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

  filename <- file.path( dir, legal.filename( name) %&% '.R') # .R to avoid...
  #... editors loading e.g. .First.lib as a binary file!

  old.warn <- options(warn = -1, width = 180)[1:2] # wide to avoid line breaks
  failed.to.edit <- TRUE # usual pessimism
  on.exit({
    if( failed.to.edit && file.exists( filename))
      unlink(filename)
    if( trace.was.on)
      mtrace( char.fname=name)
    options(old.warn) })
  
  if( fixing && !is.new && !is.null( try.load.from)) # only do backup if task
    deal.with.backups( name, num.load.from)

  write.sourceable.function( x, filename)

#  OK <- shell( proged(name, filename), translate=TRUE, wait = FALSE) # shell doesn't work on Linux
  cmd <- proged( name, filename)
  if( dos.or.windows()) {
    cmd <- gsub( '/', '\\\\', cmd)
    OK <- system( cmd, wait = FALSE) 
  } else
    OK <- system( cmd)
    
  if(OK != 0)
stop("Couldn't launch editor")

# Save info about what function this file refers to etc.
  if(!exists("fix.list", 'mvb.session.info'))
    put.in.session( fix.list= empty.data.frame( name= , file= , where=, dataclass='', 
      file.time=0))
 
# Avoid returning focus to console
  put.in.session( just.created.window=TRUE)

# Zap duplicates
  if( fixing) {
    fix.list <<- fix.list[ fix.list$name != name,]
    fix.list <<- rbind(fix.list,
        list( name = name, file = filename, where = load.from, dataclass = "function",
        file.time=unclass( file.info( filename)[1,'mtime'])))
  }

  failed.to.edit <- FALSE
  invisible(NULL)
}


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
  if( missing( funs))
    funs <- unique( unlist( lapply( where, find.funs)))
  else if( funs %is.a% 'foodweb') { # basically redisplay
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


"formalize.package" <-
function( funs=find.funs( where), package, where=1,
    dir.=attr( pos.to.env( where), 'path'),
    description.file=file.path( dir., 'DESCRIPTION'),
    new.index=TRUE, README.goes.first=TRUE, keep.flatdoc=FALSE) {
  if( is.character( where))
    where <- index( search()==where)[1]
  if( is.na( where))
stop( "Can't figure out 'where'")

  if( missing( package)) {
    package <- attr( pos.to.env( where), 'name')
    if( is.null( package))
      package <- attr( attr( pos.to.env( where), 'path'), 'names')
    if( !is.character( package) || (length( package) != 1))
stop( "Can't deduce package name; please set 'package' argument")
    package <- sub( '.+:', '', package)
  }

  if( !all( mkdir( file.path( dir., package, cq( R, man)))))
stop( "couldn't make directories")

  if( file.exists( description.file))
    description <- readLines( con=description.file)
  else
    description <- c( "Package: ", "Title: What the package does",
        "Version: 1.0", "Author: R.A. Fisher", "Description: More about what it does",
        "Maintainer: Who to complain to <yourfault@somewhere.net>",
        "License: ???") # adapted from 'package.skeleton'

  if( regexpr( '^Package:', description[1]) < 0)
    description <- c( '', description)
  description <- description[ regexpr( '^Built:', description) < 0]
  description[ 1] <- 'Package: ' %&% package

  cat( description, file = file.path( package, 'DESCRIPTION'), sep = "\n")

  if( file.exists( changes.file <- file.path( dir., 'changes.txt'))) {
    mkdir( file.path( package, 'inst'))
    file.copy( changes.file, file.path( package, 'inst', 'changes.txt'))
  }

  # Augment functions to include all that are named in each others aliasses
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
    namelines }

  searchfun.Rd <- function( x) named.in.doc( attr( get( x, pos=where, inherits=FALSE), 'doc'))
  more.funs <- unique( unlist( lapply( funs, searchfun.Rd)))
  funs <- unique( c( funs, more.funs))
  funs <- funs[ sapply( funs, exists, where=where)]

  # Source code:
  ff <- function( x) {
    cat( '\n"', x, '" <-\n', sep='', file=rfile, append=TRUE)
    x <- get( x, pos=where)
    if( is.function( x)) {
      if( !keep.flatdoc) # then strip doc (& other?) attributes
        attributes( x) <- list( source=attr( x, 'source'))
      write.sourceable.function( x, rfile, append=TRUE, doc.special=FALSE) }
    else
      print( x)
  }

  file.remove( file.path( dir., package, 'R',
      dir( file.path( dir., package, 'R'), all.files=TRUE))) # clean out oldies
  rfile <- file.path( dir., package, 'R', package %&% '.R')
  # cat( '.packagename <- "', package, '"\n', sep='', file=rfile)
  cat( '# This is package', package, '\n', file=rfile)
  sapply( funs, ff)

  # Documentation:
  file.remove( file.path( dir., package, 'man',
      dir( file.path( dir., package, 'man'), all.files=TRUE)))
  docfuns <- intersect( funs, find.documented( where, doctype='own'))
  for( i in docfuns) {
    docco <- doc2Rd( get( i))
    fname <- sub( '}', '', sub( '\\\\name{', '', docco[1])) %&% '.Rd'
    if( length( grep( '^\\.', fname)))
      fname <- '01' %&% fname
    if( README.goes.first && length( grep( '^README', fname)))
      fname <- '00' %&% fname
    cat( docco, file=file.path( dir., package, 'man', fname), sep='\n')
  }
  cat( file=file.path( dir., package, 'man', package %&% '-internals.Rd'),
      doc2Rd( make.internal.doc( funs %except% find.documented( where, doctype='any'), package)),
      sep='\n')

  if( new.index && require( tools)) {
    index.file <- file.path( dir., package, 'INDEX')
    Rdindex( file.path( dir., package, 'man'), index.file)
    # Rdindex won't put README.mypackage first, so will need to fix it
    index.stuff <- scan( index.file, what='', sep='\n')
    if( README.goes.first && length( i <- grep( '^README\\.' %&% package, index.stuff))) {
      i <- i[1]
      index.stuff <- index.stuff[ c( i, (1:length( index.stuff)) %except% i)]
      cat( index.stuff, sep='\n', file=index.file)
    }
  }

  if( file.exists( file.path( dir., package, 'NAMESPACE')))
    make.NAMESPACE()

  invisible( NULL)
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


"get.backup" <-
function( name, where=1, rev=TRUE, zap.name=TRUE) {
  if( length( where) != 1)
stop( "'where' should be length 1")

  where <- named( search())[ where] # to character
  where <- index( search()==where) # to numeric
  if( !is.numeric( where) || is.na( where))
stop( "'where'?")

  bdd <- file.path(attr(pos.to.env(where), "path"))
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
        file.nums <- as.integer(unlist(strsplit(fob$files, "BU")))
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
    tasks <- character(0)

  catstop <- function() {
      cat( 'No ')
stop( 'merely quitting cd', call.=FALSE) 
    }    
    
  line.end <- ifelse( option.or.default( 'cd.extra.CR', FALSE), '\n', '')
  
  can.go.up <- ifelse( length( .Path) > 1, 1, 0)
  to <- menu(c(names(tasks), if( can.go.up) '..' else NULL, "CREATE NEW TASK"),
      graphics = !is.null(options()$gui), title = "Task menu")
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
    to <- names(tasks)[to]

return( parse( text=to)[[1]])
}


"get.image.filename" <-
function( full.path) {
  fpR <- file.path( full.path, 'R')
  # The if's below ensure no addition if no existo
  filename <- c( file.path( full.path, '.Rdata'),
      if( is.dir( fpR)) file.path( fpR, 'all.rda'),
      if( is.dir( fpR)) dir( fpR, patt='\\.rdb$', full.names=TRUE)[1])
  while( length( filename) && !is.na( filename[ 1]) && !file.exists( filename[1]))
    filename <- filename[-1]
  if( !length( filename))
    filename <- file.path( full.path, '.Rdata') # back to square 1
  filename[1]
}


"get.mcache.reffun" <-
function( whati, envir) {
  fx <- function( x) NULL
  body( fx) <- substitute(
      if( missing( x))
        qwhati
      else {
        mc <- attr( envir, 'mcache')
        mc[ whati] <- -abs( mc[ whati]) # signal a change
        attr( envir, 'mcache') <- mc
        qwhati <<- x
    }, list( whati=whati, qwhati=as.name( whati)))

  e <- new.env( parent=NULL)
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


"get.Rdata.path" <-
function( path) {
  path <- gsub( '\\\\', '/', path)
  if (is.dir(path)) {
    path <- get.image.filename( path)
    if( regexpr( '\\.rdb$', path) > 0)
      attr( path, 'mvb.db.type') <- 'LazyLoad'
  }
  
  if( is.null( attr( path, 'mvb.db.type')))
    attr( path, 'mvb.db.type') <- 'normal'
  path
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
    derefs <- lscache %that.are.in% (ls( envo, all.names=TRUE) %except% refs)
    prom.func <- function( x) {cache[[x]] %is.a% 'promise'}
    promises <- names( which( sapply( lscache, prom.func)))
    fnum.func <- function( x) unclass( envo[[ x]])$nfile
    if( length( refs))
      file.numbers <- sapply( refs, fnum.func)
  }
})


"group" <-
function( m, ...) {
  l <- list( ...)
  if( length( l)==1 && is.list( l))
    l <- l[[ 1]]
  rep( names( l), sapply( l, length))[ match( m, unlist( l), NA)]
}


"hack.help" <-
function() {
  replacement.help <- function( ...) {
    help <- get( 'base.help', pos='mvb.session.info') # makes error messages clearer

    mc <- as.list( match.call( expand.dots=TRUE))
    # Check it's a simple call
    if( !is.null( mc$topic) && !is.call( mc$topic) &&
        is.null( mc$type) && is.null( mc$lib.loc) && is.null( mc$try.all.packages)) {
      h1 <- try( eval( as.call( mc)), silent=TRUE)
      if( ((h1 %is.not.a% 'try-error') && length( unclass( h1))>0) ||
          ( (h1 <- dochelp( as.character( mc$topic))) %is.a% 'pagertemp' ))
  return( h1) # TRUE if dochelp; NULL in 1.9; class( 'help_files_with_topic') in 2.x
    }

    eval( as.call( mc)) # either do complicated call, or force crash if simple without 'doc'
  }

  formals( replacement.help) <- formals( help)
  environment( replacement.help) <- .GlobalEnv
  replacement.help
}


"hack.history" <-
function( which=cq( load, save)[1]) {
# Returns modified 'loadhistory' or 'savehistory' so that file checks R_HISTFILE by default
  whichhistory <- get( which %&% 'history')
  e1 <- environment( whichhistory)
  formals( whichhistory)$file <- quote( 
      if( nchar( histfile <- Sys.getenv( 'R_HISTFILE'))) histfile else '.Rhistory')
  environment( whichhistory) <- e1
  whichhistory
}


"hack.library" <-
function() {
# Returns modified 'library' so that attaching happens below ROOT

# In R 1.8, 'pos' arg of 'library' is respected so no need to hack 'attach'
#    attach <- attach; formals( attach)$pos <- quote( lib.pos()); 
#    attachNamespace <- formals( attachNamespace)$pos <- quote( lib.pos())
#    formals( this.library)$attach <- attach; 
#    formals( this.library)$attachNamespace <- attachNamespace
#  if( !all(  c( 'attach', 'attachNamespace') %in% names( formals( library)))) {

  if( !identical( formals( library)$pos, quote( lib.pos()))) {
    this.library <- library
    formals( this.library)$pos <- quote( lib.pos())
    environment( this.library) <- environment( library) # is this needed?
    library <- this.library
  }
  
  library
}


"index" <-
function (lvector) 
seq(along = lvector)[lvector]


"install.proged" <-
function( option.name='program.editor') {
  readonly <- ifelse( option.name=='program.reader', 'in read-only mode', '')
  cat( 'Must set up program editor information before "fixr" works.')
  repeat {
    cat( '\nType whatever you\'d type in a command window to',
      'invoke your editor', readonly, 'on a file called "myfun.r".',
      '  For example, on Unix-like systems: myedit myfun.r &',
      '  In Windows, use double quotes around a path if it contains spaces,',
      '  and use \\ not \\\\ or / as the separator.',
      '  Or type <ENTER> to quit: ', sep='\n')
    pe.path <- readline()
    if( !nchar( pe.path))
return()
    if( length( grep( 'myfun\\.r', pe.path))==1)
  break
  }
  
  pe.path <- strsplit( pe.path, 'myfun.r')[[1]]
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
    deparse( o), '', sep='\n')
    
  options()[[ option.name]]
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
    filenames <- strsplit(substr(name, 1, length.limit), "", 
        FALSE)[[1]]
    filenames[filenames %in% c(":", "*", "?", "'", "/", "\\", 
        "\"", ">", "<")] <- "."
    if (!(upper.case(filenames[1]) %in% LETTERS)) 
        filenames <- c("X", filenames)
    paste(filenames, collapse = "")
}


"lib.pos" <-
function() {
  if( exists( '.First.top.search')) { 
    task.paths <- sapply( seq( along=search()),
        function( x) { x <- attr( pos.to.env( x), 'path'); if( is.null( x)) x <- ''; x })
    pos <- index( task.paths==.Path[1])[1]+1
    if( is.na( pos))
stop( "Can't work out where to attach library-- no sensible .Path")
  } # setting pos
  pos
}


"load.mvb" <-
function (filename, name, pos, attach.new=pos != 1, path, ...) {
  if (attach.new)
    env <- attach(NULL, pos = pos, name = name)
  else {
    env <- pos.to.env(pos)
    attr(env, "name") <- name
  }

# This stuff used to be after the load, but load.refdb needs the path attr set

  if( rev( splitto <- strsplit( filename, '.', fixed=TRUE)[[1]])[1]=='rdb')
    lazyLoad( paste( clip( splitto), collapse='.'), envir=env)
  else
    load.refdb(filename, env = env, fpath=path)

  attr( env, 'path') <- path
  ll <- list(...)
  if (length(ll))
    for (attro in names(ll)) attr(env, attro) <- ll[[attro]]
}


"load.refdb" <-
function( file=get.image.filename( fpath), envir, fpath=attr( envir, 'path')) {
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


"lsall" <-
function( ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$all.names <- TRUE
  mc[[1]] <- as.name( 'ls')
  eval( mc, parent.frame())
}


"make.arguments.section" <-
function( funs=find.funs() %except% find.documented( doctype='Rd'), file=stdout()) {
  arguments <- function( x) { 
      ax <- names( formals( x))
      if( length( ax))
        ' ' %&% ax %&% ': (' %&% x %&% ')'
      else
        character( 0)
    }
 funs <- unlist( lapply( funs, arguments))
 cat( funs, sep='\n', file=file)
 invisible( funs)
}


"make.internal.doc" <-
function( funs, package) {
  # xfuns is to cope with operators,
  # whose names start with %. This is interpreted as a "don't-show-rest-of-line" 
  # by the standard flatdoc system, and is removed by 'doc2Rd'.
  # So we need to add a 
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
function( pos=1, path=attr( pos.to.env( pos), 'path')) {
  if( is.character( pos))
    pos <- match( pos, search())

  if( is.null( path))
stop( "Can't find path")

  description <- readLines( file.path( path, 'DESCRIPTION'))
  imports <- description[ regexpr( '^Depends:', description)>0]
  imports <- sub( '^Depends:', '', imports)
  imports <- gsub( '\\([^)]*\\)', '', imports)
  imports <- gsub( ' *', '', imports)
  imports <- paste( imports, collapse=',')
  imports <- strsplit( imports, ',')[[1]]
  imports <- imports %except% 'R'
  imports <- paste( imports, collapse=', ')

  owndoc <- find.documented( pos, doctype='own')
#  internals <- character(0)
#  for( internaldoc in owndoc[ sapply( owndoc, function( x) regexpr( '-internal', attr( get( x), 'doc')[1])>0)]) {
#    tc <- unclass( attr( get( internaldoc), 'doc'))[-1]
#    gap <- index( regexpr( '[^ ]', tc)<0)[1]
#    internals <- c( internals, gsub( ' +', '', tc[ 1 %upto% (gap-1)]))
#  }
  force.exports <- possible.methods <- find.funs( pos)
  force.exports <- force.exports[ sapply( force.exports,
      function( x) !is.null( attr( get( x, pos=pos), 'export.me')))]
  possible.methods <- possible.methods %except% force.exports
  exports <- unique( c( find.funs( pos) %that.are.in% find.documented( pos), force.exports))

  methods <- list()
  for( gen in names( .knownS3Generics))
    methods[[ gen]] <- possible.methods %that.match% ('^' %&% to.regexpr( gen) %&% '\\.')
  methods <- methods[ sapply( methods, length)>0]
  exports <- exports %except% unlist( methods, use.names=FALSE)
  exports <- 'export( "' %&% paste( exports, collapse='",\n"') %&% '")\n'

  for( gen in names( methods))
    for( meth in substring( methods[[ gen]], nchar( gen)+2))
      exports <- exports %&% 'S3method( "' %&% gen %&% '", "' %&% meth %&% '")\n'

  path <- file.path( path, sub( '^package:', '', attr( pos.to.env( pos), 'name')))
  mkdir( path)
  if( length( imports))
    exports <- c( 'import( ' %&% imports %&% ')\n', exports)
  cat( exports, file=file.path( path, 'NAMESPACE'))
}


"make.new.cd.task" <-
function( task.name, nlocal=sys.parent(), answer, dir.name) mlocal({
  dir.name <- file.path( task.home(), legal.filename( task.name))
  line.end <- ifelse( option.or.default( 'cd.extra.CR', FALSE), '\n', '')
  
  repeat {
    cat("Default directory = ", dir.name, "\n(names will be expanded relative to ", task.home(), 
        ")\nDirectory: " %&% line.end)
    answer <- readline()
    if(answer == "")
      answer <- dir.name
    else if( ! (substr( answer, 1, 1) %in% c( '/', '\\')) && !pos(":", answer)[1])
      answer <- file.path( task.home(), answer)

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
  rdata.path <- get.Rdata.path( dir.name)
  if( !file.exists( rdata.path))
    save( list=character(0), file=rdata.path)
  names( dir.name) <- task.name
  dir.name
})


"make.usage.section" <-
function( funs=find.funs() %except% find.documented( doctype='Rd'), file=stdout()) {
  usage <- function( x) { 
      if( regexpr( '^%.*%', x)>0) {
        # Assumes binary op with no defaults
        y <- names( formals( get( x)))   
        y <- paste( y[1], x, y[2], sep=' ')
      } else {
				y <- clip( deparse( args( x)))
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


"maybe.save.after.move" <-
function (to.from) {
  if (is.na(to.from$saving)) 
    to.from$saving <- yes.no("Save workspace of \"" %&% names(to.from$path) %&% 
          "\"? ")
  if (to.from$saving) {
    is.right.env <- function( x) identical( pos.to.env(x), to.from$env)
    pos <- index( sapply( seq( along=search()), is.right.env))[1]
    if( !is.na( pos))
      Save.pos( pos) # de-mtrace temporarily
    else
      save.mvb.db( to.from$env, get.Rdata.path( to.from$path))  # 7/2/2005
  }
}


"mcachees" <-
function( envir=.GlobalEnv)
  if( is.null( mcache <- attr( envir, 'mcache'))) character(0) else names( mcache)


"mkdir" <-
function( dirlist) {
  outcome <- logical(length(dirlist))
  for (dir in 1:length(dirlist)) {
    answer <- strsplit(strsplit(dirlist[dir], "/", FALSE)[[1]], "\\\\", FALSE)
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
function( ..., what, envir=.GlobalEnv) {
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

  move.to.mcache( what, envir, sys.frame( mvb.sys.parent()))
  if( !identical( envir, .GlobalEnv))
    save.refdb( envir=envir) # not until asked
}


"mlibrary" <-
function ( ..., character.only=FALSE, logical.return=FALSE, 
    task, pos=lib.pos(), execute.First=TRUE) {
  if( missing( task)) { # call system version, perhaps with 'pos' specified
    hack.library() # just in case; should be hacked already
return( library( pos=pos, character.only=character.only, 
    logical.return=logical.return, ...))
  }

  # Otherwise use MVB's version

  task.name <- character( 0)

  if( !character.only) {
    if( is.name( substitute( task)))
      task <- as.character( substitute( task))
    if( !is.null( names( task))) { # an artificial way to set 'tasks', for people not fully using 'mvbutils'
      tasks <- task
      task <- names( task) }
  }
  
  package <- task
  pkgname <- paste("package", package, sep = ":")
  task.name <- package

  if( !exists( 'tasks') || is.na( pkgpath <- tasks[ package])) {
    warning( "Can't find task '" %&% package %&% "'")
    if( logical.return)
return( FALSE)
    else
stop()
  }

  names( pkgpath) <- package # important, so "cd" spots it as a task  
  
  # Call basic routine

  libpath <- tasks[ task]
  libpath <- substring( libpath, 1, rev( pos( '/', libpath))[1]-1)
  l <- list( ...)
  mc <- as.call( c( list( as.name( 'library')), l, 
      list( package=task, lib.loc=libpath, character.only=character.only)))
  mc <- match.call( library, mc)

# Disable "no R code" warnings
  ow <- options( warn=-1)$warn
  try( eval( mc))
  options( warn=ow)
  
  # Figure out where the results went
  s <- search()
  m <- match( 'package:' %&% task, s, 0)
  if( !m) {
    warning( "package '" %&% task %&% "' not loaded")
    if( logical.return)
return( FALSE)
    else
stop()    
  }

  env <- pos.to.env( m)
  set.path.attr( env, pkgpath) # so cd understands it

  if( !length( o <- objects( envir=env, all.names=TRUE)) || identical( o, '.packageName')) {
    if( file.exists( file.path( pkgpath, '.RData'))) # mvb
      load( file.path( pkgpath, '.RData'), envir=env)
    else 
      warning( "Package " %&% task %&% "contains no R code")

# .First:
    if (execute.First && exists( ".First.lib", envir = env, inherits = FALSE)) {
      firstlib <- get( ".First.lib", envir = env, inherits = FALSE)
      tt <- try( firstlib( libpath, package))
      if( tt %is.a% "try-error") {
        warning(".First.lib failed in" %&% task)
return( FALSE)  
      }
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
      savers <- lapply( savers, get, envir=nlocal.env)
    }

#   Parameters and temporary working variables:

    for( i in names( params)) {
      if( eval( call( 'missing', i), envir=sp.env)) {
        if( is.symbol( params[[ i]]) && !nchar( as.character( params[[ i]])) && exists.mvb( i, env=nlocal.env))
          remove( list=i, envir=nlocal.env)
        else
          assign( i, params[[ i]], envir=nlocal.env) }
      else
        assign( i, eval( call( 'get', i), envir=sp.env), envir=nlocal.env)
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
  if( !is.character( to))
    to <- deparse( to)
  if( !is.character( from))
    from <- deparse( from)

  from <- find.path( char=from)
  to <- find.path( char=to)
  if( from==to)
stop( '"from" and "to" point to the same place!')

  from <- prepare.for.move( from)
  to <- prepare.for.move( to)

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
    assign( i, get( i, env=from$env), envir=to$env)
    move.backup.file( i, old.dir=from$path, new.dir=to$path)
  }

  if( length( whatrefs)) {
    new.to.mcache <- mupdate.mcache( whatrefs, to.mcache)

    from.obj.files <- file.path( from$path, 'obj' %&% from.mcache[ whatrefs] %&% '.rda')
    to.obj.files <- file.path( to$path, 'obj' %&% new.to.mcache[ whatrefs] %&% '.rda')

    file.remove( to.obj.files)
    renamed <- file.rename( from.obj.files, to.obj.files)
    if( any( !renamed)) 
      file.copy( from.obj.files[ !renamed], to.obj.files[ !renamed] )

    attr( to$env, 'mcache') <- new.to.mcache
    setup.mcache( to$env, refs=whatrefs) # change nfile & env
  }

  move.fix.list()

  maybe.save.after.move( to)

  if( !copy) {
    remove( list=what, envir=from$env)
    if( length( whatrefs)) {
      file.remove( from.obj.files)
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
  if( exists( 'fix.list', where='mvb.session.info')) {
    path.list <- get.path.list()
    fix.list <- get( 'fix.list', pos='mvb.session.info')
    fixing <- match( fix.list$name, what, 0) > 0
    if( any( fixing)) { # must all be moving to the same place
      to.match.fun <- function( env) { 
          x <- attr( env, 'path')
          if( is.null( x)) 
        return( 0)
        
          x <- names( x)[1]
          if( is.null( x))
        return( 0)
        
          match( x, path.list, 0)
        }

      to.match <- to.match.fun( to$env)
      if( to.match)
        fix.list$where[ fixing] <- path.list[ to.match]
      else {
        cat( 'Warning: the following will now be saved to ROOT:', fix.list$name[ fixing], sep='\n')
        fix.list$where[ fixing] <- 'ROOT'
      }
      put.in.session( fix.list=fix.list)
    }
  }
})


"move.to.mcache" <-
function( what, envir, getfrom) {
  mcache <- attr( envir, 'mcache')
  if( is.null( mcache))
    mcache <- numeric(0)

  if( !length( what))
return( mcache)

  what <- (what %SUCH.THAT% exists( ., envir=envir)) %SUCH.THAT% !bindingIsActive( ., env=envir)
#  what <- what %SUCH.THAT% exists( ., envir=getfrom, inherits=TRUE)

  ow <- options( warn=-1)
  on.exit( options( ow))
  attr( envir, 'mcache') <- mcache <- mupdate.mcache( what, mcache)

  path <- attr( envir, 'path')
  for( i in what) {
    # Anything moved to the cache must be saved
    this.file <- file.path( path, 'obj' %&% mcache[ i] %&% '.rda')
    save( list=i, file=this.file, envir=getfrom, compress=TRUE)

    fx <- get.mcache.reffun( i, envir)
    environment( fx)[[ i]] <- envir[[ i]]
    remove( list=i, envir=envir)
    suppressWarnings( makeActiveBinding( i, fx, envir))
  }

return( mcache)
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


"mupdate.mcache" <-
function( what, mcache) {
  had.num <- what %such.that% (. %in% names( mcache))
  need.num <- what %except% had.num
  if( !length( need.num))
return( mcache)

  if( !length( mcache))
    new.mcache <- seq( along=need.num)
  else {
    new.mcache <- (1 %upto% (max( mcache) + length( need.num))) %except% abs( mcache)
    new.mcache <- new.mcache[ 1:length( need.num)]
  }

  names( new.mcache) <- need.num

  c( mcache, new.mcache)
}


"mvb.match.call" <-
function (definition = sys.function( mvb.sys.parent()), call = sys.call(mvb.sys.parent()), expand.dots = TRUE) 
.Internal(match.call(definition, call, expand.dots))


"mvb.nargs" <-
function() 
  length( sys.calls()[[ mvb.sys.parent()]])-1


"mvb.parent.frame" <-
function (n = 1) 
  sys.frame( mvb.sys.parent( n))


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
    callio <- substitute(value, envir = parent.frame())
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
function (x) 
{
    names(x) <- as.character(x)
    x
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


"plot.cdtree" <-
function( x, ...) {
  foodweb( x, ...)
  invisible( x)
}


"plot.foodweb" <-
function( x, textcolor, boxcolor, xblank, border, textargs=list(), use.centres=TRUE, color.lines=TRUE, poly.args=list(),
    expand.xbox=1.05, expand.ybox=expand.xbox*1.2, plotmath=FALSE, ...) {
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
  charscale <- par('1em')
  if( is.null( charscale))
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
    text( x[i], level[i], funs[[i]], col=textcolor)
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


"prepare.for.move" <-
function( path) {
  env <- index( sapply( 1:length( search()), function( x) !is.null( spath <- attr( pos.to.env( x), 'path')) && spath==path))[1]
  if( is.na( env)) {
    env <- new.env()
    attr( env, 'path') <- path
    load.refdb( file=get.image.filename( path), envir=env)
    saving <- TRUE
  } else {
    env <- pos.to.env( env)
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
    load.mvb( file = get.image.filename( full.path), name=names( full.path), pos=1, path=full.path)
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
function( dir) {
  files <- object.names <- character( 0) # in case can't find
  
  index.file <- file.path( dir, '.Backup.mvb/index')
  if( file.exists( index.file)) {
    ow <- options( warn=-1)
    files <- readLines( index.file)
    options( ow)
    files <- files[ substr( files, 1, 2)=='BU']
  }
  
  if( length(files)) {
    object.names <- strsplit( files, '=', FALSE)
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


"README.mvbutils" <-
function() 
  help( 'README.mvbutils')


"readr" <-
function( x, install=FALSE) {
  progrdr <- option.or.default( 'program.reader', NULL)
  if( is.null( progrdr) || install)
    progrdr <- install.proged( option.name='program.reader')

  if( missing( x))
return( 'Nothing to edit!')

  fixr.guts( as.character( substitute( x)), new=FALSE, proged=progrdr, fixing=FALSE)
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
  mcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names(.) %in% lsall( envir))

  changed.objs <- objs %such.that% (mcache[.]<0)
  if( length( changed.objs)) {
#		e <- new.env() # looks as if 'e' is unnecessary-- acbins get saved as normal objects
		for( i in changed.objs) 
			save( list=i, file=file.path( path, 'obj' %&% -mcache[ i] %&% '.rda'),
					envir=envir, compress=TRUE)

    mcache[ changed.objs] <- -mcache[ changed.objs]
  }

  attr( envir, 'mcache') <- mcache
}


"save.mvb.db" <-
function( env, path, dbtype=attr( path, 'mvb.db.type'), objs, ...) {
  if( missing( objs))
    objs <- lsall(envir = env) %except% dont.save()

  if( dbtype=='LazyLoad')  {
    for( i in objs)
      get( i, envir=env)
    save(list = objs, file = path, envir = env, ...)
    lazify( path, package=sub( '\\.rdb$', '', basename( path)), pkgpath= dirname( dirname( path)))
  } else
    save.refdb( path, env, ...)
}


"Save.pos" <-
function (pos, path, ascii = FALSE) {
  set.pos.and.path()
#    on.exit(save.pos(pos)) # in R2.0, can't safely default to this

  if ("mvb.session.info" %!in% search()) {
    warn("Can't find session info")
return(invisible(NULL))
  }

  in.this <- character( 0)
  repeat{ # to allow BREAK
    if( !exists("tracees", "mvb.session.info", inherits = FALSE))
  break
    tracees <- get("tracees", "mvb.session.info", inherits = FALSE)
    in.this <- lsall(pos) %such.that% (. %in% names(tracees))
    pe <- pos.to.env( pos)
    in.this <- in.this %SUCH.THAT% (mode(pe[[.]])=='function')
    if (!length(in.this))
  break
    # Should maybe change this to assign into a temp envir born by pe
    # Then we never end up zapping the original
    # I think save.mvb.db can cope
    
    assign("[[", my.index)
    tr.body <- list()
    for (n in in.this) {
      f <- get(n, pos = pos)
      old.env <- environment(f)
      old.attr <- attributes(f)
      tr.body[[n]] <- body(f)
      if (is.recursive(body(f)) && body(f)[[1]] == "{" && length(body(f)) >= 2 &&
          is.recursive(body(f)[[2]]) && body(f)[[2, 1]] == "return" &&
          length(body(f)[[2]]) > 1) {
        formals(f) <- formals(body(f)[[3]])
        if (body(f)[[2, 2, 1]] == "evaluator")
          body(f) <- list(body(f)[[4]])
        else if (body(f)[[2, 2, 1]] == "mlocal" && body(f)[[2, 2, 2, 1]] == "evaluator")
          body(f) <- substitute(mlocal(x), list(x = body(f)[[4]]))
        else if (body(f)[[2, 2, 1]] == "do.in.envir" && is.recursive(body(f)[[2, 2]]$fbody) &&
            body(f)[[2, 2]]$fbody[[1]] == "evaluator") {
          if (any(names(body(f)[[2, 2]]) == "envir"))
            body(f) <- substitute(do.in.envir(envir = this.envir, x),
               list(this.envir = body(f)[[2, 2]]$envir, x = body(f)[[4]]))
          else
            body(f) <- substitute(do.in.envir(x), list(x = body(f)[[3]]))
        }
        environment(f) <- old.env
        attributes(f) <- old.attr
        assign(n, f, pos = pos)
      }
    }
  break # always
  }

  save.mvb.db( pos.to.env( pos), path, mvb.db.type)
  if( !is.null( option.or.default( 'backup.fix', NULL)))
    create.backups( pos=pos)

  for (n in in.this) {
    f <- get(n, pos = pos)
    old.env <- environment(f)
    old.attr <- attributes(f)
    body(f) <- tr.body[[n]]
    environment(f) <- old.env
    attributes(f) <- old.attr
    assign(n, f, pos = pos)
  }
  return(invisible(NULL))
}


"save.refdb" <-
function( file, envir, ...) {
  if( missing( file)) {
    path <- attr( envir, 'path')
    if( !is.dir( path))
      mkdir( path)
    file <- get.image.filename( path)
  }

  mcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names( .) %in% lsall( envir))
  attr( envir, 'mcache') <- mcache

  # Housekeep dead files
  objfiles <- list.files( dirname( file), '^obj[0-9]+\\.rda$')
  file.remove( objfiles %except% ('obj' %&% mcache %&% '.rda'))

  if( length( mcache)) {
		cache.name <- get.mcache.store.name( envir) %&% '0' # guaranteed not to exist & to be findable
		e <- new.env( parent=envir)
		assign( cache.name, abs( mcache), e) # avoid assigning into envir
		ans <- save( list = c( cache.name, lsall( envir=envir) %except% c( names( mcache), dont.save())),
				file=file, envir=e, ...)
		rm( e) # ?not necessary?

    save.mchanged( names( mcache), envir)
  } else
    ans <- save( list=lsall( envir=envir) %except% dont.save(), file=file, envir=envir, ...)

  ans
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
  if (!is.numeric(pos))
    pos <- index(search() == pos)[1]
  if (is.na(pos))
stop("Can't figure out what pos '", pos, "' means!")
  if (missing(path)) {
    path <- attr(pos.to.env(pos), "path")
    if (is.null(path))
    {
      cat("No obvious place to save it. What is the filename (single slashes only please)? ")
      path <- readline()
    }
  }

  path <- get.Rdata.path( path)
  mvb.db.type <- attr( path, 'mvb.db.type')
  path <- c( path)
})


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

  # Create promises to load
  remove( list=refs %such.that% (. %in% lsall( envir=envir)), envir=envir) # only needed with 'move'
  for( i in refs) {
    objfile <- file.path( fpath, 'obj' %&% mcache[ i] %&% '.rda')
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
    prompt.echo=getOption( 'prompt')) {
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

    tryo <- try( list( parse( file=con, n=1))) 
    if( tryo %is.a% 'try-error') {
#      print( readLines( con))
      if( echo) 
        cat( "parse error; not echoing expression\n")
      errline <- as.numeric( rev( strsplit( geterrmessage(), ' ')[[1]])[1])
      if( !is.na( errline))
stop( "parse error in line " %&% errline)
      else
stop( geterrmessage())        
    }
    
    if( echo) {
      dp <- unlist( lapply( tryo[[1]], deparse), use.names=FALSE)
      dp[ 1] <- prompt.echo %&% dp[1]
      dp[ 2 %upto% length( dp)] <- getOption( 'continue') %&% dp[ 2 %upto% length( dp)]
      cat( '', dp, sep='\n')
    }
    
    last <- eval( tryo[[ 1]], env=envir)
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
    get.i <- get( i, sp)
    if( try( mode( get.i), silent=TRUE) %is.a% 'try-error')
      obs <- obs %except% i
  }
  obs
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
            ")", "^"), 0)
        xx[m > 0] <- "\\" %&% xx[m > 0]
        paste(xx, collapse = "")
    }
    sapply(x, repfun)
}


"upper.case" <-
function (s) 
{
    a <- attributes(s)
    if (exists("casefold", mode = "function")) 
        s <- casefold(s, upper = TRUE)
    else {
        s <- strsplit(s, "", FALSE)
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


"write.sourceable.function" <-
function( x, con, append=FALSE, print.name=FALSE, doc.special=TRUE) {
  if( is.character( con))
    con <- file( con)
  if( need.to.close <- !isOpen( con))
    open( con, open=ifelse( append, 'a', 'w'))

  if( !identical( con, stdout())) {
    sink( con)
    on.exit( sink())
  }

  on.exit( if( need.to.close) try( close( con)), add=TRUE)

  if( print.name) {
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

  natts <- names( attributes( x)) %except% 'source'
  if( length( natts)) {
    # Prepare to have other attributes
    cat( 'structure( ')
    atts <- attributes( x)[ natts]
    attributes( x)[ natts] <- NULL
  }

  environment( x) <- .GlobalEnv # avoid <environment: x> after definition
  print(x)

  if( length( natts)) {
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

