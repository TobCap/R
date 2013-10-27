load.packages <- (function(){
  if(!.Platform$OS.type == "windows")
    message("I don't check if it works on mac or linux.")
  repos.mem <- character(0)
  p <- function(...) cat("###", ..., "\n")
  # "cran.packages" will be created in this environment by delayedAssign function.
  
  function(packageNames, repos = getOption("repos"), lib.loc = NULL){
    stopifnot(is.character(packageNames))
    loaded.package <- sub("package:", "", search())
    if(is.null(lib.loc))
      local.lib <- utils:::installed.packages()[,c("Package", "Version")]
    foundInCRAN <- TRUE
    
    if(!setequal(repos.mem, repos)){
      repos.mem <<- unique(c(repos.mem, repos))
      delayedAssign("cran.packages", {
          p("Accessing CRAN via Internet takes few seconds")
          utils::available.packages(contriburl = utils::contrib.url(repos.mem))[, c("Package", "Version")]
        }
        , assign.env = parent.env(environment()))
    }
    
    for(pkg in packageNames){
      if(pkg %in% loaded.package) {
        p(pkg, "is in use and nothing is done.")
      } else if(pkg %in% local.lib[,"Package"]){
        suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, lib.loc = lib.loc))
        p(local.lib[pkg,], "loaded")
      } else if(length(cran.packages) == 0){
        p(pkg, "is not found in local and you cannot access to CRAN.")
      } else if(!(pkg %in% cran.packages[,"Package"])){    
        p(pkg, "is not found in local and not listed in current CRAN.")
        foundInCRAN <- FALSE
      } else {
        utils::install.packages(pkg, repos = repos.mem, dependencies = TRUE)
        suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, lib.loc = lib.loc))
        p(cran.packages[pkg,], "is newly installed and loaded.")
      }
      utils::flush.console()
    }
    
    if(!foundInCRAN) p("If you want to install some packages from r-forge or Omegahat,\nplease firstly do setRepositories() and select repos, then call me again.")
    
    invisible(utils::flush.console())
  }
})()

# refers to http://ofmind.net/doc/r-tips
read.cb <- function(header = TRUE, ...){
  utils::read.table(file = "clipboard", header = header, ...)
}
write.cb <- function(data, sep = "\t", header = TRUE, row.names = FALSE, 
  col.names = ifelse(header && row.names, NA, header), qmethod="double", ...){
  utils::write.table(data, file = "clipboard", sep = sep, row.names = row.names,
  col.names = col.names, qmethod = qmethod, ...)
}

# quit instead of q()
# environment name is along with this file name itself. 
makeActiveBinding("Q", q, env = as.environment("startFunctions.r")) 
# class(Q) <- Q <- "ask"; print.ask <- q

`%!in%` <- Negate(`%in%`)
`%??%` <- function(x, y) if(is.null(x)) y else x
is.nil <- function(x) is.null(x) || length(x) == 0 && is.list(x)

## matrix operation
`%^%` <- `%**%` <- function(mat, n, acc = diag(1, dim(mat))){
  if (n == 0) acc
  else if (n %% 2 == 0) Recall(mat %*% mat, n / 2, acc)
  else Recall(mat, n - 1, mat %*% acc)
}
rot90 <- function(m, k = 1){
  if(length(dim(m)) != 2) stop("only 2 dim is acceptable")
  (function(M, n){
    if(n == 0) return(M)
    else if(n == 2) return(M[nrow(M):1, ncol(M):1])
    else Recall(t(M)[ncol(M):1, ], n - 1)
  })(m, as.integer(k) %% 4)
}
fliplr <- function(m){
  if(length(dim(m)) != 2) stop("only 2 dim is acceptable")
  m[, ncol(m):1]
}
flipud <- function(m){
  if(length(dim(m)) != 2) stop("only 2 dim is acceptable")
  m[nrow(m):1, ]
}
repmat <- function(m, nr, nc = nr) {
  if(length(dim(m)) > 2) stop("over 3 dim is unacceptable")
  if(nr <= nc) 
    do.call(cbind, rep.int(list(do.call(rbind, rep.int(list(m), nr))), nc))
  else
    do.call(rbind, rep.int(list(do.call(cbind, rep.int(list(m), nc))), nr))
}

## extended list definition; can refer to other variables 
# list2(x = 1, y = x)
# list2(x = 1:5, y = 1, z=length(x))
# list2(x = 1, y = x + 10,z = y * 2)
# list2(x = 1:10, y = x + 1, z = sum(x))
# list2(x = y, y = 10, z = y + 3)
# z <- 1000; list2(x = z, y = x + 1); rm(z)
# list2(x = z, y = 10 + x, z = y + 3) #circuler reference
# list2(m = matrix(1:12,4,3), nr = nrow(m), nc = ncol(m))
# list2(x = 1, y = function(k) x * k)
list2 <- function(..., env = parent.frame()){
  args.orig <- as.list(match.call(expand.dots=FALSE)$...)
  simplify <- function(lst) {
    evaled.lst <- lst
    for(i in seq_along(args.orig)){
      evaled.lst <- eval(substitute(substitute(e, args.orig), list(e = evaled.lst)))
    }
    ## recursive call is costly and slower than `for`
    # make.call <- function(x){
    #   if(length(x) == 0) return(lst)
    #   else eval(substitute(substitute(e, args.orig), list(e = make.call(x[-1]))))
    # }    
    # evaled.lst <- make.call(substitute(lst))
    if(is.call(evaled.lst) && evaled.lst[[1]] != quote(`function`) && 
      isTRUE(!all(sapply(all.vars(evaled.lst), exists, envir = env))))
      stop("circulaer reference is not allowed")
    eval(evaled.lst, args.orig, env)
  }
  lapply(args.orig, simplify)
}

## The answer keeps as matrix.
apply.mat <- function(X, MARGIN, FUN, ...) {
  if (!(MARGIN == 1 || MARGIN == 2)) stop("MARGIN must be 1 or 2.")
  if (!is.matrix(X)) stop("X must be matrix")
  
  if (MARGIN == 1) {
    ans <- do.call(rbind, lapply(seq_len(dim(X)[[1]]), function(z) FUN(X[z,], ...)))
    `dimnames<-`(ans, list(dimnames(X)[[1]], `if`(ncol(ans) == ncol(X), dimnames(X)[[2]], NULL)))
  } else {
    ans <- do.call(cbind, lapply(seq_len(dim(X)[[2]]), function(z) FUN(X[,z], ...)))
    `dimnames<-`(ans, list(`if`(nrow(ans) == nrow(X), dimnames(X)[[1]], NULL), dimnames(X)[[2]]))
  }
}

# m <- structure(matrix(1:12, nrow=4), dimnames=list(letters[1:4], paste0("x", 1:3)))
# apply(m, 1, sum) # vectored
# apply.mat(m, 1, sum) # keep matrix
# apply(m, 1, cumsum) # transposed
# apply.mat(m, 1, cumsum) # keep layout
#
# m2 <- matrix(0, 1e3, 1e3)
# > microbenchmark(apply=apply(m2, 1, sum), apply.mat = apply.mat(m2, 1, sum), rowSums=rowSums(m2))
# Unit: milliseconds
#       expr       min       lq     median         uq       max neval
#      apply 56.161379 93.45846 114.844119 119.433256 126.45192   100
#  apply.mat 24.092916 44.84445  52.180738  54.140633  84.65420   100
#    rowSums  4.125745  7.26764   9.172035   9.301981  10.17059   100

### lookup for matrix or data.frame
lookup <- function(values, tbl, search.col = 1) {
  stopifnot(is.data.frame(tbl) || is.matrix(tbl))
  
  if(is.character(search.col))
    search.col <- match(search.col, colnames(tbl))
  stopifnot(!is.na(search.col))
  
  target <-  
    if (search.col == 0) rownames(tbl)
    else tbl[,search.col]
    
  tbl[target %in% values, ]
}

###
cd <- function(dir) {
  if (missing(dir)) dir <- Sys.getenv("HOME")
  if (!nzchar(dir)) stop("Enter valid directory path")
  on.exit(utils::setWindowTitle(getwd()))
  setwd(dir)
}

### get current order of lapply or vapply
current.order <- function() sys.call(-1)[[2]][[3]]
current.name <- function() names(eval.parent(sys.call(-2)[[2]])[sys.call(-1)[[2]][[3]]])
# lapply(letters[3:5], function(x) current.order())
# vapply(letters[3:5], function(x) current.order(), 0)
# lapply(list(a=1, b="b", c=33),
#   function(x) {
#     n <- current.name()
#     o <- current.order()
#     list(n, o)})

###
rm.variables <- function(env = .GlobalEnv) {
  rm(list = setdiff(utils::ls.str(envir = env), utils::lsf.str(envir = env)), envir= env)
}
rm.functions <- function(env = .GlobalEnv) {
  rm(list = utils::lsf.str(all = TRUE, envir = env), envir = env)
}
rm.all <- function(env = .GlobalEnv) {
  rm(list = ls(all = TRUE, envir = env), envir = env)
}

### assignment
## see examples in https://gist.github.com/TobCap/6713338
## class is bound when assigning
assign2 <- function(x.char, init.val, check.funs = class, envir = parent.frame()) {
  ## The idea of this function comes from makeActiveBinding's example.
  ## Enter ?makeActiveBinding in R console.

  checker <- c(check.funs)
  if(!all(vapply(checker, is.function, FALSE)))
    stop("check.funs must be function")

  x.sym <- as.symbol(x.char)
  
  x <- init.val
  checked.x <- lapply(checker, function(f) f(x))
  checked.x1 <- checked.x[[1]] # for speed-up when length(checked.funs) == 1

  msg <- paste0(
    "Right-side's ",
    if(length(checker) == 1) substitute(check.funs)
    else paste0(as.character(as.list(substitute(check.funs))[-1]), collapse = " or "),
    " is different from existing value.", collapse = "")

  out.fun <- function(v) {
    if (!missing(v)){
      for(i in seq_along(checker))
        if(checker[[i]](v) != checked.x[[i]])
          stop(msg)
      x <<- v
    }
    x
  }
  if(length(checker) == 1) # for speed-up
    body(out.fun)[[2]][[3]][[2]] <- quote(if (check.funs(v) != checked.x1) stop(msg))

  cat(x.sym, "is created!", "\n")
  invisible(makeActiveBinding(x.sym, out.fun, envir))
}

## assign immutable variable
assign3 <- function(var.char, val, envir = parent.frame()){
  if(!is.character(var.char)) stop("1st argument must be character")
  assign(var.char, val, envir = envir)
  invisible(lockBinding(var.char, envir))
}

`%<-@%` <- function(lhs, rhs){
  assign2(as.character(substitute(lhs)), rhs, envir = parent.frame())
}

`%<-!%` <- function(lhs, rhs) {
  assign3(as.character(substitute(lhs)), rhs, envir = parent.frame())
}

### memory inspection:
## http://stackoverflow.com/questions/10912729/r-object-identity/10913296#10913296
## http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2010-March/000508.html
## http://tolstoy.newcastle.edu.au/R/e16/devel/11/11/0263.html

## my examples are in https://gist.github.com/TobCap/6792752
## A promise is copied so dot-dot-dot is suitable.
## inspect(arg, depth (recursive display) or NULL, max display items (more than one))  
## inspect(.GlobalEnv, 3, 30)
## inspect(.GlobalEnv, NULL, NULL) 
inspect <- function(...) .Internal(inspect(...))

## address() is already used in package "pryr" and "data.table"
address2 <- function(...){
  capture.output.cat <- function (...) {
    # simplify body(capture.output) and substitute `cat` for `print`
    rval <- NULL
    file <- textConnection("rval", "w", local = TRUE)
    sink(file)
    on.exit({sink(); close(file)})
    cat(eval(substitute(list(...))[[2]],  parent.frame()))
    
    substring(strsplit(rval[[1]], " ")[[1]][[1]], 2)
  }
  # capture.output(inspect(...))
  # It works well seemingly, but after calling this function more than two times,
  # the result of inspect(x) indicates that NAM is converted from 1 to 2.
  capture.output.cat(inspect(...))
}
