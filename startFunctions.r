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
    
    if(!identical(unname(repos.mem), unname(repos))){
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
  if(nr <= nc) 
    do.call(cbind, rep(list(do.call(rbind, rep(list(m), nr))), nc))
  else
    do.call(rbind, rep(list(do.call(cbind, rep(list(m), nc))), nr))
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

## 
apply.mat <- function(X, MARGIN, FUN, ...){
  if(!(MARGIN == 1 || MARGIN == 2)) stop("MARGIN must be 1 or 2.")
  if(!is.matrix(X)) stop("X must be matrix")

  if(MARGIN == 1){
    ans <- do.call(rbind, lapply(seq_len(dim(X)[[MARGIN]]), function(z) FUN(X[z,], ...)))
     `attributes<-`(ans, list(dim = dim(ans), dimnames = list(dimnames(X)[[1]], `if`(ncol(ans) == ncol(X), dimnames(X)[[2]],
 NULL))))
   } else {
     ans <- do.call(cbind, lapply(seq_len(dim(X)[[MARGIN]]), function(z) FUN(X[,z], ...)))
     `attributes<-`(ans, list(dim = dim(ans), dimnames = list(`if`(nrow(ans) == nrow(X), dimnames(X)[[1]], NULL),
 dimnames(X)[[2]])))
   }
 }

# 'attributes<-' is faster than structure because of less copies

# m <- structure(matrix(1:12, nrow=4), dimnames=list(letters[1:4], paste("x", 1:3, sep="")))
# apply(m, 1, cumsum) ## dropped
# apply.mat(m, 1, cumsum) # matrix layout
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
current.order <- function() sys.call(sys.parent())[[2]][[3]]
# lapply(letters[3:5], function(x) current.order())
# vapply(letters[3:5], function(x) current.order(), 0) 

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
