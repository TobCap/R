load.packages <- (function(){
  if(!.Platform$OS.type == "windows")
    message("I don't check if it works on mac or linux.")
  repos.mem <- character(0)
  #p <- function(...) cat("###", ..., "\n")
  p <- (function() {
    msg <- character(0)
    function(...) {
      msg <<- paste(msg, "###", ..., "\n", collapse = " ")
    }
  })()
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
    
    cat(environment(p)$msg)
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

# faster functions
load.packages("plyr"); lapply.seq <- plyr:::loop_apply;
# install.package("rbenchmark"); library("rbenchmark")
# benchmark(
  # lapply(1:1e4, function(x){x}),
  # lapply(seq_len(1e4), function(x){x}),
  # lapply.seq(1e4, function(x){x})
# )
apply.mat <- function(X, MARGIN, FUN, ...){
  if(!(MARGIN == 1 | MARGIN == 2)) stop("MARGIN must be 1 or 2.")
  if(!is.matrix(X)) stop("X must be matrix")
  if(MARGIN == 1){
    ans <- do.call(rbind, lapply.seq(nrow(X), function(z) FUN(X[z,], ...)))
    `attributes<-`(ans, list(dim = dim(ans), dimnames = list(dimnames(X)[[1]], `if`(ncol(ans) == ncol(X), dimnames(X)[[2]], NULL))))
  } else {
    ans <- do.call(cbind, lapply.seq(ncol(X), function(z) FUN(X[,z], ...)))
    `attributes<-`(ans, list(dim = dim(ans), dimnames = list(`if`(nrow(ans) == nrow(X), dimnames(X)[[1]], NULL), dimnames(X)[[2]])))
  }
} 
# 'attributes<-' is faster than structure because of less copies.
# apply.mat0 <- function(X, MARGIN, FUN, ...){
  # if(!(MARGIN == 1 | MARGIN == 2)) stop("MARGIN must be 1 or 2.")
  # if(!is.matrix(X)) stop("X must be matrix")
  # if(MARGIN == 1){
    # ans <- structure(do.call(rbind, lapply.seq(nrow(X), function(z) FUN(X[z,], ...))), dimnames = list(dimnames(X)[[1]], NULL))
    # if(ncol(ans) == ncol(X)) colnames(ans) <- colnames(X)
    # return(ans)
  # } else {
    # ans <- structure(do.call(cbind, lapply.seq(ncol(X), function(z) FUN(X[,z], ...))), dimnames = list(NULL, dimnames(X)[[2]]))
    # if(nrow(ans) == nrow(X)) rownames(ans) <- rownames(X)
    # return(ans)
  # }
# } 

# xx <- matrix(1:4,2,2)
# `attributes<-`(xx, list(dim=dim(xx), dimnames = list(letters[1:2], LETTERS[1:2])))
# `attributes<-`(xx, c(list(dim=dim(xx)), list(dimnames = list(letters[1:2], LETTERS[1:2]))))
#
# > benchmark(l=list(a=1, b=list(2,3)), c=c(list(a=1), list(b=list(2,3))),replications=1e6, columns=1:4)
  # test replications user.self sys.self
# 2    c      1000000      4.15     0.01
# 1    l      1000000      3.32     0.00

# benchmark(x1 = 'if'(TRUE, 1, 0), x2 = .Primitive("if")(TRUE, 1, 0), x3 = if(TRUE) 1 else 0, replications=1e5)[,1:4]
# almost equal time

# m <- structure(matrix(1:12, nrow=4), dimnames=list(letters[1:4], paste("x", 1:3, sep="")))
# apply(m, 1, cumsum) ## transposed!
# apply.mat(m, 1, cumsum) # the same layout
#
# m2 <- matrix(0, 1e3, 1e3)#
# > benchmark(apply(m2, 1, sum), apply.mat(m2, 1, sum), rowSums(m2))[,1:4]
#                    test replications elapsed relative
# 1     apply(m2, 1, sum)          100    4.38 3.421875
# 2 apply.mat(m2, 1, sum)          100    1.28 1.000000

# y <- matrix(0, 1e3, 1e3)
# > benchmark("ncol"=ncol(y), "[2]"=dim(y)[2], "[2L]"=dim(y)[2L], "[[2]]"=dim(y)[[2]], "[[2L]]"=dim(y)[[2L]], replications=1e5)[,1:5]
# > benchmark("nrow"=nrow(y), "[1]"=dim(y)[1], "[1L]"=dim(y)[1L], "[[1]]"=dim(y)[[1]], "[[1L]]"=dim(y)[[1L]], replications=1e5)[,1:5]

###
cd <- function(dir) {
  if (missing(dir)) dir <- Sys.getenv("HOME")
  on.exit(utils::setWindowTitle(getwd()))
  setwd(dir)
}

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
