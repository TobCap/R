library2 <- function(package.name, ...) {
  stopifnot(try(is.character(package.name)))
  if (!package.name %in% .packages(TRUE)) utils:::install.packages(package.name)
  library(package.name, character.only = TRUE, ...)
}

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
    
    if(!foundInCRAN) p("Some packages are not found in a current CRAN.\n 
      If you want to install them from r-forge or Omegahat,\n
      do firstly setRepositories() and select repos, then call me again.")
    
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
makeActiveBinding("Q", q, env = attach(NULL, name = "q"))
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

## delayed assign list; capture other variables 
list2 <- function(...) {
  dots <- match.call(expand.dots = FALSE)$...
  e <- new.env(parent = parent.frame())
  for(i in seq_along(dots)) {
    eval(bquote(delayedAssign(names(dots[.(ii)]), eval(dots[[.(ii)]], e), assign.env = e), list(ii = i)))
  }   
  as.list(e, all.names = TRUE)
}
## examples
# list2(x = 1, y = x)
# list2(x = y, y = 1) # capture a posterior variable  
# list2(x = y + z + 1, y = 10, z = y + 3) # capture nested variables
# list2(x = 1:5, y = x + 1, z = sum(x)) # can use function
# z <- 100; list2(x = y + 1 , y = z + 10); rm(z) # capture an enclosure's variable

## create new environment when a function is defined, which is defferent from list()
# l2 <- list2(x = 10, y = function(k) x * k)
# environment(l2$y); l2$y(1) # => 10
# l <- list(x = 10, y = function(k) x * k)
# environment(l$y); l$y(1) # => error

# list2(x = z, y = 10 + x, z = y + 3) # error; circuler reference
# list2(x = z, 1, z = 2) # error; all variables requires its name

## more readable 
# params <- list2(mat = matrix(1:12, 4, 3), nr = nrow(mat), nc = ncol(mat))
# params <- within(list(), {mat <- matrix(1:12, 4, 3); nr <- nrow(mat); nc <- ncol(mat)})



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
current.order <- function() 0L + sys.call(-1)[[2]][[3]] # need 0L+ after R.Version 3.1
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
assign2 <- function(x_char, value, check_funs = c(class, length), envir = parent.frame()) {
  ## The idea of this function comes from makeActiveBinding's example.
  ## Enter ?makeActiveBinding in R console.
  
  if (!is.character(x_char))
    stop("lhs must be symbol or character")
  
  checker <- c(check_funs)
  if(!all(vapply(checker, is.function, FALSE)))
    stop("check_funs must be function")

  x_sym <- as.symbol(x_char)
  
  checked_value <- lapply(checker, function(f) f(value))
  checked_1 <- checked_value[[1]] # for speed-up when length(checked.funs) == 1

  msg <- paste0(
    "Right-side's ",
    if (length(checker) == 1) shQuote(substitute(check_funs))
    else paste0(shQuote(as.character(as.list(substitute(check_funs))[-1])), collapse = " or "),
    " is different from existing value.\n", collapse = "")

  out.fun <- function(v) {
    if (!missing(v)) {      
      for (i in seq_along(checker))
        if (!isTRUE(checker[[i]](v) == checked_value[[i]]))
          stop(msg)
      value <<- v
    }
    value
  }

  if (length(checker) == 1) # for speed-up
    body(out.fun)[[2]][[3]][[2]] <- quote(if (check_funs(v) != checked_1) stop(msg))
  
  cat(x_sym, "is trying to assign...", "\n")
  makeActiveBinding(x_sym, out.fun, envir)
  
}

## sugar
`%<-@%` <- function(lhs, rhs){
  assign2(as.character(substitute(lhs)), rhs, envir = parent.frame())
}

## assign immutable variable
assign3 <- function(var.char, val, envir = parent.frame()){
  if(!is.character(var.char)) stop("1st argument must be character")
  assign(var.char, val, envir = envir)
  invisible(lockBinding(var.char, envir))
}

## := is parsable due to a historical reason and can be assigned.
## See http://developer.r-project.org/equalAssign.html
## Also see gram.y #2807-2809 in R source code.
`:=` <- `%<-!%` <- function(lhs, rhs) {
  assign3(as.character(substitute(lhs)), rhs, envir = parent.frame())
}

## multiple return values and assignments
`%<~%` <- function(symbol.list, expr, e = parent.frame()) {
  symbols_ <- substitute(symbol.list)
  var.chars <- as.character(symbols_[-1])
  values <- eval(substitute(expr), enclos = e)
   
  # x::xs %<~% 1:5
  # x::xs %<~% list(1,2,3,4,5)
  if (symbols_[[1]] == "::" && length(symbols_) == 3) {
    assign(var.chars[[1]], values[[1]], envir = e)
    assign(var.chars[[2]], values[-1], envir = e)
    return(invisible())
  }
  
  # recycling is not allowed.
  if (length(values) != length(var.chars))
    stop("The length of answers and variables must be the same.")
  if (typeof(values) != "list")
    stop("Only list can be assigned to multiple variables.")
    
  for(i in seq_along(var.chars)) {
    assign(var.chars[[i]], values[[i]], envir = e)
  }
}

# {x; y; z} %<~% {
#   a <- 1 + 2
#   b <- 3 * 4
#   c <- 5 / 6
#   list(a, b, c)
# }
# rm(x, y, z)

# c(x, y, z) %<~% {
#   a <- 1 + 2
#   b <- 3 * 4
#   c <- 5 / 6
#   list(a, b, c)
# }
# rm(x, y, z)

# counter <- function(i) {
#   force(i)
#   list(
#     increase = function(x = 1) {i <<- i + x; i},
#     decrease = function(x = 1) {i <<- i - x; i}
#   )
# }

# {inc; dec} %<~% counter(10)
# inc()
# inc()
# dec()
# dec(5)
# rm(inc, dec, counter)

# x::xs %<~% 1:5
# rm(x, xs)
# x::xs %<~% list(1,2,3,4,5)
# rm(x, xs)


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
    cat(eval(substitute(list(...))[[2]],  parent.frame())[[1]])
    substring(strsplit(rval[[1]], " ")[[1]][[1]], 2)
  }
  # capture.output(inspect(...))
  # It works well seemingly, but after calling this function more than two times,
  # the result of inspect(x) indicates that NAM is converted from 1 to 2.
  capture.output.cat(inspect(...))
}



## goes-to function can check arguments class.
`%->%` <- function(lhs, rhs, env_ = parent.frame()) {
  as.formals <- function(xs) as.pairlist(tools:::as.alist.call(xs))

  expr <- substitute(lhs)
  if (length(expr) > 1) {
    args_expr <- as.vector(expr, "list")[-1]
  } else if (length(expr) == 1 && class(expr) != "{") {
    args_expr <- list(expr)
  } else {
    args_expr <- NULL
  }
  
  # short-cut for non-class-defined situation
  if (!any(c(":", "=") %in% all.names(expr)))
    return(eval(call("function", as.formals(args_expr), substitute(rhs)), env_))

  arglist.converted <- mapply(
    function(arg_expr, expr_named) {
      arg_expr_char <- as.character(arg_expr)
      has_name <- !is.null(expr_named) && nzchar(expr_named)
      if (!has_name) {
        if (is.call(arg_expr) && arg_expr[[1]] == quote(`:`)) {
          ## in case class is defined
          if (arg_expr_char[[3]] %in% sub("is.", "", ls(pattern = "^is\\.", baseenv()))) {
            arg_expr_new <- as.formals(arg_expr_char[[2]])
            arg_class <- arg_expr_char[[3]]
          } else if (tolower(arg_expr_char[[3]]) == "any"){
            arg_expr_new <- as.formals(arg_expr_char[[2]])
            arg_class <- NA
          } else {
            stop("'", paste0(arg_expr_char[[3]], "' is not appropriate class designation."))
          }
        } else if (is.call(arg_expr) && arg_expr[[1]] == quote(`=`)) {
          ## default value is set
          arg_expr_new <- as.formals(`names<-`(list(arg_expr[[3]]), arg_expr_char[[2]]))
          arg_class <- class(eval(arg_expr[[3]], env_))
        } else if (is.symbol(arg_expr)) {
          ## only a symbol. This allows any class.
          arg_expr_new <- as.formals(arg_expr_char)
          arg_class <- NA
        } else {
          stop("An argument must be a symbol.")
        }
      } else { ## When has_name, assigning value must be able to be evaluate.
        arg_expr_new <- as.formals(expr_named, list(arg_expr))
        arg_class <- class(eval(arg_expr, env_))
      }

      call_of_is_checking <-
        if (is.na(arg_class)) NULL
        else call(paste0("is.", arg_class), as.symbol(names(arg_expr_new)))

      list(arg_expr_new = arg_expr_new, call_of_is_checking = call_of_is_checking)
    }, 
    args_expr, 
    rep_len(as.list(names(args_expr)), length(args_expr)),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  arglist <- unlist(lapply(arglist.converted, function(x) x$arg_expr_new), recursive = FALSE)
  calls_of_is_checking <- lapply(arglist.converted, function(x) x$call_of_is_checking)
  select_not_NULL <- function(x) x[!vapply(x, is.null, logical(1))]
  
  and_bool_expr <- (function(x){
    n <- length(x)
    if (n == 0) quote(TRUE)
    else if (n == 1) x[[1]]
    else call("&&", Recall(x[-n]), x[[n]])
  })(select_not_NULL(calls_of_is_checking))

  expr_add <-
    call("if",
     call("!",
      call("(", and_bool_expr)),
        quote(stop("Some inputs are not appropriate.")))

  body_ <-
    if (all(as.character(expr_add[1:2]) == c("if", "!(TRUE)"))) substitute(rhs)
    else as.call(append(as.list(substitute(rhs)), expr_add, 1))

  eval(call("function", as.pairlist(arglist), body_), env_)
}

# {} %->% {x + 2}
# x %->% {x + 1}
# {x; y} %->% {x + y}
# {x = 1L; y = 2L} %->% {x + y}
# {x:numeric; y:numeric} %->% {x + y}
# {x:character; e:environment} %->% {get(x, envir = e, inherits = FALSE)}
## see more examples in https://gist.github.com/TobCap/6826123


## fun compares vecter elements next to each other
compare_ <- function(fun, vals) {
  iter <- function(vals){
    if (length(vals) == 1) TRUE
    else if (is.na(vals[[1]])) NA
    else if (!fun(vals[[1]], vals[[2]])) FALSE
    else out.fun(vals[-1])
  }
  iter(vals)
}

compare_vec <- function(fun) function(xs) {
  n <- length(xs)
  if (!is.atomic(xs)) xs <- unlist(xs, recursive = FALSE)
  all(fun(xs[-n], xs[-1]))
}

`<.`  <- compare_vec(`<`)
`<=.` <- compare_vec(`<=`)
`>.`  <- compare_vec(`>`)
`>=.` <- compare_vec(`>=`)
`==.` <- compare_vec(`==`)
## `!=` is not a transitive relation, so '!=.' cannnot be defined as the same way. 

# `<.`(1:100)
# `<=.`(c(1:100,99))

###
# avoiding conflict with utils::zip
# just using mapply()
zip_ <- function(..., FUN = list) {
  dots <- as.list(...)
  args_seq <- seq_len(min(vapply(dots, length, 0)))
  args_new <- lapply(dots, function(x) x[args_seq])
  do.call(mapply, c(FUN = FUN, args_new, SIMPLIFY = FALSE, USE.NAMES = FALSE))
}

unzip_ <- function(lst, fun = `c`) {
  do.call(function(...) mapply(fun, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE), lst)
}

# zip_(list(1,2,3), list(4,5,6))
# zip_(1:3, 4:6)

# zip_(list(1,2,3), list(4,5,6), list(7,8,9))
# zip_(1:3, 4:6, 7:9)
# zip_(1:3, letters[1:3])

# zip2 <- function(x, y, FUN = list){
#   if(length(x) == 0 || length(y) == 0) NULL
#   else append(list(FUN(x[[1]], y[[1]])), zip2(x[-1], y[-1], FUN = FUN))
# }
# zip3 <- function(x, y, z, FUN = list){
#   if(length(x) == 0 || length(y) == 0 || length(z) == 0) NULL
#   else append(list(FUN(x[[1]], y[[1]]), z[[1]]), zip2(x[-1], y[-1], z[-1], FUN = FUN))
# }
# zip_ <- function(... , FUN = list) {
#   dots <- list(...)
#   elem.len <- min(vapply(dots, length, 0))
#   if(elem.len == 0) NULL
#   else append(
#     list(do.call(FUN, lapply(dots, `[[`, 1))),
#     do.call(zip_, c(lapply(dots, `[`, -1), FUN = FUN)))
# }

zip_with <- function(fun, ..., do_unlist = FALSE) {
  if (do_unlist) unlist(zip_(..., FUN = match.fun(fun)))
  else zip_(..., FUN = match.fun(fun))
}
# zip_with(`+`, list(1, 2), list(11, 12))
# zip_with(paste0, c("a", "b"), list(11, 12))
