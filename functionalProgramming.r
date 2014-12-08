### functional operators

## Auxiliary function
as.formals <- function(x, value = list(quote(expr=))) {
  ## a faster version of tools:::as.alist.call
  if (!all(nzchar(x)))
    stop('Including "" or substitute() is invalid input.')
  if (length(x) == 0)
    return(NULL)    
  if (is.null(names(x)))
    return(`names<-`(as.pairlist(rep_len(value, length(x))), as.character(x)))
  
  ans <- as.list(x)
  idx <- which(!nzchar(names(ans)))
  names(ans)[idx] <- vapply(ans[idx], as.character, "")
  ans[idx] <- rep_len(value, length(idx))
  as.pairlist(ans)
}
is.formals <- function(x) {
  is.pairlist(x) && length(x) == sum(nzchar(names(x)))
}

### sugar-syntax for lambda
### adopt `f.` instead of `f` because `f` often causes conflicts in many sample codes.
f. <- function(..., env = parent.frame()) {
  # see https://gist.github.com/TobCap/6366396 for how to handle unevaluated `...` 
  d <- as.pairlist(as.vector(substitute((...)), "list")[-1])
  # need to be pairlist to return NULL when nothing is passed to `...`.
  
  n <- length(d)
  eval(call("function", as.formals(d[-n]), d[[n]]), env)
}

# f. <- function(..., env = parent.frame()){
#   d <- lapply(substitute(list(...)), identity)[-1]
#   as.function(c(tools:::as.alist.call(d[-length(d)]), d[length(d)]), envir = env) 
# }
# The above commented code that I made first time is bit slower than the current code.
# See https://gist.github.com/TobCap/6255804 for comparison of cost of creating funciton.
# See also https://github.com/hadley/pryr/blob/master/benchmark/make-function.r

# f.(x, x * 2)
# f.(x, y, x + y)
# f.(x, x * 2)(3)
# f.(x, y, x + y)(1, 2)
# f.(x, f.(y, x + y))(1)(2)

### saves to type anonymous function
# > Reduce(function(x, y) x + y, 1:10)
# [1] 55
# > Reduce(f.(x, y, x + y), 1:10)
# [1] 55

# > f.(y, f.(z, y+z))(1)(2)
# [1] 3
# > f.(y=1, f.(z=2, y+z))()()
# [1] 3

## goes-to function can check arguments class.
`%->%` <- function(lhs, rhs, env = parent.frame()) {
  expr <- substitute(lhs)
  if (length(expr) > 1) {
    arglist.raw <- as.vector(expr, "list")[-1]
  } else if (length(expr) == 1 && expr != quote(`{`)) {
    arglist.raw <- list(expr)
  } else {
    arglist.raw <- NULL
  }
  
  # short-cut for non-class-defined situation
  if (!any(c(":", "=") %in% all.names(expr)))
    return(eval(call("function", as.formals(arglist.raw), substitute(rhs)), env))

  arglist.converted <- mapply(
    function(x, name) {
      x.char <- as.character(x)
      has.name <- !is.null(name) && nzchar(name)
      if (!has.name) {
        if (is.call(x) && x[[1]] == quote(`:`)) {
          ## in case class is defined
          if (x.char[[3]] %in% sub("is.", "", ls(pattern = "^is\\.", baseenv()))) {
            elem <- as.formals(x.char[[2]])
            class_ <- x.char[[3]]
          } else if (tolower(x.char[[3]]) == "any"){
            elem <- as.formals(x.char[[2]])
            class_ <- NA
          } else {
            stop("'", paste0(x.char[[3]], "' is not appropriate class designation."))
          }
        } else if (is.call(x) && x[[1]] == quote(`=`)) {
          ## default value is set
          elem <- as.formals(x.char[[2]], list(x[[3]]))
          class_ <- class(eval(x[[3]], env))
        } else if (is.symbol(x)) {
          ## only a symbol. It allows any class.
          elem <- as.formals(x.char)
          class_ <- NA
        } else {
          stop("An argument must be a symbol.")
        }
      } else { ## When has.name, assigning value must be able to be evaluate.
        elem <- as.formals(name, list(x))
        class_ <- class(eval(x, env))
      }

      check.fun <-
        if (is.na(class_)) NULL
        else call(paste0("is.", class_), as.symbol(names(elem)))

      list(elem = elem, check.fun = check.fun)
    }, 
    arglist.raw, 
    rep_len(as.list(names(arglist.raw)), length(arglist.raw)),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  arglist <- unlist(lapply(arglist.converted, function(x) x$elem), recursive = FALSE)

  check.funs <- lapply(arglist.converted, function(x) x$check.fun)
  rm.null.element <- function(x) x[!vapply(x, is.null, logical(1))]
  
  check.call <- (function(x){
    n <- length(x)
    if (n == 0) quote(TRUE)
    else if (n == 1) x[[1]]
    else call("&&", Recall(x[-n]), x[[n]])
  })(rm.null.element(check.funs))

  expr.add <-
    call("if",
     call("!",
      call("(", check.call)),
        quote(stop("Some inputs are not appropriate.")))

  body_ <-
    if (all(as.character(expr.add[1:2]) == c("if", "!(TRUE)"))) substitute(rhs)
    else as.call(append(as.list(substitute(rhs)), expr.add, 1))

  eval(call("function", as.pairlist(arglist), body_), env)
}
### The previous simple version (not have class checking insertion) was:
# `%->%` <- function(lhs, rhs, env = parent.frame()){
#   l <- substitute(lhs)
#   # coerce list() into NULL by as.pairlist
#   if (length(l) > 1 || class(l) == "{") l <- as.pairlist(as.vector(l, "list")[-1])
#   eval(call("function", as.pairlist(tools:::as.alist.call(l)), substitute(rhs)), env)
# }

# {} %->% {x + 2}
# x %->% {x + 1}
# {x; y} %->% {x + y}
# {x = 1L; y = 2L} %->% {x + y} 
# {x:numeric; y:numeric} %->% {x + y}
# {x:character; e:environment} %->% {get(x, envir = e, inherits = FALSE)}
## see more examples in https://gist.github.com/TobCap/6826123

curry <- function (fun, env = parent.frame()) {
  has.quoted <- FALSE
  recursiveCall <- function(len, arg) {
    if (len == 0) do.call(fun, arg, quote = has.quoted, envir = env)
    else function(x) {
      if (is.language(x)) has.quoted <<- TRUE
      recursiveCall(len - 1, append(arg, list(x)))}}
  recursiveCall(length(formals(args(fun))), list())
}
# > curry(function(x, y, z) x + y + z)(1)(2)(3)
# [1] 6
### `f.` is already defined above and save your typing.
# > curry(f.(x, y, z, x + y + z))(1)(2)(3)
# [1] 6

# Arrow operaters defined later are useful.
# > f.(x, y, z, x + y + z) %|>% curry %<|% 1 %<|% 2 %<|% 3
# [1] 6

# plus2 <- curry(`+`)(2)
# plus2(10) # 12

## If curried function has dots argument, you can recogize its finish by empty (not NULL) argument just like f().
# > curry(sum)(1)(2)
# [1] 3
# > curry.dots(sum)(1)(2)(NA)(3)()(na.rm = TRUE)
# [1] 6

# > curry(lapply)(1:5)(function(x) x ^ 2)()
# Error in ((curry(lapply)(1:5))(function(x) x^2))() : 
#   argument "x" is missing, with no default

# > curry.dots(lapply)(1:5)(function(x) x ^ 2)()
# [[1]]
# [1] 1

# > call("rnorm", 5, 100)
# rnorm(5, 100)
# > curry(call)("rnorm")(5)(100)()
# Error: attempt to apply non-function
# > curry.dots(call)("rnorm")(5)(100)()
# rnorm(5, 100)

curry.dots <- function (fun, env = parent.frame()) {
  args_ <- formals(args(fun))
  has.quoted <- FALSE
  is.dots <- function(n) names(args_)[length(args_) - n + 1] == "..."
  
  recursiveCall <- function(len, arg) {
    if (len == 0) do.call(fun, arg, quote = has.quoted, envir = env)
    else
      function(...) {
        x <- list(...)
        if (is.dots(len)) {
          if (length(x) == 0) recursiveCall(len - 1, arg)
          else {
            if (is.language(x[[1]])) has.quoted <<- TRUE
            recursiveCall(len, append(arg, as.list(x)))}}
        else {
          if (is.language(x[[1]])) has.quoted <<- TRUE
          recursiveCall(len - 1, append(arg, as.list(x)))}}}
  recursiveCall(length(args_), list())
}

# This recursively composes language-tree and is faster than curry(), but only closure is acceptable
curry.closure <- function(f, e = parent.frame()){
  stopifnot(is.function(f), typeof(f) == "closure")
  make.body <- function(args_){
    if (length(args_) == 0) body(f)
    else call("function", as.pairlist(args_[1]), make.body(args_[-1]))
  }
  eval(make.body(formals(args(f))), envir = environment(f), enclos = e)
}

# > c1 <- curry(rnorm); c2 <- curry.closure(rnorm)
# > library(microbenchmark)
# > microbenchmark(c1(10)(100)(1), c2(10)(100)(1))
# Unit: microseconds
#              expr    min      lq median     uq     max neval
#  (c1(10)(100))(1) 42.796 44.3555 45.025 46.362 145.771   100
#  (c2(10)(100))(1) 10.700 11.5910 12.037 12.928  19.169   100

# h1 <- curry(rnorm); h1(10)(100)(1)
# h2 <- curry.closure(rnorm)(10)(100); h2(1)
# h3 <- curry.closure(D); h3(quote(x^5))("x")
# h4 <- curry.closure(D)(quote(x^5)); h4("x")

# particial application
# an undercore symbol `_` is requited to bind variales
pa <- function(expr, e = parent.frame()){
  all.vars <- all.names(substitute(expr), functions = FALSE)
  underscores <- all.vars[grep("^\\_$|^\\_[0-9]+$", all.vars)]
  if (length(underscores) == 0)
    stop("A binding variable must start with underscore and ends with numeric.")
  if (anyDuplicated.default(underscores) > 0)
    stop("Binding variables must be different from each other.")
  created.formals <- as.formals(underscores[order(underscores)])

  make.body <- function(args_){
    if (length(args_) == 0) substitute(expr, parent.env(environment()))
    else call("function", as.pairlist(args_[1]), make.body(args_[-1]))
  }
  eval(make.body(created.formals), e)
}
# f1 <- pa(`_` * 2); f1(10)
# f2 <- pa(D(`_`, "x")); f2(quote(x^4))
# f3 <- pa(D(quote(x^5+2*y^4), `_`)); f3("x"); f3("y")
# 
# g <- function(x, y, z, w) 1000*x + 100*y + 10*z + 1*w
# f4 <- pa(g(1, `_1`, 7, `_2`)); f4(3)(9)
# f5 <- pa(g(1, `_2`, 7, `_1`)); f5(3)(9)

cr <- function(f) {
  call.fun <- 
    if (any(names(formals(args(f))) == "...")) curry.dots
    else if (typeof(f) == "closure") curry.closure
    else curry
  call.fun(f)
}

### curried function creator
`λ` <- l. <- function(...) curry(f.(..., env = parent.frame()), env = parent.frame())
### Address of λ (lambda) in Unicode is U+03BB or \u03BB
# λ(g, x, g(g(x)))(λ(y, y+1))(5)
# f.(g, f.(x, g(g(x))))(f.(y, y+1))(5)
# l.(g, l.(x, g(g(x))))(l.(y, y+1))(5) # can work as a substitute for f.()
# l.(g, x, g(g(x)))(l.(y, y+1))(5) # no need to nest f.()

### http://www.angelfire.com/tx4/cus/combinator/birds.html
# S <- f.(x, f.(y, f.(z, (x(z))(y(z)) )))
# K <- f.(x, f.(y, x))
# I <- S(K)(K) # == f.(x, x) == identity()

# see https://gist.github.com/TobCap/6255395
uncurry <- function(fun){ 
  function(...){
    rec <- function(f, dots){
      if (length(dots) == 0) f
      else rec(f(dots[[1]]), dots[-1])
    }
    rec(fun, list(...))
  }
}
# > g <- function(x) function(y) function(z) x + y + z
# > g(1)(2)(3)
# [1] 6
# > uncurry(g)(1, 2, 3)
# [1] 6

###
## http://cran.r-project.org/doc/manuals/r-release/R-ints.html#Prototypes-for-primitives
flip <- function(fun, l = 1, r = 2, parent_env = parent.frame()) {
  args_new <- args_orig <- formals(args(match.fun(fun)))
  stopifnot(1 < r, l < length(args_orig), l < r)
  
  names(args_new)[c(r, l)] <- names(args_orig)[c(l, r)]
  args_new[c(r, l)] <- args_orig[c(l, r)]
  
  if (typeof(fun) == "closure") {
    eval(call("function", as.pairlist(args_new), body(fun)), environment(fun), parent_env)
  } else { ## special & builtin  
    fun_sym <- substitute(fun)
    body_ <- quote({
      called <- sys.call()
      called[-1] <- called[-1][c(r, l)]
      called[[1]] <- fun_sym
      eval(called, environment(fun), parent.frame())
    })
    eval(call("function", as.pairlist(args_new), body_), environment(), environment(fun))
  }
}

flip.cr <- function(fun, .env = parent.frame()) {
  arg1 <- formals(args(fun))
  arg2 <- body(fun)[[2]]
  eval(call("function", arg2, call("function", arg1, body(fun)[[3]])), environment(fun), .env)
}

### examples
# > flip(`-`)(2, 5)
# [1] 3

# divBy10 <- curry(flip(`/`))(10)
# divBy10(24) # == 2.4

# > flip(sapply)(sqrt, 1:4)
# [1] 1.000000 1.414214 1.732051 2.000000

# > flip(sapply)(round, 1:10/100, 2)
#  [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10

# (function(x){
#   s <- flip(substitute)
#   print(substitute(x, environment()))
#   print(s(environment(),x))
# })(x = 1:5)

# > Dx <- cr(flip(D))("x") # or Dx <- pa(D(`_`, "x"))
# > nest.fun(Dx, 5)(quote(x^10)) # nest.fun is defined below.
# 10 * (9 * (8 * (7 * (6 * x^5))))

# > flip.cr(cr(D))("x")(quote(x^10))
# 10 * x^9

###
# fun compares vecter elements next to each other
.compare <- function(fun, vals) {
  out.fun <- function(vals){
    if (length(vals) == 1) TRUE
    else if (is.na(vals[[1]])) NA
    else if (!fun(vals[[1]], vals[[2]])) FALSE
    else out.fun(vals[-1])
  }
  out.fun(vals)
}

.compare.vec <- function(fun, vals, type = c("all", "any")) {
  all.or.any <- match.fun(match.arg(type))
  all.or.any(match.fun(fun)(vals[-length(vals)], vals[-1]))
}

`<.`  <- function(.) .compare(`<` , .) # .compare.vec("<" , .)
`<=.` <- function(.) .compare(`<=`, .) # .compare.vec("<=", .)
`>.`  <- function(.) .compare(`>` , .) # .compare.vec("<" , .)
`>=.` <- function(.) .compare(`>=`, .) # .compare.vec("<=", .)
`==.` <- function(.) .compare(`==`, .) # .compare.vec("==", .)
# `!=` is not a transitive relation, so '!=.' cannnot be defined as the same way. 

# `<.`(1:100)
# `<=.`(c(1:100,99))

.compare.element <- function(vals, default.val){
  if (length(vals) == 0) default.val
  else if (is.na(vals[[1]])) NA
  else if (!vals[[1]] == default.val) !default.val
  else .compare.element(vals[-1], default.val)
}
.all <- function(x) .compare.element(x, TRUE)
.any <- function(x) .compare.element(x, FALSE)

###
# avoiding conflict with utils::zip
# just using mapply()
zip. <- function(..., FUN = list) {
  dots <- list(...)
  args.seq <- seq_len(min(vapply(dots, length, 0)))
  args.new <- lapply(dots, function(x) x[args.seq])
  do.call(mapply, c(FUN = FUN, args.new, SIMPLIFY = FALSE, USE.NAMES = FALSE))
}

# zip2 <- function(x, y, FUN = list){
#   if(length(x) == 0 || length(y) == 0) NULL
#   else append(list(FUN(x[[1]], y[[1]])), zip2(x[-1], y[-1], FUN = FUN))
# }
# zip3 <- function(x, y, z, FUN = list){
#   if(length(x) == 0 || length(y) == 0 || length(z) == 0) NULL
#   else append(list(FUN(x[[1]], y[[1]]), z[[1]]), zip2(x[-1], y[-1], z[-1], FUN = FUN))
# }
# zip. <- function(... , FUN = list) {
#   dots <- list(...)
#   elem.len <- min(vapply(dots, length, 0))
#   if(elem.len == 0) NULL
#   else append(
#     list(do.call(FUN, lapply(dots, `[[`, 1))),
#     do.call(zip., c(lapply(dots, `[`, -1), FUN = FUN)))
# }

zipWith. <- function(fun, ..., do.unlist = FALSE) {
  if (do.unlist) unlist(zip.(..., FUN = match.fun(fun)))
  else zip.(..., FUN = match.fun(fun))
}

# zip.(list(1,2,3), list(4,5,6))
# zip.(1:3, 4:6)

# zip.(list(1,2,3), list(4,5,6), list(7,8,9))
# zip.(1:3, 4:6, 7:9)
# zip..(1:3, letters[1:3])


### Pipeline like operator
## Left value can be passed by ".." just like scala's underscore "_".
## I use ".."; "." is sometimes used within a model formula expression or "package:plyr".
## It is not fast but easy to read and understand because of using fewer parentheses.
`%|%` <- (function() {  
  replace_two_dots <- function(expr, expr_new) {
    cnv <- function(x){
      if (length(x) <= 1) {
        if (is.symbol(x) && x == "..") expr_new
        else if (is.call(x) && x[[1]] == "..") as.call(list(expr_new))
        else x }      
      else if (x[[1]] == "%|%") { x }
      else if (is.pairlist(x)) { as.pairlist(lapply(x, cnv)) }
      else { as.call(lapply(x, cnv)) }
    }
    cnv(expr)
  }
  
  strip_parenthesis <- function(expr) {
    if (length(expr) != 2 || (expr[[1]] != "(" && expr[[1]] != "{")) { expr }
    else { strip_parenthesis(expr[[2]]) } 
  }
  
  function(lhs, rhs, p = parent.frame()) {
    # rhs_expr <- substitute(rhs)
    rhs_expr <- strip_parenthesis(substitute(rhs))
    
    if (is.symbol(rhs_expr) || is.call(rhs_expr) && rhs_expr[[1]] == "function") {
      # eval(rhs_expr, envir = p, enclos = p)(lhs) }
      rhs(lhs) }
    else if (any(all.names(rhs_expr) == "..")) {
      ## has two_dots
      rhs_expr_mod <- replace_two_dots(rhs_expr, substitute(lhs))
      eval(rhs_expr_mod, envir = p, enclos = p) }
    else {
      stop("missing pattern") }
  }
})()

### other binary operators
## http://neue.cc/2011/02/14_302.html
"%|>%" <- function(x, f) f(x)
"%<|%" <- function(f, x) f(x)

## functional composition
"%>>%" <- function(f, g) function(x) g(f(x))
"%<<%" <- function(f, g) function(x) f(g(x))
compose. <- function(f, g) function(x) f(g(x))

## The old version was simpler but a left side is evaluated before passing it into a right side function
# "%|%" <- function(lhs, rhs){
#   ans <- eval(substitute(rhs), envir = list(.. = lhs), enclos = parent.frame())  
#   if (is.function(ans))
#     ans(lhs)  
#   else
#     ans
# }

## examples
# > 1:5 %|% f.(x, x-1) %|% f.(x, x^2)
# [1]  0  1  4  9 16
# > 1:5 %|% (..-1) %|% (..^2)
# [1]  0  1  4  9 16
# > 1:5 %|% {..-1} %|% {..^2}
# [1]  0  1  4  9 16

### same result
# > Filter(function(x) x%%2==0, 1:5)
# [1] 2 4
# > 1:5 %|% ..[..%%2==0]
# [1] 2 4

# With %|% operator, I think, right assignment is an intuitive way to define a variable
# > 1:10 %|% ..[..%%2==0] %|% ..^2 -> x
# > x
# [1]   4  16  36  64 100

### set break lines after typing `%|%`
# (function() {
#   mu <- 0
#   m <- 252
#   sigma <- 0.2 / sqrt(m)
#   rnorm(m, mu - sigma ^ 2 / 2, sigma) %|% 
#     cumsum %|% 
#     c(0, ..)
# }) %|% 
#   replicate(n=1000, ..()) %|% 
#   matplot(.., type = "l", ann = FALSE)

# library(magrittr)
# library(microbenchmark)

# > microbenchmark("%|%" = 1 %|% sum, "%>%" = 1 %>% sum)
# Unit: microseconds
#  expr     min      lq  median      uq     max neval
#   %|%   9.363  10.254  13.374  14.266  58.845   100
#   %>% 165.387 167.616 168.953 171.851 353.954   100

# > microbenchmark(
#     "%|%" = 1 %|% sum(.., rm.na = TRUE),
#     "%>%" = 1 %>% sum(rm.na = TRUE))
# Unit: microseconds
#  expr     min       lq   median       uq     max neval
#   %|% 162.266 198.5990 205.2845 210.4115 359.304   100
#   %>% 269.255 337.2375 341.0270 347.9370 687.402   100

# microbenchmark(
# "%>%" =   
#   airquality %>% 
#     transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>% 
#     aggregate(. ~ Date %>% format("%W"), ., mean) %>%
#     subset(Wind > 12, c(Ozone, Solar.R, Wind))
# ,
# "%|%" =  
#   airquality %|% 
#     transform(.., Date = paste(1973, Month, Day, sep = "-") %|% as.Date) %|% 
#     aggregate(. ~ Date %|% format(.., "%W"), .., mean) %|%
#     subset(.., Wind > 12, c(Ozone, Solar.R, Wind))
# )
## Unit: milliseconds
##  expr      min       lq   median       uq      max neval
##   %>% 10.02374 10.19620 10.28466 10.53446 12.14736   100
##   %|% 14.99472 15.07445 15.18651 15.36291 50.14812   100

##                  %|%                 %>%
## receiving symbol ..                  .
## speed                  depends on its situation
## syntax           R like              can omit first argument
## evaluation       lazy                eager
## addOne           (..+1)              `+`(1) or add(1)

# f1 <- f.(x, {cat("f1(x = ", x, ") -> ", sep = ""); x + 1})
# f2 <- f.(x, {cat("f2(x = ", x, ") -> ", sep = ""); x * 2})
# f3 <- f.(x, {cat("f3(x = ", x, ") -> ", sep = ""); x / 3})
# f4 <- f1 %>>% f2 %>>% f3
# f5 <- f1 %<<% f2 %<<% f3

# > f4(1)
# f1(x = 1) -> f2(x = 2) -> f3(x = 4) -> [1] 1.333333
# > f5(1)
# f3(x = 1) -> f2(x = 0.3333333) -> f1(x = 0.6666667) -> [1] 1.666667



## date passing
`<--` <- function(...){
  Reduce("%<|%", list(...), right = TRUE)
}
`-->` <- function(...){
  Reduce("%|>%", list(...), right = FALSE)
}
# > `-->`(1:5, f.(x, x-1), f.(x, x*10))
# [1]  0 10 20 30 40
 
## composing one more functions
`<<--` <- function(...){
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, list(...), x, right = TRUE)
}
`-->>` <- function(...){
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, rev(list(...)), x, right = TRUE)
}
 
# > `-->>`(f.(x, x-1), f.(x, x*10))(1:5)
# [1]  0 10 20 30 40

### invokes interceptor; the idea comes from underscore javascript.
### it easy to debug.
tap <- function(x, fun = print) {
  fun(x)
  x
}
# > 1:10 %|% ..[..%%2==0] %|% tap %|% ..^2
# [1]  2  4  6  8 10
# [1]   4  16  36  64 100

# > cos(log(sin(pi/2)))
#
# > cos %<<% log %<<% sin %<|% (pi/2)
# > (pi/2) %|>% {cos %<<% log %<<% sin}
# > sin %>>% log %>>% cos %<|% (pi/2)
# > (pi/2) %|>% {sin %>>% log %>>% cos}
#
### code flows from left to right; no need to use round bracket!
### http://yuroyoro.hatenablog.com/category/%E9%96%A2%E6%95%B0%E5%9E%8B%E8%A8%80%E8%AA%9E
### http://yuroyoro.hatenablog.com/entry/20120203/1328248662
# > (pi/2) %|% sin %|% log %|% cos
# > (pi/2) %|>% sin %|>% log %|>% cos

### see the calculation processes at https://gist.github.com/TobCap/6668817
### fixed-point combinator
### R can write very simply
fix. <- function(g) f <- g(f)

### http://en.wikipedia.org/wiki/Fixed-point_combinator

# fix0 <- function(g) f <- g(f)
# fix1 <- function(g) g(fix1(g))
# fix2 <- function(f) (function(x) f(x(x)) )(function(x) f(x(x)))
# fix3 <- function(f) (function(x) f(function(y) x(x)(y)))(function(x) f(function(y) x(x)(y)))
# fix4 <- function(f) (function(x) function(y) f(x(x))(y))(function(x) function(y) f(x(x))(y))

### g = Y(f), f(g) = g; f = Y(g), g(f) = f
### Y g = g(Y g)
### Y = λf.(λx.f (x x)) (λx.f (x x))
### Z = λf.(λx.f (λy. x x y)) (λx.f (λy. x x y))

# > mult.maker <- function(f) function(x) if (x == 1) 1 else x * f(x-1)
# > microbenchmark(fix0(mult.maker)(100),fix1(mult.maker)(100),fix2(mult.maker)(100), fix3(mult.maker)(100), fix4(mult.maker)(100))
# Unit: microseconds
#                   expr      min        lq   median        uq      max neval
#  fix0(mult.maker)(100)  499.726  505.2985  509.088  522.4610  824.258   100
#  fix1(mult.maker)(100)  853.234  878.4210  887.336  923.4455 2320.313   100
#  fix2(mult.maker)(100)  871.065  889.7885  899.150  920.3245 2251.663   100
#  fix3(mult.maker)(100) 1114.910 1136.3075 1151.688 1226.8025 2951.100   100
#  fix4(mult.maker)(100) 1115.802 1138.3135 1153.693 1195.8200 2729.991   100

### refers to javascript.
### http://www.kmonos.net/wlog/52.php
### http://d.hatena.ne.jp/r-west/20090422/1240400570

# avoid to conflict with `memoise` that is already taken in library("memoise")
memoizer <- function(f) {
  .memo <- new.env()
  f_orig <- f
  function(...){
    key <- paste(list(...), collapse=",")
    if (is.null(.memo[[key]])) .memo[[key]] <- f_orig(...)
    .memo[[key]]
  }
}

tracer <- function(f) {
  num <- 0
  function(...){
    key <- paste(list(...), collapse=",")
    num <<- num + 1 
    suffix <- if (num <= 3) switch(num, "st", "nd", "rd") else "th"
    cat(num, suffix, " call with argument: ", key, "\n", sep = "")
    f(...)
  }
}

###
fib.maker <- function(f) function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
# > fix.(fib.maker)(5)
# [1] 5
# > fix.(tracer %>>% fib.maker)(5)
# 1st call with argument: 4
# 2nd call with argument: 3
# 3rd call with argument: 2
# 4th call with argument: 1
# 5th call with argument: 0
# 6th call with argument: 1
# 7th call with argument: 2
# 8th call with argument: 1
# 9th call with argument: 0
# 10th call with argument: 3
# 11th call with argument: 2
# 12th call with argument: 1
# 13th call with argument: 0
# 14th call with argument: 1
# [1] 5
## see the chart in http://mitpress.mit.edu/sicp/full-text/sicp/book/node16.html

# > fibmemo <- fix.(tracer %>>% fib.maker %>>% memoizer)
### or fibmemo <- fix.(function(x) memoizer(fib.maker(tracer(x))))
# fibmemo(5)
# 1st call with argument: 4
# 2nd call with argument: 3
# 3rd call with argument: 2
# 4th call with argument: 1
# 5th call with argument: 0
# 6th call with argument: 1
# 7th call with argument: 2
# 8th call with argument: 3
# [1] 5
#
# > fibmemo(5)
# [1] 5
#
### only new arguments are passed to calculation
# > fibmemo(6)
# 9th call with argument: 5
# 10th call with argument: 4
# [1] 8
#
# > fibmemo(6)
# [1] 8
#
# > unlist(eapply(environment(fibmemo)$.memo, identity))
# 0 1 2 3 4 5 6 
# 0 1 1 2 3 5 8 


### the same semantics
# fix.(tracer %>>% fib.maker %>>% memoizer)(5)
# (tracer %>>% fib.maker %>>% memoizer %|>% fix.)(5)
# {tracer %>>% fib.maker %>>% memoizer %|>% fix.}(5)
# 5 %|>% {tracer %>>% fib.maker %>>% memoizer %|>% fix.}
# tracer %>>% fib.maker %>>% memoizer %|>% fix. %<|% 5

### memoizer() can apply to a simple recursive function.
# fib2 <- memoizer(function(x) if(x <= 1) x else fib2(x - 1) + fib2(x - 2))

# > fib2(500)
# [1] 1.394232e+104

# install.packages("gmp"); library("gmp")
# > fib3 <- memoizer(function(x) if(x <= 1) as.bigz(x) else fib3(x - 1) + fib3(x - 2))
# > fib3(500)
# Big Integer ('bigz') :
# [1] 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125

## http://d.hatena.ne.jp/einblicker/20110108/1294448477
## http://d.hatena.ne.jp/einblicker/20110113/1294920315

### U Combinator 
## http://www.ucombinator.org/
## U <- function(f) f(f)
## fib.u <- function(f) function(n) if(n <= 1) n else f(f)(n - 1)  + f(f)(n - 2)
## U(fib.u)(10)
### Is there a memoise function for U combinator?

### language object operater
## promise.tracker can track promise's original symbol
promise.tracker <- function(., n = 1, strict = TRUE) {
  # see https://gist.github.com/TobCap/6473028
  if (strict) stopifnot(n <= sys.parent())
  stack.adjust <- 2
  
  make.call1 <- function(x){
    if (x == 0) call("substitute", quote(.))
    else call("substitute", make.call1(x - 1))
  }
  make.call2 <- function(x){
    if (x == 0) make.call1(n)
    else call("eval", make.call2(x - 1), substitute(parent.frame(k), list(k = x + stack.adjust)))
  }
  eval(make.call2(n))
}
# lapply(0:3, function(n) (function(x) (function(y) (function(z) promise.tracker(z, n))(y))(x))(n))

### convert call to list and vice versa
call.list.cnv <- function(f.arg){
  arg.is.lang <- is.call(f.arg)
  cnv <- function(x){
    if (length(x) == 1) x
    else if (is.pairlist(x)) as.pairlist(lapply(x, cnv))
    else if (arg.is.lang) lapply(x, cnv)
    else as.call(lapply(x, cnv))
  }
  cnv(f.arg)
}
as.list.recursive <- function(x) {stopifnot(is.call(x)); call.list.cnv(x)}
as.call.recursive <- function(x) {stopifnot(is.list(x)); call.list.cnv(x)}
length.recursive <- function(x) {
  if (!is.recursive(x)) length(x) 
  else do.call(sum, lapply(as.list(x), length.recursive))
}

# language replacement
# see https://gist.github.com/TobCap/6348892
replace.symbol <- function(expr, before, after) {
  stopifnot(is.language(expr), is.symbol(before))
  eval(substitute(substitute(e, `names<-`(list(after), as.character(before))), list(e = expr)))
}

replace.call <- function(expr, before, after) {
  stopifnot(is.language(expr))
  conv <- function(x) {
    if (is.pairlist(x) && !is.null(x)) {
      if (is.symbol(before)){ # for formal parameter
        ind <- match(as.character(before), names(x))
        names(x)[ind] <- as.character(after)
      }
      as.pairlist(lapply(x, conv))
    }
    else if (identical(x, before)) after
    else if (length(x) == 1 || is.null(x)) x
    else as.call(lapply(x, conv))
  }
  conv(expr)
}

replace.multi <- function(expr, befores, after, replace.fun = replace.symbol){
  make.call <- function(x){
    if (length(x) == 0) expr
    else replace.fun(make.call(x[-1]), x[[1]], after)
  }
  make.call(c(befores)) # need c() if length(befores) == 1
}

replace.lang <- function (expr, before, after, can.accept.undefined.var = FALSE){
  stopifnot(is.language(expr))
  if (can.accept.undefined.var) {
    stopifnot(exists("accept.undefined.var"))
    before <- accept.undefined.var(before, parent.frame())
    after <- accept.undefined.var(after, parent.frame())
  }
  # before is passed as list even though it is a single symbole
  before <- 
    if (is.function(before)) before 
    else if (is.list(before) || is.expression(before)) as.list(before) 
    else list(before) # Being passed to function `matched` requires to be list object.
  after <- if (is.expression(after)) as.list(after) else after
  
  matched <- 
    if (is.function(before)) before 
    else function(m) any(vapply(before, identical, logical(1), m))
    
  conv <- function(x) {
    if (is.pairlist(x) && !is.null(x)) {
      if (is.function(before)){
        ind <- match(TRUE, vapply(lapply(names(x), as.symbol), before, logical(1)))
        names(x)[ind] <- as.character(after)
      } else if (is.symbol(before[[1]])){       
        ind <- match(as.character(before), names(x))
        names(x)[ind] <- as.character(after)
      }
      as.pairlist(lapply(x, conv))
    }
    else if (matched(x)) after
    else if (length(x) == 1 || is.null(x)) x
    else as.call(lapply(x, conv))
  }
  conv(expr)
}

accept.undefined.var <- function(.x, env) {
  # access to parent.frame(2)
  expr <- eval.parent(substitute(substitute(.x)))
  lang.fun.names <- c("quote", "expression", "as.name", "as.symbol", "call")
  is.list.vector <- !is.symbol(expr) && (expr[[1]] == quote(c) || expr[[1]] == quote(list))
  w <- function(x) warning(
    paste0("the '", x,"' is an existing language object,so not interpreted as undefined variable"))
 
  force.lang <- function(y){
    first.char <- as.character(y)[[1]]
    if (is.character(y)) parse(text = y)[[1]] # character
    else if (first.char %in% lang.fun.names[1:2]) y[[2]] # quote or expression
    else if (first.char %in% lang.fun.names[3:5]) eval(y) # as.symbol, as.name or call
    else if (is.symbol(y) && exists(first.char, envir = env)) {w(first.char); eval(y)} # assigned language object
    else y # undefined, not language object, or not character object
  }
  
  if (is.list.vector) lapply(as.list(expr)[-1], force.lang)
  else force.lang(expr)
}

# replace.symbol(quote(x+y*z), quote(y), quote(www))
# replace.lang(quote(x + y ^ z), y, www, TRUE)

# > replace.symbol(quote(1+2+x^3), quote(x), quote(y))
# 1 + 2 + y^3
# > replace.symbol(quote(1+2+x^3), quote(2), quote(99)) 
# 1 + 2 + x^3 # cannot replace value
# > replace.call(quote(1+2+x^3), quote(2), quote(99))
# 1 + 99 + x^3 # ok

# > replace.symbol(quote(1+2+x^3) , quote(`+`), quote(`-`))
# 1 - 2 - x^3
# > eval(replace.symbol(quote(1+2+x^3) , quote(`+`), quote(`-`)), list(x = 2))
# [1] -9

# arguments need to be quoted
nest.formula <- function(expr, variable, num){
  if (num == 1) expr
  else replace.call(nest.formula(expr, variable, num - 1), variable, expr)
}
# > nest.formula(quote((1 + x)^2), quote(x), 3)
# (1 + (1 + (1 + x)^2)^2)^2
# > nest.formula(quote((1 + x)^2), quote(1 + x), 3)
# (((1 + x)^2)^2)^2
# > eval(nest.formula(quote(1 + 1 / x), quote(x), 40), list(x = 1)) == (1 + sqrt(5))/2
# [1] TRUE

### tail recursive
nest.fun <- function(f, n, acc.fun = identity){
  # compose. is already defined above.
  if (n == 0) acc.fun
  else if (n %% 2 == 0) nest.fun(compose.(f, f), n / 2, acc.fun)
  else nest.fun(f, n - 1, compose.(f, acc.fun))
}
nest.fun2 <- function(f, n){
  if (n == 0) identity
  else compose.(f, nest.fun2(f, n - 1))
}

# > microbenchmark(tail.rec = nest.fun(function(x) {x}, 100)(0), simple = nest.fun2(function(x) {x}, 100)(0), times=1e4)
# Unit: microseconds
#      expr     min      lq  median      uq       max neval
#  tail.rec 140.867 184.554 205.951 234.927  5700.185 10000
#    simple 308.480 406.998 453.804 525.575 42763.638 10000

# > microbenchmark(tail.rec = nest.fun(function(x) {x}, 10)(0), simple = nest.fun2(function(x) {x}, 10)(0), times=1e5)
# Unit: microseconds
#      expr    min     lq median     uq      max neval
#  tail.rec 24.964 27.639 30.760 37.892  4637.00 1e+05
#    simple 29.868 33.435 37.446 45.916 51108.19 1e+05

# > microbenchmark(tail.rec = nest.fun(function(x) {x}, 1)(0), simple = nest.fun2(function(x) {x}, 1)(0), times = 1e6)
# Unit: microseconds
#      expr   min    lq median    uq      max neval
#  tail.rec 4.458 5.796  6.687 7.579 41602.83 1e+06
#    simple 3.567 4.459  5.350 6.242 63045.72 1e+06

# > nest.fun(f.(x, 1 + 1/x), 100)(1) == (1+sqrt(5))/2
# [1] TRUE

### When using nest.fun(), tracer() can check passing arguments.
# > nest.fun(tracer(f.(x, 1 + 1/x)), 6)(1)
# 1st call with argument: 1
# 2nd call with argument: 2
# 3rd call with argument: 1.5
# 4th call with argument: 1.66666666666667
# 5th call with argument: 1.6
# 6th call with argument: 1.625
# [1] 1.615385

### http://en.wikipedia.org/wiki/Church_encoding
### http://taiju.hatenablog.com/entry/20120529/1338299884
# zero <- l.(f, x, x)                    # f.(f, f.(x, x))
# one  <- l.(f, x, f(x))                 # f.(f, f.(x, f(x)))
# two  <- l.(f, x, f(f(x)))              # f.(f, f.(x, f(f(x))))
# num  <- l.(n, f, x, nest.fun(f, n)(x)) # f.(n, f.(f, f.(x, nest.fun(f, n)(x)))) 
#
# plus <- l.(m, n, f, x, m(f)(n(f)(x)))  # f.(m, f.(n, f.(f, f.(x, m(f)(n(f)(x))))))
# succ <- l.(n, f, x, f(n(f)(x)))        # f.(n, f.(f, f.(x, f(n(f)(x)))))
# mult <- l.(m, n, f, m(n(f)))           # f.(m, f.(n, f.(f, m(n(f)))))
# exp  <- l.(m, n, n(m))                 # f.(m, f.(n, n(m)))
# toInt <- l.(n, n(l.(n, n + 1))(0))     # f.(n, n(f.(n, n + 1))(0))
# 
# toInt(plus(zero)(one))
# toInt(plus(one)(two))
# toInt(succ(one))
# toInt(num(99))
# toInt(plus(num(7))(num(8)))
# toInt(mult(num(3))(num(5)))
# num(3) %|>% mult %<|% num(5) %|% toInt

# true <- f.(a, f.(b, a))
# false <- f.(a, f.(b, b))
# and <- f.(m, f.(n, m(n)(m)))
# or <- f.(m, f.(n, m(m)(n)))
# not1 <- f.(m, f.(a, f.(b, m(b)(a)))) ## this does not work in R
# not2 <- f.(m, m(f.(a, f.(b, b)))(f.(a, f.(b, a)))) # ok
# if_ <- f.(m, f.(a, f.(b, m(a)(b))))

# identical(and(true)(true), true) 
# identical(and(true)(false), false)
# identical(and(false)(true), false)
# identical(and(false)(false), false)
#
# identical(or(true)(true), true) 
# identical(or(true)(false), true)
# identical(or(false)(true), true)
# identical(or(false)(false), false)
#
# identical(not2(false), true, ignore.environment=TRUE)
# identical(not2(true), false, ignore.environment=TRUE)
#
# toInt(if_(true)(one)(two))
# toInt(if_(false)(one)(two))

lang2char <- function(expr, type = c("rexp", "sexp")){
  type <- match.arg(type)
  stopifnot(is.call(expr))
  
  r <- list(
    fst = function(x) as.character(list(x[[1]])), 
    coll = ", ", 
    print.out = function(first, rest) paste0(first, "(", rest, ")"))
    
  s <- list(
    fst = function(x) as.character(x[[1]]),
    coll = " ",
    print.out = function(first, rest) paste0("(", first, " ", rest, ")")) 
    
  . <- if (type == "rexp") r else s
  
  .$rst <- function(k) paste0(
    lapply(k, function(x) {if (length(x) == 1) as.character(x) else as.char(x)}) , collapse = .$coll)
  
  as.char <- function(k) .$print.out(first = .$fst(k), rest = .$rst(k[-1]))
  
  noquote(as.char(expr))
}

# > lang2char(quote(x+sin(y*z)))
# [1] `+`(x, sin(`*`(y, z)))
# > eval(parse(text="`+`(x, sin(`*`(y, z)))"), list(x=1, y=2, z=3))
# [1] 0.7205845

### S-expression
# > lang2char(quote(x+sin(y*z)), "s")
# [1] (+ x (sin (* y z)))

# library(codetools)
# > showTree(quote(x+sin(y*z)))
# (+ x (sin (* y z)))
