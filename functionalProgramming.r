### functional operators and useful functions

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
f. <- function(..., env_ = parent.frame()) {
  # see https://gist.github.com/TobCap/6366396 for how to handle unevaluated `...`
  d <- as.pairlist(as.vector(substitute((...)), "list")[-1])
  # need to be pairlist to return NULL when nothing is passed to `...`.

  n <- length(d)
  eval(call("function", as.formals(d[-n]), d[[n]]), env_)
}

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

### Pipeline like operator
## Left value can be passed by ".." just like scala's underscore "_".
## I use ".."; "." is sometimes used within a model formula expression or other packages as a meaningful symbol.
## It is not fast but easy to read and understand because of using fewer parentheses.
`%|%` <- (function() {
  two_dots_arg <- as.pairlist(alist(..=))

  function(lhs, rhs, env_ = parent.frame()) {
    rhs_expr <- substitute(rhs)
    if (length(rhs_expr) == 1 && is.symbol(rhs_expr)) {
      # shortcut purpose for speedup
      rhs(lhs) }
    else if (rhs_expr[[1]] == "{") {
      # eager eval
      ans <- eval(rhs_expr, envir = list(.. = lhs), enclos = env_)
      if (is.function(ans)) ans(lhs)
      else ans }
    else if (any(all.names(rhs_expr) == "..")) {
      # lazy eval .. with lambda
      rhs_closure <- eval(call("function", two_dots_arg, rhs_expr), env_)
      rhs_closure(lhs) }
    else {
      rhs(lhs) }
  }
})()

## examples
# > 1:5 %|% (..-1) %|% (..^2)
# [1]  0  1  4  9 16
# > 1:5 %|% {..-1} %|% {..^2}
# [1]  0  1  4  9 16

## f.() is defined at line 25 in this file.
# > 1:5 %|% f.(x, x-1) %|% f.(x, x^2)
# [1]  0  1  4  9 16

### base function v.s. `%|%`
# > Filter(function(x) x%%2==0, 1:5)
# [1] 2 4
# > 1:5 %|% ..[..%%2==0]
# [1] 2 4

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

### switch eager or lazy
# my_sum <- function(...) {print("my_sum is called"); sum(...)}
# my_log <- function(...) {print("my_log is called"); log(...)}
#
# {print("1:5 is called"); 1:5} %|% my_sum
# {print("1:5 is called"); 1:5} %|% {my_sum}
#
# {print("1:5 is called"); 1:5} %|% my_sum(..)
# {print("1:5 is called"); 1:5} %|% {my_sum(..)}
#
# {print("1:5 is called"); c(NA, 1:5)} %|% my_sum(.., na.rm = TRUE)
# {print("1:5 is called"); c(NA, 1:5)} %|% {my_sum(.., na.rm = TRUE)}
#
# {print("1:5 is called"); 1:5} %|% my_sum %|% my_log
# {print("1:5 is called"); 1:5} %|% {my_sum} %|% my_log
# {print("1:5 is called"); 1:5} %|% my_sum %|% {my_log}
# {print("1:5 is called"); 1:5} %|% {my_sum} %|% {my_log}

# library(magrittr)
# library(microbenchmark)

# > microbenchmark("%|%" = 1 %|% sum, "%>%" = 1 %>% sum)
# Unit: microseconds
# expr     min      lq      mean  median       uq     max neval
# %|%   8.024   8.916  15.83488  12.928  14.2655 263.011   100
# %>% 257.661 263.011 297.08162 268.137 301.7935 789.922   100

# > microbenchmark(
#     "%|%" = 1 %|% sum(.., rm.na = TRUE),
#     "%>%" = 1 %>% sum(rm.na = TRUE))
## Unit: microseconds
## expr     min      lq      mean   median       uq      max neval
## %|%  24.518  27.639  39.22936  35.8855  38.1155  183.662   100
## %>% 382.033 396.299 468.14073 420.5940 462.9430 1113.558   100

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
## expr      min       lq     mean   median       uq      max neval
## %>% 25.89355 27.31938 31.88473 28.17238 34.17859 90.42461   100
## %|% 24.65473 25.79749 30.46729 28.68302 32.91770 69.69452   100

##                    %|%                 %>%
## placeholder symbol ..                  .
## syntax             cannot omit arg     UFCS-like
## evaluation         lazy or {eager}     eager
## addOne             (..+1)              `+`(1) or add(1) or {.+1}

## The old version was simpler but a left side is evaluated before passing it into a right side function
# "%|%" <- function(lhs, rhs) {
#   ans <- eval(substitute(rhs), envir = list(.. = lhs), enclos = parent.frame())
#   if (is.function(ans)) ans(lhs)
#   else ans
# }

### other pipe operators like F#
### http://msdn.microsoft.com/en-us/library/dd233228.aspx
"%|>%" <- function(x, f) f(x) # forward pipe operator
"%<|%" <- function(f, x) f(x) # backward pipe operator

### functional composition
"%>>%" <- function(f, g) function(x) g(f(x)) # forward composition
"%<<%" <- function(f, g) function(x) f(g(x)) # backward composition
compose_ <- function(f, g) function(x) f(g(x)) # rename

# add1 <- f.(x, {cat("add1(x = ", x, ") -> ", sep = ""); x + 1})
# mul2 <- f.(x, {cat("mul2(x = ", x, ") -> ", sep = ""); x * 2})
# div3 <- f.(x, {cat("div3(x = ", x, ") -> ", sep = ""); x / 3})
# f4 <- add1 %>>% mul2 %>>% div3
# f5 <- add1 %<<% mul2 %<<% div3

# > f4(1)
# add1(x = 1) -> mul2(x = 2) -> div3(x = 4) -> [1] 1.333333
# > f5(1)
# div3(x = 1) -> mul2(x = 0.3333333) -> add1(x = 0.6666667) -> [1] 1.666667


## date passing
`<--` <- function(...) {
  Reduce("%<|%", list(...), right = TRUE)
}
`-->` <- function(...) {
  Reduce("%|>%", list(...), right = FALSE)
}
# > `-->`(1:5, f.(x, x-1), f.(x, x*10))
# [1]  0 10 20 30 40

## composing one more functions
`<<--` <- function(...) {
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, list(...), x, right = TRUE)
}
`-->>` <- function(...) {
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, rev(list(...)), x, right = TRUE)
}

# > `-->>`(f.(x, x-1), f.(x, x*10))(1:5)
# [1]  0 10 20 30 40

### invokes interceptor; the idea comes from underscore javascript.
### it easy to debug.
tap <- function(x, fun = print, ...) {
  fun(x, ...)
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


### making an arbitrary function curried
curry <- function(f, env_ = parent.frame(), as_special = FALSE) {
  # if there are any language object handling functions such as substitute() or match.call() inside body(f),
  # `as_special = TRUE` is required
  # curry(bquote)(a+.(b))(list(b=10)) => expr
  # curry(bquote, as_special = TRUE)(a+.(b))(list(b=10)) => a + 10

  # `ls`, `ls.str` # a function that checks a formal parameter w/o default value by `missing` will error

  # http://cran.r-project.org/doc/manuals/r-release/R-ints.html#Prototypes-for-primitives
  stopifnot(is.function(f) && !(typeof(f) == "special" && is.null(args(f))))
  # > names(Filter(function(f) is.function(f) && (typeof(f) == "special" && is.null(args(f))), as.list(baseenv())))
  #  [1] "$"        "="        "@"        "["        "{"        "~"        "repeat"   "return"   "&&"
  # [10] "next"     "@<-"      "<-"       "break"    "[["       "[[<-"     "if"       "$<-"      "||"
  # [19] "function" "while"    "for"      "[<-"      "<<-"

  make_body <- function(args_) {
    if (length(args_) == 0)
      switch(if (isTRUE(as_special)) "special" else typeof(f)
      , closure = body(f)
      , special = make_special_body()
      , builtin = as.call(c(f, stats:::setNames(lapply(names(f_args), as.symbol), names(f_args))))
      )
    else call("function", as.pairlist(args_[1]), make_body(args_[-1]))
  }

  make_special_body <- function() {
    # `f_sym`, `f_args`, `env_` are parent environment's variable
    call(
      "{",
      quote(
        sub2 <- function(expr, env_target) {
          stopifnot(is.language(expr))
          eval(substitute(substitute(e, env = env_target), list(e = expr)))
        }
      ),
      as.call(c(quote(`<-`), quote(f_sym), f_sym)),
      as.call(c(quote(`<-`), quote(f_args_rev),
                list(lapply(rev(names(f_args)), function(x) as.symbol(x))))),
      quote({
        args_ <- list()
        e <- environment()
        while (length(f_args_rev) > 0) {
          f_args_head <- f_args_rev[[1]]
          arg_ <-
            if (f_args_head == "...")  as.list(sub2(call("list", f_args_head), e))[-1]
          else sub2(f_args_head, e)
          if (is.null(arg_)) arg_ <- list(NULL) # if default value is NULL
          args_ <- append(arg_, args_)
          f_args_rev <- f_args_rev[-1]
          e <- parent.env(e)
        }
        fun_new <- as.call(c(f_sym, args_))
      }),
      as.call(c(quote(eval), quote(fun_new), env_))
    )
  }
  f_sym <- substitute(f)
  f_args <- formals(args(f))

  if (is.null(f_args)) f
  else eval(make_body(f_args), envir = environment(f), baseenv())
}

# > curry(function(x, y, z) x + y + z)
# function (x)
#   function(y) function(z) x + y + z

# > curry(function(x, y, z) x + y + z)(1)(2)(3)
# [1] 6

# plus2 <- curry(`+`)(2)
# plus2(10) # 12

curry_dots <- function (fun, env_ = parent.frame()) {
  fun_args <- formals(args(fun))
  dots_pos_rev <- which(rev(names(fun_args)) == "...")
  if (length(dots_pos_rev) == 0) stop("`fun` do not have a dot-dot-dot argument")

  iter <- function(pos, acc_arg) {
    if (pos == 0) {
      eval(as.call(c(fun, acc_arg)), env_)
    } else {

      arg_ <-
        if (pos == dots_pos_rev) as.pairlist(alist(...=))
      else as.pairlist(rev(fun_args)[pos])

      body_ <- call(
        "{",
        call("<-", quote(pos), pos),
        call("<-", quote(dots_pos_rev), dots_pos_rev),
        call("<-", quote(acc_arg), acc_arg),
        call("<-", quote(iter), iter),
        quote({
          if (pos == dots_pos_rev && nargs() > 1) stop("... accepts only one argument.")
          if (pos == dots_pos_rev && nargs() == 0) {
            return(iter(pos - 1, acc_arg))
          }
          x_val <- as.pairlist(as.list(sys.call())[-1])
          x_formal <- formals(sys.function())
          x <-
            if (is.null(x_val)) x_formal # default value
            else stats:::setNames(x_val, if (pos == dots_pos_rev) names(x_val) else names(x_formal))
          iter(if (pos == dots_pos_rev) pos else pos - 1, append(acc_arg, x))
        })
      )

      eval(call("function", arg_, body_), parent.frame())
    }
  }
  iter(length(fun_args), list())
}

## you can recogize `...` end by calling empty (not NULL) argument just like f().
# > curry_dots(sum)(1)(2)(NA)(3)()(TRUE)
# [1] 6

# > curry(sum)(1, 2, NA, 3)(TRUE)
# [1] 6

# > curry(vapply)(1:5)(function(x, y, z) x + y + z)(numeric(1))(y=100, z = 1/100)()
# [1] 101.01 102.01 103.01 104.01 105.01

# > curry_dots(vapply)(1:5)(function(x, y, z) x + y + z)(numeric(1))(y=100)(z = 1/100)()()
# [1] 101.01 102.01 103.01 104.01 105.01

# > call("rnorm", 5, 100)
# rnorm(5, 100)

# > curry(call)("rnorm")(5, 100)
# rnorm(5, 100)

# > curry_dots(call)("rnorm")(5)(100)()
# rnorm(5, 100)

# h1 <- curry(rnorm); h1(10)(100)(1)
# h2 <- curry(rnorm)(10)(100); h2(1)
# h3 <- curry(D); h3(quote(x^5))("x")
# h4 <- curry(D)(quote(x^5)); h4("x")

# particial application
# an undercore symbol `_` is requited to bind variables
pa <- function(expr, env_ = parent.frame()) {
  all_vars <- all.names(substitute(expr), functions = FALSE)
  underscores <- all_vars[grep("^\\_$|^\\_[0-9]+$", all_vars)]
  if (length(underscores) == 0)
    stop("A binding variable must start with underscore and ends with numeric.")
  if (anyDuplicated.default(underscores) > 0)
    stop("Binding variables must be different from each other.")
  created_formals <- as.formals(underscores[order(underscores)])

  make_body <- function(args_) {
    if (length(args_) == 0) substitute(expr, parent.env(environment()))
    else call("function", as.pairlist(args_[1]), make_body(args_[-1]))
  }
  eval(make_body(created_formals), env_)
}
# f1 <- pa(`_` * 2); f1(10)
# f2 <- pa(D(`_`, "x")); f2(quote(x^4))
# f3 <- pa(D(quote(x^5+2*y^4), `_`)); f3("x"); f3("y")
#
# g <- function(x, y, z, w) 1000*x + 100*y + 10*z + 1*w
# f4 <- pa(g(1, `_1`, 7, `_2`)); f4(3)(9)
# f5 <- pa(g(1, `_2`, 7, `_1`)); f5(3)(9)

# see https://gist.github.com/TobCap/6255395
uncurry <- function(fun) {
  function(...) {
    rec <- function(f, dots) {
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
flip <- function(fun, l = 1, r = 2, env_ = parent.frame()) {
  args_new <- args_orig <- formals(args(match.fun(fun)))
  stopifnot(1 < r, l < length(args_orig), l < r)

  names(args_new)[c(r, l)] <- names(args_orig)[c(l, r)]
  args_new[c(r, l)] <- args_orig[c(l, r)]
  fun_sym <- substitute(fun)

  make_special_body <- function() {
    call("{",
         quote(called <- sys.call()), # match.call(expand.dot = FALSE) ?
         as.call(c(quote(`<-`), quote(idx), list(c(r, l)))),
         quote(called[-1] <- called[-1][idx]),
         as.call(c(quote(`<-`), quote(called[[1]]), fun_sym)),
         as.call(c(quote(eval), quote(called), env_))
    )
  }

  body_ <-
    switch(typeof(fun)
           , closure = body(fun)
           , builtin = as.call(c(fun_sym, lapply(names(args_orig), as.symbol)))
           , special = make_special_body()
    )

  eval(call("function", as.pairlist(args_new), body_), environment(fun), baseenv())
}

# > flip(substitute)(list(x=10), x+y)
# 10 + y

flip_cr <- function(fun) {
  arg1 <- formals(args(fun))
  arg2 <- body(fun)[[2]]
  stopifnot(is.pairlist(arg1) && is.pairlist(arg2))

  eval(call("function", arg2, call("function", arg1, body(fun)[[3]])), environment(fun))
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

## current code returns correct result
# (function(x) {
#   s <- flip(substitute)
#   print(substitute(x, environment()))
#   print(s(environment(),x))
# })(1:5)

# > nest_fun(Dx, 5)(quote(x^10)) # nest_fun is defined below near the line #750.
# 10 * (9 * (8 * (7 * (6 * x^5))))

# > Dx1 <- curry(flip(D))("x")
# > Dx2 <- flip_cr(curry(D))("x")
# > Dx1(quote(x^10)); Dx2(quote(x^10))
# 10 * x^9
# 10 * x^9

### curried function creator
`λ` <- l. <- function(..., env_ = parent.frame()) curry(f.(..., env_ = env_))
### Address of λ (lambda) in Unicode is U+03BB or \u03BB
# λ(g, x, g(g(x)))(λ(y, y+1))(5)
# f.(g, f.(x, g(g(x))))(f.(y, y+1))(5)
# l.(g, l.(x, g(g(x))))(l.(y, y+1))(5) # can work as a substitute for f.()
# l.(g, x, g(g(x)))(l.(y, y+1))(5) # no need to nest f.()

### http://www.angelfire.com/tx4/cus/combinator/birds.html
# S <- f.(x, f.(y, f.(z, (x(z))(y(z)) )))
# K <- f.(x, f.(y, x))
# I <- S(K)(K) # == f.(x, x) == identity()

# http://en.wikipedia.org/wiki/Fixed-point_combinator
# http://upload.wikimedia.org/math/9/c/c/9ccb07cb4f99bef41be7043990ac5eb3.png
# Y <- λ(f, (λ(x, f(x(x))))(λ(x, f(x(x)))))
## compare above express with the definition written in png file!
# fib_maker <- function(f) function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
# Y(fib_maker)(10) # => 55

### see the calculation processes at https://gist.github.com/TobCap/6668817
### fixed-point combinator
### R can write very simply
fix_ <- function(g) f <- g(f)

### http://en.wikipedia.org/wiki/Fixed-point_combinator

# fix0 <- function(g) f <- g(f)
# fix1 <- function(g) g(fix1(g))
# fix2 <- function(f) (function(x) f(x(x)))(function(x) f(x(x)))
# fix3 <- function(f) (function(x) f(function(y) x(x)(y)))(function(x) f(function(y) x(x)(y)))
# fix4 <- function(f) (function(x) function(y) f(x(x))(y))(function(x) function(y) f(x(x))(y))

### g = Y(f), f(g) = g; f = Y(g), g(f) = f
### Y g = g(Y g)
### Y = λf.(λx.f (x x)) (λx.f (x x))
### Z = λf.(λx.f (λy. x x y)) (λx.f (λy. x x y))

# > mult_maker <- function(f) function(x) if (x == 1) 1 else x * f(x-1)
# > microbenchmark(fix0(mult_maker)(100),fix1(mult_maker)(100),fix2(mult_maker)(100), fix3(mult_maker)(100), fix4(mult_maker)(100))
# Unit: microseconds
#                   expr      min        lq   median        uq      max neval
#  fix0(mult_maker)(100)  499.726  505.2985  509.088  522.4610  824.258   100
#  fix1(mult_maker)(100)  853.234  878.4210  887.336  923.4455 2320.313   100
#  fix2(mult_maker)(100)  871.065  889.7885  899.150  920.3245 2251.663   100
#  fix3(mult_maker)(100) 1114.910 1136.3075 1151.688 1226.8025 2951.100   100
#  fix4(mult_maker)(100) 1115.802 1138.3135 1153.693 1195.8200 2729.991   100

### refers to javascript.
### http://www.kmonos.net/wlog/52.php
### http://d.hatena.ne.jp/r-west/20090422/1240400570

# avoid to conflict with `memoise` that is already taken in library("memoise")
memoizer <- function(f) {
  memo_ <- new.env()
  f_orig <- f
  function(...) {
    key <- paste(list(...), collapse=",")
    if (is.null(memo_[[key]])) memo_[[key]] <- f_orig(...)
    memo_[[key]]
  }
}

tracer <- function(f) {
  num <- 0
  function(...) {
    key <- paste(list(...), collapse=",")
    num <<- num + 1
    suffix <- if (num <= 3) switch(num, "st", "nd", "rd") else "th"
    cat(num, suffix, " call with argument: ", key, "\n", sep = "")
    f(...)
  }
}

###
fib_maker <- function(f) function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
# > fix_(fib_maker)(5)
# [1] 5
# > fix_(tracer %>>% fib_maker)(5)
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

# > fibmemo <- fix_(tracer %>>% fib_maker %>>% memoizer)
### or fibmemo <- fix_(function(x) memoizer(fib_maker(tracer(x))))
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
# > unlist(eapply(environment(fibmemo)$memo_, identity))
# 0 1 2 3 4 5 6
# 0 1 1 2 3 5 8


### the same semantics
# fix_(tracer %>>% fib_maker %>>% memoizer)(5)
# (tracer %>>% fib_maker %>>% memoizer %|>% fix_)(5)
# {tracer %>>% fib_maker %>>% memoizer %|>% fix_}(5)
# 5 %|>% {tracer %>>% fib_maker %>>% memoizer %|>% fix_}
# tracer %>>% fib_maker %>>% memoizer %|>% fix_ %<|% 5

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
##
## fix6 <- function(f) U(function(x) f(x(x)))
## fix6(fib_maker)(10) # => 55

### language object operater
## promise_tracker can track promise's original symbol
promise_tracker <- function(`__sym__`, n = 1, strict = TRUE) {
  # see https://gist.github.com/TobCap/6473028
  if (strict) stopifnot(n <= sys.parent())
  stack_adjust <- 2

  make_call1 <- function(x) {
    if (x == 0) call("substitute", quote(`__sym__`))
    else call("substitute", make_call1(x - 1))
  }
  make_call2 <- function(x) {
    if (x == 0) make_call1(n)
    else call("eval", make_call2(x - 1), substitute(parent.frame(k), list(k = x + stack_adjust)))
  }
  eval(make_call2(n))
}
# f1 <- function(x1) f2(x1)
# f2 <- function(x2) f3(x2)
# f3 <- function(x3) f4(x3)
# f4 <- function(x4) promise_tracker(x4, n)
# n <- 1; f1() # => x3
# n <- 2; f1() # => x2
# n <- 3; f1() # => x1
# rm(n, f1, f2, f3, f4)

### convert call to list and vice versa
call_list_cnv <- function(f.arg) {
  arg_is_lang <- is.call(f.arg)
  cnv <- function(x) {
    if (length(x) == 1) x
    else if (is.pairlist(x)) as.pairlist(lapply(x, cnv))
    else if (arg_is_lang) lapply(x, cnv)
    else as.call(lapply(x, cnv))
  }
  cnv(f.arg)
}
as.list_recursive <- function(x) {stopifnot(is.call(x)); call_list_cnv(x)}
as.call_recursive <- function(x) {stopifnot(is.list(x)); call_list_cnv(x)}
length_recursive <- function(x) {
  if (!is.recursive(x)) length(x)
  else do.call(sum, lapply(as.list(x), length_recursive))
}

# language replacement
# see https://gist.github.com/TobCap/6348892
replace_symbol <- function(expr, before, after) {
  stopifnot(is.language(expr), is.symbol(before))
  eval(substitute(substitute(e, `names<-`(list(after), as.character(before))), list(e = expr)))
}

replace_call <- function(expr, before, after) {
  stopifnot(is.language(expr))
  conv <- function(x) {
    if (is.pairlist(x) && !is.null(x)) {
      if (is.symbol(before)) { # for formal parameters
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

replace_multi <- function(expr, befores, after, replace_fun = replace_symbol) {
  make_call <- function(x) {
    if (length(x) == 0) expr
    else replace_fun(make_call(x[-1]), x[[1]], after)
  }
  make_call(c(befores)) # need c() if length(befores) == 1
}

replace_lang <- function (expr, before, after, can_accept_undefined_var = FALSE) {
  stopifnot(is.language(expr))
  if (can_accept_undefined_var) {
    stopifnot(exists("accept_undefined_var"))
    before <- accept_undefined_var(before, parent.frame())
    after <- accept_undefined_var(after, parent.frame())
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
      if (is.function(before)) {
        ind <- match(TRUE, vapply(lapply(names(x), as.symbol), before, logical(1)))
        names(x)[ind] <- as.character(after)
      } else if (is.symbol(before[[1]])) {
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

accept_undefined_var <- function(.x, env) {
  # access to parent.frame(2)
  expr <- eval.parent(substitute(substitute(.x)))
  lang_fun_names <- c("quote", "expression", "as.name", "as.symbol", "call")
  is_list_vector <- !is.symbol(expr) && (expr[[1]] == quote(c) || expr[[1]] == quote(list))
  w <- function(x) warning(
    paste0("the '", x,"' is an existing language object,so not interpreted as undefined variable"))

  force_lang <- function(y) {
    first_char <- as.character(y)[[1]]
    if (is.character(y)) parse(text = y)[[1]] # character
    else if (first_char %in% lang_fun_names[1:2]) y[[2]] # quote or expression
    else if (first_char %in% lang_fun_names[3:5]) eval(y) # as.symbol, as.name or call
    else if (is.symbol(y) && exists(first_char, envir = env)) {w(first_char); eval(y)} # assigned language object
    else y # undefined, not language object, or not character object
  }

  if (is_list_vector) lapply(as.list(expr)[-1], force_lang)
  else force_lang(expr)
}

# replace_symbol(quote(x+y*z), quote(y), quote(www))
# replace_lang(quote(x + y ^ z), y, www, TRUE)

# > replace_symbol(quote(1+2+x^3), quote(x), quote(y))
# 1 + 2 + y^3
# > replace_symbol(quote(1+2+x^3), quote(2), quote(99))
# 1 + 2 + x^3 # cannot replace value
# > replace_call(quote(1+2+x^3), quote(2), quote(99))
# 1 + 99 + x^3 # ok

# > replace_symbol(quote(1+2+x^3) , quote(`+`), quote(`-`))
# 1 - 2 - x^3
# > eval(replace_symbol(quote(1+2+x^3) , quote(`+`), quote(`-`)), list(x = 2))
# [1] -9

# arguments need to be quoted
nest_formula <- function(expr, variable, num) {
  if (num == 1) expr
  else replace_call(nest_formula(expr, variable, num - 1), variable, expr)
}
# > nest_formula(quote((1 + x)^2), quote(x), 3)
# (1 + (1 + (1 + x)^2)^2)^2
# > nest_formula(quote((1 + x)^2), quote(1 + x), 3)
# (((1 + x)^2)^2)^2
# > eval(nest_formula(quote(1 + 1 / x), quote(x), 40), list(x = 1)) == (1 + sqrt(5))/2
# [1] TRUE

### tail recursive
nest_fun <- function(f, n, acc_fun = identity) {
  # compose_ is already defined above.
  if (n == 0) acc_fun
  else if (n %% 2 == 0) nest_fun(compose_(f, f), n / 2, acc_fun)
  else nest_fun(f, n - 1, compose_(f, acc_fun))
}
nest_fun2 <- function(f, n) {
  if (n == 0) identity
  else compose_(f, nest_fun2(f, n - 1))
}

# > microbenchmark(tail.rec = nest_fun(function(x) {x}, 100)(0), simple = nest_fun2(function(x) {x}, 100)(0), times=1e4)
# Unit: microseconds
#      expr     min      lq  median      uq       max neval
#  tail.rec 140.867 184.554 205.951 234.927  5700.185 10000
#    simple 308.480 406.998 453.804 525.575 42763.638 10000

# > microbenchmark(tail.rec = nest_fun(function(x) {x}, 10)(0), simple = nest_fun2(function(x) {x}, 10)(0), times=1e5)
# Unit: microseconds
#      expr    min     lq median     uq      max neval
#  tail.rec 24.964 27.639 30.760 37.892  4637.00 1e+05
#    simple 29.868 33.435 37.446 45.916 51108.19 1e+05

# > microbenchmark(tail.rec = nest_fun(function(x) {x}, 1)(0), simple = nest_fun2(function(x) {x}, 1)(0), times = 1e6)
# Unit: microseconds
#      expr   min    lq median    uq      max neval
#  tail.rec 4.458 5.796  6.687 7.579 41602.83 1e+06
#    simple 3.567 4.459  5.350 6.242 63045.72 1e+06

# > nest_fun(f.(x, 1 + 1/x), 100)(1) == (1+sqrt(5))/2
# [1] TRUE

### When using nest_fun(), tracer() can check passing arguments.
# > nest_fun(tracer(f.(x, 1 + 1/x)), 6)(1)
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
# num  <- l.(n, f, x, nest_fun(f, n)(x)) # f.(n, f.(f, f.(x, nest_fun(f, n)(x))))
#
# plus <- l.(m, n, f, x, m(f)(n(f)(x)))  # f.(m, f.(n, f.(f, f.(x, m(f)(n(f)(x))))))
# succ <- l.(n, f, x, f(n(f)(x)))        # f.(n, f.(f, f.(x, f(n(f)(x)))))
# mult <- l.(m, n, f, m(n(f)))           # f.(m, f.(n, f.(f, m(n(f)))))
# exp  <- l.(m, n, n(m))                 # f.(m, f.(n, n(m)))
# to_int <- l.(n, n(l.(n, n + 1))(0))     # f.(n, n(f.(n, n + 1))(0))
#
# to_int(plus(zero)(one))
# to_int(plus(one)(two))
# to_int(succ(one))
# to_int(num(99))
# to_int(plus(num(7))(num(8)))
# to_int(mult(num(3))(num(5)))
# num(3) %|>% mult %<|% num(5) %|% to_int

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
# identical(not2(false), true, ignore_environment = TRUE)
# identical(not2(true), false, ignore_environment = TRUE)
#
# to_int(if_(true)(one)(two))
# to_int(if_(false)(one)(two))

lang2char <- function(expr, type = c("rexp", "sexp")) {
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
