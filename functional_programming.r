### functional operators and useful functions

### Pipeline like operator
## Left value can be passed by ".." just like scala's underscore "_".
## I use ".."; "." is sometimes used within a model formula expression or other packages as a meaningful symbol.
## It is not fast but easy to read and understand because of using fewer parentheses.
`%|%` <- (function() {
  replace_two_dots <- function(rhs, lhs_replaced) {
    iter <- function(x) {
      if (length(x) <= 1 && !is.recursive(x)) {
        if (is.symbol(x) && x == "..") { lhs_replaced }
        else { x } }
      else if (x[[1]] == "%|%") { call("%|%", iter(x[[2]]), x[[3]]) }
      else if (is.pairlist(x)) { as.pairlist(lapply(x, iter)) }
      else { as.call(lapply(x, iter)) }
    }
    iter(rhs)
  }
  
  function(lhs, rhs, pf_ = parent.frame()) {
    rhs_expr <- substitute(rhs)
    
    ## short-cut
    if (length(rhs_expr) == 1 && is.symbol(rhs_expr)) {
      return(rhs(lhs))
    }
    
    all_syms <- all.names(rhs_expr)
    has_nest_pipe <- any(all_syms == "%|%")
    has_dots <- any(all_syms == "..")
    
    if (!has_dots) {
      rhs(lhs)
    } else if (has_nest_pipe) {
      rhs_expr_mod <- replace_two_dots(rhs_expr, substitute(lhs))
      eval(rhs_expr_mod, envir = pf_)
    } else {
      rhs_expr_mod <- substituteDirect(rhs_expr, list(.. = substitute(lhs)), FALSE)
      eval(rhs_expr_mod, envir = pf_)
      
      # eval(rhs_expr, envir = list(.. = substitute(lhs)), enclos = pf_)
      # The above commented code does not evaluate appropriate environment when 
      # a function of rhs_expr uses parent.frame() as its default argument value.
      #
      # 1:5 %|% assign(x = "test_var", value = ..)
    }
  }
})()

## examples
# > 1:5 %|% (..-1) %|% (..^2)
# [1]  0  1  4  9 16
# > 1:5 %|% {..-1} %|% {..^2}
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

## Benchmarking
# library(magrittr)
# library(pipeR)
# library(microbenchmark)

# a <- 1
# > microbenchmark(sum(a), a %|% sum, a %|% sum(..), a %>% sum, a %>>% sum)
# Unit: nanoseconds
#           expr    min       lq      mean   median       uq     max neval
#         sum(a)    446    892.0   1520.89   1338.0   1784.0   17386   100
#      a %|% sum  11591  12929.0  15790.48  14712.0  16494.5   34772   100
#  a %|% sum(..)  29868  33435.0  57997.16  38784.0  46585.5 1230363   100
#      a %>% sum 178314 187676.0 233867.58 203723.5 259892.0  473869   100
#     a %>>% sum  25856  29199.5  36786.80  32989.0  37892.5   95844   100

# > microbenchmark(
#   "%|%" = 1 %|% sum(.., rm.na = TRUE),
#   "%>%" = 1 %>% sum(rm.na = TRUE),
#   "%>>%"= 1 %>>% sum(rm.na = TRUE))
# Unit: microseconds
#  expr     min       lq      mean  median       uq      max neval
#   %|%  29.422  32.3195  44.48998  39.676  43.6875  191.241   100
#   %>% 256.772 265.9105 324.78078 280.399 334.3385 1078.796   100
#  %>>%  39.676  44.5790  64.02426  52.158  58.3985  582.640   100


# microbenchmark(
#   "%|%" =
#     airquality %|%
#     transform(.., Date = paste(1973, Month, Day, sep = "-") %|% as.Date) %|%
#     aggregate(. ~ Date %|% format(.., "%W"), .., mean) %|%
#     subset(.., Wind > 12, c(Ozone, Solar.R, Wind))
#   ,
#   "%>%" =
#     airquality %>%
#     transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>%
#     aggregate(. ~ Date %>% format("%W"), ., mean) %>%
#     subset(Wind > 12, c(Ozone, Solar.R, Wind))
# )
# Unit: milliseconds
#  expr      min       lq     mean   median       uq      max neval
#   %|% 23.92341 35.60762 37.86391 36.87141 41.19685 56.69919   100
#   %>% 17.24023 23.82489 25.76603 24.93935 28.00768 36.33135   100

# pipeR does not work. How should I modify this code?
# list(
#   "%>>%" =
#     airquality %>>%
#     transform(Date = paste(1973, Month, Day, sep = "-") %>>% as.Date) %>>%
#     aggregate(. ~ Date %>>% format("%W"), ., mean) %>>%
#     subset(Wind > 12, c(Ozone, Solar.R, Wind))
# )

##                    %|%                 %>%, %>>%
## placeholder symbol ..                  .
## syntax             cannot omit arg     UFCS-like
## evaluation         lazy                eager
## addOne             (..+1)              `+`(1) or add(1) or {.+1}

## The old version was simpler but a left side is evaluated before passing it into a right side function
## and does not support nested `%|%`
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

## composing one more functions
`<<--` <- function(...) {
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, list(...), x, right = TRUE)
}
`-->>` <- function(...) {
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, rev(list(...)), x, right = TRUE)
}

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
  # `as_special = TRUE` is required if typeof(f) is "closure" and there are any language object handling 
  # functions such as substitute() or match.call() inside body(f)
  # 
  # curry(bquote)(a + .(b))(list(b = 10)) => expr
  # curry(bquote, as_special = TRUE)(a + .(b))(list(b = 10)) => a + 10

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
      , builtin = as.call(c(f, `names<-`(lapply(names(f_args), as.symbol), names(f_args))))
      , special = make_special_body()
      )
    else call("function", as.pairlist(args_[1]), make_body(args_[-1]))
  }

  make_special_body <- function() {
    # `f_sym`, `f_args`, `env_` are parent environment's variable
    bquote({
      
      f_sym <- .(f_sym)
      f_args_rev <- lapply(rev(names(.(f_args))), function(x) as.symbol(x))
      
      args_ <- list()
      e <- environment()
      
      while (length(f_args_rev) > 0) {
        f_args_head <- f_args_rev[[1]]
        
        arg_ <-
          if (f_args_head == "...")  as.list(methods::substituteDirect(call("list", f_args_head), e))[-1]
          else methods::substituteDirect(f_args_head, e)
          
        if (is.null(arg_)) arg_ <- list(NULL) # if default value is NULL
        args_ <- append(arg_, args_)
        
        # next loop
        f_args_rev <- f_args_rev[-1]
        e <- parent.env(e)
      }
      
      fun_new <- as.call(c(f_sym, args_))
      eval(fun_new, .(env_))
      # do.call(f_sym, args_, quote = TRUE, envir = .(env_))

    }, parent.env(environment()))
  }
  
  f_sym <- substitute(f)
  f_args <- formals(args(f))

  if (is.null(f_args)) f
  else eval(make_body(f_args), environment(f), baseenv())
  # baseenv() is used only if environment(f) is NULL; that means `f` is special or builtin function in baseenv()
  # except methods::Quote and methods::`el<-` that are S-Plus compatible functions
}

# > curry(function(x, y, z) x + y + z)
# function (x)
#   function(y) function(z) x + y + z

# > curry(function(x, y, z) x + y + z)(1)(2)(3)
# [1] 6

# plus2 <- curry(`+`)(2)
# plus2(10) # 12

### `seq.int` is exceptional due to constructing formal parameters inside C-lang level
### https://github.com/wch/r-source/blob/ed415a8431b32e079100f50a846e4769aeb54d5a/src/main/seq.c#L725-L733

# > seq.int_c <- curry(seq.int)
# > seq.int_c(1)(5)()()()()
# Error in ((((seq.int_c(1)(5))())())())() : 
#   argument "by" is missing, with no default

### what you need is to use `quote(expr=)`
# > seq.int_c(1)(5)(quote(expr=))(quote(expr=))(quote(expr=))()
# [1] 1 2 3 4 5


curry_dots <- function (fun, env_ = parent.frame()) {
  fun_args <- formals(args(fun))
  dots_pos_rev <- which(rev(names(fun_args)) == "...")
  
  if (length(dots_pos_rev) == 0) stop("`fun` do not have a dot-dot-dot argument")

  iter <- function(pos, acc_arg) {
    if (pos == 0) return(eval(as.call(c(fun, acc_arg)), env_))
    
    arg_ <-
      if (pos == dots_pos_rev) as.pairlist(alist(...=))
      else as.pairlist(rev(fun_args)[pos])

    body_ <- bquote({
      pos <- .(pos)
      dots_pos_rev <- .(dots_pos_rev)
      acc_arg <- .(acc_arg)
      iter <- .(iter)
      
      if (pos == dots_pos_rev && nargs() > 1) stop("... accepts only one argument.")
      if (pos == dots_pos_rev && nargs() == 0) return(iter(pos - 1, acc_arg))
      
      x_val <- as.pairlist(as.list(sys.call())[-1])
      x_formal <- formals(sys.function())
      x <-
        if (is.null(x_val)) x_formal # default value
        else if (pos == dots_pos_rev) x_val
        else `names<-`(x_val, names(x_formal))
        
      iter(if (pos == dots_pos_rev) pos else pos - 1, append(acc_arg, x))
    })
    
    eval(call("function", arg_, body_), parent.frame())
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
    iter <- function(f, dots) {
      if (length(dots) == 0) f
      else iter(f(dots[[1]]), dots[-1])
    }
    iter(fun, list(...))
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
    bquote({
      called <- sys.call() # match.call(expand.dot = FALSE) ?
      idx <- .(c(r, l))
      called[-1] <- called[-1][idx]
      called[[1]] <- .(fun_sym)
      eval(called, .(env_))
    }, parent.env(environment()))
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
# devtools::install_github("tobcal/lambdass") 
`λ` <- l. <- function(..., env_ = parent.frame()) curry(f.(..., env_ = env_))
### Address of λ (lambda) in Unicode is U+03BB or \u03BB
# cat("\u03BB\n")
# λ

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

# `expr` and `variables` need to be quoted
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
