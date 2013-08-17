### functional operators

### adopt `f.` instead of `f` because `f` often causes conflicts in many sample codes.
f. <- function(..., env = parent.frame()){
  # d <- match.call(expand.dots = FALSE)$`...`
  # the above does not resolve when ... is passed by other wraped function.
  d <- as.pairlist(lapply(substitute(list(...)), identity)[-1])
  # need to be pairlist when ... is nothing; NULL
  n <- length(d)
  eval(call("function", as.pairlist(tools:::as.alist.call(d[-n])), d[[n]]), env)
}
# f. <- function(..., env = parent.frame()){
#   d <- lapply(substitute(list(...)), identity)[-1]
#   as.function(c(tools:::as.alist.call(d[-length(d)]), d[length(d)]), envir = env) 
# }
# The above commented code which I made first time is bit slower than the current code.
# See reference https://github.com/hadley/pryr/blob/master/benchmark/make-function.r

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
curry <-  function(fun, env = parent.frame()) {
  recursiveCall <- function(len, arg){
    if (len == 0) return(do.call(fun, arg, envir = env))
    function(x) recursiveCall(len - 1, append(arg, list(x)))
  }
  recursiveCall(length(formals(args(target.fun))), list())
}

curry <-  function(fun, env = parent.frame()) {
  recursiveCall <- function(len, arg){
    if (len == 0) return(do.call(fun, arg, envir = env))
    function(x) recursiveCall(len - 1, append(arg, list(x)))
  }
  fun.orig <- substitute(fun)
  target.fun <- 
    if(length(fun.orig) > 1 && fun.orig[[1]] == quote(flip)) eval(fun.orig[[2]])
    else fun
  recursiveCall(length(formals(args(target.fun))), list())
}

# plus2 <- curry(`+`)(2)
# plus2(10) # 12

### flip is defined below
# div10 <- curry(flip(`/`))(10)
# div10(24) # = 2.4

# > curry(function(x, y, z) x + y + z)(1)(2)(3)
# [1] 6
### `f.` is already defined above and save your typing.
# > curry(f.(x, y, z, x + y + z))(1)(2)(3)
# [1] 6
# > f.(x, y, z, x + y + z) %|>% curry %<|% 1 %<|% 2 %<|% 3
# [1] 6

λ <- l. <- function(...) curry(f.(..., env = parent.frame()))
### Address of λ (lambda) in Unicode is U+03BB or \u03BB
# λ(g, x, g(g(x)))(λ(y, y+1))(5)
# f.(g, f.(x, g(g(x))))(f.(y, y+1))(5)
# l.(g, l.(x, g(g(x))))(l.(y, y+1))(5)
# l.(g, x, g(g(x)))(l.(y, y+1))(5)

### http://www.angelfire.com/tx4/cus/combinator/birds.html
# S <- f.(x, f.(y, f.(z, (x(z))(y(z)) )))
# K <- f.(x, f.(y, x))
# I <- S(K)(K) # f.(x, x) 

uncurry <- function(fun){
  function(...){
    args <- c(...)
    if (length(args) == 1) fun(args[1])
    else uncurry(fun(args[1]))(args[-1])
  }
}
# > g <- function(x) function(y) function(z) x + y + z
# > g(1)(2)(3)
# [1] 6
# > uncurry(g)(1, 2, 3)
# [1] 6

###
flip <- function(FUN, l = 1, r = 2){
  args.orig <- formals(args(match.fun(FUN)))
  stopifnot(1 < r && l < length(args.orig) && l < r)

  function(...) {
    dots <- list(...)
    dots[c(r, l)] <- dots[c(l, r)]
    do.call(FUN, dots)
  }
}
### example
# > flip(`-`)(2, 5)
# [1] 3
# > flip(sapply)(sqrt, 1:4)
# [1] 1.000000 1.414214 1.732051 2.000000

# avoiding conflict with utils::zip
# 結局 mapply
zip. <- function(..., FUN = list){
  dot.args <- list(...)
  args.seq <- seq_len(min(vapply(dot.args, length, 0)))
  args.new <- lapply(dot.args, function(x) x[args.seq])
  do.call(mapply, c(FUN = FUN, args.new, SIMPLIFY = FALSE, USE.NAMES = FALSE))
}

zipWith. <- function(fun, ..., do.unlist = FALSE) {
  if (do.unlist) unlist(zip.(..., FUN = match.fun(fun)))
  else zip.(..., FUN = match.fun(fun))
}

# zip.(list(1,2,3), list(4,5,6))
# zip.(1:3, 4:6)

# zip.(list(1,2,3), list(4,5,6), list(7,8,9))
# zip.(1:3, 4:6, 7:9)
# zip..(1:3, letters[1:3])

# zipWith(f.(x,y, 2*x+y), 1:4, 5:8)
#
# > func <- function(x) x %|% zip.(.., 0:(length(x)-1)) %|% Map(prod, ..) %|% Reduce(`+`, ..)
# > func(seq(10, 50, by=10))
# [1] 400
#
### http://mew.org/~kazu/material/2011-haskell.pdf
### Ruby
# def func (ar)
# ar.zip((0..ar.length).to_a) \
# .map{|(x,i)|x*i} \
# .reduce(:+);
# end
# func([10,20,30,40,50]);
# → 400

### http://neue.cc/2011/02/14_302.html
"%|>%" <- function(x, f) f(x)
"%<|%" <- function(f, x) f(x)

### functional composition
"%>>%" <- function(f, g) function(x) g(f(x))
"%<<%" <- function(f, g) function(x) f(g(x))
compose. <- function(f, g) function(x) f(g(x))
# f1 <- f.(x, {cat("f1(x = ", x, ") -> ", sep = ""); x + 1})
# f2 <- f.(x, {cat("f2(x = ", x, ") -> ", sep = ""); x * 2})
# f3 <- f.(x, {cat("f3(x = ", x, ") -> ", sep = ""); x / 3})
# f4 <- f1 %>>% f2 %>>% f3
# f5 <- f1 %<<% f2 %<<% f3

# > f4(1)
# f1(x = 1) -> f2(x = 2) -> f3(x = 4) -> [1] 1.333333
# > f5(1)
# f3(x = 1) -> f2(x = 0.3333333) -> f1(x = 0.6666667) -> [1] 1.666667

### I refer to the code in help("Reduce")
### only works when value is at first or end and functions, rest of args, require one argument.
# `<--` <- function(...){
  # Reduce("%<|%", list(...), right = TRUE)
# }
# `-->` <- function(...){
  # Reduce("%|>%", list(...), right = FALSE)
# } 
# > `-->`(1:5, f.(x, x-1), f.(x, x*10))
# [1]  0 10 20 30 40
# `<<--` <- function(...){
    # Funcall <- function(f, ...) f(...)
    # function(x) Reduce(Funcall, list(...), x, right = TRUE)
# }
# `-->>` <- function(...){
    # Funcall <- function(f, ...) f(...)
    # function(x) Reduce(Funcall, rev(list(...)), x, right = TRUE)
# }
# `-->>`(f.(x, x-1), f.(x, x*10))(1:5)
# [1]  0 10 20 30 40

### pipeline like operator
### left value can be passed by ".." just like scala's underscore "_".
### I use "..", "." is already used in "package:plyr".
### not fast but easy to read and understand because of using fewer parentheses.
"%|%" <- function(lhs, rhs){
  ans <- eval(substitute(rhs), envir = list(.. = lhs), enclos = parent.frame())  
  if (is.function(ans))
    ans(lhs)  
  else if (all(typeof(ans)=="logical", length(lhs)==length(ans)))
    lhs[ans]
  else
    ans
}

# > 1:5 %|% f.(x, x-1) %|% f.(x, x^2)
# [1]  0  1  4  9 16
# > 1:5 %|% (..-1) %|% (..^2)
# [1]  0  1  4  9 16
# > 1:5 %|% {..-1} %|% {..^2}
# [1]  0  1  4  9 16

### the same
# > Filter(function(x) x%%2==0, 1:5)
# [1] 2 4
# > 1:5 %|% (..%%2==0)
# [1] 2 4

# here, with %|% operator, right assignment is useful to define a variable
# > 1:10 %|% (..%%2==0) %|% ..^2 -> x
# > x
# [1]   4  16  36  64 100

### plot brownian motion in one liner
# > f.({mu<-0;m<-252;sigma<-0.2/sqrt(m) ;rnorm(m, mu-sigma^2/2, sigma) %|% cumsum %|% c(0, ..)}) %|% replicate(n=1000, ..()) %|% matplot(.., type = "l", ann = FALSE)

### set break lines after typing `%|%`
# f.({mu<-0;m<-252;sigma<-0.2/sqrt(m) ;rnorm(m, mu-sigma^2/2, sigma) %|% 
#   cumsum %|% 
#   c(0, ..)}) %|% 
# replicate(n=1000, ..()) %|% 
# matplot(.., type = "l", ann = FALSE);

### invokes interceptor; the idea comes from underscore javascript.
### it easy to debug.
tap <- function(x, fun) {
  if(missing(fun)) print(x) else print(fun(x))
  x
}
# > 1:10 %|% (..%%2==0) %|% tap %|% ..^2
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
# > (pi/2) %|% sin %|% log %|% cos
# > (pi/2) %|>% sin %|>% log %|>% cos

### fixed-point combinator
### I tried to check four types of fixed-point combinator function and blow is the simplest and fastest
fix. <- function(g) {f <- g(f)}

### http://en.wikipedia.org/wiki/Fixed-point_combinator

# fix0 <- function(g) g(fix0(g))
# fix1 <- function(g) f <- g(f)
# fix2 <- function(f) (function(x) f(x(x)) )(function(x) f(x(x)))
# fix3 <- function(f) (function(x) f(function(y) x(x)(y)))(function(x) f(function(y) x(x)(y)))
# fix4 <- function(f) (function(x) function(y) f(x(x))(y))(function(x) function(y) f(x(x))(y))

### Y g =  g (Y g)
### g = Y(f), f(g) = g
### Y = λf・(λx・f (x x)) (λx・f (x x))
### Z = λf.(λx.f (λy. x x y)) (λx.f (λy. x x y))

# mult_maker <- function(f) function(x) if (x == 1) 1 else x * f(x-1)
# > microbenchmark(fix0(mult_maker)(100),fix1(mult_maker)(100),fix2(mult_maker)(100), fix3(mult_maker)(100), fix4(mult_maker)(100))
# Unit: microseconds
# expr     min       lq   median       uq      max
# 1 fix0(mult_maker)(100) 200.585 207.0100 211.7595 219.1625  289.423
# 2 fix1(mult_maker)(100) 119.010 124.0385 126.9725 131.8615  191.086
# 3 fix2(mult_maker)(100) 204.775 212.8770 217.6260 224.0510  302.553
# 4 fix3(mult_maker)(100) 250.312 260.7875 267.2135 277.9690 1124.725
# 5 fix4(mult_maker)(100) 250.591 263.3020 267.9115 279.7845 1121.372

### refers to javascript.
### http://d.hatena.ne.jp/r-west/20090422/1240400570
# gen.memoizer <- function (use.global = FALSE) {
gen.memoizer <- function(stored.env, reset.all = FALSE){
  if(missing(stored.env)) {
    stored.env <-  tryCatch(as.environment("functionalProgramming.r"),
      error = function(e) {attach(NULL, name="functionalProgramming.r")})
  } else {
    stopifnot(is.environment(stored.env))
  }
  
  if(exists(".memo", envir = stored.env) & !reset.all) {
    .memo <- get(".memo", envir = stored.env)
  } else {
    .memo <- assign(".memo", new.env(), envir = stored.env)
  }
  
  function(f) {
    #fun.names <- paste0(lapply(sys.calls(), `[`, 1), collapse = ",")
    all.funs <- lapply(sys.calls(), 
      function(x) as.call(lapply(x, function(y) if(!is.language(y)) NULL else y))
    )
    fun.names <- paste0(all.funs, collapse = ",")
    if (!exists(fun.names, envir = .memo)) 
      assign(fun.names, new.env(), envir = .memo)
    function(...){
      key <- paste0(c(...), collapse=",")
      if (is.null(.memo[[fun.names]][[key]])) .memo[[fun.names]][[key]] <- f(...)
      .memo[[fun.names]][[key]]
    }
  }
}
# previous simple version
# gen.memoizer <- function () {
  # function(f) {
    # function(...){
      # key <- paste(list(...), collapse=",")
      # if (is.null(.memo[[key]])) .memo[[key]] <- f(...)
      # .memo[[key]]
    # }
  # }
# }
gen.tracer <- function () {
  num <- 0
  function(f) {
    function(...){
      key <- paste(list(...), collapse=",")
      num <<- num + 1 # keyより下でないといけない 副作用の弊害
      cat(num, if(num <= 3) switch(num, "st", "nd", "rd") else "th", " call with argument: ", key, "\n", sep = "")
      f(...)
    }
  }
}
###

# fib_maker <- function(f) {
#   function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
# }
# > fix.(fib_maker)(5)
# [1] 5
# > fix.(gen.tracer() %>>% fib_maker)(5)
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
### see the chart in http://mitpress.mit.edu/sicp/full-text/sicp/book/node16.html
### and compare it with above outputs.

# > fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(5)
# 1st call with argument: 4
# 2nd call with argument: 3
# 3rd call with argument: 2
# 4th call with argument: 1
# 5th call with argument: 0
# [1] 5

### all results are the same, but memoized only if input is after the second execution.
# fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(5)
# (gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.)(5)
# {gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.}(5)
# 5 %|>% {gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.}
# gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix. %<|% 5

### gen.memoizer() set results into `.memo` in a given environment.
### gen.memoizer(reset.all = TRUE) resets and creates a new `.memo` in the given environment.

# > invisible(gen.memoizer(reset.all = TRUE)) # reset
# > ls(.memo)
# character(0)

# > fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(5)
# 1st call with argument: 4
# 2nd call with argument: 3
# 3rd call with argument: 2
# 4th call with argument: 1
# 5th call with argument: 0
# [1] 5

### the result is memoised.
# > fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(5)
# [1] 5

### only new arguments are passed to calculation
# > fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(7)
# 1st call with argument: 6
# 2nd call with argument: 5
# [1] 13

# > ls(.memo)
# [1] "fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(NULL),f(x),g(f(x))"
#
# > lapply(.memo, function(x) ls(x))
# $`fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(NULL),f(x),g(f(x))`
# [1] "0" "1" "2" "3" "4" "5" "6"
#
# > lapply(.memo, function(x) lapply(x, identity))
# > lapply(.memo, function(x) rapply(as.list(x), identity))
# $`fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(NULL),f(x),g(f(x))`
# 0 1 2 3 4 5 6 
# 0 1 1 2 3 5 8 

# fib <- gen.memoizer()(function(x) if(x<=1) x else fib(x-1) + fib(x-2))
# > fib(1000)
# [1] 4.346656e+208
# > tail(ls(as.list(.memo)[[2]]))
# [1] "994" "995" "996" "997" "998" "999"

# install.packages("gmp"); library("gmp")
# > fib2 <- gen.memoizer()(function(x) if(x<=1) as.bigz(x) else fib2(x-1) + fib2(x-2))
# > fib2(1000)
# Big Integer ('bigz') :
# [1] 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875

### I refered to http://d.hatena.ne.jp/einblicker/20110108/1294448477, http://d.hatena.ne.jp/einblicker/20110113/1294920315

### convert call to list and vice versa
call.list.cnv <- function(f.arg){
  arg.is.lang <- is.language(f.arg)
  cnv <- function(x){
    if (length(x) == 1) x
    else if (is.pairlist(x)) as.pairlist(lapply(x, cnv))
    else if (arg.is.lang) lapply(x, cnv) else as.call(lapply(x, cnv))
  }
  cnv(f.arg)
}

# auxiliay function
.quote.if.not <- function(.x){
  expr.original <- eval(substitute(substitute(.x)), parent.frame())
  var.name.first <- as.character(expr.original)[[1]] 
  if (var.name.first %in% c("quote", "as.name", "as.symbol")) .x
  else if (var.name.first == "expression") expr.original[[2]]
  else expr.original
}

# replace variable name
cnv <- function(expr, before, after, allow.non.quote = TRUE){
  if (allow.non.quote && exists(".quote.if.not", envir = parent.env(environment()), mode="function")) {
    expr <- .quote.if.not(expr)
    before <- .quote.if.not(before)
    after <- .quote.if.not(after)
  }
  conv <- function(x) {
    if (is.pairlist(x)) {
      # for function's arguments and its names
      ind <- match(as.character(before), names(x))
      names(x)[ind] <- as.character(after)
      as.pairlist(lapply(x, conv))
    } else if (length(x) == 1) {
      if (x == before) after
      else x
    }
    else as.call(lapply(x, conv))
  }
  conv(expr)
}
# cnv(quote(x+y*z), quote(y), quote(www))
# cnv(x + y ^ z, y, www)
# cnv(x + y ^ z, y, as.name("www"))
# cnv(expression(x + y ^ z), y, www)

# > cnv(quote(1+2+x^3), quote(x), quote(y))
# 1 + 2 + y^3
# > cnv(quote(1+2+x^3), quote(2), quote(99))
# 1 + 99 + x^3

# > cnv(quote(1+2+x^3) , quote(`+`), quote(`-`))
# 1 - 2 - x^3
# > eval(cnv(quote(1+2+x^3) , quote(`+`), quote(`-`)), list(x = 2))
# [1] -9

# arguments need to be quoted
nest.formula <- function(expr, variable, num){
  if (num == 1) return(expr)
  else cnv(nest.formula(expr, variable, num - 1), variable, expr, FALSE)
}
# > nest.formula(quote(sqrt(1+x^2)), quote(x), 3)
# sqrt(1 + sqrt(1 + sqrt(1 + x^2)^2)^2)
# > eval(nest.formula(quote(1+1/x), quote(x), 40), list(x = 1)) == (1+sqrt(5))/2
# [1] TRUE

### tail recursive
nest.fun <- function(f, n, acc.fun = identity){
  # compose. is already defined above.
  if (n == 0) return(acc.fun)
  else if (n %% 2 == 0) nest.fun(compose.(f, f), n / 2, acc.fun)
  else nest.fun(f, n - 1, compose.(f, acc.fun))
}
nest.fun2 <- function(f, n){
  if (n == 0) function(x) identity(x)
  else compose.(f, nest.fun2(f, n - 1))
}
# > benchmark(tail.rec = nest.fun(function(x) {x}, 100)(0), simple = nest.fun2(function(x) {x}, 100)(0), replications=1e4)[,1:4]
#       test replications elapsed relative
# 2   simple        10000     3.2 2.666667
# 1 tail.rec        10000     1.2 1.000000
#
# > benchmark(tail.rec = nest.fun(function(x) {x}, 10)(0), simple = nest.fun2(function(x) {x}, 10)(0), replications=1e5)[,1:4]
#       test replications elapsed relative
# 2   simple       100000    3.30 1.240602
# 1 tail.rec       100000    2.66 1.000000
#
# > benchmark(tail.rec = nest.fun(function(x) {x}, 1)(0), simple = nest.fun2(function(x) {x}, 1)(0), replications=1e6)[,1:4]
#       test replications elapsed relative
# 2   simple      1000000    5.52 1.000000
# 1 tail.rec      1000000    8.31 1.505435



# > nest.fun(f.(x, 1 + 1/x), 100)(1) == (1+sqrt(5))/2
# [1] TRUE

### checking arguments using gen.tracer() function.
# > nest.fun(gen.tracer()(f.(x, 1 + 1/x)), 3)(1)
# 1st call with argument: 1
# 2nd call with argument: 2
# 3rd call with argument: 1.5
# [1] 1.666667

### http://en.wikipedia.org/wiki/Church_encoding
### http://taiju.hatenablog.com/entry/20120529/1338299884
# zero <- l.(f, x, x)
# one  <- l.(f, x, f(x))
# two  <- l.(f, x, f(f(x)))
# num  <- l.(n, f, x, nest.fun(f, n)(x))
#
# plus <- l.(m, n, f, x, m(f)(n(f)(x)))
# succ <- l.(n, f, x, f(n(f)(x)))
# mult <- l.(m, n, f, m(n(f)))
# toInt <- f.(n, n(f.(n, n + 1))(0))
# 
# toInt(plus(zero)(one))
# toInt(plus(one)(two))
# toInt(succ(one))
# toInt(num(99))
# toInt(plus(num(7))(num(8)))
# toInt(mult(num(3))(num(5)))
# num(3) %|>% mult %<|% num(5) %|% toInt

# tail call optimization, not completely tested
tco <- function(f, var.ind = 1, out.ind = length(formals(f)), stop.num = 0){
  g <- function(){}
  body(g) <- cnv(body(f), sys.call()[[2]], quote(list), FALSE)

  out.fun <- function(){
    f.arg.names <- names(formals(f))
    stop.var.name <- f.arg.names[[var.ind]]
    inherited.env <- new.env(parent = environment(f))
    for(i in seq_along(f.arg.names)) {
      assign(f.arg.names[i], get(f.arg.names[i]), envir = inherited.env)
    }
    is.first <- TRUE
    while(TRUE){
      if(is.first) {environment(g) <- inherited.env; is.first <- FALSE}
      if(environment(g)[[ f.arg.names[[var.ind]] ]] == stop.num) break
      environment(g) <- list2env(stats:::setNames(g(), f.arg.names), envir = inherited.env)
    }
    return(environment(g)[[f.arg.names[[out.ind]]]])
  }
  
  formals(out.fun) <- formals(f)
  return(out.fun)
}

sum.rec <- function(n, acc = 0){
  if(n == 0) acc
  else sum.rec(n - 1, acc + n)
}
# > sum.rec(10)
# [1] 55
# > sum.rec(1e5)
# エラー：  protect()：プロテクションスタック
# > tco(sum.rec)(1e5)
# [1] 5000050000

# pow.rec <- function(x, n, acc = 1){
#   if(n == 0) acc
#   else if(n %% 2 == 0) pow.rec(x * x, n / 2, acc)
#   else pow.rec(x, n - 1, x * acc)
# }
# > tco(pow.rec)(2, 10)
# 以下にエラー setNames(g(), f.arg.names) : 
# 'names' 属性 [3] はベクトル [1] の長さと同じでなければなりません 
# > tco(pow.rec, var.ind = 2)(2, 10) # designate the second argument of tco.
# [1] 1024

lang2char <- function(expr, type = c("rexp", "sexp")){
  type <- match.arg(type)
  if(exists(".quote.if.not", envir = parent.env(environment()), mode="function"))
    expr <- .quote.if.not(expr)
  stopifnot(length(expr) > 1)
  
  r <- list(fst = function(x) as.character(list(x[[1]])), coll = ", ", print = function(first, rest) paste0(first, "(", rest, ")"))
  s <- list(fst = function(x) as.character(x[[1]]), coll = " ", print = function(first, rest) paste0("(", first, " ", rest, ")")) 
  . <- if(type == "rexp") r else s
  
  as.char <- function(k){
    rest <- paste0(lapply(k[-1], function(x) {if(length(x) == 1) as.character(x) else as.char(x)}) , collapse = .$coll)
    .$print(.$fst(k), rest)
  }
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
