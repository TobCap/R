### functional operators

### the reason why I define `f.` instead of `f` is to avoid the situation where many sample codes use `f` that causes conflict.
f. <- function(..., env = parent.frame()){
  d <- lapply(substitute(list(...)), identity)[-1]
  as.function(c(tools:::as.alist.call(d[-length(d)]), d[length(d)]), envir = env) 
}
### the same as Reduce(function(x, y) x + y, 1:10)
# > Reduce(f.(x, y, x + y), 1:10)
# [1] 55

# > f.(y, f.(z, y+z))(1)(2)
# [1] 3

λ <- l. <- function(...) curry(f.(..., env = parent.frame()))
### lambda's Unicode address is U+03BB or \u03BB
### http://d.hatena.ne.jp/mickey24/20110119/1295432161

# λ(g, x, g(g(x)))(λ(y, y+1))(5) # run
# f.(g, f.(x, g(g(x))))(f.(y, y+1))(5) # run
# l.(g, l.(x, g(g(x))))(l.(y, y+1))(5)
# l.(g, x, g(g(x)))(l.(y, y+1))(5)

### http://www.angelfire.com/tx4/cus/combinator/birds.html
# S <- f.(x, f.(y, f.(z, (x(z))(y(z)) )))
# K <- f.(x, f.(y, x))
# I <- S(K)(K) # f.(x, x) 

curry <-  function(fun, env = parent.frame()) {
  recursiveCall <- function(len, arg){
    if (len == 0) return(do.call(fun, arg, envir = env))
    function(x) recursiveCall(len - 1, append(arg, list(x)))
  }
  recursiveCall(length(formals(fun)), list())
}
# > curry(function(x) function(y) function(z) x + y + z)(1)(2)(3)
# [1] 6
### `f.` is already defined above and I love using it.
# > curry(f.(x, y, z, x + y + z))(1)(2)(3)
# [1] 6

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
flip <- function(f){
  function(l, r) match.fun(f)(r, l)
}

### http://neue.cc/2011/02/14_302.html
"%|>%" <- function(x, f) f(x)
"%<|%" <- function(f, x) f(x)

### functional composition
"%>>%" <- function(f, g) function(x) g(f(x))
"%<<%" <- function(f, g) function(x) f(g(x))
### same as "%<<%" <- flip(`%>>%`)

# f1 <- f.(x, {cat("f1(x = ", x, ") -> ", sep = ""); x + 1})
# f2 <- f.(x, {cat("f2(x = ", x, ") -> ", sep = ""); x * 2})
# f3 <- f.(x, {cat("f3(x = ", x, ") -> ", sep = ""); x / 3})
# f4 <- f1 %>>% f2 %>>% f3
# f5 <- f1 %<<% f2 %<<% f3

# > f4(1)
# f1(x = 1) -> f2(x = 2) -> f3(x = 4) -> [1] 1.333333
# > f5(1)
# f3(x = 1) -> f2(x = 0.3333333) -> f1(x = 0.6666667) -> [1] 1.666667

### chain methods
### I refer to the code of help("Reduce")
`-->` <- function(...){
    Funcall <- function(f, ...) f(...)
    function(x) Reduce(Funcall, list(...), x, right = TRUE)
}
# > `-->`(cos, log, sin)(pi/2)
# [1] 1
# > (pi/2) %|% sin %|% log %|% cos
# [1] 1
# > cos %<<% log %<<% sin %<|% (pi/2)
# [1] 1
# > sin %>>% log %>>% cos %<|% (pi/2)
# [1] 1
# > cos(log(sin(pi/2)))
# [1] 1

### pipeline like operator
### left value can be passed by ".." just like scala's underbar "_".
### "." is already used in "package:plyr" so I use "..".
"%|%" <- function(lhs, rhs){
  ans <- eval(substitute(rhs), envir = list(.. = lhs), enclos = parent.frame())  
  if (is.function(ans))
    ans(lhs)  
  else if (all(typeof(ans)=="logical", length(lhs)==length(ans)))
    lhs[ans]
  else
    ans
}

### the same
# Filter(function(x) x%%2==0, 1:5)
# 1:5 %|% (..%%2==0)

# > 1:10 %|% (..%%2==0) %|% ..^2
# [1]   4  16  36  64 100

### plot brownian motion in one liner
# n<-1000; m<-252; mu<-0; sigma <- 0.2/sqrt(m); {f.(rnorm(m, mu-sigma^2/2, sigma) %|% cumsum %|% c(0, ..))} %|%  replicate(n, ..()) %|% matplot(exp(..), type = "l", ann = FALSE) 

# > f.({mu<-0;m<-252;sigma<-0.2/sqrt(m) ;rnorm(m, mu-sigma^2/2, sigma) %|% cumsum %|% c(0, ..)}) %|% replicate(1000, ..()) %|% matplot(exp(..), type = "l", ann = FALSE)



### fixed-point combinator
### I tried to check four types of fixed-point combinator function and blow is the simplest and fastest
fix. <- function(g)	{f <- g(f)}

### http://en.wikipedia.org/wiki/Fixed-point_combinator

# fix0 <- function(g) g(fix0(g))
# fix1 <- function(g)	f <- g(f)
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

### http://d.hatena.ne.jp/r-west/20090422/1240400570
gen.memoizer <- function (use.global = FALSE) {
  if(!isTRUE(use.global) & !is.null(use.global)) {
    .memo <- new.env()
  } else if(isTRUE(use.global) & exists(".memo", envir = .GlobalEnv)) {
    .memo <- get(".memo", envir = .GlobalEnv)
  } else {
    .memo <- assign(".memo", new.env(), envir = .GlobalEnv)
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
fib_maker <- function(f) {
  function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
}
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

### all the same 
# fix.(gen.tracer() %>>% gen.memoizer() %>>% fib_maker)(5)
# (gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.)(5)
# {gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.}(5)
# 5 %|>% {gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix.}
# gen.tracer() %>>% gen.memoizer() %>>% fib_maker %|>% fix. %<|% 5

### gen.memoizer(TRUE) use `.memo` in .GlobalEnv and set the results.
### gen.memoizer(NULL) reset and creates new `.memo` in .GlobalEnv.

# > invisible(gen.memoizer(NULL)) # reset
# > ls(.memo)
# character(0)

# > fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(5)
# 1st call with argument: 4
# 2nd call with argument: 3
# 3rd call with argument: 2
# 4th call with argument: 1
# 5th call with argument: 0
# [1] 5

### the result is memoised.
# > fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(5)
# [1] 5

### only new arguments are passed to calculation
# > fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(7)
# 1st call with argument: 6
# 2nd call with argument: 5
# [1] 13

# > ls(.memo)
# [1] "fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(),f(),g()"
# > ls(.memo$`fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(),f(),g()`)
# [1] "0" "1" "2" "3" "4" "5" "6"
# > .memo$`fix.(gen.tracer() %>>% gen.memoizer(TRUE) %>>% fib_maker)(),f(),g()`$`6`
# [1] 8

# fib <- gen.memoizer(TRUE)(function(x) if(x<=1) x else fib(x-1) + fib(x-2))
# > fib(1000)
# [1] 4.346656e+208
# > ls(.memo)
# install.packages("gmp"); library("gmp")
# > fib2 <- gen.memoizer(TRUE)(function(x) if(x<=1) as.bigz(x) else fib2(x-1) + fib2(x-2))
# > fib2(1000)
# Big Integer ('bigz') :
# [1] 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875

### Firstly, I refered to http://d.hatena.ne.jp/einblicker/20110108/1294448477
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

# cnv(quote(x+y^z), quote(z), quote(m))
cnv <- function(expr, before, after){
  stopifnot(is.language(expr), length(before) == 1)
	conv <- function(x){
        if (is.pairlist(x)) {
          # names are not converted except formals' pairlist
          ind <- match(as.character(before), names(x))
          names(x)[ind] <- as.character(after)
          as.pairlist(lapply(x, conv))
        } else if (length(x) == 1) {
          if(x == before) after else x
				} else as.call(lapply(x, conv))
			}
  conv(expr)    
}  
# > cnv(quote(1+2+x^3), quote(x), quote(y))
# 1 + 2 + y^3
# > cnv(quote(1+2+x^3), quote(2), quote(99))
# 1 + 99 + x^3
# > cnv(quote(1+2+x^3) , quote(`+`), quote(`-`))
# 1 - 2 - x^3
# > eval(cnv(quote(1+2+x^3) , quote(`+`), quote(`-`)), list(x = 2))
# [1] -9

nest.formula <- function(expr, variable, num){
  if(num == 1) return(expr)
  cnv(nest.formula(expr, variable, num - 1), variable, expr)
}
# > nest.formula(quote(sqrt(1+x^2)), quote(x), 3)
# sqrt(1 + sqrt(1 + sqrt(1 + x^2)^2)^2)
# > eval(nest.formula(quote(1+1/x), quote(x), 40), list(x = 1)) == (1+sqrt(5))/2
# [1] TRUE

### tail recursive
nest.fun <- function(f, n, acc.fun = identity){
  f2 <- function(g, h) function(x) g(h(x))
  if (n == 0) return(acc.fun)
  if (n %% 2 == 0) nest.fun(f2(f, f), n / 2, acc.fun)
  else nest.fun(f, n - 1, f2(f, acc.fun))
}

# > nest.fun(f.(x, 1 + 1/x), 100)(1) == (1+sqrt(5))/2
# [1] TRUE

### checking arguments using gen.tracer() function.
# > nest.fun(gen.tracer()(f.(x, 1 + 1/x)), 3)(1)
# 1st call with argument: 1
# 2nd call with argument: 2
# 3rd call with argument: 1.5
# [1] 1.666667

# > nest.fun(f.(x, 1 + 1/x) %|>% gen.tracer() %|>% gen.memoizer(TRUE), 3)(1)
# 1st call with argument: 1
# 2nd call with argument: 2
# 3rd call with argument: 1.5
# [1] 1.666667
# > nest.fun(f.(x, 1 + 1/x) %|>% gen.tracer() %|>% gen.memoizer(TRUE), 3)(1)
# [1] 1.666667
# > 


# tail call optimization, not completely tested
tco <- function(f, var.ind = 1, out.ind = length(formals(f)), stop.num = 0){
  f.arg.names <- names(formals(f))
  g <- function(){}
  body(g) <- cnv(body(f), as.list(match.call())[["f"]], quote(list))
  is.first <- TRUE

  out.fun <- function(){
    while(TRUE){
      if(is.first) {environment(g) <- environment(); is.first <<- FALSE}
      if(as.list(environment(g))[[ f.arg.names[[var.ind]] ]] == stop.num) break
      environment(g) <- list2env(stats:::setNames(g(), f.arg.names))
    }
    return(environment(g)[[f.arg.names[[out.ind]]]])
  }
  formals(out.fun) <- formals(f)
  return(out.fun)
}
# sum.rec <- function(n, acc = 0){
# if(n == 0) acc
# else sum.rec(n - 1, acc + n)
# }
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
# > tco(pow.rec, var.ind = 2)(2, 10)
# [1] 1024

lang2char  <- function(expr, type = c("rexp", "sexp")){
  type <- match.arg(type)
  f <- if(type == "rexp") `[` else `[[`
  coll <- if(type == "rexp") ", " else  " "
  pst <- if(type == "rexp") function(first, rest) paste0(first, "(", rest, ")")
         else function(first, rest) paste0("(", first, " ", rest, ")")

  as.char <- function(k){
    first <- as.character(f(k, 1))
    rest <- paste(
      lapply(k[-1], function(x) if(is.list(x)) as.char(x) else as.character(x))
      , collapse = coll)
    if(nchar(rest) > 0) pst(first, rest) else first
  }
  c2l <- function(x){
    call2list <- function(e) {
      if(length(e) == 1) return(e) 
      else lapply(e, call2list)
    }
    if(!typeof(x) == "list") call2list(x) else x
  }
  as.char(c2l(expr))
}

# > lang2char(quote(x+sin(y*z)))
# [1] "`+`(x, sin(`*`(y, z)))"
### useful to interpretate by lisp
# > lang2char(quote(x+sin(y*z)), "s")
# [1] "(+ x (sin (* y z)))"
# > txt <- lang2char(quote(x+sin(y*z)))
# > eval(parse(text = txt), list(x=1,y=2,z=3))
# [1] 0.7205845
# > eval(quote(x+sin(y*z)), list(x=1,y=2,z=3))
# [1] 0.7205845

