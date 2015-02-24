# see https://gist.github.com/TobCap/6482462
tco <- function(f_, env_ = parent.frame()) {
  replace_symbol <- function(expr, before, after) {
    eval(substitute(substitute(e, `names<-`(list(after), as.character(before))), list(e = expr)))
  }
  
  body(f_) <- replace_symbol(body(f_), substitute(f_), quote(list))
  body(f_) <- replace_symbol(body(f_), quote(Recall), quote(list))
  
  f_args <- formals(args(f_))
  f_loop_body <-
    quote({
      f_args_len <- length(f_args)
      ans <- mget(names(f_args))
      while(is.list(ans) && length(ans) == f_args_len) {
        ans <- do.call(f_, ans)
      }
      ans
    })

  eval(call("function", f_args, f_loop_body), envir = environment(), enclos = env_)  
}

sum_rec <- function(n, acc = 0) {
  if (n == 0) acc
  else sum_rec(n - 1, acc + n)
}
# > sum_rec(10)
# [1] 55
# > sum_rec(1e5)
# Error: protect(): protection stack overflow
# > tco(sum_rec)(1e5)
# [1] 5000050000

pow_rec <- function(x, n, acc = 1) {
  if (n == 0) acc
  else pow_rec(x, n - 1, x * acc)
}
# > pow_rec(1+1e-5, 1e5)
# Error: protect(): protection stack overflow
# > tco(pow_rec)(1+1e-5, 1e5)
# [1] 2.718268

## trampoline
# need to convert tail calls to be wrapped by lambda (function)
# see examples at https://gist.github.com/TobCap/6332468

trampoline <- function(..., e = parent.frame()) {
  dots.quote <- as.list(substitute((...)))[-1]
  dots.len <- length(dots.quote)
  
  main.call <- function(f) {
    while(is.function(f)) {f <- f()}
    f
  }
  
  curried1 <- function(f) {
    function(...) {
      trampoline(do.call(f, list(...), envir = e))
    }
  }
  
  if (dots.len == 0) {
    stop("need to take arguments")
  } else if (dots.len == 1 && length(dots.quote[[1]]) == 1) {
    # trampoline(even2)(100)
    curried1(..1)
  } else if (dots.len == 1 && length(dots.quote[[1]]) > 1) {
    # trampoline(even2(100))
    main.call(..1)
  } else {
    # trampoline(even2, 100)
    do.call(curried1(..1), list(...)[-1], envir = e)
  } 
}

# even2 <- function(n) if (n == 0) TRUE else function() odd2(n - 1)
# odd2  <- function(n) if (n == 0) FALSE else function() even2(n - 1)
# trampoline(even2(1e4+1))
# trampoline(even2, 1e4+1)
# trampoline(even2)(1e4+1)

## CPS transformation with trampoline
# fibcps2 <- function(n, cps) {
#   if (n <= 1) cps(n)
#   else function() fibcps2(n - 1, function(x) fibcps2(n - 2, function(y) cps(x + y)))
# }
# trampoline(fibcps2(25, force))
# trampoline(fibcps2, 25, force)
# trampoline(fibcps2)(25, force)
