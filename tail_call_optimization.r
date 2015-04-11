## only a primitive recursive function can accept as an argumnet.
## so Ackerman function cannot be applied.
## http://en.wikipedia.org/wiki/Primitive_recursive_function

## fibonacci examples
## https://gist.github.com/TobCap/e5ab9e0c74b34346328d

tco <- function(f_, env_ = parent.frame()) {
  stopifnot(typeof(f_) == "closure")
  
  target_char <- as.character(c(substitute(f_), quote(Recall)))
  marked_name <- "tail_call_opt"
  
  mark_recursive_call <- function(expr) {
    if (length(expr) <= 1 && !is.call(expr))
      expr
    else if (as.character(expr[[1]]) %in% target_char)
      call("class<-",  as.call(c(quote(list), lapply(expr[-1], mark_recursive_call))), marked_name)
    else if (is.pairlist(expr))
      as.pairlist(lapply(expr, mark_recursive_call))
    else 
      as.call(lapply(expr, mark_recursive_call))
  }
  
  body(f_) <- mark_recursive_call(body(f_))
  f_args <- formals(args(f_))
  
  f_loop_body <-
    bquote({
      f_ <- .(f_)
      
      # initial value
      # ans <- `class<-`(mget(.(names(f_args))), .(marked_name))
      ans <- `class<-`(lapply(.(names(f_args)), get, envir = environment()), .(marked_name))
      while(inherits(ans, "tail_call_opt")) {
        # FIX-ME: need 'quote = TRUE' ?
        ans <- do.call(f_, ans, quote = TRUE)
      }
      ans
    })
    
  eval(call("function", f_args, f_loop_body), envir = environment(f_), enclos = baseenv())  
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



### trampoline
## need to convert tail calls to be wrapped by lambda (function)
## see examples at https://gist.github.com/TobCap/6332468
trampoline <- function(..., e = parent.frame()) {
  dots <- as.list(substitute((...)))[-1]
  dots_len <- length(dots)
  
  main_call <- function(f) {
    while(is.function(f)) {f <- f()}
    f
  }
  
  curried1 <- function(f) {
    function(...) {
      trampoline(do.call(f, list(...), envir = e))
    }
  }
  
  if (dots_len == 0) {
    stop("need to take arguments")
  } else if (dots_len == 1 && length(dots[[1]]) == 1) {
    # trampoline(even2)(100)
    curried1(..1)
  } else if (dots_len == 1 && length(dots[[1]]) > 1) {
    # trampoline(even2(100))
    main_call(..1)
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

