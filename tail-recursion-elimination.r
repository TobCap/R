## "Tail call" http://en.wikipedia.org/wiki/Tail_call
## tail-call-optimization is broader concept than tail-recursion-elimination.

## fibonacci examples
## https://gist.github.com/TobCap/e5ab9e0c74b34346328d

## only a primitive recursive function can accept as an argumnet.
## so Ackerman function cannot be applied.
## http://en.wikipedia.org/wiki/Primitive_recursive_function


## In do.call() version, the function passed to don't need to specify return() call in exit condition.
tre <- function(f_) {
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
# > tre(sum_rec)(1e5)
# [1] 5000050000

pow_rec <- function(x, n, acc = 1) {
  if (n == 0) acc
  else pow_rec(x, n - 1, x * acc)
}
# > pow_rec(1+1e-5, 1e5)
# Error: protect(): protection stack overflow
# > tre(pow_rec)(1+1e-5, 1e5)
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

## You need to specify `return()` in tail call of if-condition for loop version.
tail_recursive_elimination <- function(f_) {
  if (!typeof(f_) == "closure")
    stop("only closure is acceptable")
  if (!any(all.names(body(f_)) == "return"))
    stop("return() must be included inside body of f_()")
      
  target_char <- as.character(c(substitute(f_), quote(Recall)))
  f_args <- formals(args(f_))
  f_args_len <- length(f_args)
  f_args_name <- names(f_args)
  f_args_sym_named <- setNames(lapply(f_args_name, as.symbol), f_args_name)
  
  gensyms(0) # initalize for debugging
  
  make_simple_loop <- function(actuals) {
    new_syms <- gensyms(f_args_len)    
    new_call <- mapply(
      function(actual, formal_sym, new_sym) {
        list(
          call("<-", new_sym, formal_sym),
          call("<-", formal_sym, substituteDirect(actual, setNames(new_syms, f_args_name)))
        )
      }
      , actuals, f_args_sym_named, new_syms
      , SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    odd <- seq.int(length.out = f_args_len, by = 2)
    even <- odd + 1L
    as.call(c(quote(`{`), unlist(new_call)[c(odd, even)]))
  }

  make_cps_loop <- function(actuals) {
    dotted_call <- lapply(f_args_sym_named, function(x) call(".", x))
    lst1 <- as.call(c(quote(list), f_args_sym_named))
    cal1 <- mapply(
      function(actual, formal_sym) {
        call("<-", formal_sym, substituteDirect(actual, dotted_call))
      }, actuals, f_args_sym_named, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    call("{", call("eval", call("bquote", as.call(c(quote(`{`), cal1)), lst1))
    )
  }
  
  iter <- function(x, translate) {
    iter2 <- function(x) {
      if (length(x) <= 1 && !is.list(x)) x
      else if (length(x) > 1 && any(x[[1]] == target_char)) translate(as.list(x)[-1])
      else if (is.pairlist(x)) as.pairlist(lapply(x, iter2))
      else as.call(lapply(x, iter2))
    }
    iter2(x)
  }
  
  loop_inner <- list(
    loop_simple = iter(body(f_), make_simple_loop), 
    loop_macro = iter(body(f_), make_cps_loop))
  
  f_loop_body <- bquote({
    has_func <- any(unlist(eapply(environment(), is.function, all.names = TRUE, USE.NAMES = FALSE)))
    if (has_func) repeat .(loop_macro)
    else repeat .(loop_simple)
  }, loop_inner)
  eval(call("function", f_args, f_loop_body), envir = environment(f_))
}

gensyms <- (function() {
  base_name <- "#:G"
  num <- 0
  
  function(n, allow_overlapping = FALSE){
    if (missing(n) || n < 0) 
      stop("n must be greater than one")
      
    if (n == 0) {
      num <<- 0
      return(invisible())
    }

    current_num <- num
    if (!allow_overlapping) num <<- num + n
    
    lapply(paste0(base_name, seq_len(n) + current_num), as.symbol)
  }
})()

sum_rec_ret <- function(n, acc = 0) {
  if (n == 0) return(acc)
  else sum_rec_ret(n - 1, acc + n)
}
 
## error
# > sum_rec_ret(1e5)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
 
## run
# > s1 <- tail_recursive_elimination(sum_rec_ret)
# > s1(1e5)
# [1] 5000050000 
 
sum_cps <- function(n, k) {
  if (n == 0) return(k(0))
  else sum_cps(n - 1, function(x) k(n + x))
}
 
# > s2 <- tail_recursive_elimination(sum_cps)
# > s2(1e3, identity)
# [1] 500500
 
## check what has happened
# sum_cps2 <- function(n, k) {
#   if (n == 0) {dput(k);return(k(0))}
#   else sum_cps2(n - 1, function(x) k(n + x))
# }
# tail_recursive_elimination(sum_cps2)(10, identity)
 
# function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# (function (x) 
# x)(10 + x))(9 + x))(8 + x))(7 + x))(6 + x))(5 + x))(4 + x))(3 + 
#     x))(2 + x))(1 + x)
# [1] 55
