# see https://gist.github.com/TobCap/6482462
tco <- function(f_) {
  replace.symbol <- function(expr, before, after){
    eval(substitute(substitute(e, `names<-`(list(after), as.character(before))), list(e = expr)))
  }
  # f_ is copied object; the original f_ is not changed.
  f.symbol <- substitute(f_)  
  body(f_) <- replace.symbol(body(f_), f.symbol, quote(list))
  body(f_) <- replace.symbol(body(f_), quote(Recall), quote(list))
  arg.names <- names(formals(f_))

  out.fun <- function(){
    environment(f_) <- e <- list2env(mget(arg.names), parent = environment(f_))
    while(TRUE){
      ans <- f_() # evaluate
      if (!is.list(ans) || length(ans) != length(arg.names)) break
      e <- list2env(`names<-`(ans, arg.names), envir = e) #update values
    }
    ans
  }
  formals(out.fun) <- formals(f_)
  formals(f_) <- NULL # let "f" find variables in "e".
  out.fun
}

sum.rec <- function(n, acc = 0){
  if (n == 0) acc
  else sum.rec(n - 1, acc + n)
}
# > sum.rec(10)
# [1] 55
# > sum.rec(1e5)
# Error: protect(): protection stack overflow
# > tco(sum.rec)(1e5)
# [1] 5000050000

# pow.rec <- function(x, n, acc = 1){
#   if(n == 0) acc
#   else pow.rec(x, n - 1, x * acc)
# }
# > pow.rec(1+1e-5, 1e5)
# Error: protect(): protection stack overflow
# > tco(pow.rec)(1+1e-5, 1e5)
# [1] 2.718268
