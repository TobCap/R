match.with <- function(...) {
  dots <- as.vector(substitute((...)), "list")[-1]
  conds <- dots[-1]
  p.frame <- parent.frame()
  expr.raw <- dots[[1]]
  expr.value <- eval(expr.raw, p.frame)
  expr.name <- names(dots[1])
  #expr <- parse(text = deparse(expr.value))[[1]]
  delayedAssign("expr", parse(text = deparse(expr.value))[[1]])
 
  wildcards <- c(quote(.), quote(`_`))
  wildcards.char <- lapply(wildcards, as.character)
 
  equals.recursive <- function(c1, c2, wildcard = NULL, strict.int.dbl= FALSE) {
    if (!is.null(wildcard) && !is.language(c2))
      stop("when using a wildcard, c2 must be a call or a symbol")
 
    # coerce characters into symbols
    if (!is.null(wildcard) && is.character(wildcard))
      wildcard <- lapply(wildcard, as.symbol)
 
    # shortcut
    if (is.null(wildcard) && strict.int.dbl)
      return(identical(c1, c2))
    if (is.null(wildcard) && !strict.int.dbl &&
        base::xor(is.numeric(c1), is.numeric(c2)))
      return(FALSE)
 
    # c2 may have wildcard symbol
    out.fun <- function(c1, c2) {
      ## When doing `==`, symbols are coerced into character by
      ## deparse() in C lang level (See R source's relop.c#80-81)
      ## and comparison between a symbol and list of symbols may 
      ## mistake in a special situation: quote(`_`) == list(quote(`_`), "_")
      ## The result in above is c(FALSE, TRUE).
      if (!is.null(wildcard) && is.symbol(c2)
        && (as.character(c2) %in% lapply(wildcard, as.character))) TRUE
      else if (length(c1) != length(c2)) FALSE
      else if (!is.recursive(c1) || !is.recursive(c2) || length(c1) == 0) {
        # compare a pair of elements here
        # for 1L == 1.0
        if (!strict.int.dbl && is.numeric(c1) && is.numeric(c2)) c1 == c2 
        else identical(c1, c2)}
      else {
        for(i in seq_along(c1)) {
          if (!out.fun(c1[[i]], c2[[i]])) return(FALSE)}
        return(TRUE)}
    }
    out.fun(c1, c2)
  }
 
  for(i in seq_along(conds)) {
    dot <- conds[[i]]
    if (missing(dot))
      stop("need to remove the last comma")
    if (dot[[1]] != quote(`<-`))
      stop("use `->` as converter")
 
    cond <- dot[[3]]
    is.just.an.atomic <- is.atomic(cond)
    cond.vars <-
      if (is.just.an.atomic) character(0)
      else all.vars(cond)
    is.wildcard <-
      if (is.just.an.atomic) FALSE
      else length(cond) == 1 && cond.vars %in% wildcards.char
    if (is.wildcard && i != length(conds))
      stop("wildcard must be last part of arguments")
    has.wildcard <- any(wildcards.char %in% cond.vars)
    has.double.colon <- length(cond) == 3 && quote(`::`) == cond[[1]]
    is.call.of.sym <- is.symbol(expr.raw) && 
      (as.character(expr.raw) %in% cond.vars) && !has.wildcard
    is.call.of.name <- !is.null(expr.name) &&
      (expr.name %in% cond.vars) && !has.wildcard
 
    evaled.list <-
      if (has.double.colon) {
        hd <- expr.value[[1]]
        tl <- if (length(expr.value[-1]) == 0) NULL else expr.value[-1] #as.pairlist(expr.value[-1]) # coerce list() to NULL
        `names<-`(list(hd, tl), c(as.character(cond[[2]]), as.character(cond[[3]])))}
      else if (is.call.of.name) {
        `names<-`(list(expr.value), c(expr.name))}
      else NULL
 
    ans <-
      if (is.atomic(ans.expr <- dot[[2]])) ans.expr
      else eval(ans.expr, envir = evaled.list, enclos = p.frame)
 
    ##
    if (is.wildcard || has.double.colon) {
      return(ans)
    } else if (is.call.of.sym) {
      # like is.null(x) or x %% 2 == 0
      if (isTRUE(eval(cond, NULL, p.frame))) return(ans)
    } else if (is.call.of.name) {
      if (isTRUE(eval(cond, evaled.list, p.frame))) return(ans)
    } else if (is.just.an.atomic) {
      if (equals.recursive(expr.value, cond)) return(ans)
    } else if (has.wildcard) {
      if (equals.recursive(expr, cond, wildcard = wildcards)) return(ans)
    } else {
      # `cond` is non-dot expression like `list(1,2)`
      if (equals.recursive(expr, cond)) return(ans)
    }
  }
  stop("The input is non-matched pattern. Need to write proper 
        yntax or set default wildcard `.` at last.")
}

foldr <- function(f, init, lst) {
  match.with(lst
    , NULL -> init
    , x::xs -> f(x, foldr(f, init, xs))
  )
}

foldl <- function(f, init, lst){
  match.with(lst
    , NULL -> init
    , x::xs -> foldl(f, f(init, x), xs)
 )
}

local({
   cat("foldr(`+`, 0, 1:10)", foldr(`+`, 0, 1:10), "\n")
   cat("foldr(`-`, 0, 1:10)", foldr(`-`, 0, 1:10), "\n")
   cat("foldl(`+`, 0, 1:10)", foldl(`+`, 0, 1:10), "\n")
   cat("foldl(`-`, 0, 1:10)", foldl(`-`, 0, 1:10), "\n")
 })

local({
  fizzbuzz <- function(z){
    match.with(list(z %% 5, z %% 3),
      list(0, 0) -> "FizzBuzz",
      list(0, .) -> "Fizz",
      list(., 0) -> "Buzz",
      . -> as.character(z)
    )
  }
 
  cat("sapply(1:30, fizzbuzz)", sapply(1:30, fizzbuzz), "\n")
 
  # you can write as below. Does it look like F# or Ocamel?
  fizzbuzz2 <- function(z){
    match.with(list(z %% 5, z %% 3)
    , list(0, 0) -> "FizzBuzz"
    , list(0, `_`) -> "Fizz"
    , list(`_`, 0) -> "Buzz"
    , `_` -> as.character(z)
    )
  }
  cat("sapply(1:30, fizzbuzz2)", sapply(1:30, fizzbuzz2), "\n")
})
 
# left expression is evaluated in parent.frame().
local({
  evenodd <- function(x){
    match.with(x,
      x %% 2 == 0 -> "even",
      x %% 2 != 0 -> "odd"
    )
  }
  sapply(1:10, evenodd)
})
 
# `::` is recognized as separater of head and tail
local({
  len <- function(n){
    match.with(n,
      NULL  -> 0, # is.null(n) -> 0,
      x::xs -> 1 + len(xs)
    )
  }
  cat(paste(len(list(1,2,3)), "\n"))
  cat(paste(len(1:5), "\n"))
})
 
# "." is treated as a woldcard symbol.
local({
 two.or.four <- function(x){
  match.with(x,
      2 -> "two",
      4 -> "four",
      . -> "_"
    )
  }
  sapply(1:10, two.or.four)
})
 
local({
   f3 <- function(x){
     match.with(x,
       list(0, 0) -> "0-0",
       list(1, 0) -> "1-0",
       list(0, .) -> "0-?",
       list(., 3) -> "?-3",
       list(., .) -> "?-?",
       . -> "others")}
   paste(
    f3(list(0, 0)), 
    f3(list(1, 0)),
    f3(list(0, 1)),
    f3(list(1, 3)),
    f3(list(1, 1)),
    f3(list(1, 2, 3)))
 })
 
local({
 z <- list(10,11,12)
 match.with(z,
   length(z) == 1 -> "one",
   length(z) == 2 -> "two",
   length(z) >= 3 -> "very long",
   . -> "._."
 )
})
## or
local({
 z <- list(10,11,12)
 match.with(x = length(z),
   1 -> "one",
   2 -> "two",
   x >= 3 -> "very long",
   . -> "._."
 )
})
 
local({
 z <- 5:11
 match.with(z,
   .:10 -> "end with 10",
   .:11 -> "end with 11",
   .:12 -> "end with 12",
   .    -> "other"
 )
})
 
local({
 z <- 5:11
 match.with(z,
   .:10 -> "end with 10",
   .:11 -> {x <- tail(z, 1) * 2; x},
   .:12 -> "end with 12",
   .    -> "other"
 )
})
 
# recursive call works! but speed is impractical.
local({
  fib.m <- function(n){
    match.with(n,
      0 -> 0,
      1 -> 1,
      . -> fib.m(n - 1) + fib.m(n - 2)
    )
  }
  sapply(1:10, fib.m)
})
 
local({
  fib.m2 <- function(n, a = 0, b = 1){
    match.with(n,
      0 -> a,
      . -> fib.m2(n - 1, b, a + b)
    )
  }
  sapply(1:10, fib.m2)
})
 
local({
  sum.m <- function(n){
   match.with(n,
     0 -> 0,
     . -> n + sum.m(n - 1)
   )
  }
  sapply(1:10, sum.m)
})
 
local({
  sum.m2 <- function(n, acc = 0){
    match.with(n,
      0 -> acc,
      . -> sum.m2(n - 1, acc + n)
    )
  }
  sapply(1:10, sum.m2)
})
 
local({
  sum.vec <- function(n.vec){
    match.with(n.vec,
      NULL  -> 0,
      x::xs -> x + sum.vec(xs)
    )
  }
  sapply(lapply(1:10, seq_len), sum.vec)
})
 
