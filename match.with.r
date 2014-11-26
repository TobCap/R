match.with <- function(...) {
  dots <- as.vector(substitute((...)), "list")[-1]
  conds <- dots[-1]
  parent_frame <- parent.frame()
  expr <- dots[[1]]
  expr_value <- eval(expr, parent_frame)
  expr_name <- names(dots[1])
  #expr_value_deparse <- parse(text = deparse(expr_value))[[1]]
  delayedAssign("expr_value_deparse", parse(text = deparse(expr_value))[[1]])

  wildcards <- c(quote(.), quote(`_`), quote(otherwise))
  wildcards_char <- sapply(wildcards, as.character)

  equals_recursive <- function(c1, c2, wildcard = NULL, strict_int_dbl= FALSE) {
    if (!is.null(wildcard) && !is.language(c2))
      stop("when using a wildcard, c2 must be a call or a symbol")

    # coerce characters into symbols
    if (!is.null(wildcard) && any(is.character(wildcard)))
      wildcard <- lapply(wildcard, as.symbol)

    # shortcut without a wildcard
    if (is.null(wildcard) && strict_int_dbl)
      return(identical(c1, c2))
    if (is.null(wildcard) && !strict_int_dbl && base::xor(is.numeric(c1), is.numeric(c2)))
      return(FALSE)

    # c2 may have wildcard symbol
    out_fun <- function(c1, c2) {
      ## When doing `==`, symbols are coerced into character by
      ## deparse() in C lang level (See R source's relop.c#80-81)
      ## and comparison between a symbol and list of symbols may
      ## mistake in a special situation: quote(`_`) == list(quote(`_`), "_")
      ## The result in above is c(FALSE, TRUE).
      c2_is_wildcard <-
        !is.null(wildcard) && is.symbol(c2) &&
        (as.character(c2) %in% lapply(wildcard, as.character))

      if (c2_is_wildcard) TRUE
      else if (length(c1) != length(c2)) FALSE
      else if (!is.recursive(c1) || !is.recursive(c2) || length(c1) == 0) {
        # compare a pair of elements here
        # for 1L == 1.0
        if (!strict_int_dbl && is.numeric(c1) && is.numeric(c2)) c1 == c2
        else identical(c1, c2) }
      else {
        for(i in seq_along(c1)) {
          if (!out_fun(c1[[i]], c2[[i]])) return(FALSE)}
        return(TRUE)}
    }
    out_fun(c1, c2)
  }

  for(i in seq_along(conds)) {
    statement <- conds[[i]]

    if (missing(statement))
      stop("need to remove the last comma")

    if (statement[[1]] != quote(`<-`))
      stop("use `->` as converter")

    cond <- statement[[3]]
    cond_is_atomic <- is.atomic(cond)
    cond_vars <- if (cond_is_atomic) "" else all.vars(cond)
    is_wildcard <- length(cond) == 1 && any(wildcards_char %in% cond_vars)

    if (is_wildcard && i != length(conds)) {
      stop("wildcard must be last part of arguments") }

    has_wildcard_in_expr <- any(wildcards_char %in% cond_vars)
    has_double_colon <- length(cond) == 3 && quote(`::`) == cond[[1]]

    simbol_is_referred <- is.symbol(expr) &&
      (as.character(expr) %in% cond_vars) && !has_wildcard_in_expr

    name_is_referred <- !is.null(expr_name) &&
      (expr_name %in% cond_vars) && !has_wildcard_in_expr

    evaled.list <-
      if (has_double_colon) {
        hd <- expr_value[[1]]
        tl <- if (length(expr_value[-1]) == 0) NULL else expr_value[-1]
        `names<-`(list(hd, tl), c(as.character(cond[[2]]), as.character(cond[[3]]))) }
      else if (name_is_referred) {
        `names<-`(list(expr_value), c(expr_name)) }
      else NULL

    ans <- eval(statement[[2]], envir = evaled.list, enclos = parent_frame)

    #
    if (is_wildcard || has_double_colon) {
      return(ans) } # `wildcard` or x::xs
    else if ((simbol_is_referred || name_is_referred) && eval(cond, evaled.list, parent_frame)) {
      return(ans) } # is.null(x) or x %% 2 == 0
    else if (cond_is_atomic && equals_recursive(expr_value, cond)) {
      return(ans) } # NULL or 1
    else if (!cond_is_atomic && has_wildcard_in_expr && equals_recursive(expr_value_deparse, cond, wildcard = wildcards)) {
      return(ans) } # list(1,.)
    else if (!cond_is_atomic && equals_recursive(expr_value_deparse, cond)){
      return(ans) } # list(1,2)
    else {
      # not matched in this loop
    }
  }
  stop("The input is non-matched pattern. Need to write proper
          syntax or set default wildcard `.` at last.")
}

foldr <- function(f, init, lst) {
  match.with(lst
    , NULL  -> init
    , x::xs -> f(x, foldr(f, init, xs))
  )
}

foldl <- function(f, init, lst){
  match.with(lst
    , NULL  -> init
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
    match.with(list(z %% 5, z %% 3)
    , list(0, 0) -> "FizzBuzz"
    , list(0, .) -> "Fizz"
    , list(., 0) -> "Buzz"
    , otherwise  -> as.character(z)
    )
  }
  cat("sapply(1:30, fizzbuzz)", sapply(1:30, fizzbuzz), "\n")
})

# left expression is evaluated in parent.frame().
local({
  evenodd <- function(x){
    match.with(x
      , x %% 2 == 0 -> "even"
      , x %% 2 != 0 -> "odd"
    )
  }
  sapply(1:10, evenodd)
})

# `::` is recognized as separater of head and tail
local({
  len <- function(n){
    match.with(n
      , NULL  -> 0 # is.null(n) -> 0,
      , x::xs -> 1 + len(xs)
    )
  }
  cat(paste(len(list(1,2,3)), "\n"))
  cat(paste(len(1:5), "\n"))
})

# "." is treated as a woldcard symbol.
local({
 two.or.four <- function(x){
  match.with(x
    , 2 -> "two"
    , 4 -> "four"
    , . -> "_"
    )
  }
  sapply(1:10, two.or.four)
})

local({
   f3 <- function(x){
     match.with(x
      , list(0, 0) -> "0-0"
      , list(1, 0) -> "1-0"
      , list(0, .) -> "0-?"
      , list(., 3) -> "?-3"
      , list(., .) -> "?-?"
      , otherwise  -> "others") }

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
  match.with(z
    , length(z) == 1 -> "one"
    , length(z) == 2 -> "two"
    , length(z) >= 3 -> "very long"
    , . -> "_"
  )
})
## or
local({
  z <- list(10,11,12)
  match.with(x = length(z)
    , 1 -> "one"
    , 2 -> "two"
    , x >= 3 -> "very long"
    , . -> "._."
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
  f <- function(x) {
    match.with(x
    , list() -> 0
    , list(.) -> 1
    , list(., .) -> 2
    , list(., ., .) -> 3
    , . -> length(x)
    )
  }
  cat(f(list(2,2)), "\n")
  cat(f(list(1,2,3,4,5)), "\n")
})

local({
  z <- 5:11
  match.with(z
    , .:10 -> "end with 10"
    , .:11 -> {x <- tail(z, 1) * 2; x}
    , .:12 -> "end with 12"
    , .    -> "other"
  )
})

# recursive call works! but speed is impractical.
local({
  fib.m <- function(n){
    match.with(n
      , 0 -> 0
      , 1 -> 1
      , . -> fib.m(n - 1) + fib.m(n - 2)
    )
  }
  sapply(1:10, fib.m)
})

local({
  fib.m2 <- function(n, a = 0, b = 1){
    match.with(n
      , 0 -> a
      , . -> fib.m2(n - 1, b, a + b)
    )
  }
  sapply(1:10, fib.m2)
})

local({
  sum.m <- function(n){
    match.with(n
    , 0 -> 0
    , . -> n + sum.m(n - 1)
   )
  }
  sapply(1:10, sum.m)
})

local({
  sum.m2 <- function(n, acc = 0){
    match.with(n
      , 0 -> acc
      , . -> sum.m2(n - 1, acc + n)
    )
  }
  sapply(1:10, sum.m2)
})

local({
  sum.vec <- function(n.vec){
    match.with(n.vec
      , NULL  -> 0
      , x::xs -> x + sum.vec(xs)
    )
  }
  sapply(lapply(1:10, seq_len), sum.vec)
})
