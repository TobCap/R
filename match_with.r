match.with <- function(...) {
  dots <- as.vector(substitute((...)), "list")[-1]
  conds <- dots[-1]
  parent_frame <- parent.frame()
  expr <- dots[[1]]
  expr_value <- eval(expr, parent_frame)
  expr_name <- names(dots[1])
  # expr_value_deparse <- parse(text = deparse(expr_value))[[1]]
  delayedAssign("expr_value_deparse", parse(text = deparse(expr_value))[[1]])
  
  wildcards <- c(quote(.), quote(`_`), quote(otherwise))
  wildcards_char <- lapply(wildcards, as.character)

  equals_recursive <- function(c1, c2, wildcard = NULL, strict_int_dbl= FALSE) {
    if (!is.null(wildcard) && !is.language(c2)) {
      stop("when using a wildcard, c2 must be a call or a symbol") }

    # coerce characters into symbols
    if (!is.null(wildcard) && any(is.character(wildcard))) {
      wildcard <- lapply(wildcard, as.symbol) }

    # shortcut without a wildcard
    if (is.null(wildcard) && strict_int_dbl) {
      return(identical(c1, c2)) }
    if (is.null(wildcard) && !strict_int_dbl && xor(is.numeric(c1), is.numeric(c2))) {
      return(FALSE) }

    # c2 may have wildcard symbol
    out_fun <- function(c1, c2) {
      ## When using `==`, symbols are coerced into character by
      ## deparse() in C lang level (See R source's relop.c#80-81)
      ## and comparison between a symbol and list of symbols may
      ## mistake in a special situation: quote(`_`) == list(quote(`_`), "_")
      ## The result in above is c(FALSE, TRUE).
      c2_is_wildcard <- 
        !is.null(wildcard) && is.symbol(c2) &&
        (as.character(c2) %in% wildcards_char)

      if (c2_is_wildcard) TRUE
      else if (length(c1) != length(c2)) FALSE
      else if (length(c1) == 0) identical(c1, c2)
      else if (length(c1) == 1) {
        if (!strict_int_dbl && is.numeric(c1) && is.numeric(c2)) {
          isTRUE(as.double(c1) == as.double(c2)) }
        else {
          identical(c1, c2) }} 
      else {
        for(i in seq_along(c1)) {
          if (!out_fun(c1[[i]], c2[[i]])) return(FALSE) }
        return(TRUE) }
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

    eval_arg_2nd <-
      if (has_double_colon) {
        `names<-`(
          list(expr_value[[1]], expr_value[-1]),
          list(as.character(cond[[2]]), as.character(cond[[3]])) ) }
      else if (name_is_referred) {
        `names<-`(list(expr_value), list(expr_name)) }
      else NULL

    ans <- eval(statement[[2]], envir = eval_arg_2nd, enclos = parent_frame)

    #
    if (is_wildcard || has_double_colon) {
      return(ans) } # `wildcard`, x::xs
    else if ((simbol_is_referred || name_is_referred) && eval(cond, eval_arg_2nd, parent_frame)) {
      return(ans) } # is.null(x), x %% 2 == 0
    else if (cond_is_atomic && equals_recursive(expr_value, cond)) {
      return(ans) } # 1, "", NULL
    else if (!cond_is_atomic && !has_double_colon && !has_wildcard_in_expr && equals_recursive(expr_value, eval(cond, parent_frame))) {
      return(ans) } # list(1,2), numeric(0)
    else if (!cond_is_atomic && has_wildcard_in_expr && equals_recursive(expr_value_deparse, cond, wildcard = wildcards)) {
      return(ans) } # list(1,.),
    else {
      # not matched in this loop
    }
  }
  stop("The input is non-matched pattern. Need to write proper
          syntax or set default wildcard `.` at last.")
}

foldr <- function(f, init, lst) {
  match.with(lst
    , length(lst) == 0 -> init
    , x::xs            -> f(x, foldr(f, init, xs))
  )
}

foldl <- function(f, init, lst) {
  match.with(lst
    , length(lst) == 0 -> init
    , x::xs            -> foldl(f, f(init, x), xs)
 )
}

local({
  cat("foldr(`+`, 0, 1:3) == (1+(2+(3+0)))", foldr(`+`, 0, 1:3), "\n")
  cat("foldr(`-`, 0, 1:3) == (1-(2-(3-0)))", foldr(`-`, 0, 1:3), "\n")
  cat("foldl(`+`, 0, 1:3) == (((0+1)+2)+3)", foldl(`+`, 0, 1:3), "\n")
  cat("foldl(`-`, 0, 1:3) == (((0-1)-2)-3)", foldl(`-`, 0, 1:3), "\n")
})

local({
  fizzbuzz <- function(z) {
    match.with(list(z %% 5, z %% 3)
    , list(0, 0) -> "FizzBuzz"
    , list(0, .) -> "Fizz"
    , list(., 0) -> "Buzz"
    , otherwise  -> as.character(z)
    )
  }
  sapply(1:30, fizzbuzz)
})

# left expression is evaluated in parent.frame().
local({
  evenodd <- function(x) {
    match.with(x
      , x %% 2 == 0 -> "even"
      , x %% 2 != 0 -> "odd"
    )
  }
  sapply(1:10, evenodd)
})

# `::` is recognized as separater of head and tail
local({
  len <- function(xs) {
    match.with(xs
      , length(xs) == 0 -> 0 
      , y::ys          -> 1 + len(ys)
    )
  }
  cat(len(list(1,2,3)), "\n")
  cat(len(1:3), "\n")
  
  len2 <- function(n) { foldl(function(x, `_`) x + 1, 0, n) }
  cat(len2(1:3), "\n")
})

local({
  len <- function(n) {
    match.with(n
      , list() -> 0 # class sensitive when using list() or integer(0) or numeric(0)
      , x::xs  -> 1 + len(xs)
    )
  }
  tryCatch(cat(len(list(1,2,3)), "\n"), error = function(e) print(e)) # run
  tryCatch(cat(len(1:3), "\n"), error = function(e) print(e)) # not run
  tryCatch(cat(len(c(1,2,3)), "\n"), error = function(e) print(e)) # not run
})

# ., `_`, and otherwise are treated as a wildcard symbol.
local({
 count_num <- function(x) {
  match.with(x
    , 1 -> "one"
    , 2 -> "two"
    , 3 -> "three"
    , 4 -> "four"
    , . -> "_"
    )
  }
  sapply(1:10, count_num)
})

local({
   f3 <- function(x) {
     match.with(x
      , list(0, 0) -> "(0, 0) pair"
      , list(1, 0) -> "(1, 0) pair"
      , list(0, .) -> "(0, ?) pair"
      , list(., 3) -> "(?, 3) pair"
      , list(., .) -> "(?, ?) pair"
      , otherwise  -> "another pair") }

   cat(
    "list(0, 0) is", f3(list(0, 0)), "\n",
    "list(1, 0) is", f3(list(1, 0)), "\n",
    "list(0, 2) is", f3(list(0, 2)), "\n",
    "list(1, 3) is", f3(list(1, 3)), "\n",
    "list(2, 1) is", f3(list(2, 1)), "\n",
    "list(1, 2, 3) is", f3(list(1, 2, 3)), "\n",
    "list(list(1,2), 3) is", f3(list(list(1,2), 3)), "\n",
    "list(0, 1:2) is", f3(list(0, 1:2)), "\n")
 })

local({
  z <- list(10,11,12)
  match.with(z
    , length(z) == 1 -> "one"
    , length(z) == 2 -> "two"
    , length(z) >= 3 -> "very long"
  )
})
## or
local({
  z <- list(10,11,12)
  match.with(x = length(z)
    , 1 -> "one"
    , 2 -> "two"
    , x >= 3 -> "very long"
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
  match.with(z
    , .:10 -> "end with 10"
    , .:11 -> {x <- tail(z, 1) * 2; x}
    , .:12 -> "end with 12"
    , .    -> "other"
  )
})

local({
  f <- function(x) {
    match.with(x
    , list() -> "0 length of list"
    , list(.) -> "1 length of list"
    , list(., .) -> "2 lenght of list"
    , list(., ., .) -> "3 length of list"
    , otherwise -> "others"
    )
  }
  cat(f(list(2,2)), "\n")
  cat(f(1:2), "\n")
  cat(f(list(1,2,3,4,5)), "\n")
})

# recursive call works but speed is impractical.
local({
  fib1 <- function(n) {
    match.with(n
      , 0 -> 0
      , 1 -> 1
      , . -> fib1(n - 1) + fib1(n - 2)
    )
  }
  sapply(1:10, fib1)
})

local({
  fib2 <- function(n, a = 0, b = 1) {
    match.with(n
      , 0 -> a
      , . -> fib2(n - 1, b, a + b)
    )
  }
  sapply(1:10, fib2)
})

local({
  sum1 <- function(n) {
    match.with(n
    , 0 -> 0
    , . -> n + sum1(n - 1)
   )
  }
  sapply(1:10, sum1)
})

local({
  sum2 <- function(n, acc = 0) {
    match.with(n
      , 0 -> acc
      , . -> sum2(n - 1, acc + n)
    )
  }
  sapply(1:10, sum2)
})

local({
  sum_vec <- function(xs) {
    match.with(xs
      , integer(0) -> 0
      , y::ys      -> y + sum_vec(ys)
    )
  }
  cat(sum_vec(1:5), "\n") # 1:5 is integer
  cat(sum_vec(c(1,2,3,4,5)), "\n") # error because numeric(0) is not matched with integer(0)
})
