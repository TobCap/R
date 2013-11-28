### infinite sequence

## TODO: going to rewrite them in C or Rccp.

## For reference:
# http://d.hatena.ne.jp/syou6162/20080831/1220126789
# http://d.hatena.ne.jp/tar0_t/20110608/1307531065
# https://sites.google.com/site/diaspar2011/functional/infinite-list

## The idea and funcition names are mainly from Scheme (Gauche) and Haskell.
# http://www.shido.info/lisp/scheme_lazy.html
# http://practical-scheme.net/gauche/man/gauche-refj_57.html
# http://guppy.eng.kagawa-u.ac.jp/~kagawa/publication/lazylanguage.pdf

## basic functions
lhead <- function(x) x$head
ltail <- function(x) if(is.null(x$tail)) NULL else x$tail()
`%:%` <- lcons <- function(x, y) pairlist(head = x, tail = function() y)
`%++%` <- function(lhs, rhs){
  if (is.null(lhs)) rhs
  else lhead(lhs) %:% (ltail(lhs) %++% rhs)
}
`%!!%` <- function(lseq, n) if (n == 0) lhead(lseq) else ltail(lseq) %!!% (n - 1)
`%l:%` <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))
  out.fun <- function(x, y) {
    if (x == y) x %:% NULL
    else if (x < y) x %:% out.fun(x + 1, y)
    else if (x > y) x %:% out.fun(x - 1, y)
    else stop("")}
  out.fun(as.integer(x), as.integer(y))
}

llist <- function(...) {
  if (is.null(pairlist(...))) NULL
  else ..1 %:% do.call(llist, list(...)[-1])
}
as.llist <- function(vec) {
  do.call(llist, as.list(vec))
}
is.llist <- function(x, check.depth = 10) {
  out.fun <- function(x, n) {
    if (n == 0 || is.null(x)) TRUE
    else length(x) == 2 && typeof(x) == "pairlist" && 
      names(x) == c("head", "tail") && out.fun(ltail(x), n - 1)
  }
  out.fun(x, check.depth)
}
# x <- llist(1, 2)
# y <- llist(3, 4, 5)
# z <- x %++% y
# ltake2(4, z))

## llist.uneval can handle infinite element
# x <- llist.uneval(1, 2, 3, while(TRUE){}, 5)
llist.uneval <- function(..., e = parent.frame()) {
  expr <- as.list(substitute((...)))[-1]
  if (length(expr) == 0) NULL
  else eval(expr[[1]], envir = e) %:% do.call(llist.uneval, c(expr[-1], e = e))
}

# The generating function f(x0, x1, ..., xn-1) takes last n arguments and returns xn (next value).
lseq.maker1 <- function(x, f) x %:% lseq.maker1(f(x), f)
lseq.maker2 <- function(x0, x1, f) x0 %:% lseq.maker2(x1, f(x0, x1), f)
lseq.maker3 <- function(x0, x1, x2, f) x0 %:% lseq.maker3(x1, x2, f(x0, x1, x2), f)

## induce a rule from above patterns.
lseq.maker. <- function(..., f) ..1 %:% do.call(lseq.maker., c(list(...)[-1], do.call(f, list(...)), f = f))
# x <- lseq.maker.(0, f = function(x) x + 1)
# ltake2(10, x)
#
# x <- lseq.maker.(0, 1, f = function(x, y) x + y)
# ltake2(10, x)
#
# x <- lseq.maker1(0:1, f = function(x) x[[1]] + x[[2]])
# ltake2(10, x)

## a sequence
lint <- lseq.maker1(0, function(x) x + 1)
leven <- lseq.maker1(0, function(x) x + 2)
lodd <- lseq.maker1(1, function(x) x + 2)

## can define generetor by using own name.
lint2  <- function(x = 0) x %:% lint2(x + 1)
leven2 <- function(x = 0) x %:% lint2(x + 2)
lodd2  <- function(x = 0) x %:% lint2(x + 1)

## generator
liota <- function(n = Inf, start = 0, step = 1){
  start %:% (if (n == 0) NULL else liota(n - 1, start + step, step))}
## arithmetic sequence generater
larith <- function(x0, x1) x0 %:% larith(x1, 2 * x1 - x0)

lforce <- function(x, acc = NULL) { # value
  hd <- lhead(x)
  if (is.null(hd)) acc
  else lforce(ltail(x), c(acc, hd))
}

ltake <- function(n, x) { # lseq
  hd <- lhead(x)
  if (n == 0 || is.null(hd)) NULL
  else hd %:% ltake(n - 1, ltail(x))
}
# short-cut of lforce(ltake(x))
ltake2 <- function(n, x) { # value
  hd <- lhead(x)
  if (length(hd) > 1) {
    hd <- list(hd)} # for lzip.
  if (n == 0 || is.null(hd)) NULL
  else if (n == 1) hd
  else c(hd, ltake2(n - 1, ltail(x)))
}
# tail recursion
ltake2.tc <- function(n, x, acc = NULL) { # value
  hd <- lhead(x)
  if (length(hd) > 1) {
    hd <- list(hd)} # for lzip.
  if (n == 0 || is.null(hd)) NULL
  else if (n == 1) c(acc, hd)
  else ltake2.tc(n - 1, ltail(x), c(acc, hd))
}
# > ltake2(10, lint)
# [1] 0 1 2 3 4 5 6 7 8 9
# > val <- ltake2(1e4, lint)
#  ?G?ñ [ F C stack usage is too close to the limit
### To avoid stack-overflow, use tco(), tail call optimization, which is defined
### in https://github.com/TobCap/R/blob/master/functionalProgramming.r
# > val <- tco(ltake2.tc)(1e4, lint) # works!

lref <- function(n, x) { # value
  if (n == 1) lhead(x)
  else lref(n - 1, ltail(x))
}

ltakeWhile <- function(FUN, x) { # lseq
  hd <- lhead(x)
  if (!FUN(hd) || is.null(hd)) NULL
  else hd %:% ltakeWhile(FUN, ltail(x))
}
# short-cut of lforce(ltakeWhile(FUN, x))
ltakeWhile2 <- function(FUN, x, acc = NULL) { # value
  hd <- lhead(x)
  if (!FUN(hd) || is.null(hd)) acc
  else ltakeWhile2(FUN, ltail(x), c(acc, hd))
}

ldropWhile <- function(FUN, x) { # lseq
  hd <- lhead(x)
  if (!FUN(hd) || is.null(hd)) x
  else ldropWhile(FUN, ltail(x))
}

ldrop <- function(n, x) { # lseq
  if (n == 0) x
  else ldrop(n - 1, ltail(x))
}

lfind <- function(FUN, x) { # value
  hd <- lhead(x)
  if (is.null(hd)) NULL
  else if(FUN(hd)) hd
  else lfind(FUN, ltail(x))
}

llength <- function(x, acc = 0) { # value
  if (is.null(lhead(x))) acc
  else llength(ltail(x), acc + 1)
}

lrep.each <- function(x, n) {
  rep.elem <- function(x, n) {
    if(n == 0) NULL
    else x %:% rep.elem(x, n - 1)}
  hd <- lhead(x)
  if (is.null(hd)) NULL
  else rep.elem(hd, n) %++% lrep.each(ltail(x), n)
}

lconcat.each <- function(x, y) {
  if (is.null(x) && is.null(y)) NULL
  else if (is.null(x)) y
  else if (is.null(y)) x
  else lhead(x) %:% (lhead(y) %:% concat.each(ltail(x), ltail(y)))
}

lmap <- function(FUN, x) {
  hd <- lhead(x)
  if (is.null(hd)) NULL
  else FUN(hd) %:% lmap(FUN, ltail(x))
}

lfilter <- function(FUN, x) {
  hd <- lhead(x)
  if (is.null(hd)) NULL
  else if (FUN(hd)) hd %:% lfilter(FUN, ltail(x))
  else lfilter(FUN, ltail(x))
}

lzipWith <- function(FUN, lseq1, lseq2) {
  FUN <- match.fun(FUN)
  FUN(lhead(lseq1), lhead(lseq2)) %:% lzipWith(FUN, ltail(lseq1), ltail(lseq2))
}

lzipWith. <- function(FUN, ...) {
  FUN <- match.fun(FUN)
  dots <- list(...)
  hd <- lapply(dots, lhead)
  tl <- lapply(dots, ltail)
  Reduce(FUN, hd) %:% do.call(lzipWith., c(FUN, tl))
}

lzip. <- function(...) {
  lzipWith.(c, ...)
}
# ltake2(10, lzipWith("*", lint, lint))
# ltake2(10, lzip.(liota(0), liota(10), liota(100)))

lall <- function(x) { # value
  hd <- lhead(x)
  if (is.null(hd)) TRUE
  else if (!isTRUE(hd)) FALSE
  else lall(ltail(x))
}

lany <- function(x){ # value
  hd <- lhead(x)
  if (is.null(hd)) FALSE
  else if (isTRUE(hd)) TRUE
  else lany(ltail(x))
}

## This is a function (a generator).
lfib0 <- function(n = 0, m = 1) n %:% lfib0(m, n + m)
# ltake(10, lfib0())

## This is a sequence.
lfib1 <- lseq.maker.(0, 1, f = function(x, y) x + y)
# ltake2(10, lfib1)

## This is also a sequence.
## In Haskell: let fib = 0 : 1 : zipWith (+) fib (tail fib)
lfib2 <- 0 %:% (1 %:% lzipWith(`+`, lfib2, ltail(lfib2)))
## library(gmp)
# lfib3 <- as.bigz(0) %:% (as.bigz(1) %:% lzipWith(`+`, lfib3, ltail(lfib3)))
# ltake(100, lfib3)


# for reference of a prime sequence in R code
# http://d.hatena.ne.jp/syou6162/20080831/1220126789
# http://d.hatena.ne.jp/tar0_t/20110608/1307531065

lprimes1 <- function(x = liota(start = 2)) {
  lhead(x) %:% lprimes1(lfilter(function(y) y %% lhead(x) != 0, ltail(x)))
}
## almost the same speed even thought even sequence is omitted.
lprimes2 <- function(x = 2 %:% liota(start = 3, step = 2)) {
  lhead(x) %:% lprimes2(lfilter(function(y) y %% lhead(x) != 0, ltail(x)))
}
## This is faster than lprimes1() and lprimes2()
sieve <- function(x) {
  hd <- lhead(x)
  hd %:% sieve(lfilter(function(y) y %% hd != 0, ltail(x)))
}
lprimes3 <- 2 %:% sieve(liota(start = 3, step = 2))

# > ltake2(10, lprimes1())
#  [1]  2  3  5  7 11 13 17 19 23 29

# > ltakeWhile2(function(x) x < 20, lprimes3)
# [1]  2  3  5  7 11 13 17 19
#
# > ltake2(5, ldropWhile(function(x) x <= 2013, lprimes3))
# [1] 2017 2027 2029 2039 2053
#
# > ltakeWhile2(function(x) x < 100, lfib2)
#  [1]  0  1  1  2  3  5  8 13 21 34 55 89

# > ltakeWhile2(function(x) x < 2100, ldropWhile(function(x) x < 2013, lprimes3))
#  [1] 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099

## %|% and f. defined in https://github.com/TobCap/R/blob/master/functionalProgramming.r
## have great readability and usability.
## lprimes3 %|% ldropWhile(f.(x, x < 2013), ..) %|% ltakeWhile2(f.(x, x < 2100), ..)
