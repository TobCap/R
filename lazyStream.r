### infinite sequence
### TODO: going to rewrite them in C or Rccp.

## Reference of existing R codes
# http://d.hatena.ne.jp/syou6162/20080831/1220126789
# http://d.hatena.ne.jp/tar0_t/20110608/1307531065
# https://sites.google.com/site/diaspar2011/functional/infinite-list

## The idea and funcition names are mainly from Scheme (Gauche) and Haskell.
# http://practical-scheme.net/gauche/man/gauche-refj_57.html
# http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html
# http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html

# This can append any type of object, different from Haskell's List

## basic functions
## To simulate WHNF, both `head` and `tail` are wrapped by closure
`%:%` <- lcons <- function(x, y) `class<-`(pairlist(head = function() x, tail = function() y), "LList")
##`%:%` <- lcons <- function(x, y) pairlist(head = x, tail = function() y)

# R's infix operator has left-associativity
# 1 %:% 2 %:% 3 %:% NULL # can be parsed but not good
# 1 %:% (2 %:% (3 %:% NULL)) # Good

# ones <- 1L %:% ones # Good

lnull <- is.null # function(x) is.null(x)
lempty <- NULL
lhead <- function(x) if (lnull(x)) stop("empty list") else x$head()
ltail <- function(x) if (lnull(x)) stop("empty list") else x$tail()

linit <- function(x) {
  if (lnull(x)) stop("empty list") 
  else if (lnull(tl <- ltail(x))) lempty 
  else lhead(x) %:% linit(tl)
}
llast <- function(x) {
  if (lnull(x)) stop("empty list") 
  else if (lnull(tl <- ltail(x))) lhead(x)
  else llast(tl)
}

`%++%` <- function(lhs, rhs){
  if (is.null(lhs)) rhs
  else lhead(lhs) %:% (ltail(lhs) %++% rhs)
}

`%!!%` <- function(x, n) {
  if (n < 0) stop("negative index")
  if (n == 0) lhead(x) else ltail(x) %!!% (n - 1)
}

`%..%` <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))
  out.fun <- function(x, y) {
    if (x == y) x %:% lempty
    else if (x < y) x %:% out.fun(x + 1L, y)
    else if (x > y) x %:% out.fun(x - 1L, y)
    else stop("")}
  out.fun(as.integer(x), if (is.finite(y)) as.integer(y) else y)
}


# llength <- function(x) {
#   if (lnull(x)) 0
#   else 1 + llength(ltail(x))
# }
llength <- function(x, acc = 0) {
  if (lnull(x)) acc
  else llength(ltail(x), acc + 1)
}
# > llength(1%..%1e4)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
# > tco(llength)(1%..%1e4)
# [1] 10000

# make a finite lazy object from R's object
llist <- function(...) {
  if (lnull(pairlist(...))) lempty
  else ..1 %:% do.call(llist, list(...)[-1])
}

# x <- llist(1, 2)
# y <- llist(3, 4, 5)
# z <- x %++% y
# lprint(z)
# => list(1, 2, 3, 4, 5)

# R's vector -> lazy object
as.llist <- function(r.vec) {
  do.call(llist, as.list(r.vec))
}
# llist(1L,2L,3L) can be expressed by as.llist(1:3)

is.llist <- function(x) class(x) == "LList" || class(x) == "NULL"
# is.llist <- function(x, check.depth = 10) {
#   out.fun <- function(x, n) {
#     if (n == 0 || lnull(x)) TRUE
#     else length(x) == 2 && typeof(x) == "pairlist" && 
#       names(x) == c("head", "tail") && out.fun(ltail(x), n - 1)
#   }
#   out.fun(x, check.depth)
# }

## llist.uneval can handle _|_ (infinite element)
# x <- llist.uneval(1, 2, 3, while(TRUE){}, 5)
# > llast(x)
# [1] 5
llist.uneval <- function(..., e = parent.frame()) {
  expr <- as.list(substitute((...)))[-1]
  if (length(expr) == 0) lempty
  else eval(expr[[1]], envir = e) %:% do.call(llist.uneval, c(expr[-1], e = e))
}

# convert lazy object to R's list
lforce <- function(x, rec.max = 100) {
  # can only handle a flatten LList
  if (lnull(x)) NULL
  else if (rec.max == 0) {warning("too many elements; stop converting"); NULL}
  else c(list(lhead(x)), lforce(ltail(x), rec.max - 1))
}

lforce.tc <- function(x, acc = NULL, rec.max = 100) {
  if (lnull(x)) acc
  else if (rec.max == 0) {warning("too many elements; stop converting"); acc}
  else lforce.tc(ltail(x), c(acc, list(lhead(x))), rec.max - 1)
}

lforce.nested <- function(x, rec.max = 100) {
  # can handle a LList of LList
  if (lnull(x)) return(lempty)
  if (rec.max == 0) {warning("too many elements; stop converting"); return(lempty)}
  
  hd <- lhead(x); tl <- ltail(x); n <- rec.max - 1
  if (is.llist(hd)) c(list(lforce.nested(hd, n)), lforce.nested(tl, n))
  else c(list(hd), lforce.nested(tl, n))
}

# just print for debug
# print.LList <- lprint <- function(x, rec.max = 100) {
  # if (!is.llist(x)) dput(x)
  # else if (is.llist(lhead(x))) dput(lforce.nested(x, rec.max))
  # else dput(lforce(x, rec.max))
# }

# S3 generic class for print()
lprint <- print.LList <- function(x, rec.max = 100) {
  if (!is.llist(x)) return(dput(x))
  if (is.llist(lhead(x))) {
    if (exists("tco"))
      warning("tco() cannot handle a LList of LList. Output size is limitted.\n")
    return(dput(lforce.nested(x, rec.max)))
  }
  ### To avoid stack-overflow, use tco(), tail call optimization, which is defined at
  ### https://github.com/TobCap/R/blob/master/tailCallOptimization.r
  ### and you need set `option(expression = ??)` as larger number as you can
  ans <- 
    if (exists("tco")) tco(lforce.tc)(x, acc = NULL, rec.max = rec.max)
    else lforce.tc(x, rec.max = rec.max)
    #else {warning("There is no tco(). Applied just lforce.tc()\n"); lforce.tc(x, rec.max = rec.max)}
  dput(ans)
}
 
# > lprint(1%..%Inf, 1e4)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
# In addition: Warning message:
# In lprint(1 %..% Inf, 10000) : There is no tco(). Applied just lforce.tc()

# > lprint(1%..%Inf, 1e4) # works if tco() is imported
#     9980L, 9981L, 9982L, 9983L, 9984L, 9985L, 9986L, 9987L, 9988L, 
#     9989L, 9990L, 9991L, 9992L, 9993L, 9994L, 9995L, 9996L, 9997L, 
#     9998L, 9999L, 10000L)
# Warning message:
# In f_() : too many elements; stop converting

## sublists
ltake <- function(n, x) {
  if (n <= 0 || lnull(x)) lempty
  else lhead(x) %:% ltake(n - 1, ltail(x))
}

ldrop <- function(n, x) {
  if (n <= 0) x
  else ldrop(n - 1, ltail(x))
}

lsplitAt <- function(n, x) list(ltake(n, x), ldrop(n, x)) # no taple, just R's list

ltakeWhile <- function(FUN, x) {
  if (lnull(x) || !FUN(hd <- lhead(x))) lempty
  else hd %:% ltakeWhile(FUN, ltail(x))
}

ldropWhile <- function(FUN, x) {
  if (lnull(x) || !FUN(hd <- lhead(x))) x
  else ldropWhile(FUN, ltail(x))
}

lspan <- function(f, x) list(ltakeWhile(f, x), ldropWhile(f, x))
lbreak <- function(f, x) lspan(Negate(f), x)
### sublists end

### infinite lists
literate <- function(f, x) {
  x %:% literate(f, f(x))
}

lrepeat <- function(x) {
  x %:% lrepeat(x) # == literate(identity, x)
}

lreplicate <- function(n, x) {
  if (n <= 0) lempty
  else x %:% lreplicate(n - 1, x)
}

lcycle <- function(x) {
  if (is.llist(x)) x %++% lcycle(x)
  else as.llist(x) %++% lcycle(x)
}
### infinite lists end

### searching 
lfilter <- function(FUN, x) {
  if (lnull(x)) lempty
  else if (FUN(hd <- lhead(x))) hd %:% lfilter(FUN, ltail(x))
  else lfilter(FUN, ltail(x))
}

lfind <- function(FUN, x) { # not Maybe Class
  if (lnull(x)) lempty
  else if (FUN(hd <- lhead(x))) hd
  else lfind(FUN, ltail(x))
}

lpartition <- function(FUN, x) list(lfilter(FUN, x), lfilter(Negate(FUN), x))

## Exercise
# lrep.each <- function(n, x) {
#   if (lnull(x)) lempty
#   else lreplicate(n, lhead(x)) %++% lrep.each(ltail(x), n)
# }
## same as function(x, n) lconcatMap(function(y) lreplicate(n, y), x)

# lconcat.each <- function(x, y) {
#   if (lnull(x) && lnull(y)) lempty
#   else if (lnull(x)) y
#   else if (lnull(y)) x
#   else lhead(x) %:% (lhead(y) %:% lconcat.each(ltail(x), ltail(y)))
# }
## same as function(x, y) lconcat(lzipWith(llist, x1, x2))

## transformations
lmap <- function(FUN, x) {
  if (lnull(x)) lempty
  else FUN(lhead(x)) %:% lmap(FUN, ltail(x))
}

lreverse <- function(x, acc = lempty) {
  if (lnull(x)) acc
  else lreverse(ltail(x), lhead(x) %:% acc)
}

# TODO
# lintersperse
# lintercalate
# lsubsequences
# lpermutations

ltranspose <- function(l) {
  if (!is.llist(l)) stop("argument must be LList")
  
  if (lnull(l)) lempty
  else if (lnull(hd <- lhead(l))) ltranspose(ltail(l))
  else {
    x <- lhead(hd)
    xs <- ltail(hd)
    xss <- ltail(l)
    x %:% llist(lhead(lhead(xss))) %:% (ltranspose(xs %:% llist(ltail(lhead(xss)))))
  }
}

# x1 <- llist(1,2,3)
# x2 <- llist(4,5,6)
# x3 <- llist(x1, x2)
# x4 <- ltranspose(x3)
# lprint(x3); lprint(x4)

## zipping
lzipWith <- function(f, x, y) {
  if (lnull(x) || lnull(y)) lempty
  else f(lhead(x), lhead(y)) %:% lzipWith(f, ltail(x), ltail(y))
}

# This can treat any number of arguments
lzipWith. <- function(f, ...) {
  dots <- list(...)
  if (any(vapply(dots, lnull, FALSE))) return(lempty)
  
  hd <- lapply(dots, lhead)
  tl <- lapply(dots, ltail)
  do.call(f, hd) %:% do.call(lzipWith., c(f, tl))
}

lzip. <- function(...) {
  lzipWith.(list, ...)
}

# lint <- 0L %:% lmap(function(x) x + 1L, lint)
# lprint(ltake(10, lzipWith(`*`, lint, lint)))
# lprint(ltake(10, lzip.(lint, lint, lint)))

## Reducing lists (folds)
lfoldl <- function(f, init, x) {
  if (lnull(x)) init
  else lfoldl(f, f(init, lhead(x)), ltail(x))
}
# > lfoldl(`+`, 0, 1%..%10)
# [1] 55
lfoldl1 <- function(f, x) {
  if (lnull(x)) stop("x must be non-empty LList")
  else lfoldl(f, lhead(x), ltail(x))
}
lfoldr <- function(f, init, x) {
  if (lnull(x)) init
  else f(lhead(x), lfoldr(f, init, ltail(x)))
}
lfoldr1 <- function(f, x) {
  if (lnull(x)) stop("x must be non-empty LList")
  else if (llength(x) == 1) lhead(x)
  else f(lhead(x), lfoldr1(f, ltail(x)))
}

## Building lists
lscanl <- function(f, init, x) {
  if (lnull(x)) init %:% lempty
  else init %:% lscanl(f, f(init, lhead(x)), ltail(x))
}
# > lscanl(`+`, 0, 1%..%10)
# list(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55)
lscanl1 <- function(f, x) {
  if (lnull(x)) lempty
  else lscanl(f, lhead(x), ltail(x))
}
lscanr <- function(f, init, x) {
  if (lnull(x)) llist(init)
  else {
    tmp <- lscanr(f, init, ltail(x))
    f(lhead(x), lhead(tmp)) %:% tmp
  }
}
lscanr1 <- function(f, x) {
  if (lnull(x)) lempty
  else if (llength(x) == 1) x
  else {
    tmp <- lscanr1(f, ltail(x))
    f(lhead(x), lhead(tmp)) %:% tmp
  }
}

# Special folds
lconcat <- function(x) {
  lfoldr(`%++%`, lempty, x)
}
lconcatMap <- function(f, x) {
  lconcat(lmap(f, x))
}

lall <- function(f, x) {
  f(lhead(x)) && lall(f, ltail(x))
}

lany <- function(f, x) {
  f(lhead(x)) || lany(f(ltail(x)))
}

## and :: [Bool] -> Bool
land <- function(x) {
  if (lnull(x)) TRUE
  else isTRUE(lhead(x)) && land(ltail(x))
}

## or :: [Bool] -> Bool
lor <- function(x) {
  if (lnull(x)) FALSE
  else isTRUE(lhead(x)) || lor(ltail(x))
}

lsum <- function(x, acc = 0) {
  if (lnull(x)) acc
  else lsum(ltail(x), acc + lhead(x))
}
# > lsum(1%..%1000)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
# > tco(lsum)(1%..%1000)
# [1] 500500

lproduct <- function(x, acc = 1) {
  if (lnull(x)) acc
  else lproduct(ltail(x), acc * lhead(x))
}

lmaximum <- function(x, acc) {
  if (lnull(x)) stop("empty LList")
  else lfoldl1(max, x)
}

lminmum <- function(x) {
  if (lnull(x)) stop("empty LList")
  else lfoldl1(min, x)
}

# The generating function f(x0, x1, ..., xn-1) takes last n arguments and returns xn (next value).
lseq.maker1 <- function(x, f) x %:% lseq.maker1(f(x), f)
lseq.maker2 <- function(x0, x1, f) x0 %:% lseq.maker2(x1, f(x0, x1), f)
lseq.maker3 <- function(x0, x1, x2, f) x0 %:% lseq.maker3(x1, x2, f(x0, x1, x2), f)

## induce a rule from above patterns.
lseq.maker. <- function(..., f) ..1 %:% do.call(lseq.maker., c(list(...)[-1], do.call(f, list(...)), f = f))
# x <- lseq.maker.(0, f = function(x) x + 1)
# lprint(ltake(10, x))
#
# x <- lseq.maker.(0, 1, f = function(x, y) x + y)
# lprint(ltake(10, x))
#
# x <- lseq.maker1(0:1, f = function(x) c(x[[2]], x[[1]] + x[[2]]))
# lprint(ltake(10, x))

## a sequence
lint  <- lseq.maker1(0, function(x) x + 1L)
leven <- lseq.maker1(0, function(x) x + 2L)
lodd  <- lseq.maker1(1, function(x) x + 2L)

## can define generetor by using its own name.
lint2  <- function(x = 0) x %:% lint2(x + 1L)
leven2 <- function(x = 0) x %:% lint2(x + 2L)
lodd2  <- function(x = 0) x %:% lint2(x + 1L)

## generator
liota <- function(n = Inf, start = 0, step = 1) {
  start %:% (if (n == 0) lempty else liota(n - 1, start + step, step))}
## arithmetic sequence generater
larith <- function(x0, x1) x0 %:% larith(x1, 2 * x1 - x0)

## This is a function (a generator).
lfib0 <- function(n = 0, m = 1) n %:% lfib0(m, n + m)
# > ltake(10, lfib0())
# list(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

## This is a sequence.
lfib1 <- lseq.maker.(0, 1, f = function(x, y) x + y)
# > ltake(10, lfib1)
# list(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

## This is also a sequence.
## In Haskell: let fib = 0 : 1 : zipWith (+) fib (tail fib)
lfib2 <- 0 %:% (1 %:% lzipWith(`+`, lfib2, ltail(lfib2)))
## library(gmp)
# lfib3 <- as.bigz(0) %:% (as.bigz(1) %:% lzipWith(`+`, lfib3, ltail(lfib3)))
# > lfib3 %!!% 500
# Big Integer ('bigz') :
# [1] 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125

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

# > ltake(10, lprimes1())
# list(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

# > ltakeWhile(function(x) x < 20, lprimes3)
# list(2, 3, 5, 7, 11, 13, 17, 19)

# > ltake(5, ldropWhile(function(x) x <= 2013, lprimes3))
# list(2017, 2027, 2029, 2039, 2053)

# > ltakeWhile(function(x) x < 100, lfib2)
# list(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)

# > ltakeWhile(function(x) x < 2100, ldropWhile(function(x) x < 2013, lprimes3))
# list(2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 
#     2089, 2099)

## `%|%` and f.() defined in https://github.com/TobCap/R/blob/master/functionalProgramming.r
## have great readability and usability.
## lprimes3 %|% ldropWhile(f.(x, x < 2013), ..) %|% ltakeWhile(f.(x, x < 2100), ..)
