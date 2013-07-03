# infinite sequence

# for reference
# http://d.hatena.ne.jp/syou6162/20080831/1220126789
# http://d.hatena.ne.jp/tar0_t/20110608/1307531065
# https://sites.google.com/site/diaspar2011/functional/infinite-list
# http://guppy.eng.kagawa-u.ac.jp/~kagawa/publication/lazylanguage.pdf

# x[2] = f(x[1]), x[3] = f(x[2]), ..., x[n] = f(x[n-1])
lseq.maker <- function(x, f){
  list(head = x, tail = function() lseq.maker(f(x), f))
}
lseq.int <- function(n = 0) lseq.maker(n, function(x) x + 1)
lseq.even <- function(n = 0) lseq.maker(n, function(x) x + 2)
lseq.odd <- function(n = 1) lseq.maker(n, function(x) x + 2)

lseq.iota <- function(n = Inf, start = 0, step = 1)
  list(head = start, tail = function() if (n == 0) NULL else lseq.iota(n - 1, start + step, step))

# lseq.int2 <- function(n = 0) list(head=n, tail = function() lseq.int2(n+1))
# lseq.even2 <- function(n = 0) list(head=n, tail = function() lseq.even2(n+2))
# lseq.odd2 <- function(n = 1) list(head=n, tail = function() lseq.odd2(n+2))

ldouble <- function(n=1) lseq.maker(n, f.(x, x*2))
lhalf <- function(n=1) lseq.maker(n, f.(x, x/2))
# ltake(10, ldouble())
# ltake(10, lhalf())

ltake <- function(n, x){
  h <- x$head
  if(length(h) > 1) h <- list(h)
  if(n == 0 || is.null(h)) NULL
  else c(h, ltake(n - 1, x$tail()))
}
ltake2 <- function(n, x, acc = NULL){
  h <- x$head
  if (length(h) > 1) h <- list(h)
  if(n == 0 || is.null(h)) acc
  else ltake2(n - 1, x$tail(), c(acc, h))
}
lref <- function(n, x) {
  if(n == 1) x$head
  else lref(n - 1, x$tail())
}
# ltake(10, lzipWith("*", lseq.int(1), lseq.int(1)))

lmap <- function(FUN, s){
  if(is.null(s$head)) NULL
  else list(head = FUN(s$head), tail = function() lmap(FUN, s$tail()))
}
lfilter <- function(FUN, s) {
  if(is.null(s$head)) NULL
  else if(FUN(s$head)) c(head = s$head, tail = function() lfilter(FUN, s$tail()))
  else lfilter(FUN, s$tail())
}

lfind <- function(FUN, s){
  if(is.null(s$head)) NULL
  else if(FUN(s$head)) s$head
  else lfind(FUN, s$tail())
}
lzip... <- function(...){
  lzipWith...(c, ...)
}
# ltake(10, lzip...(lseq.int(0), lseq.int(10), lseq.int(100)))

lzipWith <- function(FUN, lseq1, lseq2) {
  FUN <- match.fun(FUN)
  list(head = FUN(lseq1$head, lseq2$head), tail = function() lzipWith(FUN, lseq1$tail(), lseq2$tail()))
}
lzipWith... <- function(FUN, ...) {
  FUN <- match.fun(FUN)
  l <- list(...)
  h <- lapply(l, function(x) x$head)
  t <- lapply(l, function(x) x$tail())
  list(head = Reduce(FUN, h), tail = function()  do.call(lzipWith..., c(FUN, t)))
}
# lzipWith... <- function(FUN, ...) {
  # FUN <- match.fun(FUN)
  # if(length(list(...)) == 1) l <- ..1 else l <- list(...)
  # h <- lapply(l, function(x) x$head)
  # t <- lapply(l, function(x) x$tail())
  # list(head = Reduce(FUN, h), tail = function() lzipWith...(FUN = FUN, t))
# }
# ltake(10, lzipWith...("+", lseq.int(1), lseq.int(1), lseq.int(1)))

lseq.fib <- function(n = 0, m = 1){
  list(head = n, tail = function() lseq.fib(m, n + m))
}
lseq.fib2 <- function(){
  list(head = 0, tail = function() list(head = 1, tail = function() lzipWith("+", lseq.fib2(), lseq.fib2()$tail())))
}
# in Haskell
# let fib = 0 : 1 : zipWith (+) fib (tail fib)
lseq.primes <- function(s = lseq.int(2)){
  list(head = s$head, tail = function() lseq.primes(lfilter(f.(x, x %% s$head != 0), s$tail())))
}
# > ltake(10, lseq.primes())
#  [1]  2  3  5  7 11 13 17 19 23 29

# > ltake(10, lseq.int())
# [1] 0 1 2 3 4 5 6 7 8 9
# > val <- ltake(1e4, lseq.int())
#  ƒGƒ‰[F C stack usage is too close to the limit
# > val <- tco(ltake2)(1e4, lseq.int()) # works!

ltakeWhile <- function(FUN, x){
  if(FUN(x$head)) c(x$head, ltakeWhile(FUN, x$tail()))
  else NULL
}
ltakeWhile2 <- function(FUN, x, acc = NULL){
  if(FUN(x$head)) ltakeWhile2(FUN, x$tail(), c(acc, x$head))
  else acc
}
lskipWhile <- function(FUN, x){
  if(!FUN(x$head)) x
  else lskipWhile(FUN, x$tail())
}
lskip <- function(n, x){
  if(n == 0) x
  else lskip(n - 1, x$tail())
}

# ltake(5, lskipWhile(function(x) x <= 2013, lseq.primes()))
# lseq.primes() %|% lskipWhile(f.(x, x <= 2013), ..) %|% ltake(5, ..)

# ltakeWhile2(function(x) x < 20, lseq.primes())
# ltakeWhile2(f.(x, x < 20), lseq.primes())

# > ltakeWhile(function(x) x<100, lseq.fib())
#  [1]  0  1  1  2  3  5  8 13 21 34 55 89

# for reference
# http://d.hatena.ne.jp/syou6162/20080831/1220126789
# http://d.hatena.ne.jp/tar0_t/20110608/1307531065