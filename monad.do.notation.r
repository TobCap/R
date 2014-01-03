## https://gist.github.com/TobCap/8233741
## see usage and examples in above

do.maker <- function(bind, ret, ..., see.body.call = TRUE){
  vars <- append(list(...), list(bind = bind, ret = ret))
  stopifnot(all(vapply(vars, is.function, logical(1))))

  as.formals <- function(x) as.pairlist(tools:::as.alist.call(x))

  set.var <- function(lst, e){
    list2env(
      lapply(lst, function(f) if (is.function(f)) `environment<-`(f, e) else f),
      envir = e)}

  addElem <- function(lst, obj) `[[<-`(lst, 1 + length(lst), obj)

  call2str <- function(calls){
    calls1 <- deparse(calls, 500, control = "keepNA")
    calls2 <- paste0(gsub(", function\\((.+?)\\) (bind|ret)", ", function\\(\\1\\) \n\\2", calls1), "\n")
    # http://stackoverflow.com/questions/15787753/r-gsub-and-capture
    noquote(calls2)
  }

  parser <- function(x, prev_){
    if (x[[1]] == quote(`%<-%`)){
      arg <- x[[2]]
      val <- x[[3]]
      if (!is.null(prev_)) {
        val <- addElem(prev_, val)
        next_ <- NULL }}
    else if (x[[1]] == quote(`<-`)) {
      # skip binding when normally assigned
      arg <- val <- NULL
      if (is.null(prev_)) {next_ <- call("{", x)}
      else {next_ <- addElem(prev_, x)}}
    else {
      # >> then
      arg <- as.symbol("_")
      val <- x
      if (!is.null(prev_)) {
        val <- addElem(prev_, val)
        next_ <- NULL }}
  
    list(arg = arg, val = val, next_ = next_)}

  function(exprs, env = parent.frame()){
    stopifnot(substitute(exprs)[[1]] == quote(`{`))
    e <- new.env(parent = env) # key point!
    set.var(vars, e)

    make.call <- function(x, next_ = NULL) {
      if (length(x) == 1) {
        return(x[[1]])}
      else {
        parsed <- parser(x[[1]], next_)
        if (!is.null(parsed$next_)) make.call(x[-1], parsed$next_)
        else call("bind", parsed$val, call("function", as.formals(parsed$arg), make.call(x[-1])))}}

    body.call <- make.call(substitute(exprs)[-1]) # remove `{` by [-1]
    
    ans <- eval(body.call, envir = e, enclos = env)

    if (see.body.call) `comment<-`(ans, call2str(body.call)) # cat(comment(ans))
    else ans
  }
}

###############
### list monad
do.list <- do.maker(
  bind = function(x, f) unlist(lapply(x, function(y) f(y)), recursive = FALSE),
  ret = function(x) list(x),
  guard = function(expr) if (isTRUE(expr)) list(NULL) else list()

  )
  
###############
### maybe monad
do.maybe <- do.maker(
  bind = function(x, f) if (is.some(x)) f(x) else none(),
  ret = function(x) some(x),
  some = function(x) if (is.some(x)) x else none(),
  none = function() quote(none),
  is.some = function(x)
              length(x) > 0 && # not NULL nor list()
              `if`(is.atomic(x), !is.na(x), TRUE) && 
              `if`(is.numeric(x), is.finite(x), TRUE) && 
              `if`(is.character(x), nzchar(x), TRUE) &&
              `if`(is.symbol(x), !identical(x, substitute()), TRUE),
  is.none = function(x) identical(x, none())
)

###############
### continuation monad
### CPS style is required
do.cont <- do.maker(
  bind = function(x, f) function(k) x(function(y) f(y)(k)),
  ret = function(x) function(k) k(x),
  callcc = function(f) function(k) f(function(x) function(`_`) k(x))(k)
)

###############
### state monad
do.state <- do.maker(
  bind = function(x, f) function(s) {
    ##with.default(`names<-`(x(s), c("fst", "snd")), f(fst)(snd))
     tmp <- x(s); fst <- tmp[[1]]; snd <- tmp[[2]]; f(fst)(snd)
  },
  ret = function(x) function(s) list(x, s)
)
