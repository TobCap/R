### correspondences
## Basic piping: ok
## The argument placeholder: ok
## Re-using the placeholder for attributes: ok
## More advanced right-hand sides and lambdas: ok
## Building (unary) functions: ok
## Tee operations: not support
## Pipe with exposition of variables: not support
## Compound assignment pipe operations: not support

unpipe <- function(expr_, eval_ = FALSE) { 
  ops <- c("%>%")
  var_names <- c()
  
  build_fun <- function(lst) {
    make_lambda <- function(body_) {
      call("function", as.pairlist(alist(.=)), iter(body_))
    }

    assign_temp_var <- function(expr_next, expr_prev) {
      new_var <- make_var_name(tail(var_names, 1))
      var_names <<- append(var_names, new_var)
      new_sym <- as.symbol(new_var)
      
      as.call(c(
        quote(`{`), 
        as.call(list(quote(`<-`), new_sym, expr_prev)), 
        lapply(expr_next, replace_dot, new_sym) ))
    }
    
    make_var_name <- function(name) {
      new_id <- 
        if (length(name) == 0) 0
        else 1 + as.numeric(sub("tmp", "", name))
      
      new_name <- paste0("tmp", new_id)
      if (!new_name %in% all.names(target, unique = TRUE)) new_name
      else make_var_name(new_name)
    }
    
    replace_dot <- function(x, expr_new) {
      # inside tilda, kind of inner DSL, a dot symbol is reserved for model description
      if ((length(x) <= 1 && x != ".") || (is.call(x) && x[[1]] == "~")) x
      else if (is.symbol(x) && x == ".") expr_new 
      else if (length(x) == 3 && as.character(x[[1]]) %in% ops) get_pipe_info(x, build_fun)
      else if (is.pairlist(x)) as.pairlist(lapply(x, replace_dot, expr_new))
      else as.call(lapply(x, replace_dot, expr_new))
    }
    
    replace_direct_dot <- function(x, expr_new) {
      lapply(x, function(y) if (y == ".") expr_new else get_pipe_info(y, build_fun))
    }
    
    wrap <- function(lst, acc) {
      if (length(lst) == 0) return(acc)
      
      expr <- lst[[1]]$rhs
      direct_dot_pos <- which(as.list(expr) == quote(.))
      
      if (length(direct_dot_pos) > 0) wrap(lst[-1], as.call(replace_direct_dot(expr, acc)))
      else if (is.symbol(expr) || class(expr) == "(") wrap(lst[-1], as.call(c(expr, acc)))
      else if (expr[[1]] == "{") wrap(lst[-1], assign_temp_var(expr[-1], acc))
      else wrap(lst[-1], as.call(c(expr[[1]], acc, lapply(expr[-1], replace_dot, acc))))
    }

    origin <- lst[[1]]$rhs
    if (length(origin) == 1 && origin == ".") make_lambda(wrap(lst[-1], quote(.)))
    else wrap(lst[-1], origin)
  }
  
  # CPS form
  get_pipe_info <- function(x, cont) {
    if (length(x) <= 1 || !(as.character(x[[1]]) %in% ops)) cont(list(list(op = NULL, rhs = iter(x))))
    else get_pipe_info(x[[2]], function(y) cont(c(y, list(list(op = x[[1]], rhs = x[[3]])))))
  }
  
  iter <- function(x) {
    if (length(x) <= 1) x
    else if (length(x) == 3 && as.character(x[[1]]) %in% ops) get_pipe_info(x, build_fun)
    else if (is.pairlist(x)) as.pairlist(lapply(x, iter))
    else as.call(lapply(x, iter))
  }
  
  target <- if (is.symbol(tmp <- substitute(expr_))) expr_ else tmp
  new_call <- iter(target)
  
  if (eval_) eval(new_call, parent.frame())
  else new_call
}

## https://github.com/smbache/magrittr/blob/master/README.md
unpipe(x %>% f)
# f(x)
unpipe(x %>% f(y))
# f(x, y)
unpipe(x %>% f %>% g %>% h)
# h(g(f(x)))

unpipe(x %>% f(y, .))
# f(y, x)
unpipe(x %>% f(y, z = .))
# f(y, z = x)

unpipe(x %>% f(y = nrow(.), z = ncol(.)))
# f(x, y = nrow(x), z = ncol(x))
unpipe(x %>% {f(y = nrow(.), z = ncol(.))})
# {
#     tmp0 <- x
#     f(y = nrow(.), z = ncol(.))
# }

### when being wrapped by `{`, lhs is firstly bound to a new variable and passed to rhs's `.`
### unpipe(rnorm(10) %>% {f(y = mean(.), z = sd(.))}) 
### should 'not' be
### f(y = mean(rnorm(10)), z = sd(rnorm(10)))
### ; should be
### {
###    tmp0 <- rnorm(10)
###    f(y = mean(tmp0), z = sd(tmp0))
### }

unpipe(iris %>% 
  {
    n <- sample(1:10, size = 1)
    H <- head(., n)
    T <- tail(., n)
    rbind(H, T)
  } %>%
  summary)
# summary({
#   tmp0 <- iris
#   n <- sample(1:10, size = 1)
#   H <- head(tmp0, n)
#   T <- tail(tmp0, n)
#   rbind(H, T)
# })


unpipe(f <- . %>% cos %>% sin)
# f <- function(.) sin(cos(.))


## Examples other than additional pipe operators are fully covered, I believe.
## http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

unpipe(
  car_data <- 
    mtcars %>%
    subset(hp > 100) %>%
    aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
    transform(kpl = mpg %>% multiply_by(0.4251)) %>%
    print
)
# car_data <- print(transform(aggregate(. ~ cyl, data = subset(mtcars, 
#     hp > 100), FUN = function(.) round(mean(.), 2)), kpl = multiply_by(mpg, 
#     0.4251)))


unpipe(
  car_data %>%
  (function(x) {
    if (nrow(x) > 2) 
      rbind(head(x, 1), tail(x, 1))
    else x
  })
)

# (function(x) {
#     if (nrow(x) > 2) 
#         rbind(head(x, 1), tail(x, 1))
#     else x
# })(car_data)

unpipe(
  car_data %>%
  { 
    if (nrow(.) > 0)
      rbind(head(., 1), tail(., 1))
    else .
  }
)
# if (nrow(car_data) > 0) rbind(head(car_data, 1), tail(car_data, 
#     1)) else car_data

unpipe(
  rnorm(1000)    %>%
  multiply_by(5) %>%
  add(5)         %>%
  { 
     cat("Mean:", mean(.), 
         "Variance:", var(.), "\n")
     head(.)
  }
)
# {
#     tmp0 <- add(multiply_by(rnorm(1000), 5), 5)
#     cat("Mean:", mean(tmp0), "Variance:", var(tmp0), "\n")
#     head(tmp0)
# }

unpipe(
  rnorm(100) %>% `*`(5) %>% `+`(5) %>% 
  {
    cat("Mean:", mean(.), "Variance:", var(.),  "\n")
    head(.)
  }
)
# {
#     tmp0 <- rnorm(100) * 5 + 5
#     cat("Mean:", mean(tmp0), "Variance:", var(tmp0), "\n")
#     head(tmp0)
# }
