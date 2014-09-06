unpipe <- function(expr) {
  cnv <- function(x) {
    lhs <- x[[2]]
    rhs <- x[[3]]
    
    dot_pos <- which(
      vapply(rhs
      , function(x) paste0(as.character(x), collapse = "") == "."
      , logical(1)
      , USE.NAMES = FALSE))
      
    if (any(all.names(rhs) == "%>%")) rhs <- decomp(rhs)
    if (any(all.names(lhs) == "%>%")) lhs <- decomp(lhs)
    
    # main
    if (length(dot_pos) > 0) {
      rhs[[dot_pos]] <- lhs
      rhs
    } else if (is.symbol(rhs) || rhs[[1]] == "function" || rhs[[1]] == "(") {
      as.call(c(rhs, lhs))
    } else if (is.call(rhs)) {
      as.call(c(rhs[[1]], lhs, lapply(rhs[-1], decomp)))
    } else {
      stop("missing condition error")
    }
  }
  
  decomp <- function(x) {
    if (length(x) == 1) x
    else if (length(x) == 3 && x[[1]] == "%>%") cnv(x)
    else if (is.pairlist(x)) as.pairlist(lapply(x, decomp))
    else as.call(lapply(x, decomp))
  }
  decomp(expr)
}

###
# http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
# install.packages("magrittr")
library("magrittr")
exam1 <- quote(
  weekly <-
    airquality %>% 
    transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>% 
    aggregate(. ~ Date %>% format("%W"), ., mean)
)

(exam1_ <- unpipe(exam1))
# weekly <- aggregate(. ~ format(Date, "%W"), transform(airquality, 
#     Date = as.Date(paste(1973, Month, Day, sep = "-"))), mean)

identical(unname(eval(exam1)), unname(eval(exam1_)))
# [1] TRUE

exam2 <- quote(
  windy.weeks <-
    airquality %>% 
    transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>% 
    aggregate(. ~ Date %>% format("%W"), ., mean) %>%
    subset(Wind > 12, c(Ozone, Solar.R, Wind)) %>% 
    print
)
(exam2_ <- unpipe(exam2))
# print(subset(aggregate(. ~ format(Date, "%W"), transform(airquality, 
#     Date = as.Date(paste(1973, Month, Day, sep = "-"))), mean), 
#     Wind > 12, c(Ozone, Solar.R, Wind)))

{eval(exam2); eval(exam2_)}
#      Ozone  Solar.R     Wind
# 2 15.40000 192.6000 12.28000
# 3 18.14286 203.4286 12.45714
# 7 27.00000 207.6667 14.53333
#      Ozone  Solar.R     Wind
# 2 15.40000 192.6000 12.28000
# 3 18.14286 203.4286 12.45714
# 7 27.00000 207.6667 14.53333

exam3 <- quote(
  windy.weeks %>%
    (function(x) rbind(x %>% head(1), x %>% tail(1)))
)
(exam3_ <- unpipe(exam3))
# (function(x) rbind(head(x, 1), tail(x, 1)))(windy.weeks)
identical(eval(exam3), eval(exam3_))
# [1] TRUE

exam4 <- quote(1:10 %>% (substitute(f(), list(f = sum))))
(exam4_ <- unpipe(exam4))
# (substitute(f(), list(f = sum)))(1:10)
### invalid semantics
identical(eval(exam4), eval(exam4_))
# Error in eval(expr, envir, enclos) : attempt to apply non-function

exam5 <- quote(1:10 %>% (substitute(f, list(f = sum))))
exam5_ <- unpipe(exam5)
identical(eval(exam5), eval(exam5_))
# [1] TRUE

exam6 <- quote(
  rnorm(1000)    %>%
  multiply_by(5) %>%
  add(5)         %>%
  function(x) {
    cat("Mean:",     x %>% mean, 
        "Variance:", x %>% var,  "\n")
  }
)
(exam6_ <- unpipe(exam6))
# (function(x) {
#     cat("Mean:", mean(x), "Variance:", var(x), "\n")
# })(add(multiply_by(rnorm(1000), 5), 5))

### Current CRAN version has a bug when using anonymous function without wrapping by `(`
{ 
  set.seed(6); eval(exam6);
  set.seed(6); eval(exam6_)
}
# Mean: 5.006678 Variance: 24.57369 
# Mean: 4.873632 Variance: 25.47606  

## Use the latest version at github
## library(devtools)
## install_github("smbache/magrittr")
{ 
  set.seed(6); eval(exam6);
  set.seed(6); eval(exam6_)
}
# Mean: 4.873632 Variance: 25.47606 
# Mean: 4.873632 Variance: 25.47606 
# Warning message:
# In rnorm(1000) %>% multiply_by(5) %>% add(5) %>% function(x) { :
#   Using anonymous functions without enclosing parentheses has been deprecated.
# Current call has been altered, but please change your code.


### Why is `%>%` faster than normal R's syntax in exam1 and exam2?
library("microbenchmark")
microbenchmark(eval(exam1), eval(exam1_))
# Unit: milliseconds
#          expr      min       lq   median       uq      max neval
#   eval(exam1) 10.56467 13.03922 14.90216 17.23116 34.82743   100
#  eval(exam1_) 14.84287 17.21556 20.79522 26.90292 40.33020   100

microbenchmark(eval(exam2), eval(exam2_))
# Unit: milliseconds
#          expr      min       lq   median       uq      max neval
#   eval(exam2) 12.04958 14.04402 16.25512 20.00774 35.67576   100
#  eval(exam2_) 15.87620 18.25558 22.62740 28.08693 42.77801   100

microbenchmark(eval(exam3), eval(exam3_))
# Unit: milliseconds
#          expr      min       lq   median       uq      max neval
#   eval(exam3) 1.708696 1.742130 1.763305 1.798745 3.300596   100
#  eval(exam3_) 1.009259 1.041355 1.053169 1.066542 2.631472   100

microbenchmark(eval(exam4), eval(exam4_))
# Error in eval(expr, envir, enclos) : attempt to apply non-function

microbenchmark(eval(exam5), eval(exam5_))
# Unit: microseconds
#          expr     min       lq  median      uq     max neval
#   eval(exam5) 176.978 221.3335 223.785 226.905 407.895   100
#  eval(exam5_)  10.699  14.2650  14.712  17.386  81.134   100

microbenchmark(eval(exam6), eval(exam6_))
# Unit: microseconds
#          expr      min        lq    median        uq      max neval
#   eval(exam6) 1725.190 1754.3890 2161.1685 2252.7775 3921.128   100
#  eval(exam6_)  498.389  509.5335  624.1005  640.5945 2333.241   100
