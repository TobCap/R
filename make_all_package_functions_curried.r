make_all_package_functions_curried <- function(suffix = "_c") {
  # special functions that handle language object such as `quote`, `substitute`, or `call` don't work.

  # functions that check a formal parameter w/o default value by `missing` will error; `ls`, `ls.str`

  # `seq.int` will error due to constructing formal parameters inside C-lang level
  # https://github.com/wch/r-source/blob/ed415a8431b32e079100f50a846e4769aeb54d5a/src/main/seq.c#L725-L733

  ex <- c(as.character(sys.call()[[1]]), # caller
          "substitute", "quote", "call", "expression", # special functions for language object
          "bquote", "evalq", "local" # use `substitute` within body
  )

  currying <- function(f_name, env_ = parent.frame()) {
    # `...` is treated as just one paramenter
    stopifnot(is.character(f_name))

    make_body <- function(args_) {
      if (length(args_) == 0) as.call(c(f_sym, f_args_mod))
      else call("function", as.pairlist(args_[1]), make_body(args_[-1]))
    }

    f_sym <- as.symbol(f_name)
    f <- get(f_name, envir = env_)
    f_args <- formals(args(f))
    f_args_mod <- setNames(lapply(names(f_args), as.symbol), names(f_args))

    if (is.null(f_args)) f
    else eval(make_body(f_args), envir = environment(f), enclos = .GlobalEnv)
  }

  env_name <- "curried_funs"
  e <- new.env()

  for (pkg in rev(search())) {
    if (pkg == env_name) {
      detach(env_name, character.only = TRUE)
      next
    }

    pkg_e <- as.environment(pkg)
    funs <- Filter(function(x) is.function(x), as.list(pkg_e))

    funs_name <- names(funs)
    funs_name <- funs_name[which(!funs_name %in% ex)]

    if (length(funs_name) == 0) next

    names_new <- paste0(funs_name, suffix)
    funs_new <- setNames(lapply(funs_name, currying, pkg_e), names_new)
    list2env(funs_new, envir = e)
  }
  attach(e, name = env_name)
}

make_all_package_functions_curried()

# library(dplyr)
# library(nycflights13)
#
# make_all_package_functions_curried()
#
# # you never use S4 class?
# # pipeline-operator for curried functions
# `@` <- function(lhs, rhs, env_ = parent.frame()) {
#   rhs_expr <- substitute(rhs)
#   lhs_expr <- substitute(lhs)
#   eval(as.call(c(rhs_expr, lhs_expr)), env_)
# }
#
# mtcars %>% head(3)
# mtcars @ head_c(3)
#
# mtcars %>% add_rownames %>% filter(mpg > 20 & hp > 10) %>% select(1:5)
# # select(filter(add_rownames(mtcars), mpg > 20 & hp > 10), 1:5)
# mtcars @ add_rownames_c() @ filter_c(mpg > 20 & hp > 10) @ select_c(1:5)
# # select_c(filter_c(add_rownames_c(mtcars)())(mpg > 20 & hp > 10))(1:5)
#
# f1 <- flights %>%
#   group_by(year, month, day) %>%
#   select(arr_delay, dep_delay) %>%
#   summarise(
#     arr = mean(arr_delay, na.rm = TRUE),
#     dep = mean(dep_delay, na.rm = TRUE)
#   ) %>%
#   filter(arr > 30 | dep > 30)
#
# f2 <- flights @
#   group_by_c(year, month, day)() @ # add = FALSE can omit due to having a default value
#   select_c(arr_delay, dep_delay) @
#   summarise_c(
#     arr = mean(arr_delay, na.rm = TRUE),
#     dep = mean(dep_delay, na.rm = TRUE)
#   ) @
#   filter_c(arr > 30 | dep > 30)
#
# identical(f1, f2)
