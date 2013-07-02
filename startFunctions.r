#
load.packages <- function(packageNames, repos = getOption("repos"), lib.loc = NULL){
  stopifnot(is.character(packageNames))
  loaded.package <- sub("package:", "", search())
  local.lib <- .packages(all.available = TRUE, lib.loc = lib.loc)
  CRAN.packages <- suppressWarnings(utils::available.packages(contriburl = utils::contrib.url(repos))[, 1])
  w <- function(...) print(paste(...))

	for(pkg in packageNames){
    if(pkg %in% loaded.package) {
      w(pkg, "already loaded")
    } else if(pkg %in% local.lib){
      suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, verbose = FALSE))
      w(pkg, "loaded")
    } else if(length(CRAN.packages) == 0){
      w(pkg, "is not found in local and cannot access to CRAN")
    } else if(!(pkg %in% CRAN.packages)) {    
      w(pkg, "is not found in local and not listed in current CRAN")
    } else {
      utils::install.packages(pkg, dependencies = TRUE)
      suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, verbose = FALSE))
      w(pkg, "is newly installed and loaded")
    }
		utils::flush.console()
	}
}

# refers to http://ofmind.net/doc/r-tips
read.cb <- function(header=TRUE, ...){
  utils::read.table(file = "clipboard", header = header, ...)
}
write.cb <- function(data, sep = "\t", header = TRUE, row.names = FALSE, 
  col.names=ifelse(header && row.names, NA, header), qmethod="double", ...){
  utils::write.table(data, file = "clipboard", sep = sep, row.names = row.names,
  col.names = col.names, qmethod = qmethod, ...)
}

# utils
makeActiveBinding("Q", q, env = as.environment("myFuns"))
# myFuns is assigned by .Rprofile
# class(Q) <- Q <- "ask"; print.ask <- q
`%!in%` <- Negate(`%in%`)
load.packages("plyr"); lapply.seq <- plyr:::loop_apply

rm.variables <- function(env = .GlobalEnv) {
  rm(list = setdiff(utils::ls.str(envir = env), utils::lsf.str(envir = env)), envir= env)
}
rm.functions <- function(env = .GlobalEnv) {
  rm(list = utils::lsf.str(all = TRUE, envir = env), envir = env)
}
rm.all <- function(env = .GlobalEnv) {
  rm(list = ls(all = TRUE, envir = env), envir = env)
}
