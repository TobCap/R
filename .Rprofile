# save this file in your HOME directory or set environment variables as R_PROFILE_USE = this file path

# set options
local({
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.ism.ac.jp/"
  # r["R-Forge"] <- "http://r-forge.r-project.org/"
  options(repos = r)
  ## invisible(compiler:::enableJIT(3))
  
  options(stringsAsFactors = FALSE)
  options(max.print = 2e2)
  options(error = quote(utils:::dump.frames()))
  
  options(
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchArgs = TRUE)
  
  utils:::rc.settings(ipck = TRUE)

})

# s <- function(n) if (n == 0) 0 else 1 + s(n - 1)

# > sessionInfo()
# R version 3.1.2 (2014-10-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# --vanilla
# > s(1664)
# [1] 1664
# > s(1665)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?

options(expressions = 5e5)
# Valid values are 25...500000 with default 5000. see ?options and ?Memory
# Cstack_info()

# options(expressions = 5e5)
# > s(6248)
# [1] 6248
# > s(6249)
# Error: protect(): protection stack overflow

# The command-line option '--max-ppsize' controls the maximum size
# of the pointer protection stack. This defaults to 50000, the maximum value
# accepted is 500000.

# --max-ppsize=500000 and options(expressions = 5e5)
# > s(25056)
# [1] 25056
# > s(25057)
# Error: C stack usage  63754052 is too close to the limit

# load functions from github and attach them in the search path.
# load startup packages
local({
  git_dir_url <- "https://raw.github.com/TobCap/R/master"
  files <- c("start_functions.r", "functional_programming.r", "tail_call_optimization.r")
  git_files <- file.path(git_dir_url, files)
  startup_packages <- c("microbenchmark")
  # startup_packages <- c(
    # "ggplot2", "gridExtra", "reshape2", "microbenchmark",
    # "mmap", "ff", "ffbase", "gmp", "compiler", "parallel", "RODBC",
    # "data.table", "timeDate", "lubridate", "PerformanceAnalytics",
    # "quantmod", "RQuantLib", "Rcpp", "RcppDE", "sos")

  inquire <- function(ask, cancel_msg = "") {
    ans <- substr(readline(ask), 1L, 1L)
    utils::flush.console()
    switch(tolower(ans)
      ,y = TRUE
      ,n = FALSE
      ,{cat(cancel_msg, "\n"); FALSE}
    )
  }

  dummy <- readline("press Enter Key ") # for RStudio
  
  download_github <- inquire(
    ask = "  download files from your github? (y/n) ",
    cancel_msg = "quit downloading from github"
  )
  
  load_default_packages <- inquire(
    ask = "  read your default packages? (y/n) ",
    cancel_msg = "canceled"
  )
  
  isWindows <- .Platform$OS.type == "windows"
  if (isWindows) utils:::setInternet2(TRUE)
  
  get_file_github <- function(git_url){
    # returns downloaded local path
    local_path <- file.path(tempdir(), basename(git_url))
    print("downloading a file from github")
    if (isWindows) {      
      utils:::download.file(url = git_url, destfile = local_path)
    } else {
      # http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
      # http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
      rawfile <- sub("https://github.", "https://raw.github.",  git_url)
      # If you access the Internet via proxy, don't forget to set HTTPS_PROXY in environment variables.
      utils:::download.file(url = rawfile, destfile = rawfile, method = "curl", extra = "-k")
      # "-k" disables verifying its peer process. see http://curl.haxx.se/docs/sslcerts.html
    }
    return(local_path)
  }
  
  attach_file <- function(git_url){
    local_path <- get_file_github(git_url)
    sys.source(local_path, envir = attach(NULL, name = basename(local_path)))
  }
   
  if (download_github){
    for (x in git_files) {
      attach_file(x)
    }
  }
  
  if (load_default_packages) {
    # load.packages() is defined in above "start_functions.r"
    if (exists("load.packages")) load.packages(startup_packages)
    else {
      for (x in startup_packages){
        suppressPackageStartupMessages(library(x, character.only = TRUE, quietly = TRUE))
        cat("###", "package", x, "is loaded\n")
        utils:::flush.console()
      }
    }
  }
    
  # load.packages("Rbbg", repos = "http://r.findata.org")
  # load.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
})
