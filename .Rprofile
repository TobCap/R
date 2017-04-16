# save this file in your HOME directory or set environment variables as R_PROFILE_USE = this file path

# set options
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  # r["R-Forge"] <- "http://r-forge.r-project.org/"
  options(repos = r)
  ## invisible(compiler:::enableJIT(3))
  
  options(stringsAsFactors = FALSE)
  options(max.print = 2e2)
  options(error = quote(utils:::dump.frames()))
  
  # options(
    # warnPartialMatchAttr = TRUE,
    # warnPartialMatchDollar = TRUE,
    # warnPartialMatchArgs = TRUE)
  
  utils:::rc.settings(ipck = TRUE)

})

# s <- function(n) if (n == 0) 0 else 1 + s(n - 1)

# > sessionInfo()
# R version 3.3.3 (2017-03-06)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 14393)

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

  github_master_url <- "https://raw.githubusercontent.com/TobCap/R/master"
  files <- c("start_functions.r",
             "functional_programming.r",
	     "tail-recursion-elimination.r")
  github_files <- file.path(github_master_url, files)
  startup_packages <- c("microbenchmark")
  # startup_packages <- c(
    # "tidyverse", "gridExtra", "microbenchmark",
    # "gmp", "parallel", "RODBC",
    # "data.table", "dtplyr", 
    # "timeDate", "lubridate",
    # "PerformanceAnalytics", "PortfolioAnalytics",
    # "quantmod", "RQuantLib", "Rcpp", "sos")

  inquire <- function(ask, cancel_msg = "") {
    ans <- substr(readline(ask), 1L, 1L)
    utils::flush.console()
    switch(tolower(ans)
      ,y = TRUE
      ,n = FALSE
      ,{cat(cancel_msg, "\n"); FALSE}
    )
  }  
  
  isWindows <- .Platform$OS.type == "windows"
  
  get_file_github <- function(github_url){
    # returns downloaded local path
    local_path <- file.path(tempdir(), basename(github_url))
    print("downloading a file from github")
    if (isWindows) {      
      utils:::download.file(url = github_url, destfile = local_path)
    } else {
      # If you access the Internet via proxy, don't forget to set
      # HTTPS_PROXY in environment variables.
      utils:::download.file(url = github_url, destfile = local_path,
                            method = "curl", extra = "-k")
      # "-k" disables verifying its peer process. see http://curl.haxx.se/docs/sslcerts.html
    }
    return(local_path)
  }
  
  attach_file <- function(github_url){
    local_path <- get_file_github(github_url)
    sys.source(local_path, envir = attach(NULL, name = basename(local_path)))
  }
  
  # dummy <- readline("press Enter Key ") # for RStudio

  download_github <- inquire(
    ask = "  download files from your github? (y/n) ",
    cancel_msg = "quit downloading from github"
  )
  
  load_default_packages <- inquire(
    ask = "  read your default packages? (y/n) ",
    cancel_msg = "canceled"
  )
    
  if (isTRUE(download_github)) {
    for (x in github_files) {
      attach_file(x)
    }
  }
  
  if (isTRUE(load_default_packages)) {
    for (x in startup_packages) {
      if (!require(x, character.only = TRUE)) {
        suppressPackageStartupMessages(
          library(x, character.only = TRUE, quietly = TRUE))
        cat("###", "package", x, "is loaded\n")
        utils:::flush.console()
      }
    }
  }
    
})
