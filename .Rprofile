# save this file in your HOME directory or set environment variables as R_PROFILE_USE = this file path

# set options
local({
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.ism.ac.jp/"
  # r["R-Forge"] <- "http://r-forge.r-project.org/"
  options(repos = r)
  ## invisible(compiler:::enableJIT(3))
  
  options(stringsAsFactors = FALSE)
  options(max.print = 1e2)
  options(error=quote(utils::dump.frames()))
})


# s <- function(n) if (n == 1) 0 else n + s(n - 1)

# > sessionInfo()
# R version 3.0.2 (2013-09-25)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# --vanilla
# > s(1665)
# [1] 1386944
# > s(1666)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?

options(expressions = 5e5)
# Valid values are 25...500000 with default 5000. see ?options and ?Memory
# Cstack_info()

# options(expressions = 5e5)
# > s(6249)
# [1] 19528124
# > s(6250)
# Error: protect(): protection stack overflow

# The command-line option '--max-ppsize' controls the maximum size
# of the pointer protection stack. This defaults to 50000, the maximum value
# accepted is 500000.

# --max-ppsize=500000 and options(expressions = 5e5)
# > s(26040)
# [1] 339053819
# > s(26041)
# Error: C stack usage is too close to the limit


# load functions from github and attach them in the search path.
# load startup packages
local({
  git.dir.url <- "https://raw.github.com/TobCap/R/master"
  files <- c("startFunctions.r", "functionalProgramming.r", "tailCallOptimization.r")
  git.files <- file.path(git.dir.url, files)
  startup.packages <- c(
    "ggplot2", "gridExtra", "reshape2", "microbenchmark",
    "mmap", "ff", "ffbase", "gmp", "compiler", "parallel", "RODBC",
    "data.table", "timeDate", "lubridate", "PerformanceAnalytics",
    "quantmod", "RQuantLib", "Rcpp", "RcppDE", "sos")
  
  answer <- substr(readline("download files in your github (y/n)? "), 1L, 1L)
  utils::flush.console()
  
  download.github <- 
    switch(tolower(answer)
      , y = TRUE
      , n = FALSE
      , {cat("quit downloading from github"); FALSE}
    )  
  
  isWindows <- .Platform$OS.type == "windows"
  if (isWindows) utils:::setInternet2(TRUE)
  
  get.file.github <- function(git.url){
    # returns downloaded local path
    local.path <- file.path(tempdir(), basename(git.url))
    print("downloading a file from github")
    if (isWindows) {      
      utils:::download.file(url = git.url, destfile = local.path)
    } else {
      # http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
      # http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
      rawfile <- sub("https://github.", "https://raw.github.",  git.url)
      # If you access the Internet via proxy, don't forget to set HTTPS_PROXY in environment variables.
      utils:::download.file(url = rawfile, destfile = rawfile, method = "curl", extra = "-k")
      # "-k" disables verifying its peer process. see http://curl.haxx.se/docs/sslcerts.html
    }
    return(local.path)
  }
  
  attach.file <- function(git.url){
    local.path <- get.file.github(git.url)
    sys.source(local.path, envir = attach(NULL, name = basename(local.path)))
  }
   
  if (download.github){
    for (x in git.files) {
      attach.file(x)
    }
    # load.packages() is defined in above "startFunctions.r"
    load.packages(startup.packages)
  } else {
    for (x in startup.packages){
      suppressPackageStartupMessages(library(x, character.only = TRUE, quietly = TRUE))
      cat("###", "package", x, "is loaded\n")
      utils:::flush.console()
    }
  }
    
  # load.packages("Rbbg", repos = "http://r.findata.org")
  # load.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
})
