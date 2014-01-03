# set environment variables as R_PROFILE_USE = this file path

# set options
local({
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.ism.ac.jp/"
  # r["R-Forge"] <- "http://r-forge.r-project.org/"
  options(repos = r)
  invisible(compiler:::enableJIT(3))
})

options(stringsAsFactors = FALSE)
options(max.print = 1e3)
options(error=quote(utils::dump.frames()))

# Valid values are 25...500000 with default 5000. see ?options and ?Memory
options(expressions = 5e5)
# The command-line option '--max-ppsize' controls the maximum size
# of the pointer protection stack. This defaults to 50000, the maximum value
# accepted is 500000.
# s <- function(n) if (n == 1) 0 else n + s(n - 1)

# --max-ppsize=500000 and options(expressions = 5e5)
# > s(9287)
# [1] 43128827
# > s(9288)
# エラー： C stack usage is too close to the limit

# --vanilla
# >  s(1665)
# [1] 1386944
 # >  s(1666)
# エラー：  評価があまりに深く入れ子になっています。無限の再帰か options(expressions=)？ 

# options(expressions = 5e5)
# > s(6249)
# [1] 19528124
# > s(6250)
# エラー：  protect()：プロテクションスタックが溢れました 

# 
# load functions from github and attach them in the search path.
# load startup packages
local({
  startup.packages <- c("ggplot2", "gridExtra", "reshape2", "microbenchmark",
    "mmap", "ff", "ffbase", "gmp", "compiler", "parallel", "RODBC",
    "data.table", "timeDate", "lubridate", "PerformanceAnalytics",
    "quantmod", "RQuantLib", "Rcpp", "RcppDE", "sos")
  git.dir.url <- "https://raw.github.com/TobCap/R/master"
  download.github <- TRUE
  use.setInternet2 <- TRUE
  
  get.file.github <- function(git.url){
    # returns downloaded local path
    local.path <- file.path(tempdir(), basename(git.url))
    print("downloading files from github")
    if (use.setInternet2) {
      # http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
      # http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
      utils::setInternet2(TRUE)
      utils::download.file(url = git.url, destfile = local.path)
    } else {
      if (!"package:RCurl" %in% search()){
        if (!"RCurl" %in% utils::installed.packages()[,"Package"]) install.packages("RCurl")
        library("RCurl", quitely = TRUE)
      }
      # If you access the Internet via proxy, don't forget to set HTTPS_PROXY in environment variables.
      utils::download.file(url = git.url, destfile = local.path, method = "curl", extra = "-k")
    }
    return(local.path)
  }
  
  attach.file <- function(git.url){
    local.path <- get.file.github(git.url)
    sys.source(local.path, envir = attach(NULL, name = basename(local.path)))
  }
   
  if (download.github){
    attach.file(file.path(git.dir.url, "startFunctions.r"))
    attach.file(file.path(git.dir.url, "functionalProgramming.r"))
    # attach.file(file.path(git.dir.url, "lazyStream.r"))
   }
    
  if (download.github){
    # load.packages() is defined in above "startFunctions.r".
    load.packages(startup.packages)
  } else {
    for(x in startup.packages){
      ## eval(call("library", x, quietly = TRUE))
      library(x, character.only = TRUE, quietly = TRUE)
      utils::flush.console()
    }
  }
  # load.packages("Rbbg", repos = "http://r.findata.org")
  # load.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
  # "dataframe" is removed from CRAN after R 3.0.0; see http://www.timhesterberg.net/r-packages
})
