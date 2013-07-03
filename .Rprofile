# set options
local({
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.ism.ac.jp/"
  #r["R-Forge"] <- "http://r-forge.r-project.org/"
  options(repos = r)
})

options(stringsAsFactors = FALSE)
options(max.print = 1e3)
options(error=quote(utils::dump.frames()))

# Valid values are 25...500000 with default 5000. see ?options and ?Memory
options(expressions = 5e5)
# The command-line option '--max-ppsize' controls the maximum size
# of the pointer protection stack. This defaults to 50000, the maximum value
# accepted is 500000.
# s <- function(n) if(n == 1) 0 else n + s(n - 1)

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
# load functions from github and attach them in search path.
local({
  attach.file <- (function(){
    git.dir <- "https://raw.github.com/TobCap/R/master"
    
    if (!Sys.getenv("HTTPS_PROXY") == ""){
      if(!"RCurl" %in% utils:::installed.packages()[,"Package"]) install.packages("RCurl")
      if(!"package:RCurl" %in% search()) library("RCurl")
    }
    
    get.file.github <- function(git.dir, file.name){
      gitUrl <- file.path(git.dir, file.name)
      local.path <- file.path(tempdir(), file.name)
      
      print("downloading functions from github");
      if (!Sys.getenv("HTTPS_PROXY") == "") {
        # If you access the Internet via proxy, don't forget to set HTTPS_PROXY in environment variables.
        utils:::download.file(url = gitUrl, destfile = local.path, method = "curl", extra = "-k")
      } else {
        utils:::download.file(url = gitUrl, destfile = local.path)
      }
      return(local.path)
	  }
    return(function(file.name){
      sys.source(get.file.github(git.dir, file.name), envir = attach(NULL, name = file.name))
    })
  })()
  
  attach.file("startFunctions.r")
  attach.file("functionalProgramming.r")
  # load.packages is defined in above "startFunctions.r".
  # load.packages(c("ggplot2", "gridExtra", "reshape2", "microbenchmark", "rbenchmark", 
    # "mmap", "ff", "ffbase", "gmp", "compiler", "doSNOW", "RODBC", "data.table", "timeDate", "lubridate",
    # "sos", "PerformanceAnalytics", "quantmod", "DEoptim", "RQuantLib", "Rbbg"))
  # load.packages("Rbbg", repos = "http://r.findata.org")
  # load.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
  # "dataframe" is not permitted no CRAN from R 3.0.0; see http://www.timhesterberg.net/r-packages	
})
