quandl_get <- function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
        paste0("WIKI/", sym, ".8"),   # Adj. Open
        paste0("WIKI/", sym, ".9"),   # Adj. High
        paste0("WIKI/", sym, ".10"),  # Adj. Low
        paste0("WIKI/", sym, ".11"),  # Adj. Close
        paste0("WIKI/", sym, ".12")), # Adj. Volume
        start_date = start_date,
        type = "zoo"
    ))
}

f <- function() {
  cat("Hello, world!\n")
}

initdata <- function() {

cat("Hello, initdata!\n")

# Up-to-date at time of writing (May 2016)
# S <- as.character(read.csv(SP500, header = FALSE)[,1])
S <- c("MMM", "ACN", "BLK", "HRB", "BWA", "BF-B")
# Changing stocks names. Because Quandl reads stocks with "_" instead of "-"
S <- gsub("-", "_", S)
# S <- gsub("-", "_", SP500)

# save S.R with stock list from url
dump(list = "S", "S.R")

if("S.R" %in% list.files ()) {
  source ("S.R")
} else {
  url <- "http://trading.chrisconlan.com/SPstocks.csv"
  S <- as.character ( read.csv (url, header = FALSE)[,1])
  dump (list = "S", "S.R")
  }

# Load "invalid.R" file if available 
# invalid <- character(0)
setwd(root)
if("invalid.R" %in% list.files()) source("invalid.R")

# Find all symbols not in directory and not missing
setwd(datadir)
toload <- setdiff(S[!paste0(S, ".csv") %in% list.files()], invalid)

# Fetch symbols with yahoo function, save as .csv or missing
yahoo(S)

if(length(toload) != 0){
  for(i in 1:length(toload)){
    
  df <- yahoo(toload[i])
  
  if(!is.null(df)) {
    write.csv(df[nrow(df):1], file = paste0(toload[i], ".csv"),
              row.names = FALSE) 
  } else {
    invalid <- c(invalid, toload[i])
  }
    
}
}

setwd(root)
dump(list = c("invalid"), "invalid.R")

setwd(datadir) 

# S <- sub(".csv", "", list.files())
S <- Sys.glob("*.csv")
S <- sub(".csv", "", S)         

require(data.table)

DATA <- list()
for(i in S){
  suppressWarnings(
  DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- (DATA[[i]])[order(DATA[[i]][["Date"]], decreasing = FALSE)]
} 



cat("Goodbye, initdata!\n")

}


# Listing 2.2
yahoo <- function(S, current = TRUE,
                  a = 0, b = 1, c = 2000, d, e, f,
                  g = "d")
{

    cat("Hello, Yahoo!\n")

  if(current){
    f <- as.numeric(substr(as.character(Sys.time()), start = 1, stop = 4))
    d <- as.numeric(substr(as.character(Sys.time()), start = 6, stop = 7)) - 1
    e <- as.numeric(substr(as.character(Sys.time()), start = 9, stop = 10))
  }

  require(data.table)

  tryCatch(
  suppressWarnings(
  fread(paste0("http://ichart.yahoo.com/table.csv",
               "?s=", sym,
               "&a=", a,
               "&b=", b,
               "&c=", c,
               "&d=", d,
               "&e=", e,
               "&f=", f,
               "&g=", g,
               "&ignore=.csv"), sep = ",")),
  error = function(e) NULL
  )

cat("Goodbye, Yahoo!\n")
}

# Listing 2.5
loadToMemory <- function(S)
{
# S <- sub(".csv", "", list.files() )


DATA <- list()
for(i in S){
  suppressWarnings(
  DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- (DATA[[i]])[order(DATA[[i]][["Date"]], decreasing = FALSE)]
  } 
}


# https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows/34096422
mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
  # Initialize any new variables as new_init
  new_vars <- substitute(list(...))[-1]
  new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
  .data[, new_vars] <- new_init

  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
  .data
}