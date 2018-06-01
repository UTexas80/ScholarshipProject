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

 my.function <- function(data, col){
   data[, col]
 }

dup.DF <- function(df1, currAY, colIdx) {
  #colnames(df1[colIdx])
  #print(currAY)
#  df1%>%mutate_cond(cohort == currAY, ay_2223=ay_1718)
  df1%>%mutate_cond(colnames(df1[colIdx]) == currAY, ay_2223=ay_1718)                               
  df1%>%mutate_cond(cohort == currAY, ay_2324=ay_1718)                                              #This works
# -df1[, col]
#  tbl.scholar1<-tbl.scholar1%>%mutate_cond(cohort == currentAY, ay_1819=ay_1718, ay_1920=ay_1718, ay_2021=ay_1718)
}