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

rename.columns=function(df,changelist){                                                                                                     # https://is.gd/09I8gp
  #renames columns of a dataframe
  for(i in 1:length(names(df))){
    if(length(changelist[[names(df)[i]]])>0){
      names(df)[i]= changelist[[names(df)[i]]]
    }
  }
  df
}

mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {                                                     # https://is.gd/pFRUS5
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

my.otherFun <- function(dt ) {
  dt %>% filter(cohort == "water")

}

f <- function(dt, ...) {                                                                                                                    # function to select columns
  dt %>% filter(cohort == currentAY) %>% select(...) %>% mutate_at(vars(1:2), funs(change))
}

# https://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/ ####
rep.row<-function(x,n){
   matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

dup.DF <- function(df1, currAY, colIdx, name1,name2) {

  df1%>%mutate_cond(cohort == currAY, UQ(rlang::sym(name2)) :=  UQ(rlang::sym(name1)))              #This works!!!!  
  print(colnames(df1[colIdx]))
  # print(currAY)
  # print(name1)
  # print(name2)
  
#  name1 <- quo(name1)
#  name2 <- quo(name2)
  
#  print(name1)
#  print(name2)
  
#  expr1 <- enquo(name1)
#  expr2 <- quo(name2)

#  expr1 <- substitute(name1)
#  expr2 <- substitute(name2)

#  print (expr1)
#  print (expr2)
  # name2 <- enquo(name2)  
  #print(name2)
  # name1 <-quo(name1)
#  name1 <- quo(name1)
#  df1%>%mutate_cond(cohort == currAY, ay_2223=ay_1718)
#  df1%>%mutate_cond(colnames(df1[colIdx]) == currAY, ay_2223=ay_1718)                               
#  df1%>%mutate_cond(cohort == currAY, ay_2324 =!! name1)                                           #This works

# -df1[, col]0
#  tbl.scholar1<-tbl.scholar1%>%mutate_cond(cohort == currentAY, ay_1819=ay_1718, ay_1920=ay_1718, ay_2021=ay_1718)
}
 
