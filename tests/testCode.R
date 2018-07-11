st_pos <- 6                         #concerned column's start position in the given dataframe
df_bkp <- df                        #data backup

#rename concerned columns as "ay_1718", "ay_1819" etc
names(df)[st_pos:ncol(df)] <- paste("ay", paste0(as.numeric(substr(min(df$cohort), 1, 2)) + 0:(ncol(df) - st_pos),
                                                 as.numeric(substr(min(df$cohort), 3, 4)) + 0:(ncol(df) - st_pos)), 
                                    sep="_")

#copy "year" column's value to the ensuing three columns
cols <- names(df)[st_pos:ncol(df)]  #renamed columns
mapply(function(x, y) 
  df[df$cohort == x & df$studentID == y, which(grepl(x, cols)) + (st_pos-1):(st_pos+2)] <<- 
    df[df$cohort == x & df$studentID == y, which(grepl(x, cols)) + (st_pos-1)],
  df$cohort, df$studentID)



packrat::unbundle(bundle = "c:/users/gfalk/OneDrive/R_Performance.3.3.0/R_Performance.3.3..tar.gz", where="c:/users/gfalk")
packrat::unbundle(bundle = "C:/Users/gfalk/Downloads/R_Performance.3.3.0-2018-05-03.tar", where="C:/Users/gfalk/R_Performance.3.3.0/")


sapply(tbl.scholar2, data.table::uniqueN)
unique(tbl.scholar2[,4])
length(unique(tbl.scholar2[,4]))
tbl.scholar2[c(4,6)]
which(ay$V1 == currentAY)                                                                           # get the row name/number/index
tbl.scholar2[,which(ay$V1 == currentAY)]                                                            # select data.table column dynamically by index number

a<-as.character(rep(head(ay$V1,1),4))
b<-head(paste0("ay_", ay$V2),4)
tbl.scholar2[which(ay$V1 == currentAY)+6]
tbl.scholar2<-dup.DF(tbl.scholar2, currentAY, 4, "ay_1718", "ay_2324")

c<-paste0("ay_",ay[[which(ay$V1 == currentAY),which(ay$V1 == currentAY)]])

df %>%
  rowwise() %>%
  mutate(cohort_plus3 = paste(paste("AY", 
                                    paste0(as.numeric(substr(cohort, 1, 2)) + 0:3, 
                                           as.numeric(substr(cohort, 3, 4)) + 0:3), sep = "_"), 
                              collapse = ",")) %>%
  separate_rows("cohort_plus3", sep = ",") %>%
  mutate(flag = 1) %>%
  spread(cohort_plus3, flag, fill = 0) %>%
  data.frame()


tbl.scholar2 %>%
    rowwise() %>%
    mutate(cohort_plus3 = paste(paste("AY", 
                                      paste0(as.numeric(substr(cohort, 1, 2)) + 0:3, 
                                             as.numeric(substr(cohort, 3, 4)) + 0:3), sep = "_"), 
                                collapse = ",")) %>%
    separate_rows("cohort_plus3", sep = ",") %>%
    mutate(flag = 1) %>%
    spread(cohort_plus3, flag, fill = 0) %>%
    data.frame()



cohort_plus3 <- paste(paste("AY", 
                           paste0(as.numeric(substr(tbl.scholar2$cohort, 1, 2)) + 0:3, 
                                  as.numeric(substr(tbl.scholar2$cohort, 3, 4)) + 0:3), sep = "_"), 
                     collapse = ",")





fundBalance<-tbl.GaCommitmentFundList.OSFA %>% 
    group_by(fundCode, fundName) %>%
    select(fundCode, fundName, incomeBalance, principleBalance, invested.Balance) %>%
    summarize(incBal=sum(incomeBalance), principleBal=sum(principleBalance), invBal=sum(invested.Balance)) %>%
    mutate(totBal = incBal+principleBal+invBal) %>% 
    arrange(fundCode, fundName)

tbl.studentBalance<-tbl.BannerCohorts %>% 
    group_by(osfaCode) %>%
    select(osfaCode, AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425 ) %>%
    summarize(totBalStudent=sum(AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425)) %>%
    arrange(osfaCode)

tbl.Balance<-tbl.Balance%>% 
    group_by(osfaCode) %>%
    select(osfaCode, totBalFund, totBalStudent) %>%
    mutate(totBal = totBalFund - totBalStudent) %>%
    arrange(osfaCode)

tbl.Balance    

([\sI])\w+

pattern <- "([\\sI])"
trex<-grep(pattern, zfundcode, value=TRUE)

tfundcode <- fundCode
as.data.table(tfundcode)
tfundcode<-tfundcode[tbl.GaCommitmentFundList.OSFA,mult = "first",on = "fundCode", nomatch=0]
tfundcode<-tfundcode[, c(1,4)]
tfundcode<unique(tfundcode)


zfundcode <- fundCode
as.data.table(zfundcode)
zfundcode<-zfundcode[tbl.GaCommitmentFundList.OSFA,mult = "first",on = "fundCode", nomatch=0]
zfundcode<-zfundcode[, c(1,4)]
zfundcode<-zfundcode[order(fundCode, fundName)][, .SD[c(1)], by=fundCode]

zfundcode$fundName<-gsub(pattern = "[IV|X]|\\(1\\)|\\(2\\)", "", zfundcode$fundName)
zfundcode$fundName<-trim(zfundcode$fundName)
zfundcode$fundName

zfundcode < left_join(zfundcode,jctCode, by = "fundCode")

tbl.JctCode<- left_join(zfundcode, jctCode, by = "fundCode")

tDF.JctCodeA <-distinct(tbl.GaCommitmentFundList.OSFA, fundCode)

tDF.JctCodeA <-unique(tbl.GaCommitmentFundList.OSFA[c("fundCode")])
tDF.JctCodeB <-unique(tbl.GaCommit[c("fundCode", "osfaCode")])
tDF.JctCodeC <-left_join(tDF.JctCodeA, tDF.JctCodeB)


tDF.JctCodeC <-tDF.JctCodeA[tDF.JctCodeB, bb := tDF.JctCodeB.osfaCode]

band_members %>% left_join(band_instruments)


t.dfJctCodeC <-distinct(tbl.GaCommitmentFundList.OSFA, osfaCode, fundCode) 
#jctCode
jctCode <- c(tbl.scholarshipCohorts[,4], tbl.scholarshipCohorts[,3])                                            #concatenate jctCode columns
names(jctCode) <- c("fundCode", "osfaCode")                                                                     #rename columns
jctCode <- as.data.table(jctCode)                                                                               #convert to a data.table
jctCode <- subset(jctCode, fundCode!=0 & osfaCode !=0)                                                          #remove rows containing zero
jctCode <- unique(jctCode)                                                                                      #extract the unique values
jctCode <- setorder(jctCode, fundCode)                                                                          #sort the fundCode column

#Join three Tables - use `nomatch` argument-> i.e., nomatch-0
tbl.fundsByUnit<-tbl.BannerCohorts[jctCode,nomatch=0,on=c("osfaCode")][tbl.GaCommitmentFundList.OSFA,nomatch=0,on=c("fundCode")]
# dplyr three table joinleft_join(x, y, by='Flag') %>%
tbl.fundsByUnit<-left_join(tbl.BannerCohorts, jctCode, by='osfaCode') %>%
    left_join(., tbl.GaCommitmentFundList.OSFA, by='fundCode') 


unique(df[,4])
g<-data.table(df)  %>% group_by(cohort) %>% arrange(cohort)

df%>%
    group_by(cohort)%>%
    nest()
df2<-data.table(df)
df1<-split(df2,df$cohort)
lapply(names(df1),
       function(x)write.csv(df1[x],
                            file =paste0(x,'.csv')))



df1<-split(df,df$cohort)
df%>%
    group_by(cohort)%>%
    nest()%>%
    lapply(names(df),
       function(x)write.xlsx(as.data.frame(df[x]),
                            file ="output/scholar2.xlsx",
                            sheetName=x,
                            append=TRUE))

3
4
 
tbl.scholar %>% 
    select(cohort , degree)%>%
    group_by(cohort)%>%
    count(degree)



    chart.GaCommitByZip <- data.table(df) %>%
    group_by(zip) %>%
    rename(zip5=zip)

lori_1819data <- 
    lori_1819data %>% 
    filter(str_detect(Recipient.Student.ID.., "^8"))

    styler::style_file( "C:/Users/gfalk/Documents/ScholarshipProjectPackrat/R/ScholarshipProject/munge/01-C.R")
    styler::style_file( "S:/ScholarshipsProject/R/ScholarshipProject/munge/01-C.R")

tbl.scholar2$`1718`$studentid

# https://stackoverflow.com/questions/21888910/how-to-specify-names-of-columns-for-x-and-y-when-joining-in-dplyr
anti1718a<-anti_join(lori_1718data, tbl.scholar2[["1718"]], by = c("Recipient.Student.ID.." = "studentid"))
anti1718b<-anti_join(tbl.scholar2[["1718"]],lori_1718data, by = c("studentid"="Recipient.Student.ID.."))
anti1819<-anti_join(lori_1819data, tbl.scholar2[["1819"]], by = c("Recipient.Student.ID.." = "studentid"))

common<-intersect(names(anti1718a), names(anti1718b))
rbind(anti1718a[,common], anti1718b[,common])