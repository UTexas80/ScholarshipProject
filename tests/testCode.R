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




