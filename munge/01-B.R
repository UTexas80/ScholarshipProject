# Example preprocessing script.
require(stats)

data_files <- file.info(Sys.glob("y:/Reports/Banner/Argos/Test/*.csv"))
row.names(data_files)[which.max(data_files[["ctime"]])]                                                         #find the most recent version
#import .csv file
tbl.scholar<-read.csv(row.names(data_files)[which.max(data_files[["ctime"]])], header=TRUE, sep=",", colClasses=c("Date", rep("character",2),"numeric","character", rep("numeric",8)))
#tbl.scholar$spriden_id  <- as.character(tbl.scholar$spriden_id)                                                #convert spriden to character
names(tbl.scholar)[2] <- "studentID"                                                                            #rename to studentID
names(tbl.scholar)[5] <- "fundCode"                                                                             #rename to studentID
#tbl.scholar$SYSDATE <- as.Date(as.character(tbl.scholar$SYSDATE), "%m/%d/%Y")                                  #convert SYSDATE to date only
tbl.scholar[ is.na(tbl.scholar) ] <- 0                                                                          #replace na's with 0

#funcCode
fundCode <- c(tbl.Cancels[,9], tbl.scholarshipAwd[,4], tbl.scholarshipCohorts[,4], tbl.GaCommitmentFundList.OSFA[,3])   #concatenate fundCode columns
fundCode <- stack(fundCode)                                                                                     #stack columns on top of each other
fundCode <- fundCode[,1]                                                                                        #extract the first column only
fundCode <- as.data.table(fundCode)                                                                             #convert to a data.table
fundCode <- subset(fundCode, fundCode!=0)                                                                       #remove rows containing zero
fundCode <- unique(fundCode)                                                                                    #extract the unique values
fundCode <- setorder(fundCode, fundCode)                                                                        #sort the fundCode column
fundCode <- fundCode %>% left_join(select(tbl.GaCommitmentFundList.OSFA, fundCode, fundName), by.x = "fundCode")
fundCode$fundName[is.na(fundCode$fundName)] <- ""                                                               #replace na's with blanks in fund name

#jctCode
jctCode <- c(tbl.scholarshipCohorts[,4], tbl.scholarshipCohorts[,3])                                            #concatenate jctCode columns
names(jctCode) <- c("fundCode", "osfaCode")                                                                     #rename columns
jctCode <- as.data.table(jctCode)                                                                               #convert to a data.table
jctCode <- subset(jctCode, fundCode!=0 & osfaCode !=0)                                                          #remove rows containing zero
jctCode <- unique(jctCode)                                                                                      #extract the unique values
jctCode <- setorder(jctCode, fundCode)                                                                          #sort the fundCode column
jctCode<-jctCode %>% left_join(select(fundCode, fundCode, fundName), by.x = "fundCode")                         #add the fundName column

#jctCohortCode
jctCohortCode <- c(tbl.BannerCohorts[,2], tbl.BannerCohorts[,5])                                                #concatenate Banner Cohort columns
names(jctCohortCode) <- c("studentID", "fundCode")                                                              #rename columns
jctCohortCode <- as.data.table(jctCohortCode)                                                                   #convert to a data.table
jctCohortCode <- subset(jctCohortCode, fundCode!=0 & studentID !=0)                                             #remove rows containing zero
jctCohortCode <- unique(jctCohortCode)                                                                          #extract the unique values
jctCohortCode <- setorder(jctCohortCode, studentID)                                                             #sort the studentID column

#osfaCode
osfaCode <- c(tbl.BannerCohorts[,5], tbl.scholarshipAwd[,3], tbl.scholarshipCohorts[,3])                        #concatenate fundCode columns
osfaCode <- stack(osfaCode)                                                                                     #stack columns on top of each other
osfaCode <- osfaCode[,1]                                                                                        #extract the first column only
names(osfaCode) <- c("osfaCode")                                                                                #rename columns
osfaCode <- as.data.table(osfaCode)                                                                             #convert to a data.table
osfaCode <- subset(osfaCode, osfaCode!=0)                                                                       #remove rows containing zero
osfaCode <- unique(osfaCode)                                                                                    #extract the unique values
osfaCode <- setorder(osfaCode, osfaCode)                                                                        #sort the osfaCode column

#osfaCode
studentID <- c(tbl.BannerCohorts[,2], tbl.Cancels[,5], tbl.scholarshipAwd[,5], tbl.scholarshipCohorts[,11])     #concatenate studentID columns
studentID <- stack(studentID)                                                                                   #stack columns on top of each other
studentID <- studentID[,1]                                                                                      #extract the first column only
studentID <- as.data.table(studentID)                                                                           #convert to a data.table
studentID <- subset(studentID, studentID!=0)                                                                    #remove rows containing zero
studentID <- unique(studentID)                                                                                  #extract the unique values
studentID <- setorder(studentID, studentID)                                                                     #sort the studentID column

#Join three Tables - use `nomatch` argument-> i.e., nomatch-0
tbl.CohortAwd<-tbl.BannerCohorts[tbl.scholarshipAwd,nomatch=0,on=c("studentID", "osfaCode")][tbl.scholarshipCohorts,nomatch=0,on=c("studentID", "osfaCode")]
# tbl.CohortAwd<-tbl.BannerCohorts[tbl.scholarshipAwd, nomatch=0, on=c("studentID", "osfaCode")]                #Join Banner Cohorts + scholarshipAwd tables
tbl.CohortAwd$Award <- as.numeric(as.character(tbl.CohortAwd$Award))                                            #covert CohortAwd$Award from character to numeric

tbl.CohortAwdByFY <- data.table(tbl.CohortAwd)[,.(FY, Award)]
tbl.CohortAwdByFY[,meanAwdByFY:=mean(Award), by=FY]

x<-tbl.CohortAwd %>% select(FY, Award) %>%
    group_by(FY) %>%
    dplyr::summarize(AvgAward = mean(Award),
                     MedianAward = as.numeric(median(Award)),
                     total = n()) %>%
    arrange(FY)

tbl.GaCommit <- tbl.CohortAwd %>% 
select(FY, osfaCode, fundCode, AccountName, Requirement, Active, Award, MajorChanges, ReplacementScholar, studentID, NAME, AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324,AY_2425, OSFA.Notes, Rural, FundAgreement, message, eMail, Special.Circumstances, SCR.Complete ) %>%
group_by(FY, osfaCode, studentID) %>%
mutate_all(funs(replace(., is.na(.), 0)))

tbl.fundBalance<-tbl.GaCommitmentFundList.OSFA %>% 
    group_by(fundCode, fundName) %>%
    select(fundCode, fundName, incomeBalance, principleBalance, invested.Balance) %>%
    summarize(incBal=sum(incomeBalance), principleBal=sum(principleBalance), invBal=sum(invested.Balance)) %>%
    mutate(totBalFund = incBal+principleBal+invBal) %>% 
    arrange(fundCode, fundName)

tbl.fundBalance<-inner_join(tbl.fundBalance, jctCode)

tbl.BannerCohorts[ is.na(tbl.BannerCohorts) ] <- 0
tbl.BannerCohorts %>% 
    mutate_at(vars(AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425), as.numeric)

tbl.studentBalance<-tbl.BannerCohorts %>% 
    group_by(osfaCode) %>%
    select(osfaCode, AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425 ) %>%
    summarize(totBalStudent=sum(AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425)) %>%
    arrange(osfaCode)

tbl.balance<- inner_join(tbl.fundBalance, tbl.studentBalance, by = "osfaCode")

#write output .csv files
write.csv(fundCode, "output/tbl_fundCode.csv", row.names=F)
write.csv(jctCode, "output/jct_Code.csv", row.names=F)
write.csv(jctCohortCode, "output/jct_CohortCode.csv", row.names=F)
write.csv(osfaCode, "output/tbl_osfaCode.csv", row.names=F)
write.csv(studentID, "output/tbl_studentId.csv", row.names=F)

write.csv(tbl.balance, "output/tbl_balance.csv", row.names=F)
write.csv(tbl.CohortAwd, "output/tbl_CohortAwd.csv", row.names=F)
write.csv(tbl.fundBalance, "output/tbl_fundBalance.csv", row.names=F)
write.csv(tbl.GaCommit, "output/tbl_GaCommit.csv", row.names=F)
write.csv(tbl.scholar, "output/tbl_scholar.csv", row.names=F)
write.csv(tbl.studentBalance, "output/tbl_studentBalance.csv", row.names=F)

#write output .xlsx files
write.xlsx(fundCode, "output/tbl_fundCode.xlsx", row.names=F, sheetName="tblFundCode", append=FALSE)
write.xlsx(jctCode, "output/tbl_jctCode.xlsx", row.names=F, sheetName="tbl_jctCode")
write.xlsx(jctCohortCode, "output/tbl_jctCohortCode.xlsx", row.names=F, sheetName="tbl_jctCohortCode")
write.xlsx(osfaCode, "output/tbl_osfaCode.xlsx", row.names=F, sheetName="tbl_osfaCode")
write.xlsx(studentID, "output/tbl_studentID.xlsx", row.names=F, sheetName="tbl_studentID")

write.xlsx(as.data.frame(tbl.balance), "output/tbl_balance.xlsx", row.names=F, sheetName="tbl_balance")
write.xlsx(as.data.frame(tbl.BannerCohorts), "output/tbl_bannerCohorts.xlsx", row.names=F, sheetName="tbl_bannerCohorts")
write.xlsx(as.data.frame(tbl.CohortAwd), "output/tbl_cohortAwd.xlsx", row.names=F, sheetName="tbl_cohortAwd")
write.xlsx(as.data.frame(tbl.fundBalance), "output/tbl_fundBalance.xlsx", row.names=F, sheetName="tbl_fundBalance")
write.xlsx(as.data.frame(tbl.GaCommit), "output/tbl_GaCommit.xlsx", row.names=F, sheetName="tbl_GaCommit")
write.xlsx(as.data.frame(tbl.GaCommitmentFundList.OSFA), "output/tbl_GaCommitFundList.xlsx", row.names=F, sheetName="tbl_GaCommitFundList")
write.xlsx(tbl.scholar, "output/tbl_scholar.xlsx", row.names=F, sheetName="tbl_scholar")
write.xlsx(as.data.frame(tbl.studentBalance), "output/tbl_studentBalance.xlsx", row.names=F, sheetName="tbl_studentBalance")


# RegEx
# ([\sI])\w+

# zfundcode <- fundCode
# as.data.table(zfundcode)
# zfundcode<-zfundcode[tbl.GaCommitmentFundList.OSFA,mult = "first",on = "fundCode", nomatch=0]
# zfundcode<-zfundcode[, c(1,4)]
# zfundcode<-zfundcode[order(fundCode, fundName)][, .SD[c(1)], by=fundCode]

# zfundcode$fundName<-gsub(pattern = "[IV|X]|\\(1\\)|\\(2\\)", "", zfundcode$fundName)
# zfundcode$fundName<-trim(zfundcode$fundName)
# zfundcode$fundName

# zfundcode < left_join(zfundcode,jctCode, by = "fundCode")

#PIPES
# jctCode  <- c(tbl.scholarshipCohorts[,3], tbl.scholarshipCohorts[,4])                                         #concatenate scholarship codes
# jctCodes <- jctCode %>% 
#     as.data.table(jctCode) %>%
#     setorder(jctCode, AccountNumber) %>%
#     unique(jctCode)

# tbl.BannerCohorts$studentID <- as.character(as.numeric(tbl.BannerCohorts$studentID))
# tbl.Cancels$studentID <- as.character(as.numeric(tbl.Cancels$studentID))
# tbl.scholarshipAwd$studentID <- as.character(as.numeric(tbl.scholarshipAwd$studentID))
# tbl.scholarshipCohorts$studentID <- as.character(as.numeric(tbl.scholarshipCohorts$studentID))

# tbl.Cancels$fundCode <- as.character(as.numeric(tbl.Cancels$fundCode))
# tbl.scholarshipAwd$fundCode <- as.character(as.numeric(tbl.scholarshipAwd$fundCode))
# tbl.scholarshipCohorts$fundCode <- as.character(as.numeric(tbl.scholarshipCohorts$fundCode))

# tbl.BannerCohorts.osfaCode <-as.character(as.numeric(tbl.BannerCohorts$osfaCode))
# tbl.scholarshipAwd$osfaCode <- as.character(as.numeric(tbl.scholarshipAwd$osfaCode))
# tbl.scholarshipCohorts$osfaCode <- as.character(as.numeric(tbl.scholarshipCohorts$osfaCode))

#as.character(c(tbl.BannerCohorts$studentID, tbl.Cancels$studentID, tbl.scholarshipAwd$studentID, tbl.scholarshipCohorts$studentID))
#as.character(c(tbl.GaCommitmentFundList.OSFA$fundCode, tbl.scholarshipAwd$fundCode))
#as.character(c(tbl.BannerCohorts$osfaCode,tbl.scholarshipAwd$osfaCode))

#Ensure key's are formatted as character
# tbl.BannerCohorts  %>% mutate_if(is.integer,as.character)
# tbl.Cancels %>% mutate_if(is.integer,as.character)
# tbl.scholarshipAwd %>% mutate_if(is.integer,as.character)
# tbl.scholarshipCohorts %>% mutate_if(is.integer,as.character)
# # 
# tbl.BannerCohorts[] <- lapply(tbl.BannerCohorts, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.Cancels[] <- lapply(tbl.Cancels, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.scholarshipAwd[] <- lapply(tbl.scholarshipAwd, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.scholarshipCohorts[] <- lapply(tbl.scholarshipCohorts, function(x) if(is.numeric(x)) as.character(x) else x)