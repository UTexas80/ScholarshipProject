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
rbind(anti1718a[,common], anti1718b[,common])   # https://stackoverflow.com/questions/16674377/combine-two-dataframes-in-r-based-on-common-columns

anti1718c %>% 
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .)))


# https://stackoverflow.com/questions/9917545/r-define-dimensions-of-empty-data-frame
df2 <- data.table(data.frame(matrix("", ncol = 5, nrow = 0)))  # https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector

ab <- data.frame()  # create empty dataframe

sim_list = replicate(n = 10,
                     expr = {data.frame(x = rnorm(50), y = rnorm(50))},
                     simplify = F)  # https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames

exception_list = replicate(n = 10,
                     expr = {data.table(data.frame(matrix("", ncol = 5, nrow = 0)))},
                     simplify = F)

lapply(exception_list, function(x) {
    x <- as.dataframe(x)
})                     

#split the exception list into individual data frames
# https://stackoverflow.com/questions/30516325/converting-a-list-of-data-frames-into-individual-data-frames-in-r
for (i in 1:length(exception_list)) {
     assign(paste0("exception_list", i), as.data.frame(exception_list[[i]]))
}

lapply(names(exception_list), function(x) assign(x, paste0(exception_list[[x]]), as.data.table(exception_list[[i]]), envir = .GlobalEnv)
lapply(names(exception_list), function(x) assign(x, as.data.frame(exception_list[[x]]), envir = .GlobalEnv)

chart_test <-chart.GaCommitByZip %>%
    inner_join(zip_codes) %>%
    select(1, 3:9) %>%
    summarise(group_by(zip, city, latitude, longitude, fips), sum(n), sum(value))
    
chart_test <-chart.GaCommitByZip %>%
    inner_join(zip_codes) %>%
    select(1, 3:9) %>%
    group_by(zip, city, latitude, longitude, fips) %>% 
    summarise_each (funs(sum), n, value)

us<-map_data('state')


ggplot(chart_test,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = n),size=.15,alpha=.25) +
  xlim(-125,-65)+ylim(20,50)

cohort_by_zip<-  ggplot(chart_test,aes(longitude,latitude)) +
  geom_polygon(data=ga_county,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = n),size=.15,alpha=.25) +
  xlim(-125,-65)+ylim(20,50)


#### Nhttps://tinyurl.com/yar88f96
ga_base <- ggplot(data = ga_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ga_base + theme_nothing()

ga_base + theme_nothing() + 
  geom_polygon(data = ga_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

ga_base <- ggplot(data = ga_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

map_ga_county<- ga_base + theme_nothing() + 
  geom_polygon(data = ga_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top


match(tbl.scholar2$`1718`$last_ay_eligible,ay$V1)
match(xx$last_ay_eligible,ay$V1)
as.integer(match(tbl.scholar2$`1718`$last_ay_eligible,ay$V1))
match(tbl.scholar2$`1819`$last_ay_eligible,ay$V1)
st_pos
tbl.scholar2$`1718`$last_ay_eligible %in% ay$V1 # return true or false
z<- filter(tbl.scholar2$`1718`, cohort==last_ay_eligible)

'last_ay_eligible' match 'cohort'
in that row update column (st_pos + match(tbl.scholar2$`1718`$last_ay_eligible,ay$V1) : (st_pos + match(tbl.scholar2$`1718`$last_ay_eligible,ay$V1)) + 2)

# dplyr
zz<- filter(tbl.scholar2$`1718`, cohort==last_ay_eligible)

# data.table
st_pos <- as.integer(st_pos)
xxx <- as.list(tbl.scholar2$`1718`)
xxxx<-data.table(map_df(xxx, ~.x))
xxxxx<-as.data.table(xxxx[cohort ==  last_ay_eligible])
xxxxxx<-xxxxx[, (st_pos + 1) : (st_pos + 3)  := 0]

setDT(xxxxxx)[cohort ==  last_ay_eligible,(st_pos + 1) : (st_pos + 3):= 0]

abc<-xxxxx[cohort ==  last_ay_eligible, (st_pos + 1) : (st_pos + 3)  := 0] # this is correct

