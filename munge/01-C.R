setkey(tbl.GaCommit, FY)
chart.GaCommitByYear<-tbl.GaCommit[,sum(Award), by=FY]
chart.GaCommitByYear<-rename.columns(chart.GaCommitByYear,list(V1='fundAmount'))

chart.GaCommitByFund01<-tbl.GaCommit %>% group_by(FY, osfaCode, AccountName) %>% select(FY, osfaCode, AccountName, Award) %>% summarize(sum(Award))
chart.GaCommitByFund02<-tbl.GaCommit[,sum(Award), by=list(FY, osfaCode, AccountName)]
chart.GaCommitByFund02<-rename.columns(chart.GaCommitByFund02,list(V1='fundAmount'))

#setnames(chart.GaCommitByFund02, c("V1", "fundAmount"))

chart.GaCommitByUnit<-tbl.GaCommitmentFundList.OSFA[,sum(totalCommitment), by=list(fundraisingUnit)]
chart.GaCommitByUnit<-rename.columns(chart.GaCommitByUnit,list(V1='fundAmount'))

chart.rangefundAmt <- tbl.fundSummary %>% 
    mutate(cuts = cut(totBalRemain, c(-100000, 0, 100000, 200000, 300000, 400000, Inf))) %>% 
    group_by(cuts) %>% 
    summarize(n=n())
#https://stackoverflow.com/questions/28190435/changing-factor-levels-with-dplyr-mutate
chart.rangefundAmt <- chart.rangefundAmt %>% 
                      mutate(cuts = factor(cuts, labels = c("$(100000-$000,000)", 
                                                           "$000,000-$100,000", 
                                                           "$100,000-$200,000",
                                                           "$200,000-$300,000",
                                                           "$300,000-$400,000")))
chart.GaCommitByZip<-data.table(df)  %>%
    group_by(zip_code) %>%
    summarise_at(vars(ay_1718:ay_1718), funs(n(), sum(., na.rm = TRUE))) %>%
    arrange(zip_code)
names(chart.GaCommitByZip)[1:3] <- c("zip", "Number","value")
  #names(df[2])<-'Total'
chart.GaCommitByZip<-data.table(chart.GaCommitByZip)                                                                                        # convert chart to a data.table
chart.GaCommitByZip<-chart.GaCommitByZip[, zip:=as.character(zip)]                                                                          # convert zip_code column from integer to character
#names(chart.GaCommitByZip)[1] <- "zip"                                                                                                     # rename zip_code to zip
# chart.GaCommitByZip<-inner_join(chart.GaCommitByZip, zipcode)                                                                             # merge zip code table with GaCommitByZip table


chart.GaCommitByCounty<-chart.GaCommitByZip %>%
    left_join(jctCountyZip) %>%
    select(region,value) %>%
    group_by(region) %>%
    arrange(region) %>%
    summarise(value = sum(value))

# map<-get_map(location='united states', zoom=4, maptype = "terrain",
#              source='google',color='color')

# georgia<-ggmap(map) + geom_point(
#     aes(x=longitude, y=latitude, show_guide = TRUE, colour=Median), 
#     data=chart.GaCommitByZip, alpha=.8, na.rm = T)  + 
#     scale_color_gradient(low="beige", high="blue")
# plot(georgia)

write.csv(chart.GaCommitByCounty, "graphs/chart.GaCommitByCounty.csv", row.names=F)
write.csv(chart.GaCommitByUnit, "graphs/chart.GaCommitByUnit.csv", row.names=F)
write.csv(chart.GaCommitByYear, "graphs/chart.GaCommitByYear.csv", row.names=F)
write.csv(chart.GaCommitByZip, "graphs/chart.GaCommitByZip.csv", row.names=F)
write.csv(chart.rangefundAmt, "graphs/chart.rangefundAmt.csv", row.names=F)

g<-ggplot(tbl.fundSummary, aes(fundCode, totBalRemain))+geom_point() + geom_smooth(method="lm")
plot(g)


#--------------
# Create Theme
#--------------

# BASIC THEME
theme.chart_BASIC <- 
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=26, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
    theme(axis.title = element_text(size=18, family="Trebuchet MS", face="bold", color="#666666")) +
    theme(axis.title.y = element_text(angle=0)) 


# SCATTERPLOT THEME
theme.chart_SCATTER <- theme.chart_BASIC +
                            theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme.chart_HIST <- theme.chart_BASIC +
                          theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME

theme.chart_SMALLM <- theme.chart_BASIC +
                            theme(panel.grid.minor = element_blank()) +
                            theme(strip.text.x = element_text(size=16, family="Trebuchet MS", face="bold", color="#666666")) 