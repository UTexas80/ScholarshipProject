setkey(tbl.GaCommit, FY)
chart.GaCommitByYear <-
  tbl.GaCommit[, sum(Award), by = FY]
chart.GaCommitByYear <-
  rename.columns(chart.GaCommitByYear, list(V1 = "fundAmount"))

chart.GaCommitByFund01 <-
  tbl.GaCommit %>%
  group_by(FY, osfaCode, AccountName) %>%
  select(FY, osfaCode, AccountName, Award) %>%
  summarize(sum(Award))
chart.GaCommitByFund02 <-
  tbl.GaCommit[, sum(Award), by = list(FY, osfaCode, AccountName)]
chart.GaCommitByFund02 <-
  rename.columns(chart.GaCommitByFund02, list(V1 = "fundAmount"))

# setnames(chart.GaCommitByFund02, c("V1", "fundAmount"))

chart.GaCommitByUnit <- tbl.GaCommitmentFundList.OSFA[, sum(totalCommitment), by = list(fundraisingUnit)]
chart.GaCommitByUnit <- rename.columns(chart.GaCommitByUnit, list(V1 = "fundAmount"))

chart.rangefundAmt <- tbl.fundSummary %>%
  mutate(cuts = cut(totBalRemain, c(-100000, 0, 100000, 200000, 300000, 400000, Inf))) %>%
  group_by(cuts) %>%
  summarize(n = n())
# https://stackoverflow.com/questions/28190435/changing-factor-levels-with-dplyr-mutate
chart.rangefundAmt <- chart.rangefundAmt %>%
  mutate(cuts = factor(cuts, labels = c(
    "$(100000-$000,000)",
    "$000,000-$100,000",
    "$100,000-$200,000",
    "$200,000-$300,000",
    "$300,000-$400,000"
  )))
chart.GaCommitByZip <-
  data.table(df) %>%
  group_by(zip) %>%
  rename_at("zip", ~"zip5") %>% # rename zip to zip5
  summarise_at(vars(ay_1718:ay_1718), funs(n(), sum(., na.rm = TRUE))) %>%
  mutate(zip = substr(zip5, 1, 5)) %>%
  mutate_at(vars(starts_with("zip")), funs(as.character)) %>% # convert zip columns from integer to character
  select(zip, zip5, n, sum) %>% # rearrange columns
  rename_at("sum", ~"value") %>% # rename zip to zip5
  arrange(zip)

# names(df[2])<-'Total'
chart.GaCommitByZip <- data.table(chart.GaCommitByZip) # convert chart to a data.table
# chart.GaCommitByZip <- chart.GaCommitByZip[, zip := as.character(zip)]                            #convert zip columns from integer to character
# names(chart.GaCommitByZip)[1] <- "zip"
# chart.GaCommitByZip<-inner_join(chart.GaCommitByZip, zipcode)                                     #merge zip code table with GaCommitByZip table

chart.GaCommitByCounty <-
  chart.GaCommitByZip %>%
  left_join(jctCountyZip) %>%
  select(region, value) %>%
  group_by(region) %>%
  arrange(region) %>%
  summarise(value = sum(value))

chart.degreeByCohort <-
  data.table(tbl.scholar) %>%
  mutate(cohort = as.character(cohort)) %>%
  select(cohort, degree) %>%
  group_by(cohort) %>%
  count(degree)
# From on a categorical column variable
graph.degreeByCohort <- ggplot(chart.degreeByCohort, aes(degree))
graph.degreeByCohort <- graph.degreeByCohort + theme_solarized(light = FALSE)
graph.degreeByCohort <- graph.degreeByCohort +
  geom_bar(aes(weight = n, fill = cohort)) +
  theme(
    plot.title = element_text(colour = "white", face = "bold"),
    plot.subtitle = element_text(colour = "white"),
    axis.title.x = element_text(colour = "SteelBlue1"),
    axis.text.x = element_text(angle = 65, colour = "green", vjust = 0.6),
    axis.title.y = element_text(colour = "SteelBlue1"),
    axis.text.y = element_text(colour = "green")
  ) +
  labs(
    title = "Categorywise Degree by Cohort Chart",
    subtitle = "Degree by Cohort",
    caption = "Source: Degree from 'tbl.scholar' dataset"
  )

plot(graph.degreeByCohort)

# TreeMap
treemap(chart.degreeByCohort, # data frame object
  index = c("cohort", "degree"), # list of categorical variables
  vSize = "n", # quantitative variable
  type = "index", # Type sets the organization and color scheme of the treemap
  palette = "Greens", # Select the color palette from the RColorBrewer presets or make your own.
  title = "Degree by Cohorts TreeMap", # Customize the title
  fontsize.title = 14 # Change the font size of the title
)

# plot(graph.degreeByCohort)
# map<-get_map(location='united states', zoom=4, maptype = "terrain",
#              source='google',color='color')

# georgia<-ggmap(map) + geom_point(
#     aes(x=longitude, y=latitude, show_guide = TRUE, colour=Median),
#     data=chart.GaCommitByZip, alpha=.8, na.rm = T)  +
#     scale_color_gradient(low="beige", high="blue")
# plot(georgia)

write.csv(chart.GaCommitByCounty, "graphs/chart.GaCommitByCounty.csv", row.names = F)
write.csv(chart.GaCommitByUnit, "graphs/chart.GaCommitByUnit.csv", row.names = F)
write.csv(chart.GaCommitByYear, "graphs/chart.GaCommitByYear.csv", row.names = F)
write.csv(chart.GaCommitByZip, "graphs/chart.GaCommitByZip.csv", row.names = F)
write.csv(chart.rangefundAmt, "graphs/chart.rangefundAmt.csv", row.names = F)
write.csv(chart.GaCommitByZip %>% left_join(jctCountyZip), "graphs/list.GaCommitByCount.csv", row.names = F)

g <- ggplot(tbl.fundSummary, aes(fundCode, totBalRemain)) + geom_point() + geom_smooth(method = "lm")
plot(g)


#--------------
# Create Theme
#--------------

# BASIC THEME
theme.chart_BASIC <-
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 26, family = "Trebuchet MS", face = "bold", hjust = 0, color = "#666666")) +
  theme(axis.title = element_text(size = 18, family = "Trebuchet MS", face = "bold", color = "#666666")) +
  theme(axis.title.y = element_text(angle = 0))


# SCATTERPLOT THEME
theme.chart_SCATTER <- theme.chart_BASIC +
  theme(axis.title.x = element_text(hjust = 0, vjust = -.5))

# HISTOGRAM THEME
theme.chart_HIST <- theme.chart_BASIC +
  theme(axis.title.x = element_text(hjust = 0, vjust = -.5))

# SMALL MULTIPLE THEME

theme.chart_SMALLM <- theme.chart_BASIC +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 16, family = "Trebuchet MS", face = "bold", color = "#666666"))
