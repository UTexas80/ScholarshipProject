# read in the florida county shapefile
# ga <- readShapePoly("ga.shp", repair=TRUE, IDvar="NAME")

# make it work nicely with ggplot
# ga.f <- fortify(ga, region="NAME")

map.GaCommitByZip<-data.table(df)  %>%
    group_by(zip) %>%
    summarise_at(vars(ay_1718:ay_1718), funs(n(), sum(., na.rm = TRUE))) %>%
    arrange(zip)
names(map.GaCommitByZip)[2:3] <- c("Number","TotalAmount")

# start the plot
# gg <- ggplot(map.GaCommitByZip[[3]])

map.GaCommitByCounty<-county_choropleth(chart.GaCommitByCounty,
    title = "Scholarships by County",
    legend = "Scholarship Amount",
    num_colors = 1,
    state_zoom = c("georgia"))
plot(map.GaCommitByCounty)                          