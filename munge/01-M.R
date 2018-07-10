# read in the florida county shapefile
# ga <- readShapePoly("ga.shp", repair=TRUE, IDvar="NAME")

# make it work nicely with ggplot
# ga.f <- fortify(ga, region="NAME")

map.GaCommitByZip <- data.table(df) %>%
  group_by(zip) %>%
  summarise_at(vars(ay_1718:ay_1718), funs(n(), sum(., na.rm = TRUE))) %>%
  arrange(zip)
names(map.GaCommitByZip)[2:3] <- c("Number", "TotalAmount")

# start the plot
# gg <- ggplot(map.GaCommitByZip[[3]])

map.GaCommitByCounty <- county_choropleth(chart.GaCommitByCounty,
  title = "Scholarships by County",
  legend = "Scholarship Amount",
  num_colors = 1,
  state_zoom = c("georgia")
)
plot(map.GaCommitByCounty)

states <- map_data("state")
counties <- map_data("county")
ga_df <- subset(states, region == "georgia")
ga_county <- subset(counties, region == "georgia")
georgia <- subset(states, region %in% c("georgia"))

ggplot(data = georgia) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  coord_fixed(1.3)

ga_base <- ggplot(data = ga_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
ga_base + theme_nothing()

ga_base + theme_nothing() +
  geom_polygon(data = ga_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) # get the state border back on top
