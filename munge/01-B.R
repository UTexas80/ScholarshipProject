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