library(ggplot2) 
library(treemapify)


x<-tbl.GaCommitmentFundList.OSFA %>%
    group_by(fundraisingUnit) %>%
    summarize(totCommit=sum(totalCommitment))

ggplot(x, aes(area = totCommit, fill = fundraisingUnit, label=fundraisingUnit)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = TRUE)