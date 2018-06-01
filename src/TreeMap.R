library(ggplot2) 
library(treemapify)


y<-tbl.GaCommitmentFundList.OSFA %>%
    group_by(fundraisingUnit) %>%
    summarize(totCommit=sum(totalCommitment))

ggplot(x, aes(area = totCommit, fill = fundraisingUnit, label=fundraisingUnit)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = TRUE)

# Section One Cleaning and Reshaping Data --------------------------------------
# 20180517a Treemap https://rpubs.com/brandonkopp/creating-a-treemap-in-r -/-

library(xtable)
library(dplyr)

#Remove commas from numeric values in number columns
outlays$X2017 <- gsub(",","", outlays$X2017)

#Convert numeric columns to a numeric data type
outlays$X2017 <- as.numeric(outlays$X2017)

#Create new data frame for positive (spending) values
spending <- outlays %>% select(Agency.Name, Bureau.Name, Account.Name, X2017) %>%
  group_by(Agency.Name, Bureau.Name, Account.Name) %>%
  summarize(X2017 = sum(X2017, na.rm=FALSE)) %>%
  filter(X2017 > 0)

#Create new data frame for negative (income) values
receipts <- outlays %>% select(Agency.Name, Bureau.Name, Account.Name, X2017) %>%
  group_by(Agency.Name, Bureau.Name, Account.Name) %>%
  summarize(X2017 = sum(X2017, na.rm=FALSE)) %>%
  filter(X2017 < 0) %>%
  mutate(X2017 = abs(X2017))

#Display several rows of spending data frame
print(xtable(spending[700:709,]), type="html")


# Section Two: Creating Your Treemap ===========================================
library(treemap)
#treemap(spending,                                                              # Your data frame object
treemap(y,                                                                      # Your data frame object
        index=c("fundraisingUnit"),                                             # A list of your categorical variables
        vSize = "totCommit",                                                    # This is your quantitative variable
        type="index",                                                           # Type sets the organization and color scheme of your treemap
        palette = "Greys",                                                      # Select your color palette from the RColorBrewer presets or make your own.
        title="Commitment by Fund Raising Unit",                                # Customize your title
        fontsize.title = 14                                                     # Change the font size of the title
        )

treemap(receipts,                                                               #Your data frame object
        index=c("Agency.Name","Bureau.Name","Account.Name"),                    #A list of your categorical variables
        vSize = "X2017",                                                        #This is your quantitative variable
        type="index",                                                           #Type sets the organization and color scheme of your treemap
        palette = "Greens",                                                     #Select your color palette from the RColorBrewer presets or make your own.
        title="Receipts in President Trump's 2017 Budget",                      #Customize your title
        fontsize.title = 14                                                     #Change the font size of the title
        )

#Aggregate Similar Line Items By Summing Value
spending <- outlays %>% select(Agency.Name, Bureau.Name, Account.Name, BEA.Category, X2017) %>%
  group_by(Agency.Name, Bureau.Name, Account.Name, BEA.Category) %>%
  summarize(X2017 = sum(X2017, na.rm=FALSE)) %>%
  filter(X2017 > 0)

treemap(spending, #Your data frame object
        index=c("Agency.Name","Bureau.Name","Account.Name"),  #A list of your categorical variables
        vSize = "X2017",  #This is your quantitative variable
        vColor= "BEA.Category", #This is a categorical variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set1",  #Select your color palette from the RColorBrewer presets or make your own.
        title="President Obama's 2017 Budget by Budgetary Category", #Customize your title
        fontsize.title = 14 #Change the font size of the title
        )

### Section Three ##############################################################            