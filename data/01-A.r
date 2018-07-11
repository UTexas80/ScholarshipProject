currentYr <- as.numeric(format(Sys.Date(), format = "%y"))
lastYr <- currentYr - 1
currentAY <- as.numeric(paste(lastYr, currentYr, sep = ""))

# seqAY <- seq(as.numeric(currentAY), as.numeric(currentAY)+(101*(100-currentYr-1)),by=101)

ay <- as.data.table((seq(as.numeric(currentAY), as.numeric(currentAY) + (101 * (100 - currentYr - 1)), by = 101)))
ay <- ay %>% mutate(V2 = V1) %>% mutate(V3 = V2) %>% mutate(V4 = V3)
ay <- mutate_at(ay, vars(V2), funs(lead), n = 1)
ay <- mutate_at(ay, vars(V3), funs(lead), n = 2)
ay <- mutate_at(ay, vars(V4), funs(lead), n = 3)

ay1 <- transpose(ay)
data(county.fips) # choroplethrMaps package
data(county.regions) # choroplethrMaps package
data(counties) # noncensus package
data(zip_codes) # noncensus package
data(zipcode) # zipcode package

jctCountyZip <- county.regions %>% inner_join(zip_codes, by = c("region" = "fips"))

osfa_1718data <- read.xlsx("O:/Departments/Scholarships/OSFA - Foundation and Trust Accounts/Foundation and Trust Accounts Awarded by OSFA/Georgia Commitment Scholarship/--FUNDING--/Commit to Georgia Campaign Requirements with Recipients.xlsx", 2, startRow = 2, header = TRUE)
osfa_1819data <- read.xlsx("O:/Departments/Scholarships/OSFA - Foundation and Trust Accounts/Foundation and Trust Accounts Awarded by OSFA/Georgia Commitment Scholarship/--FUNDING--/Commit to Georgia Campaign Requirements with Recipients.xlsx", 3, startRow = 2, header = TRUE)
