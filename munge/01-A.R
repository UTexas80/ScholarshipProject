# Convert keys to character

tbl.BannerCohorts$osfaCode              <- as.character(tbl.BannerCohorts$osfaCode)
tbl.BannerCohorts$studentID             <- as.character(tbl.BannerCohorts$studentID)

tbl.Cancels$fundCode                    <- as.character(tbl.Cancels$fundCode)
tbl.Cancels$studentID                   <- as.character(tbl.Cancels$studentID)

tbl.GaCommitmentFundList.OSFA$fundCode  <- as.character(tbl.GaCommitmentFundList.OSFA$fundCode)
tbl.GaCommitmentFundList.OSFA$fundDate  <- as.Date(tbl.GaCommitmentFundList.OSFA$fundDate, "%m/%d/%Y")

tbl.scholarshipAwd$fundCode             <- as.character(tbl.scholarshipAwd$fundCode)
tbl.scholarshipAwd$osfaCode             <- as.character(tbl.scholarshipAwd$osfaCode)
tbl.scholarshipAwd$studentID            <- as.character(tbl.scholarshipAwd$studentID)

tbl.scholarshipCohorts$fundCode         <- as.character(tbl.scholarshipCohorts$fundCode)
tbl.scholarshipCohorts$osfaCode         <- as.character(tbl.scholarshipCohorts$osfaCode)
tbl.scholarshipCohorts$studentID        <- as.character(tbl.scholarshipCohorts$studentID)

tbl.studentID <- tbl.studentID[!apply(is.na(tbl.studentID) | tbl.studentID == "", 1, all),]                                                 # 20180516b remove both (NAs and empty):
tbl.studentID$studentID                 <- as.character(tbl.studentID$studentID)