###### COMBINGING T4 AND T7 DATA ######

#### SECTIONS ####

## 1) LOAD LIBRARIES AND DATA
## 2) ALIGN AND COMBINE T4 AND T7 DATA


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 1.1 Load libraries ----

pacman::p_load(rio, reldist, Kendall, reshape2, ggplot2, grid, gridExtra, dplyr)

# Sourcing most recent files

# Date in format YYYYMMDD (could be changed but we believe it makes most sense 
# to avoid hyphens in file names and to have the date first so files get sorted chronologically)
today.date <- gsub("-","",Sys.Date())

# Files (with package rio)
last.file <- function(dir.nam, nam){
  import(paste0(dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T)))), guess_max=50000)}

# suppress messages for experimental new group_by() and summarise() functionality in dplyr
options(dplyr.summarise.inform = FALSE)

library(janitor) # to compare T4 and T7 data


# ---- 1.2 Import data ----

setwd("C:/Users/rraso/Box/MPAMystery-master/")

## T4 data

Wellbeing4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_household')
Demographic4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_demographic')
SETTLEMENT <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_settlement')
Organization4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_marineorganizationmembership')
NmOrganization4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_nonmarineorganizationmembership')
Lthreat4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_localthreat')
Lsteps4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_localstep')
MPA.LKP <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_mpa')

## T7 data

Wellbeing7 <- read.csv("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_Tbl_WELLBEING_T7.csv", header = T)  
Demographic7 <- read.csv("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_Tbl_DEMOGRAPHIC_T7.csv", header = T)
Organization7 <- read.csv("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_Tbl_ORGANIZATION_T7.csv", header = T)
NmOrganization7 <- read.csv("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_Tbl_NMORGANIZATION_T7.csv", header = T)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: COMBINE T4 AND T7 DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 2.1 Household data ----

names(Wellbeing7) <- tolower(names(Wellbeing7)) # convert T7 variable names to lower case

compare_df_cols(Wellbeing4, Wellbeing7) # compare T4 and T7 variables

# Align T7 data to T4 data

Wellbeing7$assetbicycle <- as.numeric(Wellbeing7$assetbicycle)
Wellbeing7$assetboatinboard <- as.numeric(Wellbeing7$assetboatinboard)
Wellbeing7$assetboatnomotor <- as.numeric(Wellbeing7$assetboatnomotor)
Wellbeing7$assetboatoutboard <- as.numeric(Wellbeing7$assetboatoutboard)
Wellbeing7$assetcar <- as.numeric(Wellbeing7$assetcar)
Wellbeing7$assetcartruck <- as.numeric(Wellbeing7$assetcartruck)
Wellbeing7$assetcd <- as.numeric(Wellbeing7$assetcd)
Wellbeing7$assetcellphone <- as.numeric(Wellbeing7$assetcellphone)
Wellbeing7$assetdvd <- as.numeric(Wellbeing7$assetdvd)
Wellbeing7$assetentertain <- as.numeric(Wellbeing7$assetentertain)
Wellbeing7$assetgenerator <- as.numeric(Wellbeing7$assetgenerator)
Wellbeing7$assetlandlinephone <- as.numeric(Wellbeing7$assetlandlinephone)
Wellbeing7$assetmotorcycle <- as.numeric(Wellbeing7$assetmotorcycle)
Wellbeing7$assetphonecombined <- as.numeric(Wellbeing7$assetphonecombined)
Wellbeing7$assetradio <- as.numeric(Wellbeing7$assetradio)
Wellbeing7$assetsatellite <- as.numeric(Wellbeing7$assetsattellite)
which(colnames(Wellbeing7)=="assetsattellite") # column number for "assetsattellite"==74
Wellbeing7 <- Wellbeing7 [ ,c(1:73,75:114)] #remove column for "assetsattellite"
Wellbeing7$assetstereo <- as.numeric(Wellbeing7$assetstereo)
Wellbeing7$assettruck <- as.numeric(Wellbeing7$assettruck)
Wellbeing7$assettv <- as.numeric(Wellbeing7$assettv)
Wellbeing7$averageincomel <- as.character(NA)
Wellbeing7$averageincomeunits <- as.character(NA)
Wellbeing7$baseline_t2_pairs <- as.numeric(NA)
Wellbeing7$bestdaycatch <- as.character(NA)
Wellbeing7$bestdaycatchunits <- as.character(NA)
Wellbeing7$bestincomel <- as.character(NA)
Wellbeing7$bestincomeunits <- as.character(NA)
Wellbeing7$cookingfuel <- as.character(Wellbeing7$cookingfuel)
which(colnames(Wellbeing7)=="countryid") # column number for "countryid"==2
Wellbeing7 <- Wellbeing7 [ ,c(1,3:120)] #remove column for "countryid"
Wellbeing7$created_on <- as.character(NA)
Wellbeing7$datacheckcomplete <- as.logical(NA)
Wellbeing7$datacheckid <- as.numeric(NA)
Wellbeing7$dataentrycomplete <- as.logical(NA)
Wellbeing7$dataentryid <- as.numeric(NA)
Wellbeing7$economicstatusreasonenglish <- as.character(Wellbeing7$economicstatusreasonenglish)
Wellbeing7$economicstatustrend <- as.character(Wellbeing7$economicstatustrend)
Wellbeing7$entrycomputeridentifier <- as.character(NA)
Wellbeing7$entryhouseholdid <- as.numeric(NA)
Wellbeing7$fieldcoordinator <- as.numeric(Wellbeing7$fieldcoordinator)
Wellbeing7$freqeatfish <- as.numeric(Wellbeing7$freqeatfish)
Wellbeing7$freqfishtime <- as.numeric(Wellbeing7$freqfishtime)
Wellbeing7$freqsalefish <- as.numeric(Wellbeing7$freqsalefish)
Wellbeing7$fsadultskip <- as.character(Wellbeing7$fsadultskip)
Wellbeing7$fsbalanceddiet <- as.numeric(Wellbeing7$fsbalanceddiet)
Wellbeing7$fschildportion <- as.character(Wellbeing7$fschildportion)
Wellbeing7$fschildskip <- as.character(Wellbeing7$fschildskip)
Wellbeing7$fsdidnotlast <- as.numeric(Wellbeing7$fsdidnotlast)
Wellbeing7$fseatless <- as.character(Wellbeing7$fseatless)
Wellbeing7$fsfreqadultskip <- as.character(Wellbeing7$fsfreqadultskip)
Wellbeing7$fsfreqchildskip <- as.character(Wellbeing7$fsfreqchildskip)
Wellbeing7$fshungry <- as.character(Wellbeing7$fshungry)
Wellbeing7$fslowcostfood <- as.character(Wellbeing7$fslowcostfood)
Wellbeing7$fsnomealchild <- as.character(Wellbeing7$fsnomealchild)
Wellbeing7$fsnotenough <- as.numeric(Wellbeing7$fsnotenough)
Wellbeing7$goodcatch <- as.numeric(Wellbeing7$goodcatch)
Wellbeing7$goodfishincomel <- as.numeric(Wellbeing7$goodfishincomel)
Wellbeing7$householdbirth <- as.numeric(Wellbeing7$householdbirth)
Wellbeing7$householddeath <- as.numeric(Wellbeing7$householddeath)
Wellbeing7$householdid <- as.numeric(Wellbeing7$householdid)
Wellbeing7$interviewdate <- as.numeric(NA)
Wellbeing7$interviewday <- as.numeric(Wellbeing7$interviewday)
Wellbeing7$interviewlength <- as.character(Wellbeing7$interviewlength)
Wellbeing7$interviewmonth <- as.numeric(Wellbeing7$interviewmonth)
Wellbeing7$interviewyear <- as.numeric(Wellbeing7$interviewyear)
Wellbeing7$kkcode <- as.character(Wellbeing7$kkcode)
Wellbeing7$latdeg <- as.numeric(Wellbeing7$latdeg)
Wellbeing7$latfrac <- as.numeric(Wellbeing7$latfrac)
Wellbeing7$latmin <- as.numeric(Wellbeing7$latmin)
Wellbeing7$latsec <- as.numeric(Wellbeing7$latsec)
Wellbeing7$lessproductivedaysfishing <- as.numeric(Wellbeing7$lessproductivedaysfishing)
Wellbeing7$londeg <- as.numeric(Wellbeing7$londeg)
Wellbeing7$lonfrac <- as.numeric(Wellbeing7$lonfrac)
Wellbeing7$lonmin <- as.numeric(Wellbeing7$lonmin)
Wellbeing7$lonsec <- as.numeric(Wellbeing7$lonsec)
Wellbeing7$majorfishtechnique <- as.numeric(Wellbeing7$majorfishtechnique)
Wellbeing7$marinegroup <- as.numeric(Wellbeing7$marinegroup)
Wellbeing7$moreproductivedaysfishing <- as.numeric(Wellbeing7$moreproductivedaysfishing)
Wellbeing7$mpa <- as.numeric(Wellbeing7$mpaid)
which(colnames(Wellbeing7)=="mpaid") # column number for "mpaid"==2
Wellbeing7 <- Wellbeing7 [ ,c(1,3:128)] #remove column for "mpaid"
Wellbeing7$numbermarinegroup <- as.numeric(NA)
Wellbeing7$numberothergroup <- as.numeric(NA)
Wellbeing7$numglobalaction <- as.numeric(Wellbeing7$numglobalaction)
Wellbeing7$numglobalthreat <- as.numeric(Wellbeing7$numglobalthreat)
Wellbeing7$numlocalaction <- as.numeric(Wellbeing7$numlocalaction)
Wellbeing7$numlocalthreat <- as.numeric(Wellbeing7$numlocalthreat)
Wellbeing7$othergroup <- as.numeric(Wellbeing7$othergroup)
Wellbeing7$percentincomefish <- as.numeric(Wellbeing7$percentincomefish)
Wellbeing7$percentproteinfish <- as.numeric(Wellbeing7$percentproteinfish)
Wellbeing7$pilotreferencecode <- as.character(NA)
Wellbeing7$placebemyself <- as.character(Wellbeing7$placebemyself)
Wellbeing7$placebest <- as.character(Wellbeing7$placebest)
Wellbeing7$placefavourite <- as.character(Wellbeing7$placefavourite)
Wellbeing7$placefishhere <- as.character(Wellbeing7$placefishhere)
Wellbeing7$placehappy <- as.character(Wellbeing7$placehappy)
Wellbeing7$placemiss <- as.character(Wellbeing7$placemiss)
Wellbeing7$poorcatch <- as.numeric(Wellbeing7$poorcatch)
Wellbeing7$poorfishincomel <- as.numeric(Wellbeing7$poorfishincomel)
Wellbeing7$primaryfishtechnique <- as.numeric(NA)
Wellbeing7$primaryinterviewer <- as.numeric(Wellbeing7$primaryinterviewer)
Wellbeing7$primarylivelihood <- as.numeric(Wellbeing7$primarylivelihood)
Wellbeing7$primarymarketname <- as.character(NA)
Wellbeing7$religion <- as.numeric(Wellbeing7$religion)
Wellbeing7$rightsaccess <- as.character(Wellbeing7$rightsaccess)
Wellbeing7$rightsexclude <- as.character(Wellbeing7$rightsexclude)
Wellbeing7$rightsharvest <- as.character(Wellbeing7$rightsharvest)
Wellbeing7$rightsmanage <- as.character(Wellbeing7$rightsmanage)
Wellbeing7$rightstransfer <- as.character(Wellbeing7$rightstransfer)
Wellbeing7$secondaryfishtechnique <- as.numeric(NA)
Wellbeing7$secondaryinterviewer <- as.numeric(Wellbeing7$secondaryinterviewer)
Wellbeing7$secondarylivelihood <- as.numeric(Wellbeing7$secondarylivelihood)
Wellbeing7$secondarymarketname <- as.character(NA)
Wellbeing7$settlement <- as.numeric(Wellbeing7$settlementid)
which(colnames(Wellbeing7)=="settlementid") # column number for "settlementid"==2
Wellbeing7 <- Wellbeing7 [ ,c(1,3:135)] #remove column for "settlementid"
Wellbeing7$settlement <- ifelse(Wellbeing7$settlement== 178 | Wellbeing7$settlement==179, 76,
                                ifelse(Wellbeing7$settlement== 176 | Wellbeing7$settlement==177, 77,
                                       Wellbeing7$settlement)) # recode the settlements that are split in T7 back to their code in T4
Wellbeing7$socialconflict <- as.character(Wellbeing7$socialconflict)
Wellbeing7$surveyversionnumber <- as.numeric(Wellbeing7$surveyversionnumber)
Wellbeing7$tertiaryfishtechnique <- as.numeric(NA)
Wellbeing7$tertiarylivelihood <- as.numeric(Wellbeing7$tertiarylivelihood)
Wellbeing7$timesecondarymarket <- as.numeric(NA)
Wellbeing7$updated_by <- as.logical(NA)
Wellbeing7$updated_on <- as.character(NA)
Wellbeing7$usualfish <- as.character(Wellbeing7$usualfish)
Wellbeing7$votedistrict <- as.character(Wellbeing7$votedistrict)
Wellbeing7$votenational <- as.character(Wellbeing7$votenational)
Wellbeing7$worstdaycatch <- as.character(NA)
Wellbeing7$worstdaycatchunits <- as.character(NA)
Wellbeing7$worstincomel <- as.character(NA)
Wellbeing7$worstincomeunits <- as.character(NA)
Wellbeing7$yearsresident <- as.numeric(Wellbeing7$yearsresident)

### Ensure each household has a unique ID (no duplicate)

Wellbeing4$householdid <- paste(4,Wellbeing4$householdid, sep="") # concatenate "4" to householdid to distinguish T4 households from T7 households
Wellbeing4$householdid <- as.numeric(Wellbeing4$householdid)

Wellbeing7$householdid <- paste(7,Wellbeing7$householdid, sep="") # concatenate "7" to householdid to distinguish T7 households from T4 households
Wellbeing7$householdid <- as.numeric(Wellbeing7$householdid)

compare_df_cols(Wellbeing4, Wellbeing7)

WELLBEING <- rbind(Wellbeing4, Wellbeing7) # combined T4 and T7 data


# ---- 2.2 Demographic data ----

names(Demographic7) <- tolower(names(Demographic7)) # convert T7 variable names to lower case

compare_df_cols(Demographic4, Demographic7) # compare T4 and T7 variables


# Align T7 data to T4 data

Demographic7$created_on <- as.character(NA)
Demographic7$demographiccode <- as.numeric(Demographic7$demographiccode)
Demographic7$householdid <- as.numeric(Demographic7$householdid)
Demographic7$entryhouseholdid <- as.numeric(Demographic7$entryhouseholdid)
Demographic7$household <- as.numeric (Demographic7$householdid)
which(colnames(Demographic7)=="householdid") # column number for "householdid"==2
Demographic7 <- Demographic7 [ ,c(1,3:17)] #remove column for "householdid"
Demographic7$householdhead <- as.numeric(Demographic7$householdhead)
Demographic7$householdhead <- ifelse(Demographic7$relationhhh==0,1,Demographic7$householdhead)
Demographic7$individualage <- as.numeric(Demographic7$individualage)
Demographic7$individualdaysunwell <- as.numeric(Demographic7$individualdaysunwell)
Demographic7$individualedlevel <- as.numeric(Demographic7$individualedlevel)
Demographic7$individualenrolled <- as.numeric(Demographic7$individualenrolled)
Demographic7$individualgender <- as.numeric(Demographic7$individualgender)
Demographic7$individualgender <- ifelse(Demographic7$individualgender==0, NA, Demographic7$individualgender) # recode 0 to NA
Demographic7$individuallostdays <- as.numeric(Demographic7$individuallostdays)
Demographic7$individualunwell <- as.numeric(Demographic7$individualunwell)
Demographic7$relationhhh <- as.numeric(Demographic7$relationhhh)
Demographic7$updated_by <- as.logical(NA)
Demographic7$updated_on <- as.character(NA)

Demographic4$householdhead <- ifelse(is.na(Demographic4$relationhhh), NA, ifelse(Demographic4$relationhhh==0, 1, 0))

### Ensure each household has a unique ID (no duplicate)

Demographic4$household <- paste(4,Demographic4$household, sep="") # concatenate "4" to "household" to distinguish T4 households from T7 households
Demographic4$household <- as.numeric(Demographic4$household)

Demographic7$household <- paste(7,Demographic7$household, sep="") # concatenate "7" to "household" to distinguish T7 households from T4 households
Demographic7$household <- as.numeric(Demographic7$household)

compare_df_cols(Demographic4, Demographic7)

DEMOGRAPHIC <- rbind(Demographic4, Demographic7) # combine T4 and T7 data


# ---- 2.3 Marine organization data ----


names(Organization7) <- tolower(names(Organization7)) # convert T7 variable names to lower case

compare_df_cols(Organization4, Organization7) # compare T4 and T7 data variables

# Align T7 data to T4 data

Organization7$contribution <- as.numeric(Organization7$marinecontribution)
which(colnames(Organization7)=="marinecontribution") # column number for "marinecontribution"==8
Organization7 <- Organization7 [ ,c(1:7,9)] #remove column for "marinecontribution"
Organization7$created_on <- as.character(NA)
Organization7$days <- as.numeric(Organization7$marinedays)
which(colnames(Organization7)=="marinedays") # column number for "marinedays"==7
Organization7 <- Organization7 [ ,c(1:6,8:10)] #remove column for "marinedays"
Organization7$entryhouseholdid <- as.numeric(Organization7$entryhouseholdid)
Organization7$household <- as.numeric(Organization7$householdid)
which(colnames(Organization7)=="householdid") # column number for "householdid"==2
Organization7 <- Organization7 [ ,c(1,3:10)] #remove column for "householdid"
Organization7$meeting <- as.numeric(Organization7$marinemeeting)
which(colnames(Organization7)=="marinemeeting") # column number for "marinemeeting"==5
Organization7 <- Organization7 [ ,c(1:4,6:10)] #remove column for "marinemeeting"
Organization7$meeting <- ifelse(Organization7$meeting==7, NA, Organization7$meeting) # recode 7 to NA
Organization7$morganizationid <- as.numeric(Organization7$organizationid)
which(colnames(Organization7)=="organizationid") # column number for "organizationid"==1
Organization7 <- Organization7 [ ,c(2:10)] #remove column for "organization"
Organization7$name <- as.character(Organization7$marinegroupname)
which(colnames(Organization7)=="marinegroupname") # column number for "marinegroupname"==2
Organization7 <- Organization7 [ ,c(1, 3:10)] #remove column for "marinegroupname"
Organization7$position <- as.numeric(Organization7$marineposition)
Organization7$position <- ifelse(Organization7$position==0 | Organization7$position==4, 994, 
                                 Organization7$position) # recode 0 and 4 to 994
which(colnames(Organization7)=="marineposition") # column number for "marineposition"==2
Organization7 <- Organization7 [ ,c(1, 3:10)] #remove column for "marineposition"
Organization7$updated_by <- as.logical(NA)
Organization7$updated_on <- as.character(NA)

### Ensure each household has a unique ID (no duplicate)

Organization4$household <- paste(4,Organization4$household, sep="") # concatenate "4" to "household" to distinguish T4 households from T7 households
Organization4$household <- as.numeric(Organization4$household)

Organization7$household <- paste(7,Organization7$household, sep="") # concatenate "7" to "household" to distinguish T7 households from T4 households
Organization7$household <- as.numeric(Organization7$household)

compare_df_cols(Organization4, Organization7)

ORGANIZATION <- rbind(Organization4, Organization7) # combine T4 and T7 data


# ---- 2.4 Non-marine organization data ----

names(NmOrganization7) <- tolower(names(NmOrganization7)) # convert T7 variable names to lower case

compare_df_cols(NmOrganization4, NmOrganization7) # compare T4 and T7 variables

# Align T7 data to T4 data

NmOrganization7$contribution <- as.numeric(NmOrganization7$othergroupcontribution)
which(colnames(NmOrganization7)=="othergroupcontribution") # column number for "othergroupcontribution"==8
NmOrganization7 <- NmOrganization7 [ ,c(1:7,9)] #remove column for "othergroupcontribution"
NmOrganization7$created_on <- as.character(NA)
NmOrganization7$days <- as.numeric(NmOrganization7$othergroupdays)
which(colnames(NmOrganization7)=="othergroupdays") # column number for "othergroupdays"==7
NmOrganization7 <- NmOrganization7 [ ,c(1:6,8:10)] #remove column for "othergroupdays"
NmOrganization7$entryhouseholdid <- as.numeric(NmOrganization7$entryhouseholdid)
NmOrganization7$household <- as.numeric(NmOrganization7$householdid)
which(colnames(NmOrganization7)=="householdid") # column number for "householdid"==2
NmOrganization7 <- NmOrganization7 [ ,c(1,3:10)] #remove column for "householdid"
NmOrganization7$meeting <- as.numeric(NmOrganization7$othergroupmeeting)
which(colnames(NmOrganization7)=="othergroupmeeting") # column number for "othergroupmeeting"==5
NmOrganization7 <- NmOrganization7 [ ,c(1:4,6:10)] #remove column for "othergroupmeeting"
NmOrganization7$nmorganizationid <- as.numeric(NmOrganization7$nmorganizationid)
NmOrganization7$name <- as.character(NmOrganization7$othergroupname)
which(colnames(NmOrganization7)=="othergroupname") # column number for "othergroupname"==3
NmOrganization7 <- NmOrganization7 [ ,c(1, 2, 4:10)] #remove column for "othergroupname"
NmOrganization7$position <- as.numeric(NmOrganization7$othergroupposition)
which(colnames(NmOrganization7)=="othergroupposition") # column number for "othergroupposition"==3
NmOrganization7 <- NmOrganization7 [ ,c(1, 2, 4:10)] #remove column for "othergroupposition"
NmOrganization7$updated_by <- as.logical(NA)
NmOrganization7$updated_on <- as.character(NA)

### Ensure each household has a unique ID (no duplicate)

NmOrganization4$household <- paste(4,NmOrganization4$household, sep="") # concatenate "4" to "household" to distinguish T4 households from T7 households
NmOrganization4$household <- as.numeric(NmOrganization4$household)

NmOrganization7$household <- paste(7,NmOrganization7$household, sep="") # concatenate "7" to "household" to distinguish T7 households from T4 households
NmOrganization7$household <- as.numeric(NmOrganization7$household)

compare_df_cols(NmOrganization4, NmOrganization7)

NMORGANIZATION <- rbind(NmOrganization4, NmOrganization7) # combine T4 and T7 data