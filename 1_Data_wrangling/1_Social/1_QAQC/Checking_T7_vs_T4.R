################ALIGNING T4 and T7 data ############


setwd("C:/Users/rraso/Box/MPAMystery-master/")
# ---- 1.1 Load libraries & data ----

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


Wellbeing4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_household')
Demographic4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_demographic')
SETTLEMENT <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_settlement')
Organization4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_marineorganizationmembership')
NmOrganization4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_nonmarineorganizationmembership')
Lthreat4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_localthreat')
Lsteps4 <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_localstep')
MPA.LKP <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/', nam='tbl_mpa')

## HOUSEHOLD DATA

setwd("C:/Users/rraso/Box/T7_data/")
Wellbeing7 <- read.csv("HH_Tbl_WELLBEING_T7.csv", header = T)     
names(Wellbeing7) <- tolower(names(Wellbeing7)) 
library(janitor)                   
compare_df_cols(Wellbeing4, Wellbeing7)

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

compare_df_cols(Wellbeing4, Wellbeing7)

#WELLBEING <- rbind(Wellbeing4, Wellbeing7) # run this code and then go to "Source_social_data_flat_files.R"
                                            # The rest of the code below is to check variable values  

Wellbeing4$Series <- 1
Wellbeing7$Series <- 2

WELLBEING <- rbind(Wellbeing4, Wellbeing7)

# CHECKING CHARACTER VARIABLES

Fonc <- function(x){x <- unique(x); x}

tapply(WELLBEING$averageincomel, WELLBEING$Series, Fonc)
tapply(WELLBEING$averageincomeunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$bestdaycatch, WELLBEING$Series, Fonc)
tapply(WELLBEING$bestdaycatchunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$bestincomel, WELLBEING$Series, Fonc)
tapply(WELLBEING$bestincomeunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$cookingfuel, WELLBEING$Series, Fonc) # T4: 1-6; T7: 1-5
tapply(WELLBEING$economicstatusreason, WELLBEING$Series, Fonc)
tapply(WELLBEING$economicstatusreasonenglish, WELLBEING$Series, Fonc)
tapply(WELLBEING$economicstatustrend, WELLBEING$Series, Fonc)
tapply(WELLBEING$fsadultskip, WELLBEING$Series, Fonc)
tapply(WELLBEING$fschildportion, WELLBEING$Series, Fonc)
tapply(WELLBEING$fschildskip, WELLBEING$Series, Fonc)
tapply(WELLBEING$fseatless, WELLBEING$Series, Fonc)
tapply(WELLBEING$fsfreqadultskip, WELLBEING$Series, Fonc)
tapply(WELLBEING$fsfreqchildskip, WELLBEING$Series, Fonc)
tapply(WELLBEING$fshungry, WELLBEING$Series, Fonc)
tapply(WELLBEING$fslowcostfood, WELLBEING$Series, Fonc)
tapply(WELLBEING$fsnomealchild, WELLBEING$Series, Fonc)
tapply(WELLBEING$goodcatchunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$goodfishunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$interviewend, WELLBEING$Series, Fonc)
tapply(WELLBEING$interviewlength, WELLBEING$Series, Fonc)
tapply(WELLBEING$interviewstart, WELLBEING$Series, Fonc)
tapply(WELLBEING$kkcode, WELLBEING$Series, Fonc)
tapply(WELLBEING$latsphere, WELLBEING$Series, Fonc)
tapply(WELLBEING$lonsphere, WELLBEING$Series, Fonc)
tapply(WELLBEING$maternalethnicity, WELLBEING$Series, Fonc)
tapply(WELLBEING$paternalethnicity, WELLBEING$Series, Fonc)
tapply(WELLBEING$placebemyself, WELLBEING$Series, Fonc) # T4: 1-5; T7: 2-5
tapply(WELLBEING$placebest, WELLBEING$Series, Fonc)
tapply(WELLBEING$placefavourite, WELLBEING$Series, Fonc)
tapply(WELLBEING$placefishhere, WELLBEING$Series, Fonc)
tapply(WELLBEING$placehappy, WELLBEING$Series, Fonc)
tapply(WELLBEING$placemiss, WELLBEING$Series, Fonc)
tapply(WELLBEING$poorcatchunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$poorfishunits, WELLBEING$Series, Fonc)
tapply(WELLBEING$respondent, WELLBEING$Series, Fonc)
tapply(WELLBEING$rightsaccess, WELLBEING$Series, Fonc)
tapply(WELLBEING$rightsexclude, WELLBEING$Series, Fonc)
tapply(WELLBEING$rightsharvest, WELLBEING$Series, Fonc)
tapply(WELLBEING$rightsmanage, WELLBEING$Series, Fonc)
tapply(WELLBEING$rightstransfer, WELLBEING$Series, Fonc)
tapply(WELLBEING$secondaryrespondent, WELLBEING$Series, Fonc)
tapply(WELLBEING$socialconflict, WELLBEING$Series, Fonc)
tapply(WELLBEING$usualfish, WELLBEING$Series, Fonc)
tapply(WELLBEING$votedistrict, WELLBEING$Series, Fonc)
tapply(WELLBEING$votenational, WELLBEING$Series, Fonc)
tapply(WELLBEING$willingparticipant, WELLBEING$Series, Fonc)

# CHECKING NUMERIC VARIABLES

tapply(WELLBEING[WELLBEING$assetbicycle < 900, ]$assetbicycle, WELLBEING[WELLBEING$assetbicycle < 900, ]$Series, range) # T4: 0-6; T7: 0-3
tapply(WELLBEING[WELLBEING$assetboatinboard < 900, ]$assetboatinboard, WELLBEING[WELLBEING$assetboatinboard < 900, ]$Series, range) # T4: 0-3; T7: 0-2
tapply(WELLBEING[WELLBEING$assetboatnomotor < 900, ]$assetboatnomotor, WELLBEING[WELLBEING$assetboatnomotor < 900, ]$Series, range) # T4: 0-7; T7: 0-4
tapply(WELLBEING[WELLBEING$assetboatoutboard < 900, ]$assetboatoutboard, WELLBEING[WELLBEING$assetboatoutboard < 900, ]$Series, range) # T4: 0-10; T7: 0-4
tapply(WELLBEING[WELLBEING$assetcar < 900, ]$assetcar, WELLBEING[WELLBEING$assetcar < 900, ]$Series, range) # T4: 0-4; T7: 0-2
tapply(WELLBEING[WELLBEING$assetcartruck < 900, ]$assetcartruck, WELLBEING[WELLBEING$assetcartruck < 900, ]$Series, range) # T4: 0-6; T7: 0-3
tapply(WELLBEING[WELLBEING$assetcd < 900, ]$assetcd, WELLBEING[WELLBEING$assetcd < 900, ]$Series, range) # T4: 0-10; T7: 0-10
tapply(WELLBEING[WELLBEING$assetcellphone < 900, ]$assetcellphone, WELLBEING[WELLBEING$assetcellphone < 900, ]$Series, range) # T4: 0-10; T7: 0-9
tapply(WELLBEING[WELLBEING$assetdvd < 900, ]$assetdvd, WELLBEING[WELLBEING$assetdvd < 900, ]$Series, range) # T4: 0-11; T7: 0-11
tapply(WELLBEING[WELLBEING$assetentertain < 900, ]$assetentertain, WELLBEING[WELLBEING$assetentertain < 900, ]$Series, range) # T4: 0-16; T7: 0-13
tapply(WELLBEING[WELLBEING$assetgenerator < 900, ]$assetgenerator, WELLBEING[WELLBEING$assetgenerator < 900, ]$Series, range) # T4: 0-15; T7: 0-6
tapply(WELLBEING[WELLBEING$assetlandlinephone < 900, ]$assetlandlinephone, WELLBEING[WELLBEING$assetlandlinephone < 900, ]$Series, range) # T4: 0-18; T7: 0-10
tapply(WELLBEING[WELLBEING$assetmotorcycle < 900, ]$assetmotorcycle, WELLBEING[WELLBEING$assetmotorcycle < 900, ]$Series, range) # T4: 0-10; T7: 0-4
tapply(WELLBEING[WELLBEING$assetphonecombined < 900, ]$assetphonecombined, WELLBEING[WELLBEING$assetphonecombined < 900, ]$Series, range) # T4: 0-18; T7: 0-11
tapply(WELLBEING[WELLBEING$assetradio < 900, ]$assetradio, WELLBEING[WELLBEING$assetradio < 900, ]$Series, range) # T4: 0-6; T7: 0-6
tapply(WELLBEING[WELLBEING$assetsatellite < 900, ]$assetsatellite, WELLBEING[WELLBEING$assetsatellite < 900, ]$Series, range) # T4: 0-4; T7: 0-2
tapply(WELLBEING[WELLBEING$assetstereo < 900, ]$assetstereo, WELLBEING[WELLBEING$assetstereo < 900, ]$Series, range) # T4: 0-15; T7: 0-8
tapply(WELLBEING[WELLBEING$assettruck < 900, ]$assettruck, WELLBEING[WELLBEING$assettruck < 900, ]$Series, range) # T4: 0-3; T7: 0-3
tapply(WELLBEING[WELLBEING$assettv < 900, ]$assettv, WELLBEING[WELLBEING$assettv < 900, ]$Series, range) # T4: 0-4; T7: 0-3

tapply(WELLBEING[WELLBEING$freqeatfish < 900, ]$freqeatfish, WELLBEING[WELLBEING$freqeatfish < 900, ]$Series, range) # T4: 1-5; T7: 1-5
tapply(WELLBEING[WELLBEING$freqfishtime < 900, ]$freqfishtime, WELLBEING[WELLBEING$freqfishtime < 900, ]$Series, range) # T4: 1-5; T7: 1-5
tapply(WELLBEING[WELLBEING$freqsalefish < 900, ]$freqsalefish, WELLBEING[WELLBEING$freqsalefish < 900, ]$Series, range) # T4: 1-5; T7: 1-5
tapply(WELLBEING[WELLBEING$fsbalanceddiet < 900, ]$fsbalanceddiet, WELLBEING[WELLBEING$fsbalanceddiet < 900, ]$Series, range) # T4: 1-3; T7: 1-3
tapply(WELLBEING[WELLBEING$fsdidnotlast < 900, ]$fsdidnotlast, WELLBEING[WELLBEING$fsdidnotlast < 900, ]$Series, range) # T4: 1-3; T7: 1-3
tapply(WELLBEING[WELLBEING$fsnotenough < 900, ]$fsnotenough, WELLBEING[WELLBEING$fsnotenough < 900, ]$Series, range) # T4: 1-3; T7: 1-3

tapply(WELLBEING$goodcatch, WELLBEING$Series, range) # T4: 0-112875; T7: 0-60625
tapply(WELLBEING$goodcatch, WELLBEING$Series, mean) # T4: 606.49; T7: 326.19
tapply(WELLBEING$goodcatch, WELLBEING$Series, median) # T4: 993; T7: 20

tapply(WELLBEING$goodfishincomel, WELLBEING$Series, range) # T4: 0-2.85e+08; T7: 0-41112000
tapply(WELLBEING$goodfishincomel, WELLBEING$Series, mean) # T4: 249975.4; T7: 314913.3
tapply(WELLBEING$goodfishincomel, WELLBEING$Series, median) # T4: 994; T7: 998

tapply(WELLBEING[WELLBEING$householdbirth < 900, ]$householdbirth, WELLBEING[WELLBEING$householdbirth < 900, ]$Series, range) # T4: 0-1; T7: 0-1
tapply(WELLBEING[WELLBEING$householddeath < 900, ]$householddeath, WELLBEING[WELLBEING$householddeath < 900, ]$Series, range) # T4: 0-1; T7: 0-1
tapply(WELLBEING[WELLBEING$interviewday < 900, ]$interviewday, WELLBEING[WELLBEING$interviewday < 900, ]$Series, range) # T4: 0-31; T7: 1-31
tapply(WELLBEING[WELLBEING$interviewmonth < 900, ]$interviewmonth, WELLBEING[WELLBEING$interviewmonth < 900, ]$Series, range) # T4: 1-12; T7: 0-12
tapply(WELLBEING$interviewyear, WELLBEING$Series, range) # T4: 2010-2019; T7: 2017-2019

tapply(WELLBEING$latdeg, WELLBEING$Series, range, na.rm=T) # T4: 0-92; T7: 1-11
tapply(WELLBEING[WELLBEING$latfrac < 900, ]$latfrac, WELLBEING[WELLBEING$latfrac < 900, ]$Series, range) # T4: 0-99; T7: 0-99
tapply(WELLBEING$latmin, WELLBEING$Series, range, na.rm=T) # T4: 0-59; T7: 0-59
tapply(WELLBEING[WELLBEING$latsec < 900, ]$latsec, WELLBEING[WELLBEING$latsec < 900, ]$Series, range) # T4: 0-60; T7: 0-99

tapply(WELLBEING[WELLBEING$lessproductivedaysfishing < 900, ]$lessproductivedaysfishing, WELLBEING[WELLBEING$lessproductivedaysfishing < 900, ]$Series, range) # T4: 0-360; T7: 0-240

tapply(WELLBEING$londeg, WELLBEING$Series, range, na.rm=T) # T4: 0-138; T7: 114-144
tapply(WELLBEING[WELLBEING$lonfrac < 900, ]$lonfrac, WELLBEING[WELLBEING$lonfrac < 900, ]$Series, range) # T4: 0-99; T7: 0-99
tapply(WELLBEING$lonmin, WELLBEING$Series, range, na.rm=T) # T4: 0-59; T7: 3-71
tapply(WELLBEING[WELLBEING$lonsec < 900, ]$lonsec, WELLBEING[WELLBEING$lonsec < 900, ]$Series, range) # T4: 0-60; T7: 0-99

tapply(WELLBEING[WELLBEING$majorfishtechnique < 900, ]$majorfishtechnique, WELLBEING[WELLBEING$majorfishtechnique < 900, ]$Series, range) # T4: 1-5; T7: 1-6
tapply(WELLBEING[WELLBEING$marinegroup < 900, ]$marinegroup, WELLBEING[WELLBEING$marinegroup < 900, ]$Series, range) # T4: 0-1; T7: 0-1

tapply(WELLBEING[WELLBEING$moreproductivedaysfishing < 900, ]$moreproductivedaysfishing, WELLBEING[WELLBEING$moreproductivedaysfishing < 900, ]$Series, range) # T4: 0-365; T7: 0-360

tapply(WELLBEING$mpa, WELLBEING$Series, range) # T4: 1-21; T7: 1-6

tapply(WELLBEING[WELLBEING$numglobalaction < 900, ]$numglobalaction, WELLBEING[WELLBEING$numglobalaction < 900, ]$Series, range) # T4: 0-14; T7: 0-9
tapply(WELLBEING[WELLBEING$numglobalaction < 900, ]$numglobalaction, WELLBEING[WELLBEING$numglobalaction < 900, ]$Series, mean) # T4: 0.7208509; T7: 0.8808602 
tapply(WELLBEING[WELLBEING$numglobalthreat < 900, ]$numglobalthreat, WELLBEING[WELLBEING$numglobalthreat < 900, ]$Series, range) # T4: 0-13; T7: 0-6
tapply(WELLBEING[WELLBEING$numglobalthreat < 900, ]$numglobalthreat, WELLBEING[WELLBEING$numglobalthreat < 900, ]$Series, mean) # T4: 1.215630; T7: 1.226472
tapply(WELLBEING[WELLBEING$numlocalaction < 900, ]$numlocalaction, WELLBEING[WELLBEING$numlocalaction < 900, ]$Series, range) # T4: 0-9; T7: 0-6
tapply(WELLBEING[WELLBEING$numlocalaction < 900, ]$numlocalaction, WELLBEING[WELLBEING$numlocalaction < 900, ]$Series, mean) # T4: 1.104431; T7: 1.142243
tapply(WELLBEING[WELLBEING$numlocalthreat < 900, ]$numlocalthreat, WELLBEING[WELLBEING$numlocalthreat < 900, ]$Series, range) # T4: 0-32; T7: 0-6
tapply(WELLBEING[WELLBEING$numlocalthreat < 900, ]$numlocalthreat, WELLBEING[WELLBEING$numlocalthreat < 900, ]$Series, mean) # T4: 1.741025; T7: 1.633004

tapply(WELLBEING[WELLBEING$othergroup < 900, ]$othergroup, WELLBEING[WELLBEING$othergroup < 900, ]$Series, range) # T4: 0-1; T7: 0-1
tapply(WELLBEING[WELLBEING$percentincomefish < 900, ]$percentincomefish, WELLBEING[WELLBEING$percentincomefish < 900, ]$Series, range) # T4: 1-5; T7: 1-5
tapply(WELLBEING[WELLBEING$percentproteinfish < 900, ]$percentproteinfish, WELLBEING[WELLBEING$percentproteinfish < 900, ]$Series, range) # T4: 1-5; T7: 1-5
tapply(WELLBEING$poorcatch, WELLBEING$Series, range) # T4: 0-140208; T7: 0-998
tapply(WELLBEING$poorfishincomel, WELLBEING$Series, range, na.rm=T) # T4: 0.0e+00-2.5e+07; T7: 0-19600000
tapply(WELLBEING$poorfishincomel, WELLBEING$Series, mean, na.rm=T) # T4: 60981.36; T7: 81979.78
tapply(WELLBEING[WELLBEING$primarylivelihood < 900, ]$primarylivelihood, WELLBEING[WELLBEING$primarylivelihood < 900, ]$Series, range) # T4: 1-7; T7: 1-7
tapply(WELLBEING[WELLBEING$religion < 900, ]$religion, WELLBEING[WELLBEING$religion < 900, ]$Series, range) # T4: 1-7; T7: 1-4
tapply(WELLBEING[WELLBEING$secondarylivelihood < 900, ]$secondarylivelihood, WELLBEING[WELLBEING$secondarylivelihood < 900, ]$Series, range) # T4: 1-7; T7: 1-7
tapply(WELLBEING$settlement, WELLBEING$Series, range) # T4: 1-271; T7: 1-115
tapply(WELLBEING$surveyversionnumber, WELLBEING$Series, Fonc)
tapply(WELLBEING[WELLBEING$tertiarylivelihood < 900, ]$tertiarylivelihood, WELLBEING[WELLBEING$tertiarylivelihood < 900, ]$Series, range) # T4: 1-7; T7: 1-7
tapply(WELLBEING[WELLBEING$timemarket < 900, ]$timemarket, WELLBEING[WELLBEING$timemarket < 900, ]$Series, range) # T4: 0-83 (outliers more than 40); T7: 0-8
tapply(WELLBEING[WELLBEING$timemarket < 900, ]$timemarket, WELLBEING[WELLBEING$timemarket < 900, ]$Series, mean) # T4: 2.218004; T7: 1.478668
tapply(WELLBEING[WELLBEING$yearsresident < 900, ]$yearsresident, WELLBEING[WELLBEING$yearsresident < 900, ]$Series, range) # T4: 0-100; T7: 0-90
tapply(WELLBEING[WELLBEING$yearsresident < 900, ]$yearsresident, WELLBEING[WELLBEING$yearsresident < 900, ]$Series, mean) # T4: 32.43116; T7: 30.75645


## DEMOGRAPHIC DATA

Demographic7 <- read.csv("HH_Tbl_DEMOGRAPHIC_T7.csv", header = T)
names(Demographic7) <- tolower(names(Demographic7))
library(janitor)
compare_df_cols(Demographic4, Demographic7)

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

compare_df_cols(Demographic4, Demographic7)

#DEMOGRAPHIC <- rbind(Demographic4, Demographic7) # run this code and then go to "Source_social_data_flat_files.R"
                                                  # The rest of the code below is to check variable values


Demographic4$Series <- 1
Demographic7$Series <- 2

DEMOGRAPHIC <- rbind(Demographic4, Demographic7)

# CHECKING CHARACTER VARIABLES

Fonc <- function(x){x <- unique(x); x}

tapply(DEMOGRAPHIC$individualeducation, DEMOGRAPHIC$Series, Fonc)

# CHECKING NUMERIC VARIABLES

tapply(DEMOGRAPHIC[DEMOGRAPHIC$demographiccode < 900, ]$demographiccode, DEMOGRAPHIC[DEMOGRAPHIC$demographiccode < 900, ]$Series, range)
tapply(DEMOGRAPHIC$demographicid, DEMOGRAPHIC$Series, range)
tapply(DEMOGRAPHIC$household, DEMOGRAPHIC$Series, range) # T4: 1 - 11536; T7: 7186 - 10171 (overlap between the two)
tapply(DEMOGRAPHIC$householdhead, DEMOGRAPHIC$Series, Fonc) # T4: 0 and 1; T7: 0
tapply(DEMOGRAPHIC[DEMOGRAPHIC$individualage < 900, ]$individualage, DEMOGRAPHIC[DEMOGRAPHIC$individualage < 900, ]$Series, range) # T4: 0-110; T7: 0-104
tapply(DEMOGRAPHIC[DEMOGRAPHIC$individualdaysunwell < 900, ]$individualdaysunwell, DEMOGRAPHIC[DEMOGRAPHIC$individualdaysunwell < 900, ]$Series, range)
tapply(DEMOGRAPHIC[DEMOGRAPHIC$individualedlevel < 900, ]$individualedlevel, DEMOGRAPHIC[DEMOGRAPHIC$individualedlevel < 900, ]$Series, range) # T4: 0-5; T7:blank
tapply(DEMOGRAPHIC$individualenrolled, DEMOGRAPHIC$Series, Fonc) # T4: 0, 1; T7: 0,1,2,6,8,90
tapply(DEMOGRAPHIC$individualgender, DEMOGRAPHIC$Series, Fonc) # T4: 1,2; T7: 1,2
tapply(DEMOGRAPHIC[DEMOGRAPHIC$individuallostdays < 900, ]$individuallostdays, DEMOGRAPHIC[DEMOGRAPHIC$individuallostdays < 900, ]$Series, range)
tapply(DEMOGRAPHIC$individualunwell, DEMOGRAPHIC$Series, Fonc) # T4: 0,1; T7: 0,1,2,3,4,30,90
tapply(DEMOGRAPHIC$relationhhh, DEMOGRAPHIC$Series, Fonc)


## MARINE ORGANIZATIONS

Organization7 <- read.csv("HH_Tbl_ORGANIZATION_T7.csv", header = T)
names(Organization7) <- tolower(names(Organization7))
compare_df_cols(Organization4, Organization7)

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

compare_df_cols(Organization4, Organization7)

#ORGANIZATION <- rbind(Organization4, Organization7) # run this code and then go to "Source_social_data_flat_files.R"
                                                  # The rest of the code below is to check variable values


Organization4$Series <- 1
Organization7$Series <- 2

ORGANIZATION <- rbind(Organization4, Organization7)

# CHECKING CHARACTER VARIABLES

Fonc <- function(x){x <- unique(x); x}

tapply(ORGANIZATION$name, ORGANIZATION$Series, Fonc)

# CHECKING NUMERIC VARIABLES

tapply(ORGANIZATION$contribution, ORGANIZATION$Series, range, na.rm = T)
tapply(ORGANIZATION[ORGANIZATION$days < 900, ]$days, ORGANIZATION[ORGANIZATION$days < 900, ]$Series, range) # T4: 0-365; T7: 0 -317
tapply(ORGANIZATION$entryhouseholdid, ORGANIZATION$Series, range, na.rm=T) # T7: all NAs
tapply(ORGANIZATION$meeting, ORGANIZATION$Series, Fonc) # T4: 0,1;  T7: 0,1
tapply(ORGANIZATION$position, ORGANIZATION$Series, Fonc) # T4: 1,2;  T7: 1,2

## NON-MARINE ORGANIZATIONS

NmOrganization7 <- read.csv("HH_Tbl_NMORGANIZATION_T7.csv", header = T)
names(NmOrganization7) <- tolower(names(NmOrganization7))
compare_df_cols(NmOrganization4, NmOrganization7)

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

compare_df_cols(NmOrganization4, NmOrganization7)

#NMORGANIZATION <- rbind(NmOrganization4, NmOrganization7) # run this code and then go to "Source_social_data_flat_files.R"
                                                           # The rest of the code below is to check variable values


NmOrganization4$Series <- 1
NmOrganization7$Series <- 2

NMORGANIZATION <- rbind(NmOrganization4, NmOrganization7)

# CHECKING CHARACTER VARIABLES

Fonc <- function(x){x <- unique(x); x}

tapply(NMORGANIZATION$name, NMORGANIZATION$Series, Fonc)

# CHECKING NUMERIC VARIABLES

tapply(NMORGANIZATION$contribution, NMORGANIZATION$Series, range, na.rm = T)
tapply(NMORGANIZATION[NMORGANIZATION$days < 900, ]$days, NMORGANIZATION[NMORGANIZATION$days < 900, ]$Series, range) # T4: 0-365; T7: 0 -525 (number > 365 transformed to NA in "Source_Social_data_flat_files.R")
tapply(NMORGANIZATION$entryhouseholdid, NMORGANIZATION$Series, range, na.rm=T) # T7: all NAs
tapply(NMORGANIZATION$meeting, NMORGANIZATION$Series, Fonc) # T4: 0,1;  T7: 0,1
tapply(NMORGANIZATION$position, NMORGANIZATION$Series, Fonc) # T4: 1,2;  T7: 1,2




Lthreat7 <- read.csv("HH_Tbl_LTHREAT_T7.csv", header = T)
names(Lthreat7) <- tolower(names(Lthreat7))
compare_df_cols(Lthreat4, Lthreat7)

Lsteps7 <- read.csv("HH_Tbl_LSTEPS_T7.csv", header = T)
names(Lsteps7) <- tolower(names(Lsteps7))
compare_df_cols(Lsteps4, Lsteps7)
