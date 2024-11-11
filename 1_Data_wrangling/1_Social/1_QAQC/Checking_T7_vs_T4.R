##############ALIGNING T4 and T7 data ############

## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/1_QAQC"
  )
)

pacman::p_load(
  rio, dplyr, janitor#, grid, gridExtra, reldist, Kendall, reshape2, ggplot2
)

# Date in format YYYYMMDD (could be changed but we believe it makes 
# most sense to avoid hyphens in file names and to have the date 
# first so files get sorted chronologically)
today.date <- gsub("-","",Sys.Date())

# Files (with package rio)
last.file <- function(dir.nam, nam){
  print(last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T))))
  import(
    paste0(
      dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T)))
    ),
    guess_max=50000
  )
}

# suppress messages for experimental new group_by() and summarise() functionality in dplyr
options(dplyr.summarise.inform = FALSE)

dir_name <- "../../../x_Flat_data_files/Inputs/"
## Load data
Wellbeing4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_household"
)
Demographic4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_demographic"
)
SETTLEMENT <- last.file(
  dir.nam=dir_name,
  nam="tbl_settlement"
)
Organization4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_marineorganizationmembership")
NmOrganization4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_nonmarineorganizationmembership"
)
Lthreat4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_localthreat"
)
Lsteps4 <- last.file(
  dir.nam=dir_name,
  nam="tbl_localstep"
)
MPA.LKP <- last.file(
  dir.nam=dir_name,
  nam="tbl_mpa"
)


## type conversion function
#############################################################
#############################################################
convert_type <- function(tbl1, tbl2) {
  tbl1_ <- deparse(substitute(tbl1))
  tbl2_ <- deparse(substitute(tbl2))
  comp_rslt <- compare_df_cols(tbl1, tbl2)
  ## Fill columns presented in T4 but not T7
  ## with NA in T7 (convert type of match T4)
  fill_NA <- comp_rslt %>%
    filter(is.na(tbl2))
  fill_NA <- fill_NA$column_name
  for (col_name in fill_NA) {
    tbl2[[col_name]] <- as.character(NA)
  }
  comp_rslt <- compare_df_cols(tbl1, tbl2)
  # print(comp_rslt)
  ## to character
  to_char <- comp_rslt %>%
    filter(tbl1 == "character") %>%
    filter(tbl2 != "character")
  to_char <- to_char$column_name
  for (col_name in to_char) {
    tbl2[[col_name]] <- as.character(tbl2[[col_name]])
  }
  comp_rslt <- compare_df_cols(tbl1, tbl2)
  # print(comp_rslt)
  ## to numeric
  to_num <- comp_rslt %>%
    filter(tbl1 == "numeric") %>%
    filter(tbl2 != "numeric")
  to_num <- to_num$column_name
  for (col_name in to_num) {
    tbl2[[col_name]] <- as.numeric(tbl2[[col_name]])
  }
  comp_rslt <- compare_df_cols(tbl1, tbl2)
  # print(comp_rslt)
  ## to logical
  to_logical <- comp_rslt %>%
    filter(tbl1 == "logical") %>%
    filter(tbl2 != "logical")
  to_logical <- to_logical$column_name
  for (col_name in to_logical) {
    tbl2[[col_name]] <- as.logical(tbl2[[col_name]])
  }
  comp_rslt <- compare_df_cols(tbl1, tbl2)
  # print(comp_rslt)
  return(tbl2)
}

#############################################################
#############################################################





## compare columns of Wellingbeing 4 and Wellbeing7
#############################################################
## will change every column name and column type in Wellbing7
## to match column name and column type in Wellbeing4
## load T7 data
Wellbeing7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_WELLBEING_T7"
)
names(Wellbeing7) <- tolower(names(Wellbeing7))
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt
## deal with typos and differences in column name
Wellbeing7 <- Wellbeing7 %>%
  rename(
    assetsatellite = assetsattellite,
    mpa = mpaid,
    settlement = settlementid
  )
## remove countryid from Wellbeing7 because it is not in Wellbeing4
Wellbeing7 <- Wellbeing7 %>%
  select(-countryid)

## column type conversion
Wellbeing7 <- convert_type(Wellbeing4, Wellbeing7)

## recode settlement
Wellbeing7$settlement <- ifelse(
  Wellbeing7$settlement== 178 | Wellbeing7$settlement==179,
  76,
  ifelse(
    Wellbeing7$settlement== 176 | Wellbeing7$settlement==177,
    77,
    Wellbeing7$settlement
  )
) # recode the settlements that are split in T7 back to their code in T4


Wellbeing4$Series <- 1
Wellbeing7$Series <- 2
WELLBEING <- rbind(Wellbeing4, Wellbeing7)

## Check character variables
## not sure character or numeric
# "interviewend", "interviewstart", 

to_check_char = c(
  "mpa", "settlement", "averageincomel", "averageincomeunits", "bestdaycatch",
  "bestdaycatchunits", "religion", "primarylivelihood", "secondarylivelihood",
  "surveyversionnumber", "tertiarylivelihood", "freqfishtime", "freqsalefish",
  "percentincomefish", "freqeatfish", "percentproteinfish",
  "majorfishtechnique", "householdbirth", "householddeath", "fsbalanceddiet",
  "fsdidnotlast", "fsnotenough", "marinegroup", "othergroup", 
  "bestincomel", "bestincomeunits", "cookingfuel", "economicstatusreason",
  "economicstatusreasonenglish", "economicstatustrend", "fsadultskip",
  "fschildportion", "fschildskip", "fseatless", "fsfreqadultskip",
  "fsfreqchildskip", "fshungry", "fslowcostfood", "fsnomealchild",
  "goodcatchunits", "goodfishunits", "interviewlength",
  "kkcode", "latsphere", "lonsphere", "maternalethnicity",
  "paternalethnicity", "placebemyself", "placebest", "placefavourite",
  "placefishhere", "placehappy", "placemiss", "poorcatchunits",
  "poorfishunits", "respondent", "rightsaccess", "rightsexclude",
  "rightsharvest", "rightsmanage", "rightstransfer", "secondaryrespondent",
  "socialconflict", "usualfish", "votedistrict", "votenational",
  "willingparticipant", "anyotherinfo"
)
# for (col_name in names(WELLBEING)) {
#   print(col_name)
#   if (col_name == "Series") {
#   } else if (is.character(WELLBEING[[col_name]])) {
#     print(paste0(col_name, " for T4:"))
#     sep = WELLBEING %>% filter(Series==1)
#     print(table(sep[[col_name]]))
#   }
# }

## to do the metadata
sink("metadata_prep.txt")

for (col_name in to_check_char) {
  print(col_name)
  print(
    tapply(WELLBEING[[col_name]], WELLBEING$Series, unique)
  )
  cat("NA")
  print(sum(is.na(WELLBEING[[col_name]])))
  for (specific_value in c(
    "991", "992", "993", "994", "995", "996", "997", "998", "999"
  )) {
    cat(specific_value, ": ")
    print(sum(WELLBEING[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

to_check_num <- c(
  "householdsize", "assetbicycle", "assetboatinboard", "assetboatnomotor",
  "assetboatoutboard",
  "assetcar", "assetcartruck", "assetcd", "assetcellphone", "assetdvd",
  "assetentertain", "assetgenerator", "assetlandlinephone", "assetmotorcycle",
  "assetphonecombined", "assetradio", "assetsatellite", "assetstereo",
  "assettruck", "assettv", 
  "goodcatch",
  "goodfishincomel", "interviewday",
  "interviewmonth", "interviewyear", "latdeg", "latfrac", "latmin", "latsec",
  "lessproductivedaysfishing", "londeg", "lonfrac", "lonmin", "lonsec",
  "moreproductivedaysfishing",
  "numglobalaction", "numglobalthreat", "numlocalaction", "numlocalthreat",
  "poorcatch", "poorfishincomel", "timemarket", "yearsresident",
  "numbermarinegroup", "numberothergroup"
)

for (col_name in to_check_num) {
  print(col_name)
  print(
    tapply(
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]],
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,]$Series,
      function(x){range(x, na.rm = T)}
    )
  )
  print(
    tapply(
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]],
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,]$Series,
      function(x){mean(x, na.rm=T)})
  )
  print(
    tapply(
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]],
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,]$Series,
      function(x){median(x, na.rm=T)})
  )
  cat("overall mean: ")
  print(
    mean( WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(WELLBEING[[col_name]])))
  for (specific_value in 991:999) {
    cat(specific_value, ": ")
    print(sum(WELLBEING[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
#############################################################





## compare columns of Demographic4 and Demographic7
#############################################################
## similar to Wellbeing
Demographic7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_DEMOGRAPHIC_T7"
)
names(Demographic7) <- tolower(names(Demographic7))
comp_rslt <- compare_df_cols(Demographic4, Demographic7)
comp_rslt
## change column name
Demographic7 <- Demographic7 %>%
  rename(
    household = householdid
  )

Demographic7 <- convert_type(Demographic4, Demographic7)
## check result
comp_rslt <- compare_df_cols(Demographic4, Demographic7)
comp_rslt

Demographic4$Series <- 1
Demographic7$Series <- 2
DEMOGRAPHIC <- rbind(Demographic4, Demographic7)

to_check_char <- c(
  "demographicid", "household", "demographiccode", "individualname",
  "relationhhh", "individualgender", "individualeducation", "individualedlevel",
  "individualenrolled", "householdhead", "individualunwell"
)

for (col_name in to_check_char) {
  print(col_name)
  print(
    tapply(DEMOGRAPHIC[[col_name]], DEMOGRAPHIC$Series, unique)
  )
  cat("NA")
  print(sum(is.na(DEMOGRAPHIC[[col_name]])))
  for (specific_value in c(
    "991", "992", "993", "994", "995", "996", "997", "998", "999"
  )) {
    cat(specific_value, ": ")
    print(sum(DEMOGRAPHIC[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

to_check_num <- c(
  "individualage", "individualdaysunwell", "individuallostdays"
)
for (col_name in to_check_num) {
  print(col_name)
  print(
    tapply(
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,][[col_name]],
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,]$Series,
      function(x){range(x, na.rm = T)}
    )
  )
  print(
    tapply(
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,][[col_name]],
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,]$Series,
      function(x){mean(x, na.rm=T)})
  )
  print(
    tapply(
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,][[col_name]],
      DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,]$Series,
      function(x){median(x, na.rm=T)})
  )
  cat("overall mean: ")
  print(
    mean( DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(DEMOGRAPHIC[!DEMOGRAPHIC[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(DEMOGRAPHIC[[col_name]])))
  for (specific_value in 991:999) {
    cat(specific_value, ": ")
    print(sum(DEMOGRAPHIC[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
#############################################################





## compare columns of Organization4 and Organization7
#############################################################
## similar to Wellbeing
Organization7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_ORGANIZATION_T7"
)
names(Organization7) <- tolower(names(Organization7))
comp_rslt <- compare_df_cols(Organization4, Organization7)
comp_rslt
## change column name
Organization7 <- Organization7 %>%
  rename(
    contribution=marinecontribution,
    days=marinedays,
    household=householdid,
    meeting=marinemeeting,
    morganizationid=organizationid,
    name=marinegroupname,
    position=marineposition
  )
comp_rslt <- compare_df_cols(Organization4, Organization7)
comp_rslt
## with NA in Organization7 (convert type of match Organization4)
Organization7 <- convert_type(Organization4, Organization7)
## recode
Organization7$meeting <- ifelse(
  Organization7$meeting==7,
  NA,Organization7$meeting
)
Organization7$position <- ifelse(
  Organization7$position==0 | Organization7$position==4,
  994,
  Organization7$position
)

Organization4$Series <- 1
Organization7$Series <- 2
ORGANIZATION <- rbind(Organization4, Organization7)

to_check_char <- c(
  "entryhouseholdid", "name", "position", "meeting"
)
for (col_name in to_check_char) {
  print(col_name)
  print(
    tapply(ORGANIZATION[[col_name]], ORGANIZATION$Series, unique)
  )
  cat("NA")
  print(sum(is.na(ORGANIZATION[[col_name]])))
  for (specific_value in c(
    "991", "992", "993", "994", "995", "996", "997", "998", "999"
  )) {
    cat(specific_value, ": ")
    print(sum(ORGANIZATION[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
# tapply(ORGANIZATION$name, ORGANIZATION$Series, unique)
# cat("NA: ")
# print(sum(is.na(ORGANIZATION$name)))
# for (specific_value in c(
#   "991", "992", "993", "994", "995", "996", "997", "998", "999"
# )) {
#   cat(specific_value, ": ")
#   print(sum(ORGANIZATION$name==specific_value, na.rm=T))
# }
to_check_num <- c(
  "contribution", "days"
)
for (col_name in to_check_num) {
  print(col_name)
  print(
    tapply(
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){range(x, na.rm = T)}
    )
  )
  print(
    tapply(
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){mean(x, na.rm=T)})
  )
  print(
    tapply(
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){median(x, na.rm=T)})
  )
  cat("overall mean: ")
  print(
    mean( ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(ORGANIZATION[!ORGANIZATION[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(ORGANIZATION[[col_name]])))
  for (specific_value in 991:999) {
    cat(specific_value, ": ")
    print(sum(ORGANIZATION[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
#############################################################





## Non Marine Organizations
#############################################################
NmOrganization7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_NMORGANIZATION_T7"
)
names(NmOrganization7) <- tolower(names(NmOrganization7))
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt
## change column name
NmOrganization7 <- NmOrganization7 %>%
  rename(
    contribution=othergroupcontribution,
    days=othergroupdays,
    household=householdid,
    meeting=othergroupmeeting,
    name=othergroupname,
    position=othergroupposition
  )
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt
NmOrganization7 <- convert_type(NmOrganization4, NmOrganization7)
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt

NmOrganization4$Series <- 1
NmOrganization7$Series <- 2
NMORGANIZATION <- rbind(NmOrganization4, NmOrganization7)


to_check_char <- c(
  "entryhouseholdid", "name", "position", "meeting"
)
for (col_name in to_check_char) {
  print(col_name)
  print(
    tapply(NMORGANIZATION[[col_name]], NMORGANIZATION$Series, unique)
  )
  cat("NA")
  print(sum(is.na(NMORGANIZATION[[col_name]])))
  for (specific_value in c(
    "991", "992", "993", "994", "995", "996", "997", "998", "999"
  )) {
    cat(specific_value, ": ")
    print(sum(NMORGANIZATION[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
to_check_num <- c(
  "contribution", "days"
)
for (col_name in to_check_num) {
  print(col_name)
  print(
    tapply(
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){range(x, na.rm = T)}
    )
  )
  print(
    tapply(
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){mean(x, na.rm=T)})
  )
  print(
    tapply(
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,][[col_name]],
      NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,]$Series,
      function(x){median(x, na.rm=T)})
  )
  cat("overall mean: ")
  print(
    mean( NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(NMORGANIZATION[!NMORGANIZATION[[col_name]] %in% 990:999,][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(NMORGANIZATION[[col_name]])))
  for (specific_value in 991:999) {
    cat(specific_value, ": ")
    print(sum(NMORGANIZATION[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}
#############################################################


## Births
#############################################################
Births4 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_BIRTHS"
)
# Births7 <- last.file(
#   dir.nam=dir_name,
#   nam="" ## ?
# )

## Deaths
#############################################################
Deaths4 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_DEATHS"
)
# Deaths7 <- last.file(
#   dir.nam=dir_name,
#   nam="" ## ?
# )
sink()


Lthreat7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_LTHREAT_T7"
)
names(Lthreat7) <- tolower(names(Lthreat7))
compare_df_cols(Lthreat4, Lthreat7)

Lsteps7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_LSTEPS_T7"
)
names(Lsteps7) <- tolower(names(Lsteps7))
compare_df_cols(Lsteps4, Lsteps7)