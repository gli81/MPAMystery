###### SUBSTITUTE INVALID VALUES WITH ERROR CODE IN DATASET #####


### SECTIONS ###

## 1) Substitute invalid values with new error code in WELLBEING
## 2) Substitute invalid values with new error code in DEMOGRAPHIC
## 3) Substitute invalid values with new error code in ORGANIZATION and NMORGANIZATION

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 0: REMOVE DUPLICATES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/4_Data_cleaning"
  )
)

## read household table
pacman::p_load(
  rio, dplyr, janitor
)
last.file <- function(dir.nam, nam){
  print(last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T))))
  import(
    paste0(
      dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T)))
    ),
    guess_max=50000
  )
}
today.date <- gsub("-","",Sys.Date())

dir_ <- "../../../x_Flat_data_files/Inputs/combined/"
pref <- "HH_Tbl_"
suff <- paste0('_', today.date, ".csv")

## load household data
household_tbl <- last.file(
  dir.nam=dir_,
  nam="HH_Tbl_WELLBEING"
)

## load demographic data
demo_tbl <- last.file(
  dir.nam=dir_,
  nam="HH_Tbl_DEMOGRAPHIC"
)

## load organization data
org_tbl <- last.file(
  dir.nam=dir_,
  nam="HH_Tbl_ORGANIZATION"
)

nmorg_tbl <- last.file(
  dir.nam=dir_,
  nam="HH_Tbl_NMORGANIZATION"
)

## remove duplication in WELLBEING
household_tbl <- household_tbl %>%
  distinct()

## remove duplication in DEMOGRAPHIC
demo_tbl <- demo_tbl %>%
  distinct()

## remove duplication in ORGANIZATION
org_tbl <- org_tbl %>%
  distinct()

## create new PK in NMORGANIZATION to ensure unique PK
nmorg_tbl <- nmorg_tbl %>% mutate(
  new_nmorganizationid = paste0('0', yearID, '_', nmorganizationid)
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SUBSTITUTION IN WELLBEING ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## household valid range
HH_VALID_RANGE <- list(
  ## RELIGION_CHOICES
  "religion" = c(1:7),
  ## LATITUDE_DEGREE_CHOICES
  "latdeg" = c(0:90),
  ## LONGITUDE_DEGREE_CHOICES
  "londeg" = c(0:180),
  ## MINUTES_SECONDS_CHOICES
  "latmin" = c(0:60),
  "latsec" = c(0:60),
  "lonmin" = c(0:60),
  "lonsec" = c(0:60),
  ## SECONDS_FRACTION_CHOICES
  "latfrac" = c(0:9),
  "lonfrac" = c(0:9),
  ## LATITUDE_SPHERE_CHOICES
  "latsphere" = c(
    "N", "n", "North", "north", "S", "s", "South", "south", "990", "991",
    "992", "993", "995", "996", "997", "998", "999"
  ),
  ## LONGITUDE_SPHERE_CHOICES
  "lonsphere" = c(
    "E", "e", "East", "east", "W", "w", "West", "west", "990", "991",
    "992", "993", "995", "996", "997", "998", "999"
  ),
  ## DAY_OF_MONTH_CHOICES
  "interviewday" = c(1:31),
  ## MONTH_CHOICES
  "interviewmonth" = c(1:12),
  ## YEAR_CHOICES
  "interviewyear" = c(2000:2050),
  ## ECONOMIC_STATUS_TREND_CHOICES
  "economicstatustrend" = c(1:5),
  ## COOKING_FUEL_CHOICES
  "cookingfuel" = c(1:5),
  ## YES_NO_CHOICES
  "householddeath" = c(0:1),
  "householdbirth" = c(0:1),
  "fsadultskip" = c(0:1),
  "fseatless" = c(0:1),
  "fshungry" = c(0:1),
  "fschildportion" = c(0:1),
  "fschildskip" = c(0:1),
  "rightsaccess" = c(0:1),
  "rightsharvest" = c(0:1),
  "rightsmanage" = c(0:1),
  "rightsexclude" = c(0:1),
  "rightstransfer" = c(0:1),
  "marinegroup" = c(0:1),
  "othergroup" = c(0:1),
  "votedistrict" = c(0:1),
  "votenational" = c(0:1),
  
  ## FS_CHOICES
  "fsnotenough" = c(1:3),
  "fsdidnotlast" = c(1:3),
  "fsbalanceddiet" = c(1:3),
  "fslowcostfood" = c(1:3),
  "fsnomealchild" = c(1:3),
  
  ## FS_FREQ_CHOICES, FS_FREQ_ADULT_SKIP
  "fsfreqadultskip" = c(1:3),
  "fsfreqchildskip" = c(1:3),
  
  ## SOCIAL_CONFLICT_CHOICES
  "socialconflict" = c(1:5),
  ## ATT_SCALE_CHOICES
  "placehappy" = c(1:5),
  "placefavourite" = c(1:5),
  "placemiss" = c(1:5),
  "placebest" = c(1:5),
  "placefishhere" = c(1:5),
  "placebemyself" = c(1:5),
  
  ## MPA
  "mpa" = c(1:21),
  "settlement" = c(1:18, 20:271),
  
  
  ## LIVELIHOOD_CHOICES
  "primarylivelihood" = c(1:7),
  "secondarylivelihood" = c(1:7),
  "tertiarylivelihood" = c(1:7),
  
  ## FREQ_FISH_TIME_CHOICES
  "freqfishtime" = c(1:5),
  "freqsalefish" = c(1:5),
  "freqeatfish" = c(1:5),
  
  ## NONE_TO_ALL_SCALE
  "percentincomefish" = c(1:5),
  "percentproteinfish" = c(1:5),
  
  ## MAJOR_FISH_TECHNIQUE_CHOICES
  "majorfishtechnique" = c(1:6),
  
  ## number of days
  "lessproductivedaysfishing" = c(0:366),
  "moreproductivedaysfishing" = c(0:366)
)

## define MACROS
SKIP_CODE <- 990:999
INVALID_CODE <- 70773

## create dataframe for errors
household_err <- data.frame(
  row_number = integer(),
  household_id = character(),
  mpa_id = integer(),
  settlement_id = integer(),
  err_col_name = character(),
  err_value = integer()
)

## go over every columns need checking
for (name in names(HH_VALID_RANGE)) {
  ## get the index
  criteria <- !(is.na(household_tbl[[name]])) &
              !(household_tbl[[name]] %in% HH_VALID_RANGE[[name]]) &
              !(household_tbl[[name]] %in% SKIP_CODE)
  indices <- which(criteria)
  if (!length(indices) == 0) {
    ## add error
    for (i in 1:length(indices)) {
      household_err <- household_err %>% add_row(
        row_number=indices[i],
        household_id=household_tbl$new_household[indices[i]],
        mpa_id=household_tbl$mpa[indices[i]],
        settlement_id=household_tbl$settlement[indices[i]],
        err_col_name=name,
        err_value=household_tbl[[name]][indices[i]]
      )
    }
  }
  household_tbl[[name]][indices] <- INVALID_CODE
}

## drop variables that are to be removed
household_tbl <- household_tbl %>%
  select(-c(
    "worstdaycatch", "worstdaycatchunits", "bestdaycatch", "bestdaycatch",
    "averageincomel", "averageincomeunits", "worstincomel", "worstincomeunits",
    "bestincomel", "bestincomeunits", "entrycomputeridentifier",
    "entryhouseholdid", "pilotreferencecode", "interviewdate",
    "baseline_t2_pairs"
  ))

## output
write.csv(
  household_tbl,
  paste0("./", pref, "WELLBEING", paste0('_', today.date, "_cleaned.csv")),
  row.names=FALSE
)
write.csv(
  household_err,
  paste0("./", pref, "WELLBEING", paste0('_', today.date, "_error.csv")),
  row.names=F
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: SUBSTITUTION IN DEMOGRAPHIC ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create dataframe for errors
demo_err <- data.frame(
  row_number = integer(),
  household_id = character(),
  demographic_id = character(),
  err_col_name = character(),
  err_value = integer()
)

DEMO_VALID_RANGE <- list(
  ## YES_NO_CHOICES
  "individualenrolled" = c(0:1),
  "householdhead" = c(0:1),
  "individualunwell" = c(0:1),
  ## GENDER_CHOICES
  "individualgender" = c(1:2),
  ## EDUCATION_LEVEL_CHOICES
  "individualedlevel" = c(0:5),
  ## RELATIONSHIP_CHOICES
  "relationhhh" = c(0:13),
  ## days
  "individualdaysunwell" = c(0:31),
  "individuallostdays" = c(0:31),
  ## individual age
  "individualage" = c(0:150)
)

## go over every columns need checking
for (name in names(DEMO_VALID_RANGE)) {
  ## get the index
  criteria <- !(is.na(demo_tbl[[name]])) &
    !(demo_tbl[[name]] %in% DEMO_VALID_RANGE[[name]]) &
    !(demo_tbl[[name]] %in% SKIP_CODE)
  indices <- which(criteria)
  if (!length(indices) == 0) {
    ## add error
    for (i in 1:length(indices)) {
      demo_err <- demo_err %>% add_row(
        row_number=indices[i],
        household_id=demo_tbl$new_household[indices[i]],
        demographic_id=demo_tbl$new_demographicid[indices[i]],
        err_col_name=name,
        err_value=demo_tbl[[name]][indices[i]]
      )
    }
  }
  demo_tbl[[name]][indices] <- INVALID_CODE
}

## remove variables
demo_tbl <- demo_tbl %>%
  select(-c(
    "individualedlevel"
  ))

## output
write.csv(
  demo_tbl,
  paste0("./", pref, "DEMOGRAPHIC", paste0('_', today.date, "_cleaned.csv")),
  row.names=FALSE
)
write.csv(
  demo_err,
  paste0("./", pref, "DEMOGRAPHIC", paste0('_', today.date, "_error.csv")),
  row.names=F
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: SUBSTITUTION IN MORGANIZATION AND NMORGANIZATION ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create dataframe for errors
org_err <- data.frame(
  row_number = integer(),
  household_id = character(),
  organization_id = character(),
  err_col_name = character(),
  err_value = integer()
)

nmorg_err <- data.frame(
  row_number = integer(),
  household_id = character(),
  organization_id = character(),
  err_col_name = character(),
  err_value = integer()
)

ORG_VALID_RANGE <- list(
  ## ORGANIZATION_POSITION_CHOICES
  "positions" = c(1:2),
  ## YES_NO_CHOICES
  "meeting" = c(0:1),
  ## days
  "days" = c(0:366)
)

## go over every columns need checking
for (name in names(ORG_VALID_RANGE)) {
  ## get the index
  criteria <- !(is.na(org_tbl[[name]])) &
    !(org_tbl[[name]] %in% ORG_VALID_RANGE[[name]]) &
    !(org_tbl[[name]] %in% SKIP_CODE)
  indices <- which(criteria)
  if (!length(indices) == 0) {
    ## add error
    for (i in 1:length(indices)) {
      org_err <- org_err %>% add_row(
        row_number=indices[i],
        household_id=org_tbl$new_household[indices[i]],
        organization_id=org_tbl$organizationid[indices[i]],
        err_col_name=name,
        err_value=org_tbl[[name]][indices[i]]
      )
    }
    org_tbl[[name]][indices] <- INVALID_CODE
  }
  
  criteria <- !(is.na(nmorg_tbl[[name]])) &
    !(nmorg_tbl[[name]] %in% ORG_VALID_RANGE[[name]]) &
    !(nmorg_tbl[[name]] %in% SKIP_CODE)
  indices <- which(criteria)
  if (!length(indices) == 0) {
    ## add error
    for (i in 1:length(indices)) {
      nmorg_err <- nmorg_err %>% add_row(
        row_number=indices[i],
        household_id=nmorg_tbl$new_household[indices[i]],
        organization_id=nmorg_tbl$organizationid[indices[i]],
        err_col_name=name,
        err_value=nmorg_tbl[[name]][indices[i]]
      )
    }
    nmorg_tbl[[name]][indices] <- INVALID_CODE
  }
}

## output
write.csv(
  org_tbl,
  paste0("./", pref, "ORGANIZATION", paste0('_', today.date, "_cleaned.csv")),
  row.names=FALSE
)
write.csv(
  org_err,
  paste0("./", pref, "ORGANIZATION", paste0('_', today.date, "_error.csv")),
  row.names=F
)
write.csv(
  nmorg_tbl,
  paste0("./", pref, "NMORGANIZATION", paste0('_', today.date, "_cleaned.csv")),
  row.names=FALSE
)
write.csv(
  nmorg_err,
  paste0("./", pref, "NMORGANIZATION", paste0('_', today.date, "_error.csv")),
  row.names=F
)
