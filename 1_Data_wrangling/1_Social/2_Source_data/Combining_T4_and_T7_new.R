###### COMBINGING T4 AND T7 DATA ######

#### SECTIONS ####

## 1) ALIGN AND COMBINE T4 and T7 DATA BY SOURCING COMBINING_T4_AND_T7 SCRIPT
## 2) ADD NEW VARIABLES TO DATASET AND SAVE DATASET AS CSV FILES


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: ALIGN AND COMBINE T4 and T7 DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 1.1 Source combining script ----
## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)
## source the file
source("../1_QAQC/Checking_T7_vs_T4_new.R")

## set current directory as working directory
## because directory changed in the combining script
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: ADD NEW VARIABLES TO DATASET AND SAVE DATASET TO CSV FILES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## define functions for clearning ethnicity variables
# Function to remove all white space in string variables
trim <- function(x) gsub("^\\s+|\\s+$","",x)

# Function to clean string variables (lower case, remove punctuation)
str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% 
    tolower() %>% 
    removePunctuation(preserve_intra_word_dashes = FALSE) %>% 
    stripWhitespace() %>% 
    trim()
}


## set output file names
pref <- "HH_Tbl_"
suff <- paste0('_', today.date, ".csv")

# ---- 2.1.1 Create variables in WELLBEING for other table to merge with it ----
## create 1_household for T4 and 2_household for T7
## for later merge use
WELLBEING$tmp_household <- paste0(
  WELLBEING$Series,
  '_',
  WELLBEING$householdid
)
## create year post indicator
WELLBEING <- WELLBEING %>% 
  group_by(mpa) %>%
  mutate(
    base_year=interviewyear-min(interviewyear),
    yearID = paste0('0', as.character(base_year)),
    new_household=paste0(yearID, '_', as.character(householdid))
  )


# ---- 2.1.2 Create new variable for WELLBEING ----
## create new survey version variable
WELLBEING <- WELLBEING %>%
  mutate(
    surveyversion_new = case_when(
      interviewyear <= 2010 ~ "v0.13",
      interviewyear >= 2011 & interviewyear <= 2013 ~ "v2.3",
      interviewyear >= 2014 & interviewyear <= 2019 ~ "v2.4",
      TRUE ~ NA_character_
    ),
    paternalethnicity_clean = str_clean(paternalethnicity),
    maternalethnicity_clean = str_clean(maternalethnicity),
    seascape_id = ifelse(mpa >= 1 & mpa <= 6, "BHS", "SBS")
  )
# WELLBEING$surveyversion_new


# ---- 2.1.3 Write WELLBEING to csv ----
write.csv(
  WELLBEING %>% select(-c("Series", "base_year", "tmp_household")),
  paste0(pref, "WELLBEING", suff),
  row.names=FALSE
)


# WELLBEING$new_household
# DEMOGRAPHIC$household
# DEMOGRAPHIC$Series
# WELLBEING$tmp_household


# ---- 2.2.1 Create new variable for DEMOGRAPHIC ----
# DEMOGRAPHIC$relationhhh
## create new_householdhead based on relationhhh representing whether household head
DEMOGRAPHIC$new_householdhead <- ifelse(DEMOGRAPHIC$relationhhh==0, 0, 1)
## record current columns in DEMOGRAPHIC
demographic_cols <- colnames(DEMOGRAPHIC)


# ---- 2.2.2 Merge DEMOGRAPHIC with WELLBEING ----
DEMOGRAPHIC$tmp_household <- paste0(
  DEMOGRAPHIC$Series,
  '_',
  DEMOGRAPHIC$household
)
# DEMOGRAPHIC$tmp_household
# demo_merged <- merge(DEMOGRAPHIC, WELLBEING, by="tmp_household", all.x=TRUE)
demographic_merged <- left_join(
  DEMOGRAPHIC, WELLBEING, by="tmp_household", suffix=c("", "_drop")
)
# demographic_merged
# demographic_merged$yearID


# ---- 2.2.3 Create new variables for DEMOGRAPHIC ----
## Create new demographicId
demographic_merged <- demographic_merged %>%
  mutate(
    new_demographicid=paste0(yearID, '_', as.character(demographicid))
  )
# sum(is.na(demographic_merged$new_household))


# ---- 2.2.4 Write DEMOGRAPHIC to csv file ----
demographic_output <- demographic_merged %>%
  select(all_of(demographic_cols), "new_household", "new_demographicid", "yearID")
## write DEMOGRAPHIC to csv
write.csv(
  demographic_output %>% select(-c("Series")),
  paste0(pref, "DEMOGRAPHIC", suff),
  row.names=FALSE
)


# ---- 2.3.1 Merge ORGANIZATION with WELLBEING ----
## merge ORGANIZATION with WELLBEING for new_householdID
organization_cols <- colnames(ORGANIZATION)
ORGANIZATION$tmp_household <- paste0(
  ORGANIZATION$Series,
  '_',
  ORGANIZATION$household
)
# ORGANIZATION$tmp_household
organization_merged <- left_join(
  ORGANIZATION, WELLBEING, by="tmp_household", suffix=c("", "_drop")
)
# sum(is.na(organization_merged$new_household))


# ---- 2.3.2 Write ORGANIZATION to csv ----
organization_output <- organization_merged %>%
  select(all_of(organization_cols), "new_household", "yearID")
## write ORGANIZATION to csv
write.csv(
  organization_output %>% select(-c("Series")),
  paste0(pref, "ORGANIZATION", suff),
  row.names=FALSE
)


# ---- 2.4.1 Merge NMORGANIZATION with WELLBEING ----
## merge NMORGANIZATION with WELLBEING for new_householdID
nmorganization_cols <- colnames(NMORGANIZATION)
NMORGANIZATION$tmp_household <- paste0(
  NMORGANIZATION$Series,
  '_',
  NMORGANIZATION$household
)
NMORGANIZATION$tmp_household
nmorganization_merged <- left_join(
  NMORGANIZATION, WELLBEING, by="tmp_household", suffix=c("", "_drop")
)
# sum(is.na(nmorganization_merged$new_household))


# ---- 2.4.2 Write ORGANIZATION to csv ----
nmorganization_output <- nmorganization_merged %>%
  select(all_of(nmorganization_cols), "new_household", "yearID")
## write NMORGANIZATION to csv
write.csv(
  nmorganization_output %>% select(-c("Series")),
  paste0(pref, "NMORGANIZATION", suff),
  row.names=FALSE
)
