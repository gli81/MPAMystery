## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)

source("../1_QAQC/Checking_T7_vs_T4_new.R")

## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)

## add new column for each table
## 00_householdID
## create csv file
# min(WELLBEING$interviewyear)
# unique(WELLBEING$interviewyear)
# unique(Wellbeing4$interviewyear) ## 2017, 2018, 2019 are these T7?
# unique(Wellbeing7$interviewyear)
# WELLBEING <- WELLBEING %>% select(
#   -c("Series")
# )
# base_year = min(WELLBEING$interviewyear)
# WELLBEING$new_household <- paste0("0", as.character(WELLBEING$interviewyear - base_year), "_", WELLBEING$householdid)
# WELLBEING$new_household
# write.csv(WELLBEING, "WELLBEING.csv")

## SET OUTPUT FILE NAME
pref <- "HH_Tbl_"
suff <- paste0('_', today.date, ".csv")

## create 1_household for T4 and 2_household for T7
## for later merge
WELLBEING$tmp_household <- paste0(
  WELLBEING$Series,
  '_',
  WELLBEING$householdid
)
# WELLBEING$tmp_household
## create Tx indicator
WELLBEING <- WELLBEING %>% group_by(mpa) %>%
  mutate(
    base_year=interviewyear-min(interviewyear),
    yearID = paste0('0', as.character(base_year)),
    new_household=paste0(yearID, '_', as.character(householdid))
  )

WELLBEING$base_year
WELLBEING$yearID
WELLBEING$new_household
WELLBEING <- WELLBEING %>%
  mutate(surveyversion_new = case_when(
    interviewyear <= 2010 ~ "v0.13",
    interviewyear >= 2011 & interviewyear <= 2013 ~ "v2.3",
    interviewyear >= 2014 & interviewyear <= 2019 ~ "v2.4",
    TRUE ~ NA_character_
  ))
WELLBEING$surveyversion_new
## write WELLBEING to csv
write.csv(
  WELLBEING %>% select(-c("Series", "base_year", "tmp_household")),
  paste0(pref, "WELLBEING", suff)
)

## merge DEMOGRAPHIC with WELLBEING for new_householdID
WELLBEING$new_household
DEMOGRAPHIC$household
DEMOGRAPHIC$Series
WELLBEING$tmp_household

DEMOGRAPHIC$relationhhh
## create new_householdhead based on relationhhh
## to reflect whether 
DEMOGRAPHIC$new_householdhead <- ifelse(DEMOGRAPHIC$relationhhh==0, 0, 1)

demographic_cols <- colnames(DEMOGRAPHIC)
DEMOGRAPHIC$tmp_household <- paste0(
  DEMOGRAPHIC$Series,
  '_',
  DEMOGRAPHIC$household
)
DEMOGRAPHIC$tmp_household
# demo_merged <- merge(DEMOGRAPHIC, WELLBEING, by="tmp_household", all.x=TRUE)
demographic_merged <- left_join(DEMOGRAPHIC, WELLBEING, by="tmp_household", suffix=c("", "_drop"))
# demographic_merged
demographic_merged$yearID
demographic_merged <- demographic_merged %>%
  mutate(
    new_demographicid=paste0(yearID, '_', as.character(demographicid))
  )
sum(is.na(demographic_merged$new_household))
demographic_output <- demographic_merged %>%
  select(all_of(demographic_cols), "new_household", "new_demographicid")
## write DEMOGRAPHIC to csv
write.csv(
  demographic_output %>% select(-c("Series")),
  paste0(pref, "DEMOGRAPHIC", suff)
)

## merge ORGANIZATION with WELLBEING for new_householdID
organization_cols <- colnames(ORGANIZATION)
ORGANIZATION$tmp_household <- paste0(
  ORGANIZATION$Series,
  '_',
  ORGANIZATION$household
)
ORGANIZATION$tmp_household
organization_merged <- left_join(
  ORGANIZATION, WELLBEING, by="tmp_household", suffix=c("", "_drop")
)
sum(is.na(organization_merged$new_household))
organization_output <- organization_merged %>%
  select(all_of(organization_cols), "new_household")
## write ORGANIZATION to csv
write.csv(
  organization_output %>% select(-c("Series")),
  paste0(pref, "ORGANIZATION", suff)
)

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
# demographic_merged
sum(is.na(nmorganization_merged$new_household))
nmorganization_output <- nmorganization_merged %>%
  select(all_of(nmorganization_cols), "new_household")
## write NMORGANIZATION to csv
write.csv(
  nmorganization_output %>% select(-c("Series")),
  paste0(pref, "NMORGANIZATION", suff)
)
