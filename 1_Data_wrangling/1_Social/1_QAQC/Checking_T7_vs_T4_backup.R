## set current directory as working directory
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/1_QAQC"
  )
)

pacman::p_load(
  rio, dplyr, janitor#, grid, gridExtra, reldist, Kendall, reshape2, ggplot2, dplyr
)

# Date in format YYYYMMDD (could be changed but we believe it makes 
# most sense to avoid hyphens in file names and to have the date 
# first so files get sorted chronologically)
today.date <- gsub("-","",Sys.Date())

# Files (with package rio)
last.file <- function(dir.nam, nam){
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
## load T7 data
Wellbeing7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_WELLBEING_T7"
)

## compare columns of Wellingbeing 4 and Wellbeing7
## will change every column name and column type in Wellbing7
## to match column name and column type in Wellbeing4
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
  print(comp_rslt)
  integer_to_numeric <- comp_rslt %>%
    filter(tbl1 == "numeric") %>%
    filter(tbl2 == "integer")
}
convert_type(Wellbeing4, Wellbeing7)
#############################################################
#############################################################
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt
## convert integer to numeric
integer_to_numeric <- comp_rslt %>%
  filter(Wellbeing4 == "numeric") %>%
  filter(Wellbeing7 == "integer")
integer_to_numeric <- integer_to_numeric$column_name
Wellbeing7 <- Wellbeing7 %>%
  mutate(across(all_of(integer_to_numeric), ~as.numeric(.)))
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt

rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt
## Fill columns presented in Wellbeing4 but not Wellbeing7
## with NA in Wellbeing7 (convert type of match Wellbeing4)
fill_NA <- comp_rslt %>%
  filter(is.na(Wellbeing7))
fill_NA <- fill_NA$column_name
for (col_name in fill_NA) {
  Wellbeing7[[col_name]] <- as.character(NA)
}
## to character
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt
to_char <- comp_rslt %>%
  filter(Wellbeing4 == "character") %>%
  filter(Wellbeing7 != "character")
to_char <- to_char$column_name
for (col_name in to_char) {
  Wellbeing7[[col_name]] <- as.character(Wellbeing7[[col_name]])
}
## to numeric
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt %>% filter(Wellbeing4 != Wellbeing7)
to_num <- comp_rslt %>%
  filter(Wellbeing4 == "numeric") %>%
  filter(Wellbeing7 != "numeric")
to_num <- to_num$column_name
for (col_name in to_num) {
  Wellbeing7[[col_name]] <- as.numeric(Wellbeing7[[col_name]])
}
## to logical
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt %>% filter(Wellbeing4 != Wellbeing7)
to_logical <- comp_rslt %>%
  filter(Wellbeing4 == "logical") %>%
  filter(Wellbeing7 != "logical")
to_logical <- to_logical$column_name
for (col_name in to_logical) {
  Wellbeing7[[col_name]] <- as.logical(Wellbeing7[[col_name]])
}
rm(comp_rslt)
comp_rslt <- compare_df_cols(Wellbeing4, Wellbeing7)
comp_rslt %>% filter(Wellbeing4 != Wellbeing7)
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

## compare columns of Demographic4 and Demographic7
## similar to Wellbeing
Demographic7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_DEMOGRAPHIC_T7"
)
names(Demographic7) <- tolower(names(Demographic7))
rm(comp_rslt)
comp_rslt <- compare_df_cols(Demographic4, Demographic7)
comp_rslt
## change column name
Demographic7 <- Demographic7 %>%
  rename(
    household = householdid
  )
## with NA in Demographic7 (convert type of match Demographic4)
fill_NA <- comp_rslt %>%
  filter(is.na(Demographic7))
fill_NA <- fill_NA$column_name
for (col_name in fill_NA) {
  Demographic7[[col_name]] <- as.character(NA)
}
## to char
to_char <- comp_rslt %>%
  filter(Demographic4 == "character") %>%
  filter(Demographic7 != "character")
to_char <- to_char$column_name
for (col_name in to_char) {
  Demographic7[[col_name]] <- as.character(Demographic7[[col_name]])
}
## to numeric
to_num <- comp_rslt %>%
  filter(Demographic4 == "numeric") %>%
  filter(Demographic7 != "numeric")
to_num <- to_num$column_name
for (col_name in to_num) {
  Demographic7[[col_name]] <- as.numeric(Demographic7[[col_name]])
}
## to logical
rm(comp_rslt)
comp_rslt <- compare_df_cols(Demographic4, Demographic7)
comp_rslt
to_logical <- comp_rslt %>%
  filter(Demographic4 == "logical") %>%
  filter(Demographic7 != "logical")
to_logical <- to_logical$column_name
for (col_name in to_logical) {
  Demographic7[[col_name]] <- as.logical(Demographic7[[col_name]])
}
rm(comp_rslt)
comp_rslt <- compare_df_cols(Demographic4, Demographic7)
comp_rslt








## compare columns of Organization4 and Organization7
## similar to Wellbeing
Organization7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_ORGANIZATION_T7"
)
names(Organization7) <- tolower(names(Organization7))
rm(comp_rslt)
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
rm(comp_rslt)
comp_rslt <- compare_df_cols(Organization4, Organization7)
comp_rslt
## with NA in Organization7 (convert type of match Organization4)
fill_NA <- comp_rslt %>%
  filter(is.na(Organization7))
fill_NA <- fill_NA$column_name
for (col_name in fill_NA) {
  Organization7[[col_name]] <- as.character(NA)
}
## to char
to_char <- comp_rslt %>%
  filter(Organization4 == "character") %>%
  filter(Organization7 != "character")
to_char <- to_char$column_name
for (col_name in to_char) {
  Organization7[[col_name]] <- as.character(Organization7[[col_name]])
}
## to numeric
to_num <- comp_rslt %>%
  filter(Organization4 == "numeric") %>%
  filter(Organization7 != "numeric")
to_num <- to_num$column_name
for (col_name in to_num) {
  Organization7[[col_name]] <- as.numeric(Organization7[[col_name]])
}
## to logical
rm(comp_rslt)
comp_rslt <- compare_df_cols(Organization4, Organization7)
comp_rslt
to_logical <- comp_rslt %>%
  filter(Organization4 == "logical") %>%
  filter(Organization7 != "logical")
to_logical <- to_logical$column_name
for (col_name in to_logical) {
  Organization7[[col_name]] <- as.logical(Organization7[[col_name]])
}
rm(comp_rslt)
comp_rslt <- compare_df_cols(Organization4, Organization7)
comp_rslt
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








## Non Marine Organizations
NmOrganization7 <- last.file(
  dir.nam=dir_name,
  nam="HH_Tbl_NMORGANIZATION_T7"
)
names(NmOrganization7) <- tolower(names(NmOrganization7))
rm(comp_rslt)
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
rm(comp_rslt)
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt
## with NA in NmOrganization7 (convert type of match NmOrganization4)
fill_NA <- comp_rslt %>%
  filter(is.na(NmOrganization7))
fill_NA <- fill_NA$column_name
for (col_name in fill_NA) {
  NmOrganization7[[col_name]] <- as.character(NA)
}
## to char
to_char <- comp_rslt %>%
  filter(NmOrganization4 == "character") %>%
  filter(NmOrganization7 != "character")
to_char <- to_char$column_name
for (col_name in to_char) {
  NmOrganization7[[col_name]] <- as.character(NmOrganization7[[col_name]])
}
## to numeric
to_num <- comp_rslt %>%
  filter(NmOrganization4 == "numeric") %>%
  filter(NmOrganization7 != "numeric")
to_num <- to_num$column_name
for (col_name in to_num) {
  NmOrganization7[[col_name]] <- as.numeric(NmOrganization7[[col_name]])
}
## to logical
rm(comp_rslt)
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt
to_logical <- comp_rslt %>%
  filter(NmOrganization4 == "logical") %>%
  filter(NmOrganization7 != "logical")
to_logical <- to_logical$column_name
for (col_name in to_logical) {
  NmOrganization7[[col_name]] <- as.logical(NmOrganization7[[col_name]])
}
rm(comp_rslt)
comp_rslt <- compare_df_cols(NmOrganization4, NmOrganization7)
comp_rslt


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


### make a function