

### THIS SCRIPT ADD THE LEVEL OF EDUCATION OF HOUSEHOLD HEADS TO THE WELLBEING DATA
pacman::p_load(
  rio, dplyr, janitor
)

education.lkp <- import("../../../x_Flat_data_files/Inputs/education_lkp_20240206.csv")

education.lkp1 <- education.lkp %>% 
  distinct(IndividualEducation,ed.level,.keep_all = T) %>%
  filter(ed.level!="NA") %>%
  filter(IndividualEducation!="NA") %>%#last line of code added to avoid duplicates in the left_join below
  mutate(
    individualeducation = IndividualEducation
  )

HH.ed <- demo_tbl %>% 
  filter(relationhhh==0) %>% 
  dplyr::select(household,individualeducation) %>% 
  left_join(education.lkp1, by=c("individualeducation")) %>%
  dplyr::select(-IndividualEducation) # this data contain the education level of the heads of households

household_tbl <- household_tbl %>% 
  left_join(HH.ed,by=c("householdid"="household")) # this code ad the variable ed.level (education level of 
                                    # heads of household) in the well-being data

## Unique value of ed.level
# 0: did not complete formal education
# 1: kindergarten
# 2: elementary school
# 3: junior high school
# 4: senior high shool
# 5: university