
###### CALCULATE HOUSEHOLD-LEVEL INDICES FOR BHS WITH DATA THAT INCLUDE T7 #####


### SECTIONS ###

## 1) ASSET INDEX USING PCA (PCA per MPA)
## 2) OTHER INDICES
## 3) EXPORT WELLBEING TABLE ALONG WITH INDICES



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: ASSET INDEX USING PCA (PCA per MPA) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)

source("Source_social_data_flat_files_new.R")

setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/3_Calculating_indicators"
  )
)

# ---- 1.1 Create a binary variable for each asset (Yes=1, No=0) ----

HHData$Entertain_yes <- ifelse(HHData$Entertain>0, 1, ifelse(is.na(HHData$Entertain), NA, 0))
HHData$PhoneCombined_yes <- ifelse(HHData$PhoneCombined>0, 1, ifelse(is.na(HHData$PhoneCombined), NA, 0))
HHData$Satellite_yes <- ifelse(HHData$Satellite>0, 1, ifelse(is.na(HHData$Satellite), NA, 0))
HHData$TV_yes <- ifelse(HHData$TV>0, 1, ifelse(is.na(HHData$TV), NA, 0))
HHData$Generator_yes <- ifelse(HHData$Generator>0, 1, ifelse(is.na(HHData$Generator), NA, 0))
HHData$Bicycle_yes <- ifelse(HHData$Bicycle>0, 1, ifelse(is.na(HHData$Bicycle), NA, 0))
HHData$Motorcycle_yes <- ifelse(HHData$Motorcycle>0, 1, ifelse(is.na(HHData$Motorcycle), NA, 0))
HHData$CarTruck_yes <- ifelse(HHData$CarTruck>0, 1, ifelse(is.na(HHData$CarTruck), NA, 0))
HHData$Fuel_nonBiomass <- ifelse(HHData$CookingFuel.Biomass==1, 0, ifelse(HHData$CookingFuel.Biomass==0, 1, NA))
HHData$BoatInboard_yes <- ifelse(HHData$BoatInboard>0, 1, ifelse(is.na(HHData$BoatInboard), NA, 0))
HHData$BoatOutboard_yes <- ifelse(HHData$BoatOutboard>0, 1, ifelse(is.na(HHData$BoatOutboard), NA, 0))
HHData$BoatNoMotor_0 <- ifelse(HHData$BoatNoMotor>0, 0, ifelse(is.na(HHData$BoatNoMotor), NA, 0)) # no motor boat does not count
HHData$MotorBoat <- rowSums(HHData[ ,c("BoatInboard_yes", "BoatOutboard_yes", "BoatNoMotor_0")], na.rm=TRUE)
HHData$MotorBoat <- ifelse(HHData$MotorBoat>0, 1,0)
HHData$MotorBoat <- ifelse((is.na(HHData$BoatInboard) & is.na(HHData$BoatOutboard) & is.na(HHData$BoatNoMotor)),NA, HHData$MotorBoat)
HHData$BoatNoMotor_yes <- ifelse(HHData$BoatNoMotor>0, 1, ifelse(is.na(HHData$BoatNoMotor), NA, 0))
HHData$Boat <- rowSums(HHData[ ,c("BoatInboard_yes", "BoatOutboard_yes", "BoatNoMotor_yes")], na.rm=TRUE)
HHData$Boat <- ifelse(HHData$Boat>0, 1,0)
HHData$Boat <- ifelse((is.na(HHData$BoatInboard) & is.na(HHData$BoatOutboard) & is.na(HHData$BoatNoMotor)),NA, HHData$Boat)

## define get_pa function
## to get 
get_pa <- function(num){
  PA <- HHData[HHData$MPAID==num, ]
  PCA.PA <- PA %>%
    select(
      Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes,
      Bicycle_yes, Motorcycle_yes, CarTruck_yes, MotorBoat, Fuel_nonBiomass,
      Boat
    ) %>%
    na.omit()
  sapply(PCA.PA, FUN=mean)
  PCA.PA <- PCA.PA %>%
    select(-CarTruck_yes)
  PA.pca <- princomp(PCA.PA)
  summary(PA.pca)
  PA.pca$loadings[, 1:2]
  PA.pc1 <- PA.pca$loadings[, 1]
  PA.LoadSum <- sum(PA.pca$loadings[, 1])
  wght <- PA.pc1/PA.LoadSum
  sum(wght)
  PA <- PA %>%
    mutate(
      MAIndex = wght[1]*Entertain_yes + wght[2]*PhoneCombined_yes +
      wght[3]*Satellite_yes + wght[4]*TV_yes + wght[5]*Generator_yes +
      wght[6]*Bicycle_yes + wght[7]*Motorcycle_yes + wght[8]*MotorBoat + 
      wght[9]*Fuel_nonBiomass + wght[10]*Boat)
  return(PA)
}

PA.lst <- list()
for (i in 1:6) {
  PA.lst[[i]] <- get_pa(i)
}

HHData <- bind_rows(PA.lst)

HHData <- HHData %>%
  dplyr::mutate(
    PAIndex = ifelse(
      RemovePA=="No",
      round(
        rowMeans(
          dplyr::select(
            .,"PlaceHappy", "PlaceFavourite", "PlaceMiss", "PlaceBest",
            "PlaceFishHere", "PlaceBeMyself"
          ),
          na.rm = TRUE
        ),
        2
      ),
      NA
    ),
                
    MTIndex = ifelse(
      RemoveMT=="No",
      rowSums(
        dplyr::select(
          .,"RightsAccess", "RightsHarvest", "RightsManage", "RightsExclude",
          "RightsTransfer"
        ),
        na.rm = TRUE
      ),
      NA
    ),
                
    FSIndex = as.character(
      ifelse(
        RemoveFS=="No",
        rowSums(
          dplyr::select(
            ., "DidNotLast", "BalancedDiet", "AdultSkip", "EatLess",
            "FreqAdultSkip", "Hungry"
          ),
          na.rm = TRUE
        ),
        NA
      )
    ) %>%
    dplyr::recode(
      ., "0"="0", "1"="2.04", "2"="2.99", "3"="3.77", "4"="4.5", "5"="5.38",
      "6"="6.06"
    ) %>%
    as.numeric(.),
    
    FSIndex = 6.06 - FSIndex,
                
    cFS = ifelse(
      RemovecFS=="No",
      rowSums(
        dplyr::select(
          ., "LowCostFood", "ChildBalancedMeal", "ChildNotEnough",
          "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip",
          "NoMealChild"
        ),
        na.rm = TRUE
      ),
      NA
    ),
                
    cat.cFS = ifelse(
      cFS>=6.9,"Evidence",
      ifelse(
        cFS<6.9,
        "No or insufficient evidence",
        NA
      )
    ),
                
    InterviewYear = factor(
      InterviewYear,
      levels=c(
        "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
        "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027",
        "2028","2029","2030"
      ),
      ordered=T
    )
  )


HHData <- IndDemos %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(
    Household.Size=length(HouseholdID),
    NumberChild=sum(SchoolAge, na.rm=T),
    NumberEnrolled=sum(ChildEnrolled, na.rm=T),
    PercentEnrolled=ifelse(
      NumberChild!=0 & !is.na(NumberEnrolled),
      as.character(round((NumberEnrolled/NumberChild) * 100, 2)),
      ifelse(NumberChild == 0, "No School-Aged Children","No Data")
    ),
    SERate = ifelse(
      NumberChild!=0 & !is.na(NumberEnrolled),
      round((NumberEnrolled/NumberChild), 2),
      NA
    ),
    HHHAge = ifelse(
      length(DemographicID[RelationHHH == 0]) == 1, 
      IndividualAge[RelationHHH == 0 & !is.na(RelationHHH)],
      NA
    ),
    HHHGender = ifelse(
      length(DemographicID[RelationHHH == 0])==1,
      IndividualGender[RelationHHH == 0 & !is.na(RelationHHH)],
      NA
    ),
    HHHEducation = as.character(
      ifelse(
        length(DemographicID[RelationHHH==0])==1,
        IndividualEducation[RelationHHH==0 & !is.na(RelationHHH)],
        NA
      )
    ),
    DaysUnwell=sum(DaysUnwell, na.rm=T) / length(HouseholdID)
  ) %>%
  left_join(HHData, ., by="HouseholdID")


HHData <- Organization %>% 
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(
    NumMarineGroup=length(HouseholdID),
    MarineMeetingSum=ifelse(
      length(MarineMeeting[is.na(MarineMeeting) == T]) == NumMarineGroup,
      NA,
      sum(MarineMeeting,na.rm=T)
    ),
    MarineContribution=ifelse(
      length(
        MarineContribution[is.na(MarineContribution) == T]
      ) == NumMarineGroup,
      NA,
      sum(MarineContribution, na.rm=T)
    )
  ) %>%
  left_join(HHData, ., by="HouseholdID")

HHData <- arrange(HHData, HouseholdID)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXPORT WELLBEING TABLE ALONG WITH INDICES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
today.date <- gsub("-","",Sys.Date())
pref <- "HH_Tbl_"
suff <- paste0('_', today.date, ".csv")
merged_WELLBEING <- WELLBEING %>%
  left_join(
    HHData %>%
      select(HouseholdID, MAIndex), by = c("new_household" = "HouseholdID")
  )
## Write the merged dataframe to a CSV file
write.csv(
  merged_WELLBEING,
  paste0("../2_Source_data/", pref,"WELLBEING", suff),
  row.names = FALSE
)