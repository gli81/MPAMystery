
###### CALCULATE HOUSEHOLD-LEVEL INDICES FOR BHS WITH DATA THAT INCLUDE T7 #####


### SECTIONS ###

## 1) ASSET INDEX USING PCA (PCA per MPA)
## 2) OTHER INDICES

setwd("C:/Users/rraso/Box/MPAMystery-master/")

#source('Heterogeneity_analysis_including_T7/Source_social_data_flat_files_T7.R', local = T)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: ASSET INDEX USING PCA (PCA per MPA) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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


# ---- 1.2 PCA for MPA1 ----

PA1 <- HHData[HHData$MPAID==1, ]
PCA.PA1 <- PA1 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA1, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.2%)
PCA.PA1 <- PCA.PA1 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA1.pca <- princomp(PCA.PA1)
summary(PA1.pca) # PC1 accounts for 30% of variation
PA1.pca$loadings[, 1:2] # see the first two principal component loadings
PA1.pc1 <- PA1.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA1.LoadSum <- sum(PA1.pca$loadings[, 1]) # sum of the first principal component loadings (2.527468)
wght1 <- PA1.pc1/PA1.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght1)
PA1 <- PA1 %>% 
  mutate(MAIndex = wght1[1]*Entertain_yes + wght1[2]*PhoneCombined_yes + wght1[3]*Satellite_yes + wght1[4]*TV_yes +
           wght1[5]*Generator_yes + wght1[6]*Bicycle_yes + wght1[7]*Motorcycle_yes + wght1[8]*MotorBoat + 
           wght1[9]*Fuel_nonBiomass + wght1[10]*Boat) 

# ---- 1.3 PCA for MPA2 ----

PA2 <- HHData[HHData$MPAID==2, ]
PCA.PA2 <- PA2 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA2, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.4%)
PCA.PA2 <- PCA.PA2 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA2.pca <- princomp(PCA.PA2)
summary(PA2.pca) # PC1 accounts for 30% of variation
PA2.pca$loadings[, 1:2] # see the first two principal component loadings
PA2.pc1 <- PA2.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA2.LoadSum <- sum(PA2.pca$loadings[, 1]) # sum of the first principal component loadings (2.581667)
wght2 <- PA2.pc1/PA2.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght2)
PA2 <- PA2 %>% 
  mutate(MAIndex = wght2[1]*Entertain_yes + wght2[2]*PhoneCombined_yes + wght2[3]*Satellite_yes + wght2[4]*TV_yes +
           wght2[5]*Generator_yes + wght2[6]*Bicycle_yes + wght2[7]*Motorcycle_yes + wght2[8]*MotorBoat + 
           wght2[9]*Fuel_nonBiomass + wght2[10]*Boat) 

# ---- 1.4 PCA for MPA3 ----

PA3 <- HHData[HHData$MPAID==3, ]
PCA.PA3 <- PA3 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA3, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.3%)
PCA.PA3 <- PCA.PA3 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA3.pca <- princomp(PCA.PA3)
summary(PA3.pca) # PC1 accounts for 30% of variation
PA3.pca$loadings[, 1:2] # see the first two principal component loadings
PA3.pc1 <- PA3.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA3.LoadSum <- sum(PA3.pca$loadings[, 1]) # sum of the first principal component loadings (2.720286)
wght3 <- PA3.pc1/PA3.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght3)
PA3 <- PA3 %>% 
  mutate(MAIndex = wght3[1]*Entertain_yes + wght3[2]*PhoneCombined_yes + wght3[3]*Satellite_yes + wght3[4]*TV_yes +
           wght3[5]*Generator_yes + wght3[6]*Bicycle_yes + wght3[7]*Motorcycle_yes + wght3[8]*MotorBoat + 
           wght3[9]*Fuel_nonBiomass + wght3[10]*Boat) 

# ---- 1.5 PCA for MPA4 ----

PA4 <- HHData[HHData$MPAID==4, ]
PCA.PA4 <- PA4 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA4, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.2%)
PCA.PA4 <- PCA.PA4 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA4.pca <- princomp(PCA.PA4)
summary(PA4.pca) # PC1 accounts for 34% of variation
PA4.pca$loadings[, 1:2] # see the first two principal component loadings
PA4.pc1 <- PA4.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA4.LoadSum <- sum(PA4.pca$loadings[, 1]) # sum of the first principal component loadings (2.525659)
wght4 <- PA4.pc1/PA4.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght4)
PA4 <- PA4 %>% 
  mutate(MAIndex = wght4[1]*Entertain_yes + wght4[2]*PhoneCombined_yes + wght4[3]*Satellite_yes + wght4[4]*TV_yes +
           wght4[5]*Generator_yes + wght4[6]*Bicycle_yes + wght4[7]*Motorcycle_yes + wght4[8]*MotorBoat + 
           wght4[9]*Fuel_nonBiomass + wght4[10]*Boat) 

# ---- 1.6 PCA for MPA5 ----

PA5 <- HHData[HHData$MPAID==5, ]
PCA.PA5 <- PA5 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA5, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.6%)
PCA.PA5 <- PCA.PA5 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA5.pca <- princomp(PCA.PA5)
summary(PA5.pca) # PC1 accounts for 29% of variation
PA5.pca$loadings[, 1:2] # see the first two principal component loadings
PA5.pc1 <- PA5.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA5.LoadSum <- sum(PA5.pca$loadings[, 1]) # sum of the first principal component loadings (2.775214)
wght5 <- PA5.pc1/PA5.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght5)
PA5 <- PA5 %>% 
  mutate(MAIndex = wght5[1]*Entertain_yes + wght5[2]*PhoneCombined_yes + wght5[3]*Satellite_yes + wght5[4]*TV_yes +
           wght5[5]*Generator_yes + wght5[6]*Bicycle_yes + wght5[7]*Motorcycle_yes + wght5[8]*MotorBoat + 
           wght5[9]*Fuel_nonBiomass + wght5[10]*Boat) 

# ---- 1.7 PCA for MPA6 ----

PA6 <- HHData[HHData$MPAID==6, ]
PCA.PA6 <- PA6 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%                  
  na.omit()
sapply(PCA.PA6, FUN=mean) # assets owned by more than 95% or less than 5% of households to be removed: CarTruck (0.5%)
PCA.PA6 <- PCA.PA6 %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         MotorBoat, Fuel_nonBiomass, Boat) # remove CarTruck_yes
PA6.pca <- princomp(PCA.PA6)
summary(PA6.pca) # PC1 accounts for 31% of variation
PA6.pca$loadings[, 1:2] # see the first two principal component loadings
PA6.pc1 <- PA6.pca$loadings[, 1]  # first principal component loadings to be used for weighting the assets
PA6.LoadSum <- sum(PA6.pca$loadings[, 1]) # sum of the first principal component loadings (2.615539)
wght6 <- PA6.pc1/PA6.LoadSum # normalize the component loadings to sum up to 1 for each component, ensuring the weights represent relative contributions
sum(wght6)
PA6 <- PA6 %>% 
  mutate(MAIndex = wght6[1]*Entertain_yes + wght6[2]*PhoneCombined_yes + wght6[3]*Satellite_yes + wght6[4]*TV_yes +
           wght6[5]*Generator_yes + wght6[6]*Bicycle_yes + wght6[7]*Motorcycle_yes + wght6[8]*MotorBoat + 
           wght6[9]*Fuel_nonBiomass + wght6[10]*Boat) 

HHData <- rbind(PA1,PA2,PA3,PA4,PA5,PA6) ## combine data across the 6 MPAs



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: OTHER INDICES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


HHData <-
  HHData %>%
  dplyr::mutate(PAIndex = ifelse(RemovePA=="No",
                                 round(rowMeans(dplyr::select(.,"PlaceHappy", "PlaceFavourite", "PlaceMiss", "PlaceBest", 
                                                              "PlaceFishHere", "PlaceBeMyself"),
                                                na.rm = TRUE), 2),
                                 NA),
                
                MTIndex = ifelse(RemoveMT=="No",
                                 rowSums(dplyr::select(.,"RightsAccess", "RightsHarvest", "RightsManage", 
                                                       "RightsExclude", "RightsTransfer"),
                                         na.rm = TRUE),
                                 NA),
                
                FSIndex = as.character(ifelse(RemoveFS=="No",
                                              rowSums(dplyr::select(., "DidNotLast", "BalancedDiet", "AdultSkip", "EatLess", 
                                                                    "FreqAdultSkip", "Hungry"),
                                                      na.rm = TRUE),
                                              NA)) %>%
                  dplyr::recode(., "0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06") %>%
                  as.numeric(.),
                
                FSIndex = 6.06 - FSIndex,
                
                cFS = ifelse(RemovecFS=="No",
                             rowSums(dplyr::select(.,"LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                                                   "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                                                   "NoMealChild"),
                                     na.rm = TRUE),
                             NA),
                
                cat.cFS = ifelse(cFS>=6.9,"Evidence",
                                 ifelse(cFS<6.9,"No or insufficient evidence",NA)),
                
                InterviewYear = factor(InterviewYear,
                                       levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018",
                                                "2019","2020","2021","2022","2023","2024","2025","2026","2027",
                                                "2028","2029","2030"),
                                       ordered=T))


HHData <- 
  IndDemos %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(Household.Size=length(HouseholdID),
                   NumberChild=sum(SchoolAge,na.rm=T),
                   NumberEnrolled=sum(ChildEnrolled,na.rm=T),
                   PercentEnrolled=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                          as.character(round((NumberEnrolled/NumberChild)*100,2)),
                                          ifelse(NumberChild==0,
                                                 "No School-Aged Children","No Data")),
                   SERate=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                 round((NumberEnrolled/NumberChild),2),
                                 NA),
                   HHHAge=ifelse(length(DemographicID[RelationHHH==0])==1, 
                                 IndividualAge[RelationHHH==0 & !is.na(RelationHHH)],
                                 NA),
                   HHHGender=ifelse(length(DemographicID[RelationHHH==0])==1,
                                    IndividualGender[RelationHHH==0 & !is.na(RelationHHH)],
                                    NA),
                   HHHEducation=as.character(ifelse(length(DemographicID[RelationHHH==0])==1,
                                                    IndividualEducation[RelationHHH==0 & !is.na(RelationHHH)],
                                                    NA)),
                   DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID)) %>%
  left_join(HHData,.,by="HouseholdID")


HHData <- 
  Organization %>% 
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(NumMarineGroup=length(HouseholdID),
                   MarineMeetingSum=ifelse(length(MarineMeeting[is.na(MarineMeeting)==T])==NumMarineGroup,NA,sum(MarineMeeting,na.rm=T)),
                   MarineContribution=ifelse(length(MarineContribution[is.na(MarineContribution)==T])==NumMarineGroup,NA,sum(MarineContribution,na.rm=T))) %>%
  left_join(HHData,.,by="HouseholdID")

HHData <- arrange(HHData, HouseholdID)





## Generate settlement level data
#SettData <- 
#  HHData %>% 
#  dplyr::group_by(SettlementID,MonitoringYear) %>%
#  dplyr::summarise(SettlementID = unique(SettlementID),
#                   SettlementName = unique(SettlementName),
#                   MPAID = unique(MPAID),
#                   MPAName = unique(MPAName),
#                   Treatment = unique(Treatment),
#                  TimePoint = unique(MonitoringYear),
#                  MAIndex = mean(MAIndex,na.rm=T),
#                  MTIndex = mean(MTIndex,na.rm=T),
#                  FSIndex = mean(FSIndex,na.rm=T),
#                  PAIndex = mean(PAIndex,na.rm=T),
#                  SERate = mean(SERate,na.rm=T)) 
#write.csv(SettData, file="Settlement_Data.csv")
