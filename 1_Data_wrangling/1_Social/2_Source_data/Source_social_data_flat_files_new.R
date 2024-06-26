
###### SOURCE DATA FOR HETEROGENEITY ANALYSIS INCLUDING T7 DATA ######

#### SECTIONS ####

## 1) CLEAN & POST-CODE DATA
## 2) ADDRESS PECULIARITIES IN DATA
## 3) SELECT DATA FOR BHS ONLY



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: CLEAN & POST-CODE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# source('Combining_T4_and_T7_new.R', local = T)
## we are not sourcing, instead we load data from csv files

setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/2_Source_data"
  )
)

pacman::p_load(
  rio, dplyr, janitor
)

last.file <- function(dir.nam, nam){
  import(
    paste0(
      dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T)))
    ),
    guess_max=50000
  )
}


## load CSV
WELLBEING <- last.file(
  dir.nam="./",
  nam="HH_Tbl_WELLBEING"
)
DEMOGRAPHIC <- last.file(
  dir.nam="./",
  nam="HH_Tbl_DEMOGRAPHIC"
)
ORGANIZATION <- last.file(
  dir.nam="./",
  nam="HH_Tbl_ORGANIZATION"
)
NMORGANIZATION <- last.file(
  dir.nam="./",
  nam="HH_Tbl_NMORGANIZATION"
)

SETTLEMENT <- last.file(
  dir.nam='../../../x_Flat_data_files/Inputs/',
  nam='tbl_settlement'
)


# -- 1.1 Clean & post-code WELLBEING to create HHData for analysis ----

HHData <-   WELLBEING %>%
  dplyr::transmute(
    HouseholdID = new_household, 
    MPAID = mpa, 
    SettlementID = settlement, 
    InterviewYear = interviewyear,
                   
   # Food Security
   DidNotLastCoded = as.integer(
     case_when(
       fsdidnotlast == 1 | fsdidnotlast == 2 ~ 1,
       fsdidnotlast == 3 ~ 0,
       TRUE ~ 990
     )
     # ifelse((fsdidnotlast==1 | fsdidnotlast==2),1,ifelse(fsdidnotlast==3,0,990))
   ),
   BalancedDietCoded = as.integer(
     case_when(
       fsbalanceddiet == 1 | fsbalanceddiet == 2 ~ 1,
       fsbalanceddiet == 3 ~ 0,
       TRUE ~ 990
     )
     # ifelse((fsbalanceddiet==1 | fsbalanceddiet==2),1,ifelse(fsbalanceddiet==3,0,990))
   ),
   FreqAdultSkipCoded = as.integer(
     case_when(
       fsfreqadultskip == 1 | fsfreqadultskip == 2 ~ 1,
       fsfreqadultskip == 3 ~ 0,
       TRUE ~ 990
     )
     # ifelse((fsfreqadultskip==1 | fsfreqadultskip==2),1,ifelse(fsfreqadultskip==3 | fsadultskip==0,0,990))
   ),
   AdultSkipCoded = as.integer(
     case_when(
       fsadultskip == 1 ~ 1,
       fsadultskip == 0 ~ 0,
       TRUE ~ 990
   )
   # ifelse(fsadultskip==1,1,ifelse(fsadultskip==0,0,990))
   ),
   EatLessCoded = as.integer(
     case_when(
       fseatless == 1 ~ 1,
       fseatless == 0 ~ 0,
       TRUE ~ 990
     )
   # ifelse(fseatless==1,1, ifelse(fseatless==0,0,990))
   ),
   HungryCoded = as.integer(
     case_when(
       fshungry == 1 ~ 1,
       fshungry == 0 ~ 0,
       TRUE ~ 990
     )
     # ifelse(fshungry==1,1,ifelse(fshungry==0,0,990))
   ),
   
   DidNotLast = as.integer(
     case_when(
       DidNotLastCoded != 990 ~ DidNotLastCoded,
       DidNotLastCoded==990 & (BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1) ~ 1,
       TRUE ~ NA
     )
     # ifelse(DidNotLastCoded==990,
     #                              ifelse((BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1),1,NA),DidNotLastCoded)
     ),
   BalancedDiet = as.integer(
     case_when(
       BalancedDietCoded != 990 ~ BalancedDietCoded,
       BalancedDietCoded == 990 & (DidNotLast==1 & (AdultSkipCoded==1 | EatLessCoded==1 |FreqAdultSkipCoded==1 | HungryCoded==1)) ~ 1,
       TRUE ~ NA
     )
     # ifelse(BalancedDietCoded==990,
     #        ifelse((DidNotLast==1 & (AdultSkipCoded==1 | EatLessCoded==1 |FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),BalancedDietCoded)
   ),
   AdultSkip = as.integer(
     case_when(
       AdultSkipCoded != 990 ~ AdultSkipCoded,
       AdultSkipCoded == 990 & (FreqAdultSkipCoded==1 | (DidNotLast==1 & BalancedDiet==1 & (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1))) ~ 1,
       TRUE ~ NA
     )
     # ifelse(AdultSkipCoded==990,
     #        ifelse(FreqAdultSkipCoded==1 | (DidNotLast==1 & BalancedDiet==1 & (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),AdultSkipCoded)
   ),
   EatLess = as.integer(
     case_when(
       EatLessCoded != 990 ~ EatLessCoded,
       EatLessCoded == 990 & ((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & (FreqAdultSkipCoded==1 | HungryCoded==1))) ~ 1,
       TRUE ~ NA
     )
     # ifelse(EatLessCoded==990,
     #        ifelse((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & (FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),EatLessCoded)
   ),
   FreqAdultSkip = as.integer(
     case_when(
       FreqAdultSkipCoded != 990 ~ FreqAdultSkipCoded,
       FreqAdultSkipCoded == 990 & ((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & EatLess==1 & HungryCoded==1)) ~ 1,
       TRUE ~ NA
     )
     # ifelse(FreqAdultSkipCoded==990,
     #        ifelse((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & EatLess==1 & HungryCoded==1),1,NA),FreqAdultSkipCoded)
   ),
   Hungry = as.integer(
     case_when(
       FreqAdultSkipCoded != 990 ~ FreqAdultSkipCoded,
       FreqAdultSkipCoded == 990 & ((DidNotLast==1 & BalancedDiet==1 & FreqAdultSkip==1 & AdultSkip==1 & EatLess==1)) ~ 1,
       TRUE ~ NA
     )
     # ifelse(HungryCoded==990,
     #        ifelse((DidNotLast==1 & BalancedDiet==1 & FreqAdultSkip==1 & AdultSkip==1 & EatLess==1),1,NA),HungryCoded)
   ),
   
   # Assets and Economic Well-being
   Bicycle = as.integer(ifelse(assetbicycle>989,NA,assetbicycle)),
   Motorcycle = as.integer(ifelse(assetmotorcycle>989,NA,assetmotorcycle)),
   BoatNoMotor = as.integer(ifelse(assetboatnomotor>989,NA,assetboatnomotor)),
   BoatOutboard = as.integer(ifelse(assetboatoutboard>989,NA,assetboatoutboard)),
   BoatInboard =  as.integer(ifelse(assetboatinboard>989,NA,assetboatinboard)),
   TV = as.integer(ifelse(assettv>989,NA,assettv)),
   Entertain = as.integer(ifelse(assetentertain>989,NA,assetentertain)),
   Satellite = as.integer(ifelse(assetsatellite>989,NA,assetsatellite)),
   Generator = as.integer(ifelse(assetgenerator>989,NA,assetgenerator)),

   Car = as.integer(ifelse(assetcar>989,NA,assetcar)),
   Truck = as.integer(ifelse(assettruck>989,NA,assettruck)),
   CarTruck = as.integer(ifelse(assetcartruck<993,assetcartruck,
                                ifelse(assetcartruck==993,(Car+Truck),NA))),

   LandlinePhone = as.integer(ifelse(assetlandlinephone>989,NA,assetlandlinephone)),
   CellPhone = as.integer(ifelse(assetcellphone>989,NA,assetcellphone)),
   PhoneCombined = as.integer(ifelse(assetphonecombined<993,assetphonecombined,
                                     ifelse(assetphonecombined==993,(LandlinePhone+CellPhone),NA))),

   Radio = as.integer(ifelse(assetradio>989,NA,assetradio)),
   Stereo = as.integer(ifelse(assetstereo>989,NA,assetstereo)),
   CD = as.integer(ifelse(assetcd>989,NA,assetcd)),
   DVD = as.integer(ifelse(assetdvd>989,NA,assetdvd)),
   Entertain = as.integer(ifelse(assetentertain<993,assetentertain,
                                 ifelse(assetentertain==993,Radio+Stereo+CD+DVD,NA))),
   # Cooking Fuel
   CookingFuel = as.integer(ifelse(cookingfuel%in%c(1:6), cookingfuel, NA)),
   CookingFuel.Biomass = as.integer(ifelse(cookingfuel==1|cookingfuel==2,0,
                                           ifelse(cookingfuel==3|cookingfuel==4|cookingfuel==5|cookingfuel==6,1,NA))),
   
   # Place Attachment
   PlaceHappy = as.integer(ifelse(placehappy%in%c(1:5),placehappy,NA)),
   PlaceFavourite = as.integer(ifelse(placefavourite%in%c(1:5),placefavourite,NA)),
   PlaceMiss = as.integer(ifelse(placemiss%in%c(1:5),placemiss,NA)),
   PlaceBest = as.integer(ifelse(placebest%in%c(1:5),placebest,NA)),
   PlaceFishHere = as.integer(ifelse(placefishhere%in%c(1:5),placefishhere,NA)),
   PlaceBeMyself = as.integer(ifelse(placebemyself%in%c(1:5),placebemyself,NA)),
   
   # Tenure
   RightsAccess = as.integer(ifelse(rightsaccess%in%c(0:1),rightsaccess,NA)),
   RightsHarvest = as.integer(ifelse(rightsharvest%in%c(0:1),rightsharvest,NA)),
   RightsManage = as.integer(ifelse(rightsmanage%in%c(0:1),rightsmanage,NA)),
   RightsExclude = as.integer(ifelse(rightsexclude%in%c(0:1),rightsexclude,NA)),
   RightsTransfer = as.integer(ifelse(rightstransfer%in%c(0:1),rightstransfer,NA)),
   
   
   # Child's Food Security
   ChildPortionCoded = as.integer(ifelse(fschildportion==1,1,ifelse(fschildportion==0,0,990))),
   LowCostFoodCoded = as.integer(ifelse((fslowcostfood==1 | fslowcostfood==2),1,ifelse(fslowcostfood==3,0,990))),
   ChildSkipCoded =  as.integer(ifelse(fschildskip==1,1,ifelse(fschildskip==0,0,990))),
   FreqChildSkipCoded = as.integer(ifelse((fsfreqchildskip==1 | fsfreqchildskip==2),1,ifelse(fsfreqchildskip==3,0,990))),
   NoMealChildCoded = as.integer(ifelse((fsnomealchild==1 | fsnomealchild==2),1,ifelse(fsnomealchild==3,0,990))),
   
   LowCostFood = as.integer(ifelse(LowCostFoodCoded==990,
                                   ifelse((ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 |NoMealChildCoded==1),1,NA),LowCostFoodCoded)),
   ChildBalancedMeal = as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
   ChildNotEnough =  as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 |FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
   ChildPortion = as.integer(ifelse(ChildPortionCoded==990,
                                    ifelse((LowCostFood==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 |NoMealChildCoded==1)),1,NA),ChildPortionCoded)),
   ChildHungry = as.integer(ifelse((LowCostFood==1 & ChildPortion==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
   ChildSkip = as.integer(ifelse(ChildSkipCoded==990,
                                 ifelse((LowCostFood==1 & ChildPortion==1 & (FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildSkipCoded)),
   FreqChildSkip = as.integer(ifelse(FreqChildSkipCoded==990,
                                     ifelse((LowCostFood==1 & ChildPortion==1 & ChildSkip==1 & NoMealChildCoded==1),1,NA),FreqChildSkipCoded)),
   NoMealChild =  as.integer(ifelse(NoMealChildCoded==990,
                                    ifelse((LowCostFood==1 & ChildPortion==1 & ChildSkip==1 & FreqChildSkip==1),1,NA),NoMealChildCoded)),
   
   
   # Livelihoods & Occupations
   PrimaryLivelihood = as.integer(ifelse(primarylivelihood%in%c(1:7,996),primarylivelihood,NA)),
   SecondaryLivelihood = as.integer(ifelse(secondarylivelihood%in%c(1:7,996),secondarylivelihood,NA)),
   TertiaryLivelihood = as.integer(ifelse(tertiarylivelihood%in%c(1:7,996),tertiarylivelihood,NA)),
   
   
   # Fishing Characteristics
   FreqFish = as.integer(ifelse(freqfishtime%in%c(1:5),freqfishtime,NA)),
   FreqSaleFish = as.integer(ifelse(freqsalefish%in%c(1:5),freqsalefish,NA)),
   PercentIncFish = as.integer(ifelse(percentincomefish%in%c(1:5),percentincomefish,NA)),
   MajFishTechnique = as.integer(ifelse(majorfishtechnique%in%c(1:6),majorfishtechnique,NA)),
   FreqEatFish = as.integer(ifelse(freqeatfish%in%c(1:5),freqeatfish,NA)),
   PercentProteinFish = as.integer(ifelse(percentproteinfish%in%c(1:5),percentproteinfish,NA)),
   PrimaryFishTechnique = as.integer(ifelse(primaryfishtechnique%in%c(1:16,996),primaryfishtechnique,NA)),
   SecondaryFishTechnique = as.integer(ifelse(secondaryfishtechnique%in%c(1:16,996),secondaryfishtechnique,NA)),
   TertiaryFishTechnique = as.integer(ifelse(tertiaryfishtechnique%in%c(1:16,996),tertiaryfishtechnique,NA)),
   
   
   # Economic Well-being (Subjective)
   EconStatusTrend = as.integer(ifelse(economicstatustrend%in%c(1:5),economicstatustrend,NA)),
   EconStatusReason = ifelse(economicstatusreason %in% c("994", "995", "996", "997", "998", "999"), NA, as.character(economicstatusreason)), 
   
   # Community Organization
   MarineGroup = as.integer(ifelse(marinegroup%in%c(0:1),marinegroup,NA)),
   OtherGroup = as.integer(ifelse(othergroup%in%c(0:1),othergroup,NA)),    
   VoteDistrict = as.integer(ifelse(votedistrict%in%c(0:1),votedistrict,NA)),
   VoteNational = as.integer(ifelse(votenational%in%c(0:1),votenational,NA)),   
   
   NumLocalThreat = as.integer(ifelse(numlocalthreat>989,NA,numlocalthreat)), 
   NumGlobalThreat = as.integer(ifelse(numglobalthreat>989,NA,numglobalthreat)), 
   NumLocalAction = as.integer(ifelse(numlocalaction>989,NA,numlocalaction)),   
   NumGlobalAction = as.integer(ifelse(numglobalaction>989,NA,numglobalaction)), 
   
   
   # Other Characteristics
   Religion = as.integer(ifelse(religion%in%c(1:7),religion,NA)),
   YrResident = as.integer(ifelse(yearsresident>=150,NA,yearsresident)),
   TimeMarket = as.numeric(ifelse(timemarket>989,NA,timemarket)),
   SocialConflict = as.integer(ifelse(socialconflict%in%c(1:5),socialconflict,NA)),
   PaternalEthnicity = paternalethnicity,
   
   #Fishing 
   LessProductiveDaysFishing = as.integer(ifelse(lessproductivedaysfishing%in%c(0:366),lessproductivedaysfishing,NA)),
   PoorCatch = poorcatch,
   PoorCatchUnits = poorcatchunits, 
   
   MoreProductiveDaysFishing = as.integer(ifelse(moreproductivedaysfishing%in%c(0:366),moreproductivedaysfishing,NA)),
   GoodCatch = goodcatch,
   GoodCatchUnits = goodcatchunits) %>%

  
  dplyr::mutate(RemoveFS = as.factor(ifelse(rowSums(.[c("DidNotLastCoded", "BalancedDietCoded", "FreqAdultSkipCoded", 
                                                        "AdultSkipCoded", "EatLessCoded", "HungryCoded")])>2969,"Yes","No")), #2970 would be 3 or more blind codes
                RemoveMA = as.factor(ifelse(rowSums(is.na(.[c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", 
                                                              "BoatOutboard", "BoatInboard", "PhoneCombined", 
                                                              "TV", "Entertain", "Satellite", "Generator")]))>10,"Yes","No")),
                RemovePA = as.factor(ifelse(rowSums(is.na(.[c("PlaceHappy", "PlaceFavourite", "PlaceMiss", 
                                                              "PlaceBest", "PlaceFishHere", "PlaceBeMyself")]))>5,"Yes","No")),
                RemoveMT = as.factor(ifelse(rowSums(is.na(.[c("RightsAccess", "RightsHarvest", "RightsManage", 
                                                              "RightsExclude", "RightsTransfer")]))>4,"Yes","No")),
                RemovecFS = as.factor(ifelse(rowSums(.[c("ChildPortionCoded", "LowCostFoodCoded", "ChildSkipCoded", 
                                                         "FreqChildSkipCoded", "NoMealChildCoded")])>1979,"Yes","No"))) %>% #1980 would vbe 2 or more blind codes
  
  dplyr::select(HouseholdID, MPAID, SettlementID, InterviewYear, DidNotLast, BalancedDiet, AdultSkip, EatLess, FreqAdultSkip, Hungry, RemoveFS,
                CarTruck, Bicycle, Motorcycle,  BoatNoMotor, BoatOutboard, BoatInboard, PhoneCombined, TV, Entertain, Satellite, Generator, RemoveMA, CookingFuel, CookingFuel.Biomass,
                PlaceHappy,  PlaceFavourite, PlaceMiss, PlaceBest, PlaceFishHere, PlaceBeMyself, RemovePA,
                RightsAccess, RightsHarvest, RightsManage, RightsExclude, RightsTransfer, RemoveMT,
                LowCostFood, ChildBalancedMeal, ChildNotEnough, ChildPortion, ChildHungry, ChildSkip, FreqChildSkip, NoMealChild, RemovecFS,
                PrimaryLivelihood, SecondaryLivelihood, TertiaryLivelihood, FreqFish, FreqSaleFish, PercentIncFish, MajFishTechnique, FreqEatFish, PercentProteinFish, 
                PrimaryFishTechnique, SecondaryFishTechnique, TertiaryFishTechnique, 
                EconStatusTrend, EconStatusReason, Religion, YrResident, TimeMarket, SocialConflict,
                MarineGroup, OtherGroup, VoteDistrict, VoteNational, NumLocalThreat, NumGlobalThreat, NumLocalAction, NumGlobalAction, 
                LessProductiveDaysFishing, PoorCatch, PoorCatchUnits, MoreProductiveDaysFishing, GoodCatch, GoodCatchUnits, PaternalEthnicity)


# ---- 1.2 Clean & post-code DEMOGRAPHIC to create IndDemos for analysis ----

IndDemos <- 
  DEMOGRAPHIC %>%
  dplyr::transmute(DemographicID = new_demographicid,
                   HouseholdID = new_household,
                   RelationHHH = as.integer(ifelse(relationhhh%in%c(0:13),relationhhh,NA)),
                   IndividualGender = ifelse(individualgender==2,0,ifelse(individualgender>989,NA,individualgender)),
                   IndividualAge = ifelse(individualage>150,NA,individualage),
                   IndividualEducation = ifelse(individualeducation %in% c("995", "997", "998", "999"),NA,as.character(individualeducation)),
                   SchoolAge = ifelse((individualage>4 & individualage<19),1,ifelse(individualage>150 | is.na(individualage),NA,0)),
                   IndividualEnrolled = as.integer(ifelse(individualenrolled%in%c(0:1),individualenrolled,NA)),
                   #ChildEnrolled = ifelse((individualenrolled==1 & SchoolAge==1),1,ifelse((is.na(IndividualAge) | SchoolAge==0),NA,0)),
                   ChildEnrolled = ifelse((individualenrolled==1 & SchoolAge==1),1,ifelse((individualenrolled==0 | SchoolAge==0),0,NA)),
                   DaysUnwell = ifelse(individualdaysunwell>32,NA,
                                       ifelse(individualdaysunwell%in%c(29:32),28,individualdaysunwell)),
                   IndividualUnwell = as.integer(ifelse(individualunwell%in%c(0:1),individualunwell,NA)),
                   IndividualLostDays = ifelse(individuallostdays>32, NA, 
                                               ifelse(individuallostdays %in% c(29:32),28,individuallostdays))) %>%
  left_join(., HHData[,c("HouseholdID","MPAID","SettlementID")], by="HouseholdID")


# ---- 1.3 Call, clean, & post-code ORGANIZATION to create Organization & NMOrganization (non-marine organization) for analysis ----

Organization <- 
  ORGANIZATION %>%
  dplyr::transmute(OrganizationID = morganizationid,
                   HouseholdID = new_household,
                   MarineGroupName = name,
                   MarinePosition = position,
                   MarineMeeting = ifelse(meeting%in%c(0:1),meeting, NA),
                   MarineDays = ifelse(days%in%c(0:365),days, NA),
                   MarineContribution = ifelse(contribution%in%c(994, 995, 996, 997, 998, 999, 0), NA, contribution)) %>%
  left_join(HHData[,c("HouseholdID","MPAID","SettlementID")], by="HouseholdID")

NMOrganization <-
  NMORGANIZATION %>%
  dplyr::transmute(NMOrganizationID = nmorganizationid,
                   HouseholdID = new_household,
                   OtherGroupName = name,
                   OtherGroupPosition = position,
                   OtherGroupMeeting = ifelse(meeting%in%c(0:1),meeting, NA),
                   OtherGroupDays = ifelse(days%in%c(0:365),days, NA),
                   OtherGroupContribution = ifelse(contribution%in%c(994, 995, 996, 997, 998, 999, 0), NA, contribution)) %>%
  left_join(HHData[,c("HouseholdID","MPAID","SettlementID")], by="HouseholdID")


# ---- 1.4 Add seascape column to SETTLEMENTS for analysis ----

Settlements <- 
  SETTLEMENT %>%
  dplyr::transmute(SettlementID = settlementid,
                   SettlementName = name,
                   MPAID = mpa,
                   Treatment = treatment, 
                   Zone = zone,
                   Seascape = ifelse(MPAID %in% c(1,2,3,4,5,6), 1,  # Seascape 1 is Bird's Head, and Seascape 2 is Sunda Banda
                                     ifelse(MPAID %in% c(15,16,17,18,19,20,21), 2,
                                            NA)))


# ---- 1.5 Add monitoring year column to HHData for analysis ----


HHData$MonitoringYear <- factor(
  mapply(
    a=HHData$MPAID,
    b=HHData$InterviewYear,
    function(a,b){
      define <- HHData %>% 
        group_by(MPAID) %>% 
        summarise(
          Baseline=min(InterviewYear),
          TwoYear=Baseline+2,
          ThreeYear=Baseline+3,
          FourYear=Baseline+4,
          FiveYear=Baseline+5,
          SixYear=Baseline+6,
          SevenYear=Baseline+7,
          EightYear=Baseline+8,
          NineYear=Baseline+9,
          TenYear=Baseline+10
        )
      mon.year <- ifelse(
        b == define$Baseline[define$MPAID == a],
        "Baseline",
        ifelse(
          b == define$TwoYear[define$MPAID == a],
          "2 Year Post",
          ifelse(
            b == define$ThreeYear[define$MPAID == a],
            "3 Year Post",
            ifelse(
              b == define$FourYear[define$MPAID == a],
              "4 Year Post",
              ifelse(
                b == define$FiveYear[define$MPAID == a],
                "5 Year Post",
                ifelse(
                  b == define$SixYear[define$MPAID == a],
                  "6 Year Post",
                  ifelse(
                    b == define$SevenYear[define$MPAID == a],
                    "7 Year Post",
                    ifelse(
                      b == define$EightYear[define$MPAID == a],
                      "8 Year Post",
                      ifelse(
                        b == define$NineYear[define$MPAID == a],
                        "9 Year Post",
                        ifelse(
                          b==define$TenYear[define$MPAID == a],
                          "10 Year Post"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      mon.year
   }
  ),
  levels=c(
    "Baseline", "2 Year Post", "3 Year Post", "4 Year Post", "5 Year Post",
    "6 Year Post", "7 Year Post", "8 Year Post", "9 Year Post", "10 Year Post"
  ),
  ordered=T
)


# ---- 1.6 Create MPA.name table to define formal MPA names, in English and Bahasa, for plotting and automated R wrapper ----

dir_name <- "../../../x_Flat_data_files/Inputs/"
MPA.LKP <- last.file(
  dir.nam=dir_name,
  nam="tbl_mpa"
)

MPA.name <- MPA.LKP %>% 
  transmute(
    MPAID=mpaid,
    MPAName=name,
    MPAName.nospace=gsub(" ","",MPAName), # this is used for the automated R wrapper, to create a filepath with the MPA name
    MPAName.final=gsub("MPA","",MPAName.nospace), # this is used to label the MPA without "MPA" in the name
    MPAName.bahasa=ifelse(
      MPAID==15,
      "SAP Selat Pantar",
      ifelse(
        MPAID==16,
        "SAP Flores Timur",
        ifelse(
          MPAID==17,
          "KKP3K TPK Pulau Kei Kecil",
          ifelse(
            MPAID==18,
            "KKP3K Pulau Koon",
            ifelse(
              MPAID==19,
              "KKP3K TPK Kepulauan Tanimbar",
              ifelse(
                MPAID==20,
                "KKPD Sulawesi Tenggara",
                ifelse(
                  MPAID==21,
                  "Taman Nasional Wakatobi",
                  gsub("MPA","KKP",MPAName)
                )
              )
            )
          )
        )
      )
    )
  )


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: ADDRESS PECULIARITIES IN DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Remove observations from BHS & SBS that do not have post-baseline data ----

HHData <- HHData[HHData$SettlementID!=14 & # include settlement 14 because it was only surveyed at t0 and t2
                   HHData$SettlementID!=84 &
                   HHData$SettlementID!=96 &
                   HHData$SettlementID!=97 &
                   HHData$SettlementID!=98 &
                   HHData$SettlementID!=99 &
                   HHData$SettlementID!=100 &
                   HHData$SettlementID!=101 &
                   HHData$SettlementID!=124 &
                   HHData$SettlementID!=131,]

IndDemos <- IndDemos[!is.na(IndDemos$SettlementID) &
                       IndDemos$SettlementID!=14 & # include settlement 14 because it was only surveyed at t0 and t2
                       IndDemos$SettlementID!=84 &
                       IndDemos$SettlementID!=96 &
                       IndDemos$SettlementID!=97 &
                       IndDemos$SettlementID!=98 &
                       IndDemos$SettlementID!=99 &
                       IndDemos$SettlementID!=100 &
                       IndDemos$SettlementID!=101 &
                       IndDemos$SettlementID!=124 &
                       IndDemos$SettlementID!=131,]

Settlements <- Settlements[!is.na(Settlements$SettlementID) &
                             Settlements$SettlementID!=14 & # include settlement 14 because it was only surveyed at t0 and t2
                             Settlements$SettlementID!=84 &
                             Settlements$SettlementID!=96 &
                             Settlements$SettlementID!=97 &
                             Settlements$SettlementID!=98 &
                             Settlements$SettlementID!=99 &
                             Settlements$SettlementID!=100 &
                             Settlements$SettlementID!=101 &
                             Settlements$SettlementID!=124 &
                             Settlements$SettlementID!=131,]
Settlements$SettlementName <- as.character(Settlements$SettlementName)

Organization <- Organization[!is.na(Organization$SettlementID) &
                               Organization$SettlementID!=14 & # include settlement 14 because it was only surveyed at t0 and t2
                               Organization$SettlementID!=84 &
                               Organization$SettlementID!=96 &
                               Organization$SettlementID!=97 &
                               Organization$SettlementID!=98 &
                               Organization$SettlementID!=99 &
                               Organization$SettlementID!=100 &
                               Organization$SettlementID!=101 &
                               Organization$SettlementID!=124 &
                               Organization$SettlementID!=131,]

NMOrganization <- NMOrganization[!is.na(NMOrganization$SettlementID) &
                                   NMOrganization$SettlementID!=14 & # include settlement 14 because it was only surveyed at t0 and t2
                                   NMOrganization$SettlementID!=84 &
                                   NMOrganization$SettlementID!=96 &
                                   NMOrganization$SettlementID!=97 &
                                   NMOrganization$SettlementID!=98 &
                                   NMOrganization$SettlementID!=99 &
                                   NMOrganization$SettlementID!=100 &
                                   NMOrganization$SettlementID!=101 &
                                   NMOrganization$SettlementID!=124 &
                                   NMOrganization$SettlementID!=131,]

# remove household from baseline that refused every question but material assets (no demographic info, etc.)

HHData <- HHData[HHData$HouseholdID!=41347,]
IndDemos <-IndDemos[IndDemos$HouseholdID!=41347,]


# ---- 2.2 Re-code settlements in Kaimana MPA that changed designation after baseline year ----

Settlements$Treatment <- ifelse(Settlements$SettlementID==83 | Settlements$SettlementID==91 | Settlements$SettlementID==92,
                                0,Settlements$Treatment)


# ---- 2.3 Add dummy row of data for all settlements (in Bird's Head) that do not have baseline data ----

#baseline.dummy.rows <- 
#  data.frame(HouseholdID=rep(NA,13),
#             MPAID=c(5,rep(2,9),rep(3,3)),
#             SettlementID=c(72,104:112,113:115),
#             InterviewYear=c(2012,rep(2010,9),rep(2012,3)),
#             as.data.frame(matrix(rep(NA,length(colnames(HHData[5:length(colnames(HHData))]))),
#                                  ncol=length(colnames(HHData[5:length(colnames(HHData))])),
#                                  nrow=13,
#                                  dimnames=list(NULL,colnames(HHData[5:length(colnames(HHData))]))))) %>%
#  mutate(RemoveFS="No",
#         RemoveMA="No",
#         RemoveMT="No",
#         RemovePA="No",
#         MonitoringYear="Baseline")


#HHData <- 
#  rbind.data.frame(HHData,
#                   baseline.dummy.rows)


# ---- 2.4 Join Settlements and HHData tables ----

HHData <- 
  left_join(HHData,Settlements,by=c("SettlementID","MPAID"))

#rm(baseline.dummy.rows)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: SELECT DATA FOR BHS ONLY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


HHData <- HHData %>% 
  filter(MPAID%in%c(1:6))

IndDemos <- IndDemos %>% 
  filter(MPAID%in%c(1:6))

Organization <- Organization %>% 
  filter(MPAID%in%c(1:6))

NMOrganization <- NMOrganization %>% 
  filter(MPAID%in%c(1:6))

Settlements <- Settlements %>% 
  filter(MPAID%in%c(1:6))

MPA.name <- MPA.name %>% 
  filter(MPAID%in%c(1:6))

