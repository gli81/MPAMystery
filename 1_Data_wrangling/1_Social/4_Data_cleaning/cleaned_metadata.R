
setwd(
  file.path(
    "R:/ind-soc-impacts/MPASocial",
    "MPAMystery-master/1_Data_wrangling",
    "1_Social/4_Data_cleaning"
  )
)

household_tbl <- read.csv("./HH_Tbl_WELLBEING_20240706_cleaned.csv")

to_check_char_hh <- c(
  "householdid", "new_household", "kkcode", "respondent", "secondaryrespondent",
  "interviewend", "interviewstart", "interviewlength", ## or datetime?
  "usualfish", "primarymarketname", "secondarymarketname", "paternalethnicity",
  "paternalethnicity_clean", "maternalethnicity", "maternalethnicity_clean",
  "poorcatchunits", "poorfishunits", "goodcatchunits", "goodfishunits",
  "economicstatusreason", "economicstatusreasonenglish", "anyotherinfo",
  "willingparticipant"
)

to_check_categorical_hh <- c(
  "mpa", "settlement", "primaryinterviewer", "secondaryinterviewer",
  "fieldcoordinator", "latsphere", "lonsphere", "surveyversionnumber",
  "surveyversion_new", "paternalethnicity_coded", "maternalethnicity_coded",
  "religion", "primarylivelihood", "secondarylivelihood", "tertiarylivelihood",
  "majorfishtechnique", "primaryfishtechnique", "secondaryfishtechnique",
  "tertiaryfishtechnique", "cookingfuel", "householddeath", "householdbirth",
  "fsadultskip", "fseatless", "fshungry", "fschildportion", "fschildskip",
  "rightsaccess", "rightsharvest", "rightsmanage", "rightsexclude",
  "rightstransfer", "marinegroup", "othergroup", "votedistrict", "votenational",
  "yearID"
)

to_check_ordinal_hh <- c(
  "freqfishtime", "freqsalefish", "percentincomefish", "freqeatfish",
  "percentproteinfish", "economicstatustrend",
  "fsnotenough", "fsdidnotlast", "fsbalanceddiet", "fsfreqadultskip",
  "fslowcostfood", "fsfreqchildskip", "fsnomealchild", "socialconflict",
  "placehappy", "placefavourite", "placemiss", "placebest", "placefishhere",
  "placebemyself", "ed.level"
)

to_check_num_hh <- c(
  "latdeg", "latmin", "latsec", "latfrac", "londeg", "lonmin", "lonsec",
  "lonfrac", "interviewday", "interviewmonth", "interviewyear", "householdsize",
  "yearsresident", "timemarket", "timesecondarymarket",
  "lessproductivedaysfishing", "poorcatch", "poorfishincomel",
  "moreproductivedaysfishing", "goodcatch", "goodfishincomel","assetcar",
  "assettruck", "assetcartruck", "assetbicycle", "assetmotorcycle",
  "assetboatnomotor", "assetboatoutboard", "assetboatinboard",
  "assetlandlinephone", "assetcellphone", "assetphonecombined", "assettv",
  "assetradio", "assetstereo", "assetcd", "assetdvd", "assetentertain",
  "assetsatellite", "assetgenerator", "numbermarinegroup", "numberothergroup",
  "numlocalthreat", "numglobalthreat", "numlocalaction", "numglobalaction"
)


## to do the metadata
sink("metadata_prep.txt")

for (col_name in to_check_char_hh) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(household_tbl[[col_name]])))
  for (specific_value in c(
      "990", "991", "992", "993", "994", "995",
      "996", "997", "998", "999", "70773"
    )
  ) {
    cat(specific_value, ": ")
    print(sum(household_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_categorical_hh) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(household_tbl[[col_name]])))
  for (specific_value in c(
    "990", "991", "992", "993", "994", "995",
    "996", "997", "998", "999", "70773"
  )
  ) {
    cat(specific_value, ": ")
    print(sum(household_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_ordinal_hh) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(household_tbl[[col_name]])))
  for (specific_value in c(
    "990", "991", "992", "993", "994", "995",
    "996", "997", "998", "999", "70773"
  )
  ) {
    cat(specific_value, ": ")
    print(sum(household_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}


for (col_name in to_check_num_hh) {
  print(col_name)
  cat("overall mean: ")
  print(
    mean(household_tbl[
      !(household_tbl[[col_name]] %in% 990:999) &
      household_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(household_tbl[
      !(household_tbl[[col_name]] %in% 990:999) &
        household_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall max: ")
  print(
    max(household_tbl[
      !(household_tbl[[col_name]] %in% 990:999) &
        household_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall min: ")
  print(
    min(household_tbl[
      !(household_tbl[[col_name]] %in% 990:999) &
        household_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(household_tbl[[col_name]])))
  for (specific_value in 990:999) {
    cat(specific_value, ": ")
    print(sum(household_tbl[[col_name]]==specific_value, na.rm=T))
  }
  cat("70773: ")
  print(sum(household_tbl[[col_name]] == 70773, na.rm=T))
  print("===================================================")
}
#############################################################

demo_tbl <- read.csv("./HH_Tbl_DEMOGRAPHIC_20240706_cleaned.csv")

to_check_char_demo <- c(
  "demographicid", "household", "new_household", "new_dempographicid",
  "demographiccode", "individualname", "individualeducation",
  "householdhead", "individualunwell"
)

to_check_categorical_demo <- c(
  "relationhhh", "individualgender", "individualedlevel", "individualenrolled",
  "householdhead", "new_householdhead", "individualunwell"
)
to_check_num_demo <- c(
  "individualage", "individualdaysunwell", "individuallostdays"
)

for (col_name in to_check_char_demo) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(demo_tbl[[col_name]])))
  for (specific_value in c(
    "990", "991", "992", "993", "994", "995",
    "996", "997", "998", "999", "70773"
  )
  ) {
    cat(specific_value, ": ")
    print(sum(demo_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_categorical_demo) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(demo_tbl[[col_name]])))
  for (specific_value in c(
    "990", "991", "992", "993", "994", "995",
    "996", "997", "998", "999", "70773"
  )
  ) {
    cat(specific_value, ": ")
    print(sum(demo_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_num_demo) {
  print(col_name)
  cat("overall mean: ")
  print(
    mean(demo_tbl[
      !(demo_tbl[[col_name]] %in% 990:999) &
        demo_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(demo_tbl[
      !(demo_tbl[[col_name]] %in% 990:999) &
        demo_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall max: ")
  print(
    max(demo_tbl[
      !(demo_tbl[[col_name]] %in% 990:999) &
        demo_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall min: ")
  print(
    min(demo_tbl[
      !(demo_tbl[[col_name]] %in% 990:999) &
        demo_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(demo_tbl[[col_name]])))
  for (specific_value in 990:999) {
    cat(specific_value, ": ")
    print(sum(demo_tbl[[col_name]]==specific_value, na.rm=T))
  }
  cat("70773: ")
  print(sum(demo_tbl[[col_name]] == 70773, na.rm=T))
  print("===================================================")
}
#############################################################


org_tbl <- read.csv("./HH_Tbl_ORGANIZATION_20240706_cleaned.csv")

to_check_char_morg <- c(
  "morganizationid", "household", "new_household", "entryhouseholdid"
)

to_check_categorical_morg <- c(
  "name", "position", "meeting"
)

to_check_num_morg <- c("days", "contribution")

for (col_name in to_check_char_morg) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(org_tbl[[col_name]])))
  for (
    specific_value in c(
      "990", "991", "992", "993", "994", "995",
      "996", "997", "998", "999", "70773"
    )
  ) {
    cat(specific_value, ": ")
    print(sum(org_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_categorical_morg) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(org_tbl[[col_name]])))
  for (
    specific_value in c(
      "990", "991", "992", "993", "994", "995",
      "996", "997", "998", "999", "70773"
    )
  ) {
    cat(specific_value, ": ")
    print(sum(org_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_num_morg) {
  print(col_name)
  cat("overall mean: ")
  print(
    mean(org_tbl[
      !(org_tbl[[col_name]] %in% 990:999) &
        org_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(org_tbl[
      !(org_tbl[[col_name]] %in% 990:999) &
        org_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall max: ")
  print(
    max(org_tbl[
      !(org_tbl[[col_name]] %in% 990:999) &
        org_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall min: ")
  print(
    min(org_tbl[
      !(org_tbl[[col_name]] %in% 990:999) &
        org_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(org_tbl[[col_name]])))
  for (specific_value in 990:999) {
    cat(specific_value, ": ")
    print(sum(org_tbl[[col_name]]==specific_value, na.rm=T))
  }
  cat("70773: ")
  print(sum(org_tbl[[col_name]] == 70773, na.rm=T))
  print("===================================================")
}
#############################################################

nmorg_tbl <- read.csv("./HH_Tbl_NMORGANIZATION_20240706_cleaned.csv")

to_check_char_nmorg <- c(
  "nmorganizationid", "household", "new_household", "entryhouseholdid"
)

for (col_name in to_check_char_nmorg) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(nmorg_tbl[[col_name]])))
  for (
    specific_value in c(
      "990", "991", "992", "993", "994", "995",
      "996", "997", "998", "999", "70773"
    )
  ) {
    cat(specific_value, ": ")
    print(sum(nmorg_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_categorical_morg) {
  print(col_name)
  cat("NA: ")
  print(sum(is.na(nmorg_tbl[[col_name]])))
  for (
    specific_value in c(
      "990", "991", "992", "993", "994", "995",
      "996", "997", "998", "999", "70773"
    )
  ) {
    cat(specific_value, ": ")
    print(sum(nmorg_tbl[[col_name]]==specific_value, na.rm=T))
  }
  print("===================================================")
}

for (col_name in to_check_num_morg) {
  print(col_name)
  cat("overall mean: ")
  print(
    mean(nmorg_tbl[
      !(nmorg_tbl[[col_name]] %in% 990:999) &
        nmorg_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall median: ")
  print(
    median(nmorg_tbl[
      !(nmorg_tbl[[col_name]] %in% 990:999) &
        nmorg_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall max: ")
  print(
    max(nmorg_tbl[
      !(nmorg_tbl[[col_name]] %in% 990:999) &
        nmorg_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("overall min: ")
  print(
    min(nmorg_tbl[
      !(nmorg_tbl[[col_name]] %in% 990:999) &
        nmorg_tbl[[col_name]] != 70773,
    ][[col_name]], na.rm=T)
  )
  cat("NA: ")
  print(sum(is.na(nmorg_tbl[[col_name]])))
  for (specific_value in 990:999) {
    cat(specific_value, ": ")
    print(sum(nmorg_tbl[[col_name]]==specific_value, na.rm=T))
  }
  cat("70773: ")
  print(sum(nmorg_tbl[[col_name]] == 70773, na.rm=T))
  print("===================================================")
}
#############################################################

sink()