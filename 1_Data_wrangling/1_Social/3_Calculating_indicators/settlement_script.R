library(dplyr)
# Generate settlement level data
SettData <-
 HHData %>%
 group_by(SettlementID,MonitoringYear) %>%
 summarise(
   SettlementID = unique(SettlementID),
   SettlementName = unique(SettlementName),
   MPAID = unique(MPAID),
   # MPAName = unique(MPAName),
   Treatment = unique(Treatment),
   TimePoint = unique(MonitoringYear),
   MAIndex = mean(MAIndex, na.rm=T),
   MTIndex = mean(MTIndex, na.rm=T),
   FSIndex = mean(FSIndex, na.rm=T),
   PAIndex = mean(PAIndex, na.rm=T),
   SERate = mean(SERate, na.rm=T),
   YrResident = mean(YrResident, na.rm=T),
   Age = mean(HHHAge, na.rm=T),
   Gender = mean(HHHGender, na.rm=T)
  )
write.csv(SettData, file="Settlement_Data.csv")