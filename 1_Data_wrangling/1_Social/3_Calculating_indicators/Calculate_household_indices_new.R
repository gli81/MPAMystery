




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


tmp <- HHData[HHData$MPAID==num, ]
PCA.tmp <- tmp %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, 
         CarTruck_yes, MotorBoat, Fuel_nonBiomass, Boat) %>%
  na.omit()
sapply(PCA.tmp, FUN=mean)
PCA.tmp <- PCA.tmp %>%
  select(-CarTruck_yes)
tmp.PCA <- princomp(PCA.tmp)
summary(tmp.PCA)
tmp.PCA$loadings[, 1:2]
tmp.pc1 <- tmp.PCA$loadings[, 1]
tmp.LoadSum <- sum(tmp.PCA$loadings[, 1])
wght1 <- tmp.pc1 / tmp.LoadSum
sum(wght1)
tmp <- tmp %>% 
  mutate(
    MAIndex = wght1[1]*Entertain_yes + wght1[2]*PhoneCombined_yes + wght1[3]*Satellite_yes + wght1[4]*TV_yes +
      wght1[5]*Generator_yes + wght1[6]*Bicycle_yes + wght1[7]*Motorcycle_yes + wght1[8]*MotorBoat + 
      wght1[9]*Fuel_nonBiomass + wght1[10]*Boat
    )
return(tmp)

###################################################################
## could be useful
###################################################################
df <- data.frame(matrix(1:30, ncol=10))
vec <- 1:10
df <- df %>%
  rowwise() %>%
  mutate(sum_product = sum(c_across(everything())*vec)) %>%
  ungroup()
df
