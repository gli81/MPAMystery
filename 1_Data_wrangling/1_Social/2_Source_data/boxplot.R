pacman::p_load(
  rio, dplyr, janitor, ggplot2
)

last.file <- function(dir.nam, nam){
  import(
    paste0(
      dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T)))
    ),
    guess_max=50000
  )
}


DEMOGRAPHIC_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_DEMOGRAPHIC"
)
DEMOGRAPHIC_$yearID <- as.factor(DEMOGRAPHIC_$yearID)
for (i in 1:ncol(DEMOGRAPHIC_)) {
  filtered <- DEMOGRAPHIC_[(DEMOGRAPHIC_[, i] < 990 | DEMOGRAPHIC_[, i] > 999) & (!DEMOGRAPHIC_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = "yearID", y = colnames(filtered)[i])) +
    geom_boxplot() +
    labs(
      title = paste0("Boxplots of ", colnames(filtered)[i]," by yearID"),
      y = colnames(filtered)[i],
      x = "yearID"
    )
  
  # Save the plot
  ggsave(paste0("./boxplots/demographic/", colnames(filtered)[i], "_boxplot.png"), plot=p)
}





ORGANIZATION_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_ORGANIZATION"
)
ORGANIZATION_$yearID <- as.factor(ORGANIZATION_$yearID)
for (i in 1:ncol(ORGANIZATION_)) {
  filtered <- ORGANIZATION_[(ORGANIZATION_[, i] < 990 | ORGANIZATION_[, i] > 999) & (!ORGANIZATION_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = "yearID", y = colnames(filtered)[i])) +
    geom_boxplot() +
    labs(
      title = paste0("Boxplots of ", colnames(filtered)[i]," by yearID"),
      y = colnames(filtered)[i],
      x = "yearID"
    )
  
  # Save the plot
  ggsave(paste0("./boxplots/organization/", colnames(filtered)[i], "_boxplot.png"), plot=p)
}



NMORGANIZATION_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_NMORGANIZATION"
)
NMORGANIZATION_$yearID <- as.factor(NMORGANIZATION_$yearID)
for (i in 1:ncol(NMORGANIZATION_)) {
  filtered <- NMORGANIZATION_[(NMORGANIZATION_[, i] < 990 | NMORGANIZATION_[, i] > 999) & (!NMORGANIZATION_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = "yearID", y = colnames(filtered)[i])) +
    geom_boxplot() +
    labs(
      title = paste0("Boxplots of ", colnames(filtered)[i]," by yearID"),
      y = colnames(filtered)[i],
      x = "yearID"
    )
  
  # Save the plot
  ggsave(paste0("./boxplots/nmorganization/", colnames(filtered)[i], "_boxplot.png"), plot=p)
}



WELLBEING_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_WELLBEING"
)
WELLBEING_$yearID <- as.factor(WELLBEING_$yearID)
for (i in 1:ncol(WELLBEING_)) {
  filtered <- WELLBEING_[(WELLBEING_[, i] < 990 | WELLBEING_[, i] > 999) & (!WELLBEING_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = "yearID", y = colnames(filtered)[i])) +
    geom_boxplot() +
    labs(
      title = paste0("Boxplots of ", colnames(filtered)[i]," by yearID"),
      y = colnames(filtered)[i],
      x = "yearID"
    )
  
  # Save the plot
  ggsave(paste0("./boxplots/wellbeing/", colnames(filtered)[i], "_boxplot.png"), plot=p)
}