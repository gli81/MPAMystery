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



for (i in 1:ncol(DEMOGRAPHIC_)) {
  filtered <- DEMOGRAPHIC_[(DEMOGRAPHIC_[, i] < 990 | DEMOGRAPHIC_[, i] > 999) & (!DEMOGRAPHIC_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = colnames(filtered)[i])) +
    geom_histogram(stat="count") +
    facet_wrap(~ yearID) +
    theme_minimal() +
    labs(title = paste0("Histogram of ", colnames(filtered)[i]," by yearID"),
         x = colnames(filtered)[i],
         y = "Count")
  
  # Save the plot
  ggsave(paste0("./histograms/demographic/", colnames(filtered)[i], "_hist.png"), plot=p)
}





ORGANIZATION_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_ORGANIZATION"
)


for (i in 1:ncol(ORGANIZATION_)) {
  filtered <- ORGANIZATION_[(ORGANIZATION_[, i] < 990 | ORGANIZATION_[, i] > 999) & (!ORGANIZATION_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = colnames(filtered)[i])) +
    geom_histogram(stat="count") +
    facet_wrap(~ yearID) +
    theme_minimal() +
    labs(title = paste0("Histogram of ", colnames(filtered)[i]," by yearID"),
         x = colnames(filtered)[i],
         y = "Count")
  
  # Save the plot
  ggsave(paste0("./histograms/organization/", colnames(filtered)[i], "_hist.png"), plot=p)
}





NMORGANIZATION_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_NMORGANIZATION"
)


for (i in 1:ncol(NMORGANIZATION_)) {
  filtered <- NMORGANIZATION_[(NMORGANIZATION_[, i] < 990 | NMORGANIZATION_[, i] > 999) & (!NMORGANIZATION_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = colnames(filtered)[i])) +
    geom_histogram(stat="count") +
    facet_wrap(~ yearID) +
    theme_minimal() +
    labs(title = paste0("Histogram of ", colnames(filtered)[i]," by yearID"),
         x = colnames(filtered)[i],
         y = "Count")
  
  # Save the plot
  ggsave(paste0("./histograms/nmorganization/", colnames(filtered)[i], "_hist.png"), plot=p)
}







WELLBEING_ <- last.file(
  dir.nam="./",
  nam="HH_Tbl_WELLBEING"
)

print(ncol(WELLBEING_))

for (i in 1:ncol(WELLBEING_)) {
  filtered <- WELLBEING_[(WELLBEING_[, i] < 990 | WELLBEING_[, i] > 999) & (!WELLBEING_[, i] %in% as.character(990:999)), ]
  p <- ggplot(filtered, aes_string(x = colnames(filtered)[i])) +
    geom_histogram(stat="count") +
    facet_wrap(~ yearID) +
    theme_minimal() +
    labs(title = paste0("Histogram of ", colnames(filtered)[i]," by yearID"),
         x = colnames(filtered)[i],
         y = "Count")
  
  # Save the plot
  ggsave(paste0("./histograms/wellbeing/", colnames(filtered)[i], "_hist.png"), plot=p)
}