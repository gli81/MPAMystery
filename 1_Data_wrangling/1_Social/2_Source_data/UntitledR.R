for (col_name in to_check_char) {
  print(col_name)
  print(
    tapply(WELLBEING[[col_name]], WELLBEING$Series, unique)
  )
  cat("NA")
  print(sum(is.na(WELLBEING[[col_name]])))
  for (specific_value in c("995", "996", "997", "998")) {
    cat(specific_value, ": ")
    print(sum(WELLBEING[[col_name]]==specific_value))
  }
  print("===================================================")
}




for (col_name in to_check_num) {
  print(col_name)
  print(
    tapply(
      WELLBEING[[col_name]],
      WELLBEING$Series,
      function(x){range(x, na.rm = T)}
    )
  )
  print(
    tapply(
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,][[col_name]],
      WELLBEING[!WELLBEING[[col_name]] %in% 990:999,]$Series,
      function(x){mean(x, na.rm=T)})
  )
  print(
    tapply(
      WELLBEING[[col_name]],
      WELLBEING$Series,
      function(x){median(x, na.rm=T)})
  )
  cat("NA: ")
  print(sum(is.na(WELLBEING[[col_name]])))
  for (specific_value in 995:998) {
    cat(specific_value, ": ")
    print(sum(WELLBEING[[col_name]]==specific_value))
  }
  print("===================================================")
}