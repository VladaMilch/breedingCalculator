# fert <- c(13.4, 13.4, 35, 17.7)
# fert/sum(fert) * 95
# sum(fert)

strain_data <- read.table("./data_raw/strain_fertility.txt", header = T)
usethis::use_data(strain_data, internal = TRUE, overwrite = TRUE)
