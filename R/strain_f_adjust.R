# adjusts effective fertility based on Festing table by day and 
# retreave the average litter size for that strain
# http://www.informatics.jax.org/silver/tables/table4-1.shtml
strain_f_adjust <- function(
  birth_days, 
  strain
  ){

  if(!(strain %in% strain_data$strain_name)){
    stop(paste("Please put existing strain name.\nAvailable strains:", 
               paste(strain_data$strain_name, 
                     collapse = " "),
               " \n",
               collapse = " "))
  }
  
  birth_days <- min(birth_days, 4)
  
  if(!(birth_days %in% c(1,2,3,4))){
    stop("birth_days can only take values 1, 2, 3 or 4")
  }
  
  # strain effective fertility total
  streffe <- strain_data$fertility[which(strain_data$strain_name==strain)]
  # from Festing
  fert_by_day <- c(13.4, 13.4, 35, 17.7)
  result_fertility <- cumsum(fert_by_day/sum(fert_by_day))[birth_days] * streffe
  
  lit_mean <- strain_data$litter_mean[which(strain_data$strain_name==strain)]
  
  res <- list(result_fertility, lit_mean)
  names(res) <- c("fertility", "lit_mean")
  
  return(res)
}

# strain_f_adjust(birth_days = 4, strain = "A/J")
