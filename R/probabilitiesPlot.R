#' Expected total number of animals born in a breeding setup
#'
#' @param breedingObj object created by singleGenotype or multiGenotype function
#'
#' @return a ggplot with the number of matings on the X axis and the 
#' corresponding confidence in the successful breeding outcome on the Y axis
#' 
#' @examples 
#' breObj <- multiGenotype(
#'   confidence_p = 0.9, 
#'   birth_days = 3, 
#'   genotypes_p = c(0.25, 0.5, 0.25), 
#'   genotypes_N = c(10,0,10), 
#'   sex_distribution = "unimportant",
#'   litter_average = 7,
#'   strain = "C56BL/6J")
#' probabilitiesPlot(breObj)
#' 
#' @export
probabilitiesPlot <- function(breedingObj){
  kk = breedingObj$required_breedings
  
  n_breedings_interval <- seq(
    max(2,kk-5),
    kk+10
  )
  
  confi_values <- sapply(
    n_breedings_interval, 
    FUN=function(n_breedings){
      confi <- calculate_confi(
        k=n_breedings, 
        doof1=breedingObj$doof1, 
        genotypes_N=breedingObj$genotypes_N, 
        genotypes_p=breedingObj$genotypes_p)
    }
  )
  
  recommended <- ifelse(
    n_breedings_interval==breedingObj$required_breedings, 
    "Recommended", 
    "")
  
  # data to plot
  dd <- data.frame(n_breedings_interval, confi_values, recommended)
  
  ggplot2::ggplot(
    dd, 
    aes(x = n_breedings_interval, 
        y = confi_values*100, 
        label = recommended)) + 
    geom_point( # points by color
      color=dplyr::case_when(
        dd$recommended=="Recommended" ~ "darkviolet",
        dd$recommended!="Recommended" ~ "black"
      )
    ) + 
    labs(x = "Number of breedings", 
         y = "Confidence in successful outcome (%)"
    ) + 
    theme_pubr() + # from ggpubr, neat
    geom_hline( # custom confidence line
      yintercept=breedingObj$confidence_p*100, 
      col="blue", 
      linetype="dashed") + 
    geom_hline( # 95% confidence line
      yintercept=95, 
      col="red", 
      linetype="dashed") + 
    geom_label(
      label="Confidence 95%",
      x = n_breedings_interval[2],
      y=95,
      color="red"
    ) + 
    geom_label( #"custom confidence"
      label=paste0("Confidence ", breedingObj$confidence_p*100, "%"),
      x=n_breedings_interval[length(n_breedings_interval)-1],
      y=breedingObj$confidence_p*100,
      color="blue"
    ) +
    geom_text( # "Recommended"
      aes(label=recommended),
      hjust=-0.1, 
      vjust=0, 
      col="darkviolet"
    ) + 
    geom_text( # breeding values
      color=dplyr::case_when(
        dd$recommended=="Recommended" ~ "darkviolet",
        dd$recommended!="Recommended" ~ "black"),
      aes(label=n_breedings_interval),
      hjust=+1.25, 
      vjust=-0.5, 
      size = 3.5) 
  
  
}

