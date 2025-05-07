#' Animal correlation matrix
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(tidyverse)
}

# Setting times correctly to Radians ----

observations_RM23_filtered$timemin <- ((observations_RM23_filtered$hour*60)+(observations_RM23_filtered$minute))
observations_RM23_filtered$timerad <- (2 * pi * observations_RM23_filtered$timemin) / 1440 # why was this 1439
observations_RM23_filtered$timerad


# Reitdiep midden densityplots ----
Fox.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Vulpes vulpes']
Fox.r <- Fox.r[!is.na(Fox.r)]
overlap::densityPlot(Fox.r, rug=TRUE, xcenter="midnight")

Cat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Felis', 'Felis catus')]
Cat.r <- Cat.r[!is.na(Cat.r)]
densityPlot(Cat.r, rug=TRUE, xcenter="midnight")

Marten.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Martes', 'Martes foina')]
Marten.r <- Marten.r[!is.na(Marten.r)]
densityPlot(Marten.r, rug=TRUE, xcenter="midnight")

Polecat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela putorius']
Polecat.r <- Polecat.r[!is.na(Polecat.r)]
densityPlot(Polecat.r, rug=TRUE, xcenter="midnight")

Stoat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela erminea']
Stoat.r <- Stoat.r[!is.na(Stoat.r)]
densityPlot(Stoat.r, rug=TRUE, xcenter="midnight")

Weasel.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela nivalis']
Weasel.r <- Weasel.r[!is.na(Weasel.r)]
densityPlot(Weasel.r, rug=TRUE, xcenter="midnight")

# Function to check what is wrong with the data ----
checkInput <- function(y) {
  if(!is.vector(y) || !is.numeric(y))
    stop("The times of observations must be in a numeric vector.")
  if(length(unique(y)) < 2)
    stop("You have ", length(unique(y)), " different observations; at least 2 are needed to fit a density.")
  if(any(is.na(y)))
    stop("Your data have missing values.")
  if(any(y < 0 | y > 2*pi))
    stop("You have times < 0 or > 2*pi; make sure you are using radians.")
  return(NULL)
}
checkInput(Fox.r)

