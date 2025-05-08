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
# Reitdiep Middeb
observations_RM23_filtered$timemin <- ((observations_RM23_filtered$hour*60)+(observations_RM23_filtered$minute))
observations_RM23_filtered$timerad <- (2 * pi * observations_RM23_filtered$timemin) / 1440 # why was this 1439
observations_RM23_filtered$timerad

#Zuid-West Friesland
SW_data$timemin <- ((SW_data$hour*60)+(SW_data$minute))
SW_data$timerad <- (2 * pi * SW_data$timemin) / 1440 # why was this 1439
SW_data$timerad
# Plotting the activity patterns of different species RM----
#Density plots per species
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

Mustela.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Mustela', 'Mustela putorius', 'Mustela erminea', "Mustela nivalis")]
Mustela.r <- Mustela.r[!is.na(Mustela.r)]
densityPlot(Mustela.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
png("Figures/2.Animal_activity_RM.png", width = 1920, height = 1080)
densityPlot(Polecat.r, extend=NULL, lwd=5, xcenter = "m", rug = TRUE, main = NULL)
densityPlot(Fox.r, add=TRUE, lwd=5, rug=TRUE, col='red', xcenter="m" )
densityPlot(Marten.r, add=TRUE, lwd=5, rug=TRUE, col='blue', xcenter="m")
densityPlot(Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")

legend("topleft", c("Polecat", "Fox", "Marten", "Cat", "Mustela"), col=c("black", "red", "blue", "orange", "purple"), lty = 1, lwd = 5)
title("Density plot of activity patterns of different species in the Reitdiep Midden area (2023)", cex.main=1.5)

# Saving the plot
dev.off()




