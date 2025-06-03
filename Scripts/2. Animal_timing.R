#' Animal timing
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(tidyverse)
  library(circular)
}


# Plotting the activity patterns of different species RM----
#Density plots per species
RM_Fox.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Vulpes vulpes']
RM_Fox.r <- RM_Fox.r[!is.na(RM_Fox.r)]
overlap::densityPlot(RM_Fox.r, rug=TRUE, xcenter="midnight")

RM_Cat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Felis', 'Felis catus')]
RM_Cat.r <- RM_Cat.r[!is.na(RM_Cat.r)]
densityPlot(RM_Cat.r, rug=TRUE, xcenter="midnight")

RM_Marten.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Martes', 'Martes foina')]
RM_Marten.r <- RM_Marten.r[!is.na(RM_Marten.r)]
densityPlot(RM_Marten.r, rug=TRUE, xcenter="midnight")

RM_Polecat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela putorius']
RM_Polecat.r <- RM_Polecat.r[!is.na(RM_Polecat.r)]
densityPlot(RM_Polecat.r, rug=TRUE, xcenter="midnight")

RM_Stoat.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela erminea']
RM_Stoat.r <- RM_Stoat.r[!is.na(RM_Stoat.r)]
densityPlot(RM_Stoat.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
png("Figures/2.Animal_activity_RM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(RM_Polecat.r,lwd=5, xcenter = "m",col = "#FB8F67", rug = TRUE, main = NULL )
densityPlot(RM_Stoat.r, lwd=5, rug=TRUE, col='#156064', xcenter="m", add = TRUE)
densityPlot(RM_Marten.r,  add=TRUE, lwd=5, rug=TRUE, col='#00C49A', xcenter="m", main = NULL)
densityPlot(RM_Fox.r, add=TRUE, lwd=5, rug=TRUE, col='#F8E16C', xcenter="m" )
densityPlot(RM_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='#FFC2B4', xcenter="m")


legend("topleft", c("Polecat", "Fox", "Marten", "Cat", "Stoat"), col=c("#FB8F67", "#F8E16C", "#00C49A", "#FFC2B4", "#156064"), lty = 1, lwd = 5, cex = 2)
title("Density plot of activity patterns of different species in the Reitdiep Midden area (2023)", cex.main=2.0)
dev.off()

# Plotting the activity patterns of different species SW----
#Density plots per species
SW_Fox.r <- SW_data$timerad[SW_data$scientificName == 'Vulpes vulpes']
SW_Fox.r <- SW_Fox.r[!is.na(SW_Fox.r)]
overlap::densityPlot(SW_Fox.r, rug=TRUE, xcenter="midnight")

SW_Cat.r <- SW_data$timerad[SW_data$scientificName %in% c('Felis', 'Felis catus')]
SW_Cat.r <- SW_Cat.r[!is.na(SW_Cat.r)]
densityPlot(SW_Cat.r, rug=TRUE, xcenter="midnight")

SW_Marten.r <- SW_data$timerad[SW_data$scientificName %in% c('Martes', 'Martes foina')]
SW_Marten.r <- SW_Marten.r[!is.na(SW_Marten.r)]
densityPlot(SW_Marten.r, rug=TRUE, xcenter="midnight")

SW_Polecat.r <- SW_data$timerad[SW_data$scientificName == 'Mustela putorius']
SW_Polecat.r <- SW_Polecat.r[!is.na(SW_Polecat.r)]
densityPlot(SW_Polecat.r, rug=TRUE, xcenter="midnight")

SW_Stoat.r <- SW_data$timerad[SW_data$scientificName == 'Mustela erminea']
SW_Stoat.r <- SW_Stoat.r[!is.na(SW_Stoat.r)]
densityPlot(SW_Stoat.r, rug=TRUE, xcenter="midnight")


#Combining Species in one graph
#png("Figures/2.Animal_activity_SW.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(SW_Marten.r, lwd=5, rug=TRUE, col= "#00C49A" ,xcenter="m", main = NULL)
densityPlot(SW_Fox.r,  add=TRUE, lwd=5, rug=TRUE, col='#F8E16C', xcenter="m", main = NULL)
densityPlot(SW_Polecat.r, extend=NULL, lwd=5, xcenter = "m",col = "#FB8F67", rug = TRUE, add = TRUE )
densityPlot(SW_Stoat.r, add=TRUE, lwd=5, rug=TRUE,col='#156064', xcenter="m" )
densityPlot(SW_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='#FFC2B4', xcenter="m")


legend("topleft", c("Polecat", "Fox", "Marten", "Cat", "Stoat"), col=c("#FB8F67", "#F8E16C", "#00C49A", "#FFC2B4", "#156064"), lty = 1, lwd = 5, cex = 2)
title("Density plot of activity patterns of different species in the Southwest Friesland area (2023)", cex.main=2.0)
dev.off()







# Plotting the activity patterns of different species SM----
#Density plots per species
SM_Cat.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName %in% c('Felis', 'Felis catus')]
SM_Cat.r <- SM_Cat.r[!is.na(SM_Cat.r)]
densityPlot(SM_Cat.r, rug=TRUE, xcenter="midnight")

SM_Marten.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName %in% c('Martes', 'Martes foina')]
SM_Marten.r <- SM_Marten.r[!is.na(SM_Marten.r)]
densityPlot(SM_Marten.r, rug=TRUE, xcenter="midnight")

SM_Polecat.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName == 'Mustela putorius']
SM_Polecat.r <- SM_Polecat.r[!is.na(SM_Polecat.r)]
densityPlot(SM_Polecat.r, rug=TRUE, xcenter="midnight")

SM_Stoat.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName == 'Mustela erminea']
SM_Stoat.r <- SM_Stoat.r[!is.na(SM_Stoat.r)]
densityPlot(SM_Stoat.r, rug=TRUE, xcenter="midnight")


#Combining Species in one graph

#png("Figures/2.Animal_activity_SM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
# First, plot the densities WITHOUT rug
densityPlot(SM_Polecat.r, lwd=5, rug=TRUE, col="#FB8F67", xcenter="m", main=NULL)
densityPlot(SM_Marten.r, lwd=5, rug=TRUE, add=TRUE, col="#00C49A", xcenter="m")
densityPlot(SM_Stoat.r, lwd=5, rug=TRUE, add=TRUE, col="#156064", xcenter="m")
densityPlot(SM_Cat.r, lwd=5, rug=TRUE, add=TRUE, col="#FFC2B4", xcenter="m")

legend("topleft", c("Polecat", "Marten", "Cat", "Stoat"), col=c("#FB8F67", "#00C49A", "#FFC2B4", "#156064"), lty = 1, lwd = 5, cex = 2)
title("Density plot of activity patterns of different species in the Soarremoarre area (2023)", cex.main=2.0)
dev.off()

# Watson wheeler test RM----
set.seed(42)
watson.wheeler.test(list(RM_Fox.r, RM_Marten.r))
watson.wheeler.test(list(RM_Fox.r, RM_Cat.r))
watson.wheeler.test(list(RM_Fox.r, RM_Polecat.r)) 
watson.wheeler.test(list(RM_Fox.r, RM_Stoat.r))
watson.wheeler.test(list(RM_Marten.r, RM_Cat.r))
watson.wheeler.test(list(RM_Marten.r, RM_Polecat.r)) #NS
watson.wheeler.test(list(RM_Marten.r, RM_Stoat.r))
watson.wheeler.test(list(RM_Cat.r, RM_Polecat.r))
watson.wheeler.test(list(RM_Cat.r, RM_Stoat.r))
watson.wheeler.test(list(RM_Stoat.r, RM_Polecat.r))

# Watson wheeler test SM----
set.seed(42)
watson.wheeler.test(list(SM_Fox.r, SM_Marten.r)) #NS
watson.wheeler.test(list(SM_Fox.r, SM_Cat.r)) #NS
watson.wheeler.test(list(SM_Fox.r, SM_Polecat.r)) #NS 
watson.wheeler.test(list(SM_Fox.r, SM_Stoat.r)) #NS
watson.wheeler.test(list(SM_Marten.r, SM_Cat.r))
watson.wheeler.test(list(SM_Marten.r, SM_Polecat.r)) #NS
watson.wheeler.test(list(SM_Marten.r, SM_Stoat.r))
watson.wheeler.test(list(SM_Cat.r, SM_Polecat.r))
watson.wheeler.test(list(SM_Cat.r, SM_Stoat.r))
watson.wheeler.test(list(SM_Stoat.r, SM_Polecat.r))

# Watson wheeler test SW----
set.seed(42)
watson.wheeler.test(list(SW_Fox.r, SW_Marten.r))
watson.wheeler.test(list(SW_Fox.r, SW_Cat.r))
watson.wheeler.test(list(SW_Fox.r, SW_Polecat.r)) #NS
watson.wheeler.test(list(SW_Fox.r, SW_Stoat.r))
watson.wheeler.test(list(SW_Marten.r, SW_Cat.r))
watson.wheeler.test(list(SW_Marten.r, SW_Polecat.r)) 
watson.wheeler.test(list(SW_Marten.r, SW_Stoat.r))
watson.wheeler.test(list(SW_Cat.r, SW_Polecat.r))
watson.wheeler.test(list(SW_Cat.r, SW_Stoat.r))
watson.wheeler.test(list(SW_Stoat.r, SW_Polecat.r))

