#' Animal correlation matrix
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(tidyverse)
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

RM_Weasel.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName == 'Mustela nivalis']
RM_Weasel.r <- RM_Weasel.r[!is.na(RM_Weasel.r)]
densityPlot(RM_Weasel.r, rug=TRUE, xcenter="midnight")

RM_Mustela.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Mustela', 'Mustela putorius', 'Mustela erminea', "Mustela nivalis", "Mustela nivalis/erminea")]
RM_Mustela.r <- RM_Mustela.r[!is.na(RM_Mustela.r)]
densityPlot(RM_Mustela.r, rug=TRUE, xcenter="midnight")

RM_Rodent.r <- observations_RM23_filtered$timerad[observations_RM23_filtered$scientificName %in% c('Rattus rattus', 'Rodentia', 'Rattus norvegicus')]
RM_Rodent.r <- RM_Rodent.r[!is.na(RM_Rodent.r)]
densityPlot(RM_Rodent.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
#png("Figures/2.Animal_activity_RM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(RM_Polecat.r, extend=NULL, lwd=5, xcenter = "m", rug = TRUE, main = NULL)
densityPlot(RM_Fox.r, add=TRUE, lwd=5, rug=TRUE, col='red', xcenter="m" )
densityPlot(RM_Marten.r, add=TRUE, lwd=5, rug=TRUE, col='blue', xcenter="m")
densityPlot(RM_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(RM_Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")
densityPlot(RM_Rodent.r, add=TRUE, lwd=5, rug=TRUE, col='yellow', xcenter="m")

legend("topleft", c("Polecat", "Fox", "Marten", "Cat", "Mustela", "Rodentia"), col=c("black", "red", "blue", "orange", "purple", "yellow"), lty = 1, lwd = 5, cex = 2.5)
title("Density plot of activity patterns of different species in the Reitdiep Midden area (2023)", cex.main=2.5)
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

SW_Weasel.r <- SW_data$timerad[SW_data$scientificName == 'Mustela nivalis']
SW_Weasel.r <- SW_Weasel.r[!is.na(SW_Weasel.r)]
densityPlot(SW_Weasel.r, rug=TRUE, xcenter="midnight")

SW_Mustela.r <- SW_data$timerad[SW_data$scientificName %in% c('Mustela', 'Mustela putorius', 'Mustela erminea', "Mustela nivalis", "Mustela nivalis/erminea")]
SW_Mustela.r <- SW_Mustela.r[!is.na(SW_Mustela.r)]
densityPlot(SW_Mustela.r, rug=TRUE, xcenter="midnight")

SW_Rodent.r <- SW_data$timerad[SW_data$scientificName %in% c('Rattus rattus', 'Rodentia', 'Rattus norvegicus')]
SW_Rodent.r <- SW_Rodent.r[!is.na(SW_Rodent.r)]
densityPlot(SW_Rodent.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
#png("Figures/2.Animal_activity_SW.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(SW_Rodent.r,extend=NULL , lwd=5, rug=TRUE, col='yellow', xcenter="m",main = NULL)
densityPlot(SW_Polecat.r,add=TRUE , lwd=5, xcenter = "m", rug = TRUE, )
densityPlot(SW_Fox.r, add=TRUE, lwd=5, rug=TRUE, col='red', xcenter="m" )
densityPlot(SW_Marten.r, add=TRUE, lwd=5, rug=TRUE, col='blue', xcenter="m")
densityPlot(SW_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(SW_Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")

legend("topleft", c("Polecat", "Fox", "Marten", "Cat", "Mustela", "Rodentia"), col=c("black", "red", "blue", "orange", "purple", "yellow"), lty = 1, lwd = 5, cex = 2.5)
title("Density plot of activity patterns of different species in the Zuid-West Friesland area (2023)", cex.main=2.5)
dev.off()






# Plotting the activity patterns of different species SM----
#Density plots per species
SM_Fox.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName == 'Vulpes vulpes']
SM_Fox.r <- SM_Fox.r[!is.na(SM_Fox.r)]
overlap::densityPlot(SM_Fox.r, rug=TRUE, xcenter="midnight") #Only 1 observation so ignore

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

SM_Weasel.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName == 'Mustela nivalis']
SM_Weasel.r <- SM_Weasel.r[!is.na(SM_Weasel.r)]
densityPlot(SM_Weasel.r, rug=TRUE, xcenter="midnight")

SM_Mustela.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName %in% c('Mustela', 'Mustela putorius', 'Mustela erminea', "Mustela nivalis", "Mustela nivalis/erminea")]
SM_Mustela.r <- SM_Mustela.r[!is.na(SM_Mustela.r)]
densityPlot(SM_Mustela.r, rug=TRUE, xcenter="midnight")

SM_Rodent.r <- observations_SM23_filtered$timerad[observations_SM23_filtered$scientificName %in% c('Rattus rattus', 'Rodentia', 'Rattus norvegicus')]
SM_Rodent.r <- SM_Rodent.r[!is.na(SM_Rodent.r)]
densityPlot(SM_Rodent.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
# png("Figures/2.Animal_activity_SM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(SM_Polecat.r, extend=NULL, lwd=5, xcenter = "m", rug = TRUE, main = NULL)
densityPlot(SM_Marten.r, add=TRUE, lwd=5, rug=TRUE, col='blue', xcenter="m")
densityPlot(SM_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(SM_Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")
densityPlot(SM_Rodent.r, add=TRUE, lwd=5, rug=TRUE, col='yellow', xcenter="m")

legend("topleft", c("Polecat", "Marten", "Cat", "Mustela", "Rodentia"), col=c("black", "blue", "orange", "purple", "yellow"), lty = 1, lwd = 5, cex = 2.5)
title("Density plot of activity patterns of different species in the Soarremoarre area (2023)", cex.main=2.5)
dev.off()
# Plotting the activity patterns of different species all areas together----
#Density plots per species
combi_Fox.r <- combined_data$timerad[combined_data$scientificName == 'Vulpes vulpes']
combi_Fox.r <- combi_Fox.r[!is.na(combi_Fox.r)]
overlap::densityPlot(combi_Fox.r, rug=TRUE, xcenter="midnight") #Only 1 observation so ignore

combi_Cat.r <- combined_data$timerad[combined_data$scientificName %in% c('Felis', 'Felis catus')]
combi_Cat.r <- combi_Cat.r[!is.na(combi_Cat.r)]
densityPlot(combi_Cat.r, rug=TRUE, xcenter="midnight")

combi_Marten.r <- combined_data$timerad[combined_data$scientificName %in% c('Martes', 'Martes foina')]
combi_Marten.r <- combi_Marten.r[!is.na(combi_Marten.r)]
densityPlot(combi_Marten.r, rug=TRUE, xcenter="midnight")

combi_Polecat.r <- combined_data$timerad[combined_data$scientificName == 'Mustela putorius']
combi_Polecat.r <- combi_Polecat.r[!is.na(combi_Polecat.r)]
densityPlot(combi_Polecat.r, rug=TRUE, xcenter="midnight")

combi_Stoat.r <- combined_data$timerad[combined_data$scientificName == 'Mustela erminea']
combi_Stoat.r <- combi_Stoat.r[!is.na(combi_Stoat.r)]
densityPlot(combi_Stoat.r, rug=TRUE, xcenter="midnight")

combi_Weasel.r <- combined_data$timerad[combined_data$scientificName == 'Mustela nivalis']
combi_Weasel.r <- combi_Weasel.r[!is.na(combi_Weasel.r)]
densityPlot(combi_Weasel.r, rug=TRUE, xcenter="midnight")

combi_Mustela.r <- combined_data$timerad[combined_data$scientificName %in% c('Mustela', 'Mustela putorius', 'Mustela erminea', "Mustela nivalis", "Mustela nivalis/erminea")]
combi_Mustela.r <- combi_Mustela.r[!is.na(combi_Mustela.r)]
densityPlot(combi_Mustela.r, rug=TRUE, xcenter="midnight")

combi_Rodent.r <- combined_data$timerad[combined_data$scientificName %in% c('Rattus rattus', 'Rodentia', 'Rattus norvegicus')]
combi_Rodent.r <- combi_Rodent.r[!is.na(combi_Rodent.r)]
densityPlot(combi_Rodent.r, rug=TRUE, xcenter="midnight")

#Combining Species in one graph
#png("Figures/2.Animal_activity_combi.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
densityPlot(combi_Marten.r, , lwd=5, rug=TRUE, col='blue', xcenter="m", main = NULL)
densityPlot(combi_Polecat.r, extend=NULL, lwd=5, xcenter = "m", rug = TRUE,add=TRUE)
densityPlot(combi_Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(combi_Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")
densityPlot(combi_Rodent.r, add=TRUE, lwd=5, rug=TRUE, col='yellow', xcenter="m")
densityPlot(combi_Fox.r, add=TRUE, lwd=5, rug=TRUE, col='red', xcenter="m" )
legend("topleft", c("Polecat", "Fox","Marten", "Cat", "Mustela", "Rodentia"), col=c("black", "red","blue", "orange", "purple", "yellow"), lty = 1, lwd = 5, cex = 2.5)
title("Density plot of activity patterns of different species in the three areas combined (2023)", cex.main=2.5)
dev.off()
# Statistics RM----
## Fox and cat
overlapPlot(RM_Fox.r, RM_Cat.r, xcenter = "midnight", 
            linetype = c(1, 1), linecol = c("red", "blue"), linewidth = c(2, 2))
legend('topright', c("Fox", "Cat"), lty=c(1,1), col=c("red", "blue"), bty='n')
foxcatest <- overlapEst(RM_Fox.r, RM_Cat.r, type="Dhat4")
foxcatest

foxcat.b <- bootstrap(Fox.r, RM_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(foxcat.b)

bootCI(foxcatest, foxcat.b, conf=0.95)
