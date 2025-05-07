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


SW_data$timemin <- ((SW_data$hour*60)+(SW_data$minute))
SW_data$timerad <- (2 * pi * SW_data$timemin) / 1440 # why was this 1439
SW_data$timerad

# Making the plot in one function ----
plot_species_activity <- function(data, title_text = "Density plot of activity patterns", center = "midnight",
                                  plot_order = c("Polecat", "Fox", "Marten", "Cat", "Mustela")) {
  species_groups <- list(
    Fox = c('Vulpes vulpes'),
    Cat = c('Felis', 'Felis catus'),
    Marten = c('Martes', 'Martes foina'),
    Polecat = c('Mustela putorius'),
    Stoat = c('Mustela erminea'),
    Weasel = c('Mustela nivalis'),
    Mustela = c('Mustela', 'Mustela putorius', 'Mustela erminea', 'Mustela nivalis')
  )
  
  plot_colors <- c(
    Polecat = "black",
    Fox = "red",
    Marten = "blue",
    Cat = "orange",
    Mustela = "purple"
  )
  
  # Extract activity times
  species_data <- lapply(species_groups, function(names) {
    r <- data$timerad[data$scientificName %in% names]
    r[!is.na(r)]
  })
  
  # Filter plot_order to species with valid data
  plot_order <- plot_order[plot_order %in% names(species_data)]
  plot_order <- plot_order[sapply(species_data[plot_order], length) > 1]
  
  # Plot first species
  first_sp <- plot_order[1]
  densityPlot(species_data[[first_sp]], extend = NULL, lwd = 5, xcenter = center,
              rug = TRUE, main = NULL, col = plot_colors[first_sp])
  
  # Add remaining species
  for (sp in plot_order[-1]) {
    densityPlot(species_data[[sp]], add = TRUE, lwd = 5, rug = TRUE, col = plot_colors[sp], xcenter = center)
  }
  
  legend("topleft", legend = plot_order, col = plot_colors[plot_order], lty = 1, lwd = 5)
  title(title_text, cex.main = 1.5)
}


plot_species_activity(observations_RM23_filtered,
                      title_text = "Reitdiep Midden 2023 Activity",
                      plot_order = c("Polecat", "Fox","Marten", "Mustela", "Cat"))
plot_species_activity(SW_data,
                      title_text = "Zuid-West Friesland 2023 Activity",
                      plot_order = c("Marten", "Fox","Polecat", "Mustela", "Cat"))



