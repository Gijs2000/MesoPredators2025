#' Animal correlatino plots
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(corrplot)
  library(Hmisc)
}

# Setting up a presence/absence matrix RM----
RM_correlation_species <- c(
  "Vulpes vulpes",
  "Rodentia",
  "Martes",
  "Felis",
  "Felis catus",
  "Mustela putorius",
  "Mustela erminea",
  "Rattus rattus",
  "Martes foina",
  "Mustelidae",
  "Mustela nivalis/erminea",
  "Mustela nivalis",
  "Mustela",
  "Rattus norvegicus"
)

RM_order <- c(
  "Vulpes vulpes",
  "Felis catus",
  "Mustela",
  "Rodentia",
  "Mustela erminea",
  "Mustela nivalis",
  "Mustela putorius",
  "Martes foina",
  "Mustelidae"
)

RM_matrix <- observations_RM23_filtered |>
  dplyr::filter(scientificName %in% RM_correlation_species) |>
  dplyr::mutate(
    scientificName = dplyr::case_when(
      scientificName == "Mustela nivalis/erminea" ~ "Mustela",
      scientificName == "Martes" ~ "Martes foina",
      scientificName == "Felis" ~ "Felis catus",
      scientificName == "Rattus rattus" ~ "Rodentia",
      scientificName == "Rattus norvegicus" ~ "Rodentia",
      TRUE ~ scientificName
    )
  ) |>
  dplyr::mutate (present = 1) |>
  dplyr::distinct(study_date, locationName, scientificName, .keep_all = TRUE) |> #Not sure if this step is correct
  dplyr::select(study_date, locationName, scientificName, present) |>
  pivot_wider(names_from = scientificName, 
              values_from = present, 
              values_fill = list(present = 0)) |>
  dplyr::select(all_of(RM_order)) |>
  as.matrix()

# Correlation tests RM----
RM_cor_results <- rcorr(RM_matrix, type = "spearman") #should be kendall?
RM_cor_matrix <- RM_cor_results$r
RM_p_matrix <- RM_cor_results$P

png("Figures/3.Animal_correlation_RM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(RM_cor_matrix,
         method = "circle",        # circles for correlation
         type = "upper",           # upper triangle only
         order = "original",       # keep your order
         col = colorRampPalette(c("red", "white", "blue"))(200),  # color gradient
         p.mat = RM_p_matrix,         # significance matrix
         sig.level = 0.05,         # threshold for significance
         insig = "pch",            # draw something for insignificant correlations
         pch.col = "black",        # color of cross
         pch.cex = 2,              # size of cross
         tl.cex = 0.8,             # text size
         tl.col = "black",         # label color
         diag = F)             # hide diagonal
title("Correlation matrix of animals activity in the Reitdiep Midden area (2023)", cex.main=1.5)
dev.off()
# Setting up a presence/absence matrix SM----
SM_correlation_species <- c(
  "Vulpes vulpes",
  "Rodentia",
  "Martes",
  "Felis",
  "Felis catus",
  "Mustela putorius",
  "Mustela erminea",
  "Rattus rattus",
  "Martes foina",
  "Mustelidae",
  "Mustela nivalis/erminea",
  "Mustela nivalis",
  "Mustela",
  "Rattus norvegicus"
)

SM_order <- c(
  "Vulpes vulpes",
  "Felis catus",
  "Mustela",
  "Rodentia",
  "Mustela erminea",
  "Mustela nivalis",
  "Mustela putorius",
  "Martes foina",
  "Mustelidae"
)

SM_matrix <- observations_SM23_filtered |>
  dplyr::filter(scientificName %in% SM_correlation_species) |>
  dplyr::mutate(
    scientificName = dplyr::case_when(
      scientificName == "Mustela nivalis/erminea" ~ "Mustela",
      scientificName == "Martes" ~ "Martes foina",
      scientificName == "Felis" ~ "Felis catus",
      scientificName == "Rattus rattus" ~ "Rodentia",
      scientificName == "Rattus norvegicus" ~ "Rodentia",
      TRUE ~ scientificName
    )
  ) |>
  dplyr::mutate (present = 1) |>
  dplyr::distinct(study_date, locationName, scientificName, .keep_all = TRUE) |> #Not sure if this step is correct
  dplyr::select(study_date, locationName, scientificName, present) |>
  pivot_wider(names_from = scientificName, 
              values_from = present, 
              values_fill = list(present = 0)) |>
  dplyr::select(all_of(SM_order)) |>
  as.matrix()

# Correlation tests SM----
SM_cor_results <- rcorr(SM_matrix, type = "spearman") #should be kendall?
SM_cor_matrix <- SM_cor_results$r
SM_p_matrix <- SM_cor_results$P

#png("Figures/3.Animal_correlation_SM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(SM_cor_matrix,
         method = "circle",        # circles for correlation
         type = "upper",           # upper triangle only
         order = "original",       # keep your order
         col = colorRampPalette(c("red", "white", "blue"))(200),  # color gradient
         p.mat = SM_p_matrix,         # significance matrix
         sig.level = 0.05,         # threshold for significance
         insig = "pch",            # draw something for insignificant correlations
         pch.col = "black",        # color of cross
         pch.cex = 2,              # size of cross
         tl.cex = 0.8,             # text size
         tl.col = "black",         # label color
         diag = F)             # hide diagonal
title("Correlation matrix of animals activity in the Soarremoarre area (2023)", cex.main=1.5)
dev.off()


# Setting up a presence/absence matrix SW----
SW_correlation_species <- c(
  "Vulpes vulpes",
  "Rodentia",
  "Martes",
  "Felis",
  "Felis catus",
  "Mustela putorius",
  "Mustela erminea",
  "Rattus rattus",
  "Martes foina",
  "Mustelidae",
  "Mustela nivalis/erminea",
  "Mustela nivalis",
  "Mustela",
  "Rattus norvegicus"
)

SW_order <- c(
  "Vulpes vulpes",
  "Felis catus",
  "Mustela",
  "Rodentia",
  "Mustela erminea",
  "Mustela nivalis",
  "Mustela putorius",
  "Martes foina"
)

SW_matrix <- SW_data |>
  dplyr::filter(scientificName %in% SM_correlation_species,
                study_year == 2023) |>
  dplyr::mutate(
    scientificName = dplyr::case_when(
      scientificName == "Mustela nivalis/erminea" ~ "Mustela",
      scientificName == "Martes" ~ "Martes foina",
      scientificName == "Felis" ~ "Felis catus",
      scientificName == "Rattus rattus" ~ "Rodentia",
      scientificName == "Rattus norvegicus" ~ "Rodentia",
      TRUE ~ scientificName
    )
  ) |>
  dplyr::mutate (present = 1) |>
  dplyr::distinct(study_date, locationName, scientificName, .keep_all = TRUE) |> #Not sure if this step is correct
  dplyr::select(study_date, locationName, scientificName, present) |>
  pivot_wider(names_from = scientificName, 
              values_from = present, 
              values_fill = list(present = 0)) |>
  dplyr::select(all_of(SW_order)) |>
  as.matrix()

# Correlation tests SW----
SW_cor_results <- rcorr(SW_matrix, type = "spearman") #should be kendall?
SW_cor_matrix <- SW_cor_results$r
SW_p_matrix <- SW_cor_results$P

#png("Figures/3.Animal_correlation_SW.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(SW_cor_matrix,
         method = "circle",        # circles for correlation
         type = "upper",           # upper triangle only
         order = "original",       # keep your order
         col = colorRampPalette(c("red", "white", "blue"))(200),  # color gradient
         p.mat = SW_p_matrix,         # significance matrix
         sig.level = 0.05,         # threshold for significance
         insig = "pch",            # draw something for insignificant correlations
         pch.col = "black",        # color of cross
         pch.cex = 2,              # size of cross
         tl.cex = 0.8,             # text size
         tl.col = "black",         # label color
         diag = F)             # hide diagonal
title("Correlation matrix of animals activity in the Zuid-West Friesland area (2023)", cex.main=1.5)
dev.off()
