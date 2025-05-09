#' Animal correlatino plots
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(corrplot)
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
              values_fill = list(present = 0))
