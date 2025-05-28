#' Time-to-event analysis
#' Start date: 28 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte


################### REITDIEPMIDDEN ###################
# Setting up the dataset correctly for RM----
RM_time2event_data <- observations_RM23_filtered |>
  dplyr::filter(scientificName %in% Analysis_species) |>
  dplyr::mutate(scientificName =case_when(
    scientificName == "Felis" ~ "Felis catus",
    scientificName == "Martes" ~ "Martes foina",
    TRUE ~ scientificName
  )) |>
  dplyr::arrange(locationName, eventStart) |>
  dplyr::group_by(locationName) |>
  dplyr::mutate(
    next_species = lead(scientificName),
    timegap = as.numeric(difftime(lead(eventStart), eventStart, units = "mins"))) |>
  dplyr::ungroup() |>
  dplyr::select(locationName, scientificName, next_species, timegap) |>
  dplyr::filter(scientificName != next_species)

################### SOARREMOARRE ###################
# Setting up the dataset correctly for SM----
SM_time2event_data <- observations_SM23_filtered |>
  dplyr::filter(scientificName %in% Analysis_species) |>
  dplyr::mutate(scientificName =case_when(
    scientificName == "Felis" ~ "Felis catus",
    scientificName == "Martes" ~ "Martes foina",
    TRUE ~ scientificName
  )) |>
  dplyr::arrange(locationName, eventStart) |>
  dplyr::group_by(locationName) |>
  dplyr::mutate(
    next_species = lead(scientificName),
    timegap = as.numeric(difftime(lead(eventStart), eventStart, units = "mins"))) |>
  dplyr::ungroup() |>
  dplyr::select(locationName, scientificName, next_species, timegap) |>
  dplyr::filter(scientificName != next_species)

################### SOUTHWEST FRIESLAND ###################
# Setting up the dataset correctly for SM----
SW_time2event_data <- SW_data |>
  dplyr::filter(scientificName %in% Analysis_species) |>
  dplyr::mutate(scientificName =case_when(
    scientificName == "Felis" ~ "Felis catus",
    scientificName == "Martes" ~ "Martes foina",
    TRUE ~ scientificName
  )) |>
  dplyr::arrange(locationName, eventStart) |>
  dplyr::group_by(locationName) |>
  dplyr::mutate(
    next_species = lead(scientificName),
    timegap = as.numeric(difftime(lead(eventStart), eventStart, units = "mins"))) |>
  dplyr::ungroup() |>
  dplyr::select(locationName, scientificName, next_species, timegap) |>
  dplyr::filter(scientificName != next_species)


