#' Data import and preperation
#' Start date: 6 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte


# Load packages----
{
  library(tidyverse)
  library(lubridate)
  library(stringr)
}
# Import data ----
# South-West Friesland data 2021,2022,2023
SW_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCffd_hF8DAKakdt3iZ1964ah6yMfGvn_c4h4peIMucmPVG6vlqxZAVMfdmpsU9w/pub?gid=259764214&single=true&output=csv") |>
  dplyr::mutate(study_date = mdy(study_date),
                eventStart = with_tz(ymd_hms(eventStart), tzone = "Europe/Amsterdam"),
                eventEnd = with_tz(ymd_hms(eventEnd), tzone = "Europe/Amsterdam"))
str(SW_data)

# Reitdiep midden data 2023
deployment_RM23 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRnjFiCxcIg4z3je-0_CCcTDMa6uMVjccCJNoJ9LfRDrQUHTuZG610T1HfIutepjw/pub?output=csv")
str(deployment_RM23)

observations_RM23 <- read_csv ("https://docs.google.com/spreadsheets/d/e/2PACX-1vRZdYiSTRKrAdmdOUnb-r_tscKRueVX2cUdxvu3PShekqh5mZd2wl5EetNs0a4IrQ/pub?gid=1367235605&single=true&output=csv") |>
  dplyr::mutate(eventStart = with_tz(ymd_hms(eventStart), tzone = "Europe/Amsterdam"),
                eventEnd = with_tz(ymd_hms(eventEnd), tzone = "Europe/Amsterdam"))
str(observations_RM23)

RM_down_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSsKxAXP2OhDfWElQAYGlkEmKDFJg596Oyuc3hwth-KaQlcNTZWebvJJHmcJ6p9mA/pub?gid=375727651&single=true&output=csv")
str(RM_down_data)

# Soarremoarre data 2023
deployment_SM23 <- read_csv ("https://docs.google.com/spreadsheets/d/e/2PACX-1vS6uwGxVTcQZ2IzUkw7V59lQgWpCEqwYdQNHW7_6vLIofVs6ZAfRUJyc1tcInd4Cw/pub?gid=1033951230&single=true&output=csv")
str(deployment_SM23)

observations_SM23 <- read_csv ("https://docs.google.com/spreadsheets/d/e/2PACX-1vQmSi1yTINru3K_xom415DDZ8P-Su8Gm69DV6Kdtz0QrrJuBLt6X9_CX_SQ2vuV9A/pub?gid=799909957&single=true&output=csv") |>
  dplyr::mutate(eventStart = with_tz(ymd_hms(eventStart), tzone = "Europe/Amsterdam"),
                eventEnd = with_tz(ymd_hms(eventEnd), tzone = "Europe/Amsterdam"))
str(observations_SM23)

SM_down_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT25ocRok1-g2wjBhBg3DGGTmIYYYvHW9KvQoxOgLNLM84PYCrBMWpv0S0nzMo4xA/pub?gid=2002383610&single=true&output=csv") |>
  dplyr:: mutate(
    start_downtime = mdy(start_downtime),
    end_downtime = mdy(end_downtime),
    total_down_days = as.integer(end_downtime - start_downtime) + 1
  )
str(SM_down_data)

Analysis_species <-c(
  "Vulpes vulpes",
  "Martes",
  "Felis",
  "Felis catus",
  "Mustela putorius",
  "Mustela erminea",
  "Martes foina"
)

# Structure data ----
# Structuring the observations data RM----
observations_RM23_filtered <- observations_RM23 |>
  dplyr:: select(-c(
    "mediaID",
    "cameraSetupType",
    "lifeStage",
    "sex",
    "behavior",
    "individualID",
    "individualPositionRadius",
    "individualPositionAngle",
    "individualSpeed",
    "bboxX",
    "bboxY",
    "bboxWidth",
    "bboxHeight",
    "observationTags",
    "observationComments",
    "classificationProbability",
    "classificationTimestamp",
    "observationLevel",
    "observationType")
  ) |>
  dplyr::mutate(
    study_year = format(eventStart, "%Y"),
    hour = as.numeric(format(eventStart, "%H")),
    minute = as.numeric(format(eventStart, "%M")),
    study_date = format(eventStart, "%m/%d/%Y"),
  ) |>
  dplyr::filter(study_year == "2023") |>
  mutate(study_date = mdy(study_date),
         study_year = as.numeric(study_year))

# Structuring the deployment data
deployment_RM23_filtered <- deployment_RM23 |>
  dplyr::select(-c(
    "coordinateUncertainty",
    "setupBy",
    "cameraID",
    "cameraModel",
    "cameraDelay",
    "cameraHeight",
    "cameraDepth",
    "cameraDepth",
    "cameraTilt",
    "cameraHeading",
    "detectionDistance",
    "timestampIssues",
    "baitUse",
    "habitat",
    "deploymentGroups",
    "deploymentTags",
  )) |>
  dplyr::mutate(
    latitude = as.numeric(str_replace(as.character(latitude), "^(.{2})(.+)$", "\\1.\\2")),
    longitude = as.numeric(str_replace(as.character(longitude), "^(.{1})(.+)$", "\\1.\\2")),
  )|>
  dplyr::mutate(
    study_year = format(deploymentStart, "%Y"),
    hour = format(deploymentStart, "%H"),
    minute = format(deploymentStart, "%M"),
    study_date = format(deploymentStart, "%m/%d/%Y")) |>
  dplyr::filter(study_year == "2023") |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "latitude",
    "longitude",
  )
  

observations_RM23_filtered <- observations_RM23_filtered |>
  dplyr::left_join(
    deployment_RM23_filtered,
    by = "deploymentID" )


# Structuring the observations data SM----
observations_SM23_filtered <- observations_SM23 |>
  dplyr:: select(-c(
    "mediaID",
    "cameraSetupType",
    "lifeStage",
    "sex",
    "behavior",
    "individualID",
    "individualPositionRadius",
    "individualPositionAngle",
    "individualSpeed",
    "bboxX",
    "bboxY",
    "bboxWidth",
    "bboxHeight",
    "observationTags",
    "observationComments",
    "classificationProbability",
    "classificationTimestamp",
    "observationLevel",
    "observationType")
  ) |>
  dplyr::mutate(
    study_year = format(eventStart, "%Y"),
    hour = as.numeric(format(eventStart, "%H")),
    minute = as.numeric(format(eventStart, "%M")),
    study_date = format(eventStart, "%m/%d/%Y"),
  ) |>
  dplyr::filter(study_year == "2023") |>
  mutate(study_date = mdy(study_date),
         study_year = as.numeric(study_year))

# Structuring the deployment data
deployment_SM23_filtered <- deployment_SM23 |>
  dplyr::select(-c(
    "coordinateUncertainty",
    "setupBy",
    "cameraID",
    "cameraModel",
    "cameraDelay",
    "cameraHeight",
    "cameraDepth",
    "cameraTilt",
    "cameraHeading",
    "detectionDistance",
    "timestampIssues",
    "baitUse",
    "habitat",
    "deploymentGroups",
    "deploymentTags"
  )) |>
  dplyr::mutate(
    latitude = as.numeric(str_replace(as.character(latitude), "^(.{2})(.+)$", "\\1.\\2")),
    longitude = as.numeric(str_replace(as.character(longitude), "^(.{1})(.+)$", "\\1.\\2")),
    deploymentStart = lubridate::ymd_hms(deploymentStart) 
  ) |>
  dplyr::mutate(
    study_year = format(deploymentStart, "%Y"),
    hour = format(deploymentStart, "%H"),
    minute = format(deploymentStart, "%M"),
    study_date = format(deploymentStart, "%m/%d/%Y")
  ) |>
  dplyr::filter(study_year == "2023") |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "latitude",
    "longitude"
  ) # Warning occurs because 82 entries have an invalid data entry


observations_SM23_filtered <- observations_SM23_filtered |>
  dplyr::left_join(
    deployment_SM23_filtered,
    by = "deploymentID" )
# Setting times correctly to Radians ----
# Reitdiep Midden
observations_RM23_filtered$timemin <- ((observations_RM23_filtered$hour*60)+(observations_RM23_filtered$minute))
observations_RM23_filtered$timerad <- (2 * pi * observations_RM23_filtered$timemin) / 1440 # why was this 1439
observations_RM23_filtered$timerad

#Zuid-West Friesland
SW_data$timemin <- ((SW_data$hour*60)+(SW_data$minute))
SW_data$timerad <- (2 * pi * SW_data$timemin) / 1440 # why was this 1439
SW_data$timerad

# Soarremoarre
observations_SM23_filtered$timemin <- ((observations_SM23_filtered$hour*60)+(observations_SM23_filtered$minute))
observations_SM23_filtered$timerad <- (2 * pi * observations_SM23_filtered$timemin) / 1440 # why was this 1439
observations_SM23_filtered$timerad

# Combining the three areas in one dataset ----
columns_to_keep <- c(
  "deploymentID", "scientificName", "study_year", "hour", "minute", 
  "study_date", "locationName", "latitude", "longitude", "timemin", "timerad"
)

combined_data <- bind_rows(
  observations_RM23_filtered |> dplyr::select(all_of(columns_to_keep)),
  observations_SM23_filtered |> dplyr::select(all_of(columns_to_keep)),
  SW_data |> dplyr::select(all_of(columns_to_keep))
) |>
  dplyr::filter(study_year == "2023")

# Short overview of the data ----

# Species of interest
RM_species_of_interest <- c(
  "Vulpes vulpes",
  "Rodentia",
  "Martes",
  "Felis",
  "Felis catus",
  "Mustela putorius",
  "Mustela erminea",
  "Rattus rattus",
  "Martes foina",
  "Erinaceus europaeus",
  "Canis lupus familiaris",
  "canis familiaris",
  "Mustelidae",
  "Mustela nivalis/erminea",
  "Canis lupus",
  "Mustela nivalis",
  "Mustela",
  "Procyon lotor",
  "Rattus norvegicus"
)

# Zuid-West Friesland
SW_summary <- SW_data |>
  dplyr::filter(study_year == "2023") |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "scientificName",
    "eventStart",
    "eventEnd",
  ) |>
  dplyr::group_by(scientificName) |>
  dplyr::summarise(
    n = n(),
    .groups = 'drop'
  )|>
  dplyr::arrange(desc(n))
SW_summary

# Reitdiep Midden
RM_summary <- observations_RM23_filtered |>
  dplyr::filter(scientificName %in% RM_species_of_interest) |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "scientificName",
    "eventStart",
    "eventEnd",
  ) |>
  dplyr::group_by(scientificName) |>
  dplyr::summarise(
    n = n(),
    .groups = 'drop'
  )|>
  dplyr::arrange(desc(n))
RM_summary

# Soarremoarre
SM_summary <- observations_SM23_filtered |>
  dplyr::filter(scientificName %in% RM_species_of_interest) |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "scientificName",
    "eventStart",
    "eventEnd",
  ) |>
  dplyr::group_by(scientificName) |>
  dplyr::summarise(
    n = n(),
    .groups = 'drop'
  )|>
  dplyr::arrange(desc(n))
SM_summary

Combi_summary <- combined_data |>
  dplyr::filter(scientificName %in% RM_species_of_interest) |>
  dplyr::select(
    "deploymentID",
    "locationName",
    "scientificName",
  ) |>
  dplyr::group_by(scientificName) |>
  dplyr::summarise(
    n = n(),
    .groups = 'drop'
  )|>
  dplyr::arrange(desc(n))
Combi_summary

# Sum data per species per location 2023----
# Sum data per species per location 2023 SM ----
SM_down_summary <- SM_down_data |>
  dplyr::group_by(locationName) |>
  dplyr::summarise(total_down_days = sum(total_down_days), .groups = "drop")

SM_location <- observations_SM23_filtered |>
  dplyr::filter(!is.na(scientificName),
                scientificName %in% Analysis_species) |>
  dplyr::  mutate(
    scientificName = case_when(
      scientificName == "Felis" ~ "Felis catus",
      scientificName == "Martes" ~ "Martes foina",
      TRUE ~ scientificName
    )) |>
  dplyr::distinct(scientificName, locationName, study_date) |>
  dplyr::group_by(locationName, scientificName) |>
  dplyr::summarise(days_detected = n(), .groups = "drop") |>
  pivot_wider(names_from = scientificName, values_from = days_detected, values_fill = 0)

SM_location <- SM_location |>
  left_join(SM_down_summary, by = "locationName")
