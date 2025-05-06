#' Data import and preperation
#' Start date: 6 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte


# Load packages----
{
  library(tidyverse)
  library(lubridate)
}
# Import data ----
# South-West Friesland data 2021,2022,2023
SW_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBlKMSm9dDB5TPG2l7cpmG4tXDMxo8LVlPU2flR0zFZ5iVWrrA5cFDKmKv3ppmVg/pub?gid=1693148414&single=true&output=csv")
str(SW_data)

# Reitdiep midden data 2023
deployment_RM23 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRnjFiCxcIg4z3je-0_CCcTDMa6uMVjccCJNoJ9LfRDrQUHTuZG610T1HfIutepjw/pub?output=csv")
str(deployment_RM23)

observations_RM23 <- read_csv ("https://docs.google.com/spreadsheets/d/e/2PACX-1vRZdYiSTRKrAdmdOUnb-r_tscKRueVX2cUdxvu3PShekqh5mZd2wl5EetNs0a4IrQ/pub?gid=1367235605&single=true&output=csv")
str(observations_RM23)

# Structure data ----
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
    "classificationTimestamp")
  ) |>
  dplyr::mutate(
    study_year = format(eventStart, "%Y"),
    hour = format(eventStart, "%H"),
    minute = format(eventStart, "%M")
  )
