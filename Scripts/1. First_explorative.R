#' Author: Gijs van Male
#' Startdate: 28-01-2025
#' Supervisors: Chris Smit, Pieter Otte en Rienk Fokkema

#' Clear the environment
rm(list = ls())

#' restore the libraries to the version used to develop the script 
renv::restore()

#' Load the necessary packages
{
  library(tidyverse)
  library(lubridate)
  library(overlap)
}


#### Data upload ----
first_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRk5FCcLtDDDfrzosleeTR0WUoi1wrgE5q8n0XprCM8XF1_zn33uoj5-gW2RF0vOEEaGYrLzp-wX3qk/pub?gid=1891925468&single=true&output=csv")

#### Data cleaning ----
SW_sum <- first_data |>
  dplyr::mutate(study_area = dmy(study_area))|>
  dplyr::filter(month(study_area) %in% c(3,4,5))|>
  dplyr::group_by(scientificName) |>
  dplyr::summarise(Total_observations = n()) |>  
    mutate(englishName = case_when(
    scientificName == "Felis catus" ~ "Domestic_cat",
    scientificName == "Rattus norvegicus" ~ "Brown_rat",
    scientificName == "Meles meles" ~ "European_badger",
    scientificName == "Vulpes vulpes" ~ "Red_fox",
    scientificName == "Martes foina" ~ "Stone_marten",
    scientificName == "Mustela putorius" ~ "European_polecat",
    scientificName == "Canis lupus familiaris" ~ "Domestic_dog",
    scientificName == "Mustela erminea" ~ "Stoat",
    scientificName == "Mustela" ~ "Mustela",
    scientificName == "Erinaceus europaeus" ~ "European_hedgehog",
    scientificName == "Nyctereutes procyonoides" ~ "Common_raccoon_dog",
    scientificName == "Mustela nivalis" ~ "Least_weasel",
    TRUE ~ "Unknown"
  ))|>
  arrange(desc(Total_observations))
print(SW_sum)

first_data <- first_data |>
  mutate(
    TimeOfDay = (hour + minute / 60) * (2*pi/24),  # Convert hour and minute to decimal hours
    englishName = case_when(         # Map scientific names to English names
      scientificName == "Felis catus" ~ "Domestic_cat",
      scientificName == "Rattus norvegicus" ~ "Brown_rat",
      scientificName == "Meles meles" ~ "European_badger",
      scientificName == "Vulpes vulpes" ~ "Red_fox",
      scientificName == "Martes foina" ~ "Stone_marten",
      scientificName == "Mustela putorius" ~ "European_polecat",
      scientificName == "Canis lupus familiaris" ~ "Domestic_dog",
      scientificName == "Mustela erminea" ~ "Stoat",
      scientificName == "Mustela" ~ "Mustela",
      scientificName == "Erinaceus europaeus" ~ "European_hedgehog",
      scientificName == "Nyctereutes procyonoides" ~ "Common_raccoon_dog",
      scientificName == "Mustela nivalis" ~ "Least_weasel",
      TRUE ~ "Other"
    )
  )

#'Histogram of total observation per species
First_explorative_P1 <- SW_sum |>
  mutate(englishName = fct_reorder(englishName, Total_observations, .desc = TRUE)) |>
  ggplot(aes(x = englishName, y = Total_observations, fill = englishName)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Observations per Species in March - May 2023", x = "Species", y = "Total Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

First_explorative_P1

#'Pie chart of relative observation per species
First_explorative_P2 <-SW_sum |>
  ggplot( aes(x = "", y = Total_observations, fill = englishName)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Species observations (Pie Chart)") +
  theme_void()

First_explorative_P2

  
# Define the plot
First_explorative_P3 <- NULL

species_list <- unique(first_data$englishName)

colors <- c("red", "blue", "green", "purple", "orange", "cyan", "yellow", "black", "pink", "brown")


for (i in 1:length(species_list)) {
  species <- species_list[i]
    species_data <- first_data %>% filter(englishName == species)
    TimeOfDay_vector <- as.numeric(species_data$TimeOfDay)
  TimeOfDay_vector <- na.omit(TimeOfDay_vector)
  attr(TimeOfDay_vector, "na.action") <- NULL
    if (is.null(First_explorative_P3)) {
    First_explorative_P3 <- densityPlot(TimeOfDay_vector, xcenter = "midnight", col = colors[i], add = FALSE)
  } else {
    First_explorative_P3 <- densityPlot(TimeOfDay_vector, xcenter = "midnight", col = colors[i], add = TRUE)
  }
}

legend('topleft', 
       legend = species_list, 
       col = colors[1:length(species_list)], 
       bg = "white", 
       lwd = 2, 
       title = "Species")

