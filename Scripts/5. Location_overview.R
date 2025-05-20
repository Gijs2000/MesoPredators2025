#' Animal location plots
#' Start date: 20 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(tidyverse)
}

# Hetamaps----
# Heatmap SM ----

SM_frac_long <- SM_location |>
  select(locationName, starts_with("fraction_")) |>
  pivot_longer(-locationName, names_to = "Species", values_to = "Fraction") |>
  mutate(Species = gsub("fraction_", "", Species),
         Species = gsub("_", " ", Species))

ggplot(SM_frac_long, aes(x = locationName, y = Species, fill = Fraction)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), oob = scales::squish) +  # clamp values > 0.2
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Fractional Detections by Species and Location",
    x = "Location",
    y = "Species",
    fill = "Fraction"
  )

# Heatmap RM ----
RM_frac_long <- RM_location |>
  select(locationName, starts_with("fraction_")) |>
  pivot_longer(-locationName, names_to = "Species", values_to = "Fraction") |>
  mutate(Species = gsub("fraction_", "", Species),
         Species = gsub("_", " ", Species))

ggplot(RM_frac_long, aes(x = locationName, y = Species, fill = Fraction)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), oob = scales::squish) +  # clamp values > 0.2
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Fractional Detections by Species and Location",
    x = "Location",
    y = "Species",
    fill = "Fraction"
  )


# Heatmap SW ----
SW_frac_long <- SW_location |>
  select(locationName, starts_with("fraction_")) |>
  pivot_longer(-locationName, names_to = "Species", values_to = "Fraction") |>
  mutate(Species = gsub("fraction_", "", Species),
         Species = gsub("_", " ", Species))

ggplot(SW_frac_long, aes(x = locationName, y = Species, fill = Fraction)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4), oob = scales::squish) +  # clamp values > 0.2
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Fractional Detections by Species and Location",
    x = "Location",
    y = "Species",
    fill = "Fraction"
  )


