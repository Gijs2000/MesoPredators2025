#' Time-to-event analysis
#' Start date: 28 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Load packages----
{
  library(tidyverse)
  library(survival)
  library(survminer)
  library(purrr)
  library(broom)
  }

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
  dplyr::filter(scientificName != next_species)|>
  dplyr::mutate(event = 1) |>
  mutate(timegap_hours = timegap / 60) |>
  na.omit()

# Filter for three different time intervals
RM_time2event_60 <- RM_time2event_data |>
  dplyr::filter(timegap_hours <= 1)

RM_time2event_120 <- RM_time2event_data |>
  dplyr::filter(timegap_hours <= 2)

RM_time2event_24 <- RM_time2event_data |>
  dplyr::filter(timegap_hours <= 24)

#Plots RM----
RM_time2event_data_summary <- RM_time2event_data |>
  group_by(focal_species = scientificName, recent_species = next_species) |>
  summarise(
    mean_gap_hours = mean(timegap) ,          
    sd = sd(timegap),
    n = n(),
    se = sd / sqrt(n),
    lower_CI = mean_gap_hours - 1.96 * se,
    upper_CI = mean_gap_hours + 1.96 * se,
    .groups = "drop"
  ) 

ggplot(RM_time2event_data_summary, aes(x = recent_species, y = mean_gap_hours, color = recent_species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  facet_wrap(~ focal_species, scales = "free_y") +
  labs(
    title = "Effect of recent species detection on wait time Reitdiep",
    x = "Previous detected species",
    y = "Mean no. minutes between detections"
  ) +
  geom_text(aes(label = n), vjust = -1.5, size = 3.5, color = "black")+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "#CCCCCC"),
    strip.text = element_text(face = "bold")
  )

RM_surv <- Surv(time = RM_time2event_data$timegap_hours, event = RM_time2event_data$event)

RM_km_fit <- survfit(RM_surv ~ scientificName, data = RM_time2event_data)

ggsurvplot(
  RM_km_fit,
  data = RM_time2event_data,
  conf.int = TRUE,
  surv.scale = "percent",
  xlab = "Time since species detected (hours)",
  ylab = "Chance of not seeing a different species (%)",
  legend.title = "Species",
  risk.table = FALSE,
  censor = FALSE,
  xlim = c(0, 500),
  ggtheme = theme_minimal()
)
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
  dplyr::filter(scientificName != next_species) |>
  dplyr::mutate(event = 1,
                timegap_hours = timegap / 60)

# Filter for three different time intervals
SM_time2event_60 <- SM_time2event_data |>
  dplyr::filter(timegap_hours <= 1)

SM_time2event_120 <- SM_time2event_data |>
  dplyr::filter(timegap_hours <= 2)

SM_time2event_24 <- SM_time2event_data |>
  dplyr::filter(timegap_hours <= 24)

#Plots SM----
SM_time2event_data_summary <- SM_time2event_data |>
  group_by(focal_species = scientificName, recent_species = next_species) |>
  summarise(
    mean_gap_hours = mean(timegap) ,          
    sd = sd(timegap),
    n = n(),
    se = sd / sqrt(n),
    lower_CI = mean_gap_hours - 1.96 * se,
    upper_CI = mean_gap_hours + 1.96 * se,
    .groups = "drop"
  )

ggplot(SM_time2event_data_summary, aes(x = recent_species, y = mean_gap_hours, color = recent_species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  facet_wrap(~ focal_species, scales = "free_y") +
  labs(
    title = "Effect of recent species detection on wait time Reitdiep",
    x = "Previous detected species",
    y = "Mean no. minutes between detections"
  ) +
  geom_text(aes(label = n), vjust = -1.5, size = 3.5, color = "black")+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "#CCCCCC"),
    strip.text = element_text(face = "bold"))

SM_surv <- Surv(time = SM_time2event_data$timegap_hours, event = SM_time2event_data$event)

SM_km_fit <- survfit(SM_surv ~ scientificName, data = SM_time2event_data)

ggsurvplot(
  SM_km_fit,
  data = SM_time2event_data,
  conf.int = TRUE,
  surv.scale = "percent",
  xlab = "Time since species detected (hours)",
  ylab = "Chance of not seeing a different species (%)",
  legend.title = "Species",
  risk.table = FALSE,
  censor = FALSE,
  xlim = c(0, 500),
  ggtheme = theme_minimal()
)

################### SOUTHWEST FRIESLAND ###################
# Setting up the dataset correctly for SW----
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
  dplyr::filter(scientificName != next_species) |>
  dplyr::mutate(event = 1,
                timegap_hours = timegap / 60)

# Filter for three different time intervals
SW_time2event_60 <- SW_time2event_data |>
  dplyr::filter(timegap_hours <= 1)

SW_time2event_120 <- SW_time2event_data |>
  dplyr::filter(timegap_hours <= 2)

SW_time2event_24 <- SW_time2event_data |>
  dplyr::filter(timegap_hours <= 24)

#Plots SW----
SW_time2event_data_summary <- SW_time2event_data |>
  group_by(focal_species = scientificName, recent_species = next_species) |>
  summarise(
    mean_gap_hours = mean(timegap) ,          
    sd = sd(timegap),
    n = n(),
    se = sd / sqrt(n),
    lower_CI = mean_gap_hours - 1.96 * se,
    upper_CI = mean_gap_hours + 1.96 * se,
    .groups = "drop"
  )

ggplot(SW_time2event_data_summary, aes(x = recent_species, y = mean_gap_hours, color = recent_species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  facet_wrap(~ focal_species, scales = "free_y") +
  labs(
    title = "Effect of recent species detection on wait time Zuidwest",
    x = "Previous detected species",
    y = "Mean no. minutes between detections"
  ) +
  geom_text(aes(label = n), vjust = -1.5, size = 3.5, color = "black")+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "#CCCCCC"),
    strip.text = element_text(face = "bold")
  )

SW_time2event_data$facet_species <- SW_time2event_data$scientificName

SW_surv <- Surv(time = SW_time2event_data$timegap_hours, event = SW_time2event_data$event)

SW_km_fit <- survfit(Surv(timegap_hours, event) ~ next_species + facet_species, data = SW_time2event_data)


ggsurvplot_facet(
  fit = SW_km_fit,
  data = SW_time2event_data,
  facet.by = "scientificName",
  conf.int = TRUE,
  surv.scale = "percent",
  xlab = "Time since species detected (hours)",
  ylab = "Chance of not seeing another species (%)",
  legend.title = "Species seen after",
  censor = FALSE,
  xlim = c(0, 200),
  ggtheme = theme_minimal(),
  palette = "Dark2"
)

# Cox-Hazard -----
# setting up a function
fit_pairwise_models <- function(df, dataset_name = "unknown", add_frailty = FALSE) {
  cat("\n=== Processing dataset:", dataset_name, "===\n")
  print(summary(df))
  
  all_species <- sort(unique(df$scientificName))
  target_species <- sort(unique(df$next_species))
  results <- list()
  
  for (next_sp in target_species) {
    for (first_sp in all_species) {
      sub_df <- df |>
        filter(next_species == next_sp) |>
        mutate(first_species = ifelse(scientificName == first_sp, first_sp, paste0("Not_", first_sp)))
      
      if (length(unique(sub_df$first_species)) < 2) {
        cat("Skipping:", first_sp, "→", next_sp, "- only one group\n")
        next
      }
      
      sub_df$first_species <- factor(sub_df$first_species, levels = c(first_sp, paste0("Not_", first_sp)))
      
      if (nrow(sub_df) < 4) {
        cat("Skipping:", first_sp, "→", next_sp, "- less than 4 rows\n")
        next
      }
      if (sum(sub_df$event) == 0) {
        cat("Skipping:", first_sp, "→", next_sp, "- zero events\n")
        next
      }
      
      cat("Modeling:", dataset_name, "|", first_sp, "→", next_sp, "\n")
      print(table(sub_df$first_species, sub_df$event))
      
      formula <- if (add_frailty) {
        as.formula("Surv(timegap_hours, event) ~ first_species + frailty(locationName)")
      } else {
        as.formula("Surv(timegap_hours, event) ~ first_species")
      }
      
      model <- try(coxph(formula, data = sub_df), silent = TRUE)
      if (inherits(model, "try-error")) {
        cat("Model failed for:", first_sp, "→", next_sp, "\n")
        next
      }
      
      model_summary <- broom::tidy(model, conf.int = TRUE) |>
        filter(term == paste0("first_speciesNot_", first_sp)) |>
        mutate(
          next_species = next_sp,
          first_species = first_sp,
          dataset = dataset_name
        )
      
      results[[paste(dataset_name, next_sp, first_sp, sep = "_")]] <- model_summary
    }
  }
  
  bind_rows(results)
}




# putting al the data in a list
time2event_alldata <- list(
  RM60 = RM_time2event_60,
  RM120 = RM_time2event_120,
  RM24 = RM_time2event_24,
  SM60 = SM_time2event_60,
  SM120 = SM_time2event_120,
  SM24 = SM_time2event_24,  
  SW60 = SW_time2event_60,
  SW120 = SW_time2event_120,
  SW24 = SW_time2event_24
  )

time2event_order <- c("RM24", "RM120", "RM60",
                    "SW24", "SW120", "SW60",
                   "SM24", "SM120", "SM60"
                   )

# run the function over all data
time2event_models <- purrr::imap(time2event_alldata, ~ fit_pairwise_models(.x, dataset_name = .y))
time2event_results <- bind_rows(time2event_models) |>
  mutate(dataset = factor(dataset, levels = location_order)) |>
  arrange(dataset, next_species, first_species)

# Make a figure of this data
species <- sort(unique(c(time2event_results$first_species, time2event_results$next_species)))

datasets <- unique(time2event_results$dataset)
complete_grid <- expand_grid(
  first_species = species_labels,
  next_species = species_labels,
  dataset = datasets
)

plot_data <- complete_grid %>%
  left_join(time2event_results, by = c("first_species", "next_species", "dataset")) %>%
  mutate(estimate_clipped = ifelse(is.na(estimate), NA, pmax(pmin(estimate, 2), -2)))
#png("Figures/3.Cox_hazard_tiles.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(plot_data, aes(x = first_species, y = next_species, fill = estimate_clipped)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(estimate), "", sprintf("%.2f\np=%.2f", estimate, p.value))), size = 3) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0,
    limits = c(-2, 2),
    name = "Hazard\nestimate",
    na.value = "black"  # <- makes missing data tiles black
  ) +
  facet_wrap(~ dataset) +
  labs(
    title = "Species Avoidance/Attraction (Cox Estimates)",
    x = "First Detected Species",
    y = "Next Species Detected"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

