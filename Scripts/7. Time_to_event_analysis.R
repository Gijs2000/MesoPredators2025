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





# Cox-Hazard -----
# Data preparation + setting up function----
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

time2event_order <- c("RM60", "RM120", "RM24",
                      "SW60", "SW120", "SW24",
                      "SM60", "SM120", "SM24"
)
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

# Run the function over all data + making grid for plotting ----
time2event_models <- purrr::imap(time2event_alldata, ~ fit_pairwise_models(.x, dataset_name = .y))
time2event_results <- bind_rows(time2event_models) |>
  dplyr::mutate(dataset = factor(dataset, levels = location_order)) |>
  dplyr::arrange(dataset, next_species, first_species) |>
  dplyr::mutate(next_species = case_when(
    next_species == "Martes foina" ~ "Stone marten",
    next_species == "Felis catus" ~ "Domestic cat",
    next_species == "Mustela putorius" ~ "European polecat",
    next_species == "Mustela erminea" ~ "Stoat",
    next_species == "Vulpes vulpes" ~ "Red fox",
    TRUE ~ next_species
  ),first_species = case_when(
    first_species == "Martes foina" ~ "Stone marten",
    first_species == "Felis catus" ~ "Domestic cat",
    first_species == "Mustela putorius" ~ "European polecat",
    first_species == "Mustela erminea" ~ "Stoat",
    first_species == "Vulpes vulpes" ~ "Red fox",
    TRUE ~ first_species
  ))

time2event_grid <- expand_grid(
  first_species = species_labels,
  next_species = species_labels,
  dataset = time2event_order
)

time2event_plotdata <- time2event_grid |>
  left_join(time2event_results, by = c("first_species", "next_species", "dataset")) |>
  mutate(estimate_clipped = ifelse(is.na(estimate), NA, pmax(pmin(estimate, 2), -2)),
         dataset = factor(dataset, levels = time2event_order) )

# Plotting the results ----
#png("Figures/7.Cox_hazard_tiles.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(time2event_plotdata, aes(x = first_species, y = next_species, fill = estimate_clipped)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(estimate), "", sprintf("%.2f\np=%.2f", estimate, p.value))), size = 5) +
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
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 14),                        
    axis.title.x = element_text(size = 16, face = "bold"),         
    axis.title.y = element_text(size = 16, face = "bold")          
  )

dev.off()


# Kaplan meier curves ----
dataset_labels <- c(
  RM24 = "Reitdiep midden",
  SW24 = "Zuidwest Friesland",
  SM24 = "Soarremoarre"
)

# Stone marten ----
Marten_data24 <-purrr::imap_dfr(time2event_alldata, function(df, dataset_name) {
  if (!grepl("24$", dataset_name)) return(NULL)
  
  df_MF <- df |> filter(scientificName == "Martes foina")
  if (nrow(df_MF) == 0) return(NULL)
  
  fit <- survfit(Surv(timegap_hours, event) ~ next_species, data = df_MF)
  
  broom::tidy(fit) |>
    mutate(
      detect_prob = 1 - estimate,
      dataset = dataset_name,
      first_species = "Martes foina",
      species_common = case_when(
        strata == "next_species=Felis catus" ~ "Domestic cat",
        strata == "next_species=Mustela erminea" ~ "Stoat",
        strata == "next_species=Mustela putorius" ~ "European polecat",
        strata == "next_species=Vulpes vulpes" ~ "Red fox",
        TRUE ~ strata
      )
    )
})

png("Figures/7.Stone_marten_KM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(Marten_data24, aes(x = time, y = detect_prob, color = species_common)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = species_common), alpha = 0.2, color = NA) +
  facet_wrap(~ dataset, labeller = labeller(dataset = dataset_labels))+
  labs(
    title = "Detection probability after Stone marten is detected",
    x = "Time since Stone marten detection (hours)",
    y = "Detection probability ",
    color = "Next species",
    fill = "Next species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),  # facet labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
dev.off()
# Domestic cat ----
Cat_data24 <-purrr::imap_dfr(time2event_alldata, function(df, dataset_name) {
  if (!grepl("24$", dataset_name)) return(NULL)
  
  df_MF <- df |> filter(scientificName == "Felis catus")
  if (nrow(df_MF) == 0) return(NULL)
  
  fit <- survfit(Surv(timegap_hours, event) ~ next_species, data = df_MF)
  
  broom::tidy(fit) |>
    mutate(
      detect_prob = 1 - estimate,
      dataset = dataset_name,
      first_species = "Felis catus",
      species_common = case_when(
        strata == "next_species=Martes foina" ~ "Stone marten",
        strata == "next_species=Mustela erminea" ~ "Stoat",
        strata == "next_species=Mustela putorius" ~ "European polecat",
        strata == "next_species=Vulpes vulpes" ~ "Red fox",
        TRUE ~ strata
      )
    )
})

png("Figures/7.Cat_KM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(Cat_data24, aes(x = time, y = detect_prob, color = species_common)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = species_common), alpha = 0.2, color = NA) +
  facet_wrap(~ dataset, labeller = labeller(dataset = dataset_labels))+
  labs(
    title = "Detection probability after Domestic cat is detected",
    x = "Time since Domestic cat detection (hours)",
    y = "Detection probability ",
    color = "Next species",
    fill = "Next species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),  # facet labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

dev.off()
# European polecat ----
Polecat_data24 <-purrr::imap_dfr(time2event_alldata, function(df, dataset_name) {
  if (!grepl("24$", dataset_name)) return(NULL)
  
  df_MF <- df |> filter(scientificName == "Mustela putorius")
  if (nrow(df_MF) == 0) return(NULL)
  
  fit <- survfit(Surv(timegap_hours, event) ~ next_species, data = df_MF)
  
  broom::tidy(fit) |>
    mutate(
      detect_prob = 1 - estimate,
      dataset = dataset_name,
      first_species = "Mustela putorius",
      species_common = case_when(
        strata == "next_species=Felis catus" ~ "Domestic cat",
        strata == "next_species=Mustela erminea" ~ "Stoat",
        strata == "next_species=Martes foina" ~ "Stone marten",
        strata == "next_species=Vulpes vulpes" ~ "Red fox",
        TRUE ~ strata
      )
    )
})

png("Figures/7.Polecat_KM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(Polecat_data24, aes(x = time, y = detect_prob, color = species_common)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = species_common), alpha = 0.2, color = NA) +
  facet_wrap(~ dataset, labeller = labeller(dataset = dataset_labels))+
  labs(
    title = "Detection probability after European polecat is detected",
    x = "Time since European polecat detection (hours)",
    y = "Detection probability ",
    color = "Next species",
    fill = "Next species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),  # facet labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

dev.off()

# Stoat ----
Stoat_data24 <-purrr::imap_dfr(time2event_alldata, function(df, dataset_name) {
  if (!grepl("24$", dataset_name)) return(NULL)
  
  df_MF <- df |> filter(scientificName == "Mustela erminea")
  if (nrow(df_MF) == 0) return(NULL)
  
  fit <- survfit(Surv(timegap_hours, event) ~ next_species, data = df_MF)
  
  broom::tidy(fit) |>
    mutate(
      detect_prob = 1 - estimate,
      dataset = dataset_name,
      first_species = "Mustela erminea",
      species_common = case_when(
        strata == "next_species=Felis catus" ~ "Domestic cat",
        strata == "next_species=Mustela putorius" ~ "European polecat",
        strata == "next_species=Martes foina" ~ "Stone marten",
        strata == "next_species=Vulpes vulpes" ~ "Red fox",
        TRUE ~ strata
      )
    )
})

png("Figures/7.Stoat_KM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(Stoat_data24, aes(x = time, y = detect_prob, color = species_common)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = species_common), alpha = 0.2, color = NA) +
  facet_wrap(~ dataset, labeller = labeller(dataset = dataset_labels))+
  labs(
    title = "Detection probability after Stoat is detected",
    x = "Time since Stoat detection (hours)",
    y = "Detection probability ",
    color = "Next species",
    fill = "Next species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),  # facet labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

dev.off()

# Red fox ----
Fox_data24 <-purrr::imap_dfr(time2event_alldata, function(df, dataset_name) {
  if (!grepl("24$", dataset_name)) return(NULL)
  
  df_MF <- df |> filter(scientificName == "Vulpes vulpes")
  if (nrow(df_MF) == 0) return(NULL)
  
  fit <- survfit(Surv(timegap_hours, event) ~ next_species, data = df_MF)
  
  broom::tidy(fit) |>
    mutate(
      detect_prob = 1 - estimate,
      dataset = dataset_name,
      first_species = "Vulpes vulpes",
      species_common = case_when(
        strata == "next_species=Felis catus" ~ "Domestic cat",
        strata == "next_species=Mustela putorius" ~ "European polecat",
        strata == "next_species=Martes foina" ~ "Stone marten",
        strata == "next_species=Mustela erminea" ~ "Stoat",
        TRUE ~ strata
      )
    )
})

png("Figures/7.Fox_KM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
ggplot(Fox_data24, aes(x = time, y = detect_prob, color = species_common)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = species_common), alpha = 0.2, color = NA) +
  facet_wrap(~ dataset, labeller = labeller(dataset = dataset_labels))+
  labs(
    title = "Detection probability after Red fox is detected",
    x = "Time since Red fox detection (hours)",
    y = "Detection probability ",
    color = "Next species",
    fill = "Next species"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),  # facet labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
  
dev.off()
