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
RM_time2event_data_summary <- RM_time2event_data %>%
  group_by(focal_species = scientificName, recent_species = next_species) %>%
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
SM_time2event_data_summary <- SM_time2event_data %>%
  group_by(focal_species = scientificName, recent_species = next_species) %>%
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
SW_time2event_data_summary <- SW_time2event_data %>%
  group_by(focal_species = scientificName, recent_species = next_species) %>%
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
fit_cox_and_plot_km <- function(df, dataset_name = "unknown", add_frailty = FALSE, return_plots = TRUE, plot_dir = NULL) {
  unique_species <- unique(df$next_species)
  
  results <- list()
  
  for (target_species in unique_species) {
    sub_df <- df %>% filter(next_species == target_species)
    
    
    # Fit Cox model
    formula <- if (add_frailty) {
      as.formula("Surv(timegap_hours, event) ~ scientificName + frailty(locationName)")
    } else {
      as.formula("Surv(timegap_hours, event) ~ scientificName")
    }
    
    model <- try(coxph(formula, data = sub_df), silent = TRUE)
    if (inherits(model, "try-error")) next
    
    # Tidy model
    model_summary <- tidy(model, conf.int = TRUE) %>%
      mutate(next_species = target_species, dataset = dataset_name)
    
    # Kaplan-Meier plot
    survfit_obj <- survfit(Surv(timegap_hours, event) ~ scientificName, data = sub_df)
    plot_title <- paste("KM Curve:", dataset_name, "| Next:", target_species)
    
    km_plot <- ggsurvplot(
      survfit_obj,
      data = sub_df,
      conf.int = TRUE,
      pval = TRUE,
      risk.table = TRUE,
      title = plot_title,
      ggtheme = theme_minimal()
    )
    

    # Store results
    results[[paste0(dataset_name, "_", target_species)]] <- list(
      model = model_summary,
      plot = if (return_plots) km_plot else NULL
    )
  }
  
  return(results)
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

# run the function over all data
time2event_results <- imap(time2event_alldata, ~ fit_cox_and_plot_km(.x, dataset_name = .y))

print(time2event_results$SW24$`SW24_Felis catus`$plot)


