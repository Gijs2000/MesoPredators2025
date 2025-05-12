#' Hierarchical models based on Lannarilli et al. (2025)
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte
#' For reference: https://hms-activity.netlify.app/

# Loading libraries ----
{
  library(tidyverse)
  library(grid)
  library(gridExtra)
  library(GLMMadaptive)
  library(ggpubr)
  library(mgcv)
  library(lubridate)
  library(lmtest)
  library(activity)
  library(overlap)
  library(circular)
  library(nimble)
  library(brms)
  library(forcats)
  library(MESS)
  library(suncalc)
  library(grateful)
}

# Set equation parameters ----
set.seed(129)
wavelength = 24
b0 = -3
b1 = 1 
b2 = 0.7
theta0 = 3
theta1 = 2 
tau_i = gamma_i = seq(-1.5, 1.5, by = 0.5)
time <- seq(0, 23, length.out = 100)

# Vertical changes ----
# create the curves
p_df<-data.frame()
for(i in 1:7){
  p_df<-rbind(p_df, 
              data.frame(p = plogis(b0 + b1*cos(2*pi*time/(24)+theta0 + gamma_i[4]) + 
                                      b2*cos(2*pi*time/(12)+ theta1 + gamma_i[4]) + tau_i[i]
              ),
              time = time, 
              curvesID = rep(as.factor(i), length(time))
              )
  )
}
p_df$curvesLeg = as.factor(rep(c("+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD", "Conditional", "+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD"), 
                               each = length(time)
)
)
p_df$mean <- c(rep(rep("off", length(time)),3), 
               rep("on", length(time)), 
               rep(rep("off", length(time)),3)
)

p_marg <- matrix(NA,length(time), 1)
for(i in 1:length(time)){
  intfun<-function(tau_x){
    plogis(b0 + b1*cos(2*pi*time[i]/(24) + theta0 + gamma_i[4]) +
             b2*cos(2*pi*time[i]/(12) + theta1 + gamma_i[4]) + tau_x
    ) * dnorm(tau_x, mean = 0, sd = 1)
  }
  p_marg[i]<-integrate(intfun,-Inf, Inf)[1]
}

p_df2 <- data.frame(p = unlist(p_marg),
                    time = time,
                    mean = "on", 
                    curvesID = as.factor(8),
                    curvesLeg = as.factor("Marginal")
)
p_df <- rbind(p_df, p_df2)

# plotting all this
# plot activity curves
(pl_vert <- ggplot(p_df, aes(x = time, y = p, group = curvesID)) +
   geom_line(aes(color = curvesLeg, linewidth = mean, linetype = mean)) +
   scale_color_manual(values = c("grey40", "grey60", "grey80", "red", "black")) +
   scale_linewidth_manual(values = c(0.5, 1.5)) +
   scale_linetype_manual(values = c(2, 1, 2)) +
   coord_cartesian(ylim = c(-0.05, 0.45)) +
   labs(x = "Time of Day", y = "Probability of Activity", title = "A) Variability in \n frequency of site-use") +
   theme_minimal()+
   theme(legend.position = "none",
         axis.line = element_line(colour = 'black', linetype = 'solid'),
         axis.ticks.x = element_line(colour = 'black', linetype = 'solid'), #element_blank(), #
         axis.text.y = element_blank(), 
         axis.title=element_text(size = 8,face = "bold"),
         plot.title=element_text(size = 9,face = "bold"),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()
   ) + 
   geom_segment(aes(x=12, y=min(p), xend=12, yend=min(p)-0.05), arrow=arrow(length= unit(0.5, "cm")), 
                linewidth = 1, color = "orange", linejoin = "mitre") +
   geom_segment(aes(x=12, y=0.35+0.003, xend=12, yend=0.35+0.053), arrow=arrow(length= unit(0.5, "cm")), 
                linewidth = 1, color = "orange", linejoin = "mitre")+
   scale_x_continuous(breaks=seq(0,24,length.out=7), labels=seq(0,24,4))
) 
pl_vert

# Horizontal changes ----
# create the curves
p_df<-data.frame()
for(i in 1:7){
  p_df<-rbind(p_df, 
              data.frame(p = plogis(b0 + b1*cos(2*pi*time/(24)+theta0 + gamma_i[i]) + 
                                      b2*cos(2*pi*time/(12)+ theta1 + gamma_i[i]) + tau_i[4]
              ),
              time = time, 
              curvesID = rep(as.factor(i), length(time))
              )
  )
}
p_df$curvesLeg = as.factor(rep(c("+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD", "Conditional", "+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD"), 
                               each = length(time)
)
)
p_df$mean <- c(rep(rep("off", length(time)),3), 
               rep("on", length(time)), 
               rep(rep("off", length(time)),3)
)

# calculate the marginal mean activity curve by integrating over the distribution of the random effects
p_marg <- matrix(NA,length(time), 1)
for(i in 1:length(time)){
  intfun<-function(gamma_x){
    plogis(b0 + b1*cos(2*pi*time[i]/(24)+ theta0 + gamma_x) +
             b2*cos(2*pi*time[i]/(12)+theta1 + gamma_x) + tau_i[4]
    ) *dnorm(gamma_x, mean = 0, sd = 1)
  }
  p_marg[i]<-integrate(intfun,-Inf, Inf)[1]
}
p_df2 <- data.frame(p = unlist(p_marg),
                    time = time,
                    mean = "on", 
                    curvesID = as.factor(8),
                    curvesLeg = as.factor("Marginal")
)
p_df <- rbind(p_df, p_df2)

# plot
pl_hor <- ggplot(p_df, aes(x = time, y = p, group = curvesID)) +
  geom_line(aes(color = curvesLeg, linewidth = mean, linetype = mean)) +
  scale_color_manual(values = c("grey40", "grey60", "grey80", "red", "black")) +
  scale_linewidth_manual(values = c(0.5, 1.5)) +
  scale_linetype_manual(values = c(2, 1)) +
  labs(x = "Time of Day", y = "Probability of Activity", title = "B) Variability in \n timing of activity") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks.x = element_line(colour = 'black', linetype = 'solid'), 
        axis.text.y = element_blank(), 
        axis.title=element_text(size=8,face="bold"),
        plot.title=element_text(size=9,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
  ) +
  geom_segment(aes(x=12 - 0.75, y=max(p) + 0.02, xend=12 - 7, yend=max(p) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre") +
  geom_segment(aes(x=12 + 0.75, y=max(p) + 0.02, xend=12 + 7, yend=max(p) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre") +
  scale_x_continuous(breaks=seq(0,24,length.out=7), labels=seq(0,24,4))+
  guides(linetype=element_blank())
pl_hor

# Join plots
pl <- grid.arrange(pl_vert, pl_hor, ncol = 2)
# Unimodal pattern vertical----
p_df<-data.frame()
for(i in 1:7){
 p_df<-rbind(p_df, 
             data.frame(p = plogis(b0 + b1*cos(2*pi*time/(24)+theta0 + gamma_i[4]) + tau_i[i]),
                        time = time, 
                        curvesID = rep(as.factor(i), length(time))
                        )
            )
}
p_df$curvesLeg = as.factor(rep(c("+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD", "Conditional", "+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD"), 
                               each = length(time)
                               )
                           )
# change in frequency of site-use
p_df$mean <- c(rep(rep("off", length(time)),3), 
               rep("on", length(time)), 
               rep(rep("off", length(time)),3)
               )
 

# calculate the marginal mean activity curve
p_marg <- matrix(NA,length(time), 1)
for(i in 1:length(time)){
  intfun<-function(tau_x){
    plogis(b0 + b1*cos(2*pi*time[i]/(24)+ theta0 + gamma_i[4]) + tau_x
           ) *  dnorm(tau_x, mean = 0, sd = 1)
  }
    p_marg[i]<-integrate(intfun,-Inf, Inf)[1]
}
p_df2 <- data.frame(p = unlist(p_marg),
                    time = time,
                    mean = "on", 
                    curvesID = as.factor(8),
                    curvesLeg = as.factor("Marginal")
                    )
p_df <- rbind(p_df, p_df2)

# plot
pl_vert_uni <- ggplot(p_df, aes(x = time, y = p, group = curvesID)) +
  geom_line(aes(color = curvesLeg, linewidth = mean, linetype = mean)) +
  scale_color_manual(values = c("grey40", "grey60", "grey80", "red", "black")) +
  scale_linewidth_manual(values = c(0.5, 1.5)) +
  scale_linetype_manual(values = c(2, 1, 2)) +
  coord_cartesian(ylim = c(-0.05, max(p_df$p)+0.053)) +
  labs(x = "Time of Day", y = "Activity Patterns", title = "A) Variability in frequency of site-use") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks.x = element_line(colour = 'black', linetype = 'solid'), 
        axis.text.y = element_blank(), #element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
  ) + 
  geom_segment(aes(x=12, y=min(p), xend=12, yend=min(p)-0.05), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre") +
  geom_segment(aes(x=12, y=max(p)+0.003, xend=12, yend=max(p)+0.053), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre")+
  scale_x_continuous(breaks=seq(0,24,length.out=7), labels=seq(0,24,4)) 
pl_vert_uni

# Unimodal pattern horiozntal ----
p_df<-data.frame()
for(i in 1:7){
  p_df<-rbind(p_df, 
              data.frame(p = plogis(b0 + b1*cos(2*pi*time/(24)+theta0 + gamma_i[i]) +  tau_i[4]),
                         time = time, 
                         curvesID = rep(as.factor(i), length(time))
              )
  )
}
p_df$curvesLeg = as.factor(rep(c("+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD", "Conditional", "+/- 0.5 SD", "+/- 1 SD", "+/- 1.5 SD"), 
                               each = length(time)
)
)
p_df$mean <- c(rep(rep("off", length(time)),3), rep("on", length(time)), rep(rep("off", length(time)),3))

# calculate the marginal mean activity curve by integrating over the distribution of the random effects
p_marg <- matrix(NA,length(time), 1)
for(i in 1:length(time)){
  intfun<-function(gamma_x){
    plogis(b0 + b1*cos(2*pi*time[i]/(24)+ theta0 + gamma_x) +tau_i[4]
    ) * dnorm(gamma_x, mean = 0, sd = 1)
  }
  p_marg[i]<-integrate(intfun,-Inf, Inf)[1]
}
p_df2 <- data.frame(p = unlist(p_marg),
                    time = time,
                    mean = "on", 
                    curvesID = as.factor(8),
                    curvesLeg = as.factor("Marginal")
)
p_df <- rbind(p_df, p_df2)

# plot
pl_hor_uni <- ggplot(p_df, aes(x = time, y = p, group = curvesID)) +
  geom_line(aes(color = curvesLeg, linewidth = mean, linetype = mean)) +
  scale_color_manual(values = c("grey40", "grey60", "grey80", "red", "black")) +
  scale_linewidth_manual(values = c(0.5, 1.5)) +
  scale_linetype_manual(values = c(2, 1)) +
  labs(x = "Time of Day", y = "Activity Patterns", title = "B) Variability in timing of activity") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks.x = element_line(colour = 'black', linetype = 'solid'),
        axis.text.y = element_blank(), #element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
  ) +
  geom_segment(aes(x=13.5 - 0.75, y=max(p) + 0.02, xend=13.5 - 7, yend=max(p) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre") +
  geom_segment(aes(x=13.5 + 0.75, y=max(p) + 0.02, xend=13.5 + 7, yend=max(p) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
               linewidth = 1, color = "orange", linejoin = "mitre") +
  scale_x_continuous(breaks=seq(0,24,length.out=7), labels=seq(0,24,4)) +
  guides(linetype=element_blank())
pl_hor_uni

pl_uni <- grid.arrange(pl_vert_uni, pl_hor_uni, ncol = 2)

# trigonometric and cyclic cubic spline hierarchical models ----
source("source_functions/sim_activity.R")
source("source_functions/sim_to_minute.R")
set.seed(129)
# Set equations' parameters
M = 100
J = 30
wavelength = 24
n_peak = 2
b0 = -3
b1 = 1 
b2 = 0.7
theta0 = 3
theta1 = 2 
sd_tau = 1
sd_gamma = 0.3
time <- seq(0, 23, length.out = 100)

# simulate data ----
dat <- sim_activity(M = M, 
                    J = J, 
                    wavelength = wavelength, 
                    n_peak = n_peak, 
                    n_sim = 1, 
                    b0 = b0, 
                    b0_constant = TRUE, # common intercept
                    tau_constant = FALSE, 
                    sdtau = sd_tau, # ~site-specific intercept
                    b1 = b1, 
                    b2 = b2, # amplitude of the cosine terms 
                    theta0 = theta0, 
                    theta1 = theta1, # common phaseshifts for the cosine terms
                    phaseshift_constant = FALSE, 
                    sd_phaseshift = sd_gamma, # site-specific phaseshift (equal for both cosine terms)
                    plot_true_act = FALSE
)
#Observe the structure of the new object
str(dat)

# Organize data for plotting
sites_i <- sample(x = 1:M, size = 10, replace = FALSE)
sample_sites <- cbind(sites_i, dat$true_activity_prob[sites_i, 0:25]) 
colnames(sample_sites) <- c("sites", 00:24)
sample_sites <- as.data.frame(sample_sites) %>% 
  pivot_longer(cols = -sites, names_to = "time", values_to = "prob")

(pl_sites <- ggplot() +
    geom_line(data = sample_sites, aes(x = as.numeric(time), y = prob, group = sites, color = as.factor(sites)), 
              linewidth = 0.5, alpha = 0.7, linetype = 2) +
    geom_line(data = dat$Conditional, aes(x = time, y = p), linewidth = 1, color = "red", inherit.aes = TRUE) +
    geom_line(data = dat$Marginal, aes(x = time, y = p), linewidth = 1, color = "black", inherit.aes = TRUE) +
    labs(x = "Time of Day", y = "Probability of Activity", title = "C) Variability in frequency of site-use and \n timing of activity", 
         color = "Site_ID")+
    theme_minimal()+
    theme(legend.position = "none",
          legend.title = element_text(size=10,face="bold"),
          legend.text = element_text(size=10,face="bold"),
          axis.line = element_line(colour = 'black', linetype = 'solid'),
          axis.ticks.x = element_line(colour = 'black', linetype = 'solid'),
          axis.title = element_text(size=8,face="bold"),
          axis.text.y = element_blank(),
          plot.title = element_text(size=9,face="bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 9, colour = "black", face = "bold", hjust = 0))+
    geom_segment(aes(x=12, y=min(sample_sites$prob), xend=12, yend=min(sample_sites$prob)-0.05), arrow=arrow(length= unit(0.5, "cm")), 
                 linewidth = 1, color = "orange", linejoin = "mitre") +
    geom_segment(aes(x=12, y=0.45+0.003, xend=12, yend=0.45+0.053), arrow=arrow(length= unit(0.5, "cm")), 
                 linewidth = 1, color = "orange", linejoin = "mitre") +
    geom_segment(aes(x=12 - 0.75, y=max(sample_sites$prob) + 0.02, xend=12 - 7, yend=max(sample_sites$prob) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
                 linewidth = 1, color = "orange", linejoin = "mitre") +
    geom_segment(aes(x=12 + 0.75, y=max(sample_sites$prob) + 0.02, xend=12 + 7, yend=max(sample_sites$prob) + 0.02), arrow=arrow(length= unit(0.5, "cm")), 
                 linewidth = 1, color = "orange", linejoin = "mitre") +
    scale_x_continuous(breaks=seq(0,23,length.out=7), labels=seq(0,24,4)))
pl_save <- grid.arrange(pl, pl_sites, ncol = 2)

# dataprep----
y <- as.data.frame(dat$sim_data)
dim(y)

# summarize aggregated data
y <- data.frame(id=paste("A", seq(1,M,1), sep=""), 
                y=as.data.frame(y)
)
colnames(y) <- c("id", 
                 paste0("S", seq(1, ncol(y)-1, 1), sep="")
)

# Format data in long format
y_long <- pivot_longer(data = y, cols = -id, names_to = "time", values_to = "y") %>%
  mutate(time=as.numeric(substr(time,2,10)),
         id = factor(id)
  )

# create variable to describe time as hour (between 0 and 23)
y_long$hour <- sim_to_minute(y_long$time, group = wavelength)

# show first few rows of the dataset created
knitr::kable(head(y_long))

# count successes and failures at each site-occasion
occasions_cbind <- y_long %>% 
  group_by(id, hour) %>% 
  summarise(success = sum(y),
            n_events = n(),
            failure = n_events - success) %>% 
  dplyr::rename(Site = id,
                Time = hour)
knitr::kable(head(occasions_cbind))
