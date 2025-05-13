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

# run model
trig_rand_int <- mixed_model(fixed = cbind(success, failure) ~ 
                               cos(2 * pi * Time/24)  + sin(2 * pi * Time/24) +
                               cos(2 * pi * Time/12)  + sin(2 * pi * Time/12),
                             random = ~  1  |   Site, 
                             data = occasions_cbind, 
                             family = binomial(), 
                             iter_EM = 0
)
summary(trig_rand_int)

# estimated activity pattern
newdat <- with(occasions_cbind, expand.grid(Time = seq(min(Time), 24, length.out = 48)))
cond_eff0 <- effectPlotData(trig_rand_int, newdat, marginal = FALSE) %>% 
  mutate(pred = plogis(pred),
         low = plogis(low),
         upp = plogis(upp),
         Mod = "Estimated: Random Intercept-only")

# simulated conditional activity pattern
cond_true <- dat$Conditional %>% 
  mutate(low = NA,
         upp = NA,
         Mod = "Simulated Conditional") %>% 
  dplyr::rename(Time = time, pred = p) %>% 
  select(Time, pred, low, upp, Mod)

# combine the two for visualization  purposes
cond_eff <- rbind(cond_true, cond_eff0)

# plot
(pl_trig1 <- ggplot(cond_eff, aes(Time, pred)) +
    geom_ribbon(aes(ymin = low, ymax = upp, color = Mod, fill = Mod), alpha = 0.3, linewidth = 0.25) + 
    geom_line(aes(color = Mod), linewidth = 1) + #
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time of Day (Hour)", y = "Predicted Activity Pattern \n (probability)", title = "Estimated vs Simulated Activity Patterns")+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10,face="bold"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-10,-10,-10),
          plot.title = element_text(size=10,face="bold"),
          axis.line = element_line(colour = 'black', linetype = 'solid'),
          axis.ticks = element_line(colour = 'black', linetype = 'solid'),
          axis.title = element_text(size=9,face="bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth=0.5),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 9, colour = "black", face = "bold", hjust = 0),
          plot.margin = margin(0.1,0.1,0.5,0.1, "cm"))+
    scale_x_continuous(breaks=seq(0,23,length.out=7), labels=seq(0,24,4)))
# run model
#trig_rand_slope <- mixed_model(fixed = cbind(success, failure) ~ 
                                 #cos(2 * pi * Time/24)  + sin(2 * pi * Time/24) +
                                 #cos(2 * pi * Time/12)  + sin(2 * pi * Time/12),
                               #random = ~  cos(2 * pi * Time/24)  + sin(2 * pi * Time/24) +
                                # cos(2 * pi * Time/12)  + sin(2 * pi * Time/12)  ||   Site,
                               #data = occasions_cbind, 
                               #family = binomial(), 
                               #iter_EM = 0) #TAKES REALLY LONG

summary(trig_rand_slope)

# HGAM ----
# run model
cycl_rand_int <- bam(cbind(success, failure) ~ s(Time, bs = "cc", k = 12) + 
                       s(Site, bs="re"), 
                     family = "binomial", 
                     data = occasions_cbind, 
                     knots = list(Time=c(0,23))
) 
summary(cycl_rand_int)
# build estimated activity curves
newdat <- with(occasions_cbind, 
               expand.grid(Time = seq(min(Time), max(Time), 1),
                           Site = "A1") #Site doesn't matter
) 
temp <- predict.bam(cycl_rand_int, newdata = newdat,  exclude = "s(Site)", se.fit = TRUE, type = "response") 
cycl_pred <- newdat  %>% 
  mutate(pred = temp$fit,
         low = pred - 1.96*temp$se.fit,
         upp = pred + 1.96*temp$se.fit,
         Mod = "Estimated: Random Intercept-only") %>% 
  select(-Site)

# combine true and estimated curves for visualization  purposes
cond_eff <- rbind(cond_true, cycl_pred)
# plot
(pl_cycl1 <- ggplot(cond_eff, aes(Time, pred)) +
    geom_ribbon(aes(ymin = low, ymax = upp, color = Mod, fill = Mod), alpha = 0.3, linewidth = 0.25) + 
    geom_line(aes(color = Mod), linewidth = 1) + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time of Day (Hour)", y = "Predicted Activity Pattern \n (probability)", 
         title = "Estimated vs Simulated Activity Patterns"
    )+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10,face="bold"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-10,-10,-10),
          plot.title = element_text(size=10,face="bold"),
          axis.line = element_line(colour = 'black', linetype = 'solid'),
          axis.ticks = element_line(colour = 'black', linetype = 'solid'),
          axis.title = element_text(size=9,face="bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth=0.5),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 9, colour = "black", face = "bold", hjust = 0),
          plot.margin = margin(0.1,0.1,0.5,0.1, "cm")
    )+
    scale_x_continuous(breaks=seq(0,23,length.out=7), labels=seq(0,24,4))
)

# Fit model with general smoother for Time
#cycl_rand_slope <- bam(cbind(success, failure) ~ 
 #                        s(Time, bs = "cc", k = 12) + # general smoother
  #                       s(Time, bs = "cc", k = 12, by = Site, m = 1) +
   #                      s(Site, bs="re"), 
    #                   knots = list(Time=c(0,23)),
     #                  family = "binomial", 
      #                 data = occasions_cbind) #TAKES REALLY long
summary(cycl_rand_slope)