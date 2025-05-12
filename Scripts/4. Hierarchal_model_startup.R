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

# 