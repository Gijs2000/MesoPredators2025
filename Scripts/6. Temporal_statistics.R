#' Temporal statistics
#' Start date: 22 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte


#loading libraries ----
{
  library(tidyverse)
  library(lubridate)
  library(overlap)
  library(activity)
}

################### REITDIEPMIDDEN ###################
#Seeing how much overlap there is between all species combinations ----
#'Fox and Marten
RM_Fox_Marten <- overlapEst(RM_Fox.r, RM_Marten.r, type="Dhat4")
RM_Fox_Marten_B <- bootstrap(RM_Fox.r, RM_Marten.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Fox_Marten_B)
bootCI(RM_Fox_Marten, RM_Fox_Marten_B)

#'Fox and Cat
RM_Fox_Cat <- overlapEst(RM_Fox.r, RM_Cat.r, type="Dhat4")
RM_Fox_Cat_B <- bootstrap(RM_Fox.r, RM_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Fox_Cat_B)
bootCI(RM_Fox_Cat, RM_Fox_Cat_B)

#'Fox and Polecat
RM_Fox_Polecat <- overlapEst(RM_Fox.r, RM_Polecat.r, type="Dhat4")
RM_Fox_Polecat_B <- bootstrap(RM_Fox.r, RM_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Fox_Polecat_B)
bootCI(RM_Fox_Cat, RM_Fox_Polecat_B)

#'Fox and Stoat
RM_Fox_Stoat <- overlapEst(RM_Fox.r, RM_Stoat.r, type="Dhat4")
RM_Fox_Stoat_B <- bootstrap(RM_Fox.r, RM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Fox_Stoat_B)
bootCI(RM_Fox_Stoat, RM_Fox_Stoat_B)

#'Marten and Cat
RM_Marten_Cat <- overlapEst(RM_Marten.r, RM_Cat.r, type="Dhat4")
RM_Marten_Cat_B <- bootstrap(RM_Marten.r, RM_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Marten_Cat_B)
bootCI(RM_Marten_Cat, RM_Marten_Cat_B)

#'Marten and Polecat
RM_Marten_Polecat <- overlapEst(RM_Marten.r, RM_Polecat.r, type="Dhat4")
RM_Marten_Polecat_B <- bootstrap(RM_Marten.r, RM_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Marten_Polecat_B)
bootCI(RM_Marten_Polecat, RM_Marten_Polecat_B)

#'Marten and Stoat
RM_Marten_Stoat <- overlapEst(RM_Marten.r, RM_Stoat.r, type="Dhat4")
RM_Marten_Stoat_B <- bootstrap(RM_Marten.r, RM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Marten_Stoat_B)
bootCI(RM_Marten_Stoat, RM_Marten_Stoat_B)

#'Cat and Polecat
RM_Cat_Polecat <- overlapEst(RM_Cat.r, RM_Polecat.r, type="Dhat4")
RM_Cat_Polecat_B <- bootstrap(RM_Cat.r, RM_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Cat_Polecat_B)
bootCI(RM_Cat_Polecat, RM_Cat_Polecat_B)

#'Cat and Stoat
RM_Cat_Stoat <- overlapEst(RM_Cat.r, RM_Stoat.r, type="Dhat4")
RM_Cat_Stoat_B <- bootstrap(RM_Cat.r, RM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Cat_Stoat_B)
bootCI(RM_Cat_Stoat, RM_Cat_Stoat_B)

#'Polecat and Stoat
RM_Polecat_Stoat <- overlapEst(RM_Polecat.r, RM_Stoat.r, type="Dhat4")
RM_Polecat_Stoat_B <- bootstrap(RM_Polecat.r, RM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(RM_Polecat_Stoat_B)
bootCI(RM_Polecat_Stoat, RM_Polecat_Stoat_B)



#Checking for differences in activity----
RM_Fox_Fit <- fitact(RM_Fox.r, sample="data", reps=1000)
RM_Marten_Fit <- fitact(RM_Marten.r, sample="data", reps=1000)
RM_Cat_Fit <- fitact(RM_Cat.r, sample="data", reps=1000)
RM_Polecat_Fit <- fitact(RM_Polecat.r, sample="data", reps=1000)
RM_Stoat_Fit <- fitact(RM_Stoat.r, sample="data", reps=1000)

compareAct(list(RM_Fox_Fit, RM_Marten_Fit, RM_Cat_Fit, SM_Polecat_Fit, RM_Stoat_Fit))
################### SOARREMOARRE ###################
#Seeing how much overlap there is between all species combinations ----
#'Marten and Cat
SM_Marten_Cat <- overlapEst(SM_Marten.r, SM_Cat.r, type="Dhat4")
SM_Marten_Cat_B <- bootstrap(SM_Marten.r, SM_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Marten_Cat_B)
bootCI(SM_Marten_Cat, SM_Marten_Cat_B)

#'Marten and Polecat
SM_Marten_Polecat <- overlapEst(SM_Marten.r, SM_Polecat.r, type="Dhat4")
SM_Marten_Polecat_B <- bootstrap(SM_Marten.r, SM_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Marten_Polecat_B)
bootCI(SM_Marten_Polecat, SM_Marten_Polecat_B)

#'Marten and Stoat
SM_Marten_Stoat <- overlapEst(SM_Marten.r, SM_Stoat.r, type="Dhat4")
SM_Marten_Stoat_B <- bootstrap(SM_Marten.r, SM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Marten_Stoat_B)
bootCI(SM_Marten_Stoat, SM_Marten_Stoat_B)

#'Cat and Polecat
SM_Cat_Polecat <- overlapEst(SM_Cat.r, SM_Polecat.r, type="Dhat4")
SM_Cat_Polecat_B <- bootstrap(SM_Cat.r, SM_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Cat_Polecat_B)
bootCI(SM_Cat_Polecat, SM_Cat_Polecat_B)

#'Cat and Stoat
SM_Cat_Stoat <- overlapEst(SM_Cat.r, SM_Stoat.r, type="Dhat4")
SM_Cat_Stoat_B <- bootstrap(SM_Cat.r, SM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Cat_Stoat_B)
bootCI(SM_Cat_Stoat, SM_Cat_Stoat_B)

#'Polecat and Stoat
SM_Polecat_Stoat <- overlapEst(SM_Polecat.r, SM_Stoat.r, type="Dhat4")
SM_Polecat_Stoat_B <- bootstrap(SM_Polecat.r, SM_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SM_Polecat_Stoat_B)
bootCI(SM_Polecat_Stoat, SM_Polecat_Stoat_B)

#Checking for differences in activity----
SM_Fox_Fit <- fitact(SM_Fox.r, sample="data", reps=1000)
SM_Marten_Fit <- fitact(SM_Marten.r, sample="data", reps=1000)
SM_Cat_Fit <- fitact(SM_Cat.r, sample="data", reps=1000)
SM_Polecat_Fit <- fitact(SM_Polecat.r, sample="data", reps=1000)
SM_Stoat_Fit <- fitact(SM_Stoat.r, sample="data", reps=1000)

compareAct(list(SM_Fox_Fit, SM_Marten_Fit, SM_Cat_Fit, SM_Polecat_Fit, SM_Stoat_Fit))
################### SOUTHWEST FRIESLAND ###################
#Seeing how much overlap there is between all species combinations ----
#'Fox and Marten
SW_Fox_Marten <- overlapEst(SW_Fox.r, SW_Marten.r, type="Dhat4")
SW_Fox_Marten_B <- bootstrap(SW_Fox.r, SW_Marten.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Fox_Marten_B)
bootCI(SW_Fox_Marten, SW_Fox_Marten_B)

#'Fox and Cat
SW_Fox_Cat <- overlapEst(SW_Fox.r, SW_Cat.r, type="Dhat4")
SW_Fox_Cat_B <- bootstrap(SW_Fox.r, SW_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Fox_Cat_B)
bootCI(SW_Fox_Cat, SW_Fox_Cat_B)

#'Fox and Polecat
SW_Fox_Polecat <- overlapEst(SW_Fox.r, SW_Polecat.r, type="Dhat4")
SW_Fox_Polecat_B <- bootstrap(SW_Fox.r, SW_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Fox_Polecat_B)
bootCI(SW_Fox_Cat, SW_Fox_Polecat_B)

#'Fox and Stoat
SW_Fox_Stoat <- overlapEst(SW_Fox.r, SW_Stoat.r, type="Dhat4")
SW_Fox_Stoat_B <- bootstrap(SW_Fox.r, SW_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Fox_Stoat_B)
bootCI(SW_Fox_Stoat, SW_Fox_Stoat_B)

#'Marten and Cat
SW_Marten_Cat <- overlapEst(SW_Marten.r, SW_Cat.r, type="Dhat4")
SW_Marten_Cat_B <- bootstrap(SW_Marten.r, SW_Cat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Marten_Cat_B)
bootCI(SW_Marten_Cat, SW_Marten_Cat_B)

#'Marten and Polecat
SW_Marten_Polecat <- overlapEst(SW_Marten.r, SW_Polecat.r, type="Dhat4")
SW_Marten_Polecat_B <- bootstrap(SW_Marten.r, SW_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Marten_Polecat_B)
bootCI(SW_Marten_Polecat, SW_Marten_Polecat_B)

#'Marten and Stoat
SW_Marten_Stoat <- overlapEst(SW_Marten.r, SW_Stoat.r, type="Dhat4")
SW_Marten_Stoat_B <- bootstrap(SW_Marten.r, SW_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Marten_Stoat_B)
bootCI(SW_Marten_Stoat, SW_Marten_Stoat_B)

#'Cat and Polecat
SW_Cat_Polecat <- overlapEst(SW_Cat.r, SW_Polecat.r, type="Dhat4")
SW_Cat_Polecat_B <- bootstrap(SW_Cat.r, SW_Polecat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Cat_Polecat_B)
bootCI(SW_Cat_Polecat, SW_Cat_Polecat_B)

#'Cat and Stoat
SW_Cat_Stoat <- overlapEst(SW_Cat.r, SW_Stoat.r, type="Dhat4")
SW_Cat_Stoat_B <- bootstrap(SW_Cat.r, SW_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Cat_Stoat_B)
bootCI(SW_Cat_Stoat, SW_Cat_Stoat_B)

#'Polecat and Stoat
SW_Polecat_Stoat <- overlapEst(SW_Polecat.r, SW_Stoat.r, type="Dhat4")
SW_Polecat_Stoat_B <- bootstrap(SW_Polecat.r, SW_Stoat.r, 1000, type="Dhat4") # takes a few seconds
mean(SW_Polecat_Stoat_B)
bootCI(SW_Polecat_Stoat, SW_Polecat_Stoat_B)

#Checking for differences in activity----
SW_Fox_Fit <- fitact(SW_Fox.r, sample="data", reps=1000)
SW_Marten_Fit <- fitact(SW_Marten.r, sample="data", reps=1000)
SW_Cat_Fit <- fitact(SW_Cat.r, sample="data", reps=1000)
SW_Polecat_Fit <- fitact(SW_Polecat.r, sample="data", reps=1000)
SW_Stoat_Fit <- fitact(SW_Stoat.r, sample="data", reps=1000)

compareAct(list(SW_Fox_Fit, SW_Marten_Fit, SW_Cat_Fit, SW_Polecat_Fit, SW_Stoat_Fit))

