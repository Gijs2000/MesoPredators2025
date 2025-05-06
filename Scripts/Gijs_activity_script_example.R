####### daily activity patterns ########

remove(list=ls()) # clear everything in memory
{
  library(tidyverse)
  library(lubridate)
  library(overlap)
  library(activity)
}

library("readxl")
library("lubridate")

### combined data of 2021_2022_2023

pred.full <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCFH54XPrO6JoV6RyOtOXEXiwBAjrery2YbXD_6MS57IqOI5BshBzn2rKQvmHZRA/pub?gid=975015585&single=true&output=csv")

str(pred.full)

#Calculate total minutes

pred.full$timemin <- ((pred.full$hour*60)+(pred.full$minute))

# calculate time in radians, normalized on a daily scale # let op hier wordt de conversie naar dag en omrekenen naar radians in 1x gedaan.

pred.full$timerad <- (2 * pi * pred.full$timemin) / 1439

pred.full$timerad

### make graphs 

library("overlap")

## radialen selecteren per soort, voor verder gebruik, plus density plots
Fox.r <- pred.full$timerad[pred.full$scientificName == 'Vulpes vulpes']
densityPlot(Fox.r, rug=TRUE, xcenter="midnight")

Badger.r <- pred.full$timerad[pred.full$scientificName == 'Meles meles']
densityPlot(Badger.r, rug=TRUE, xcenter="midnight")

Cat.r <- pred.full$timerad[pred.full$scientificName %in% c('Felis', 'Felis catus')]
densityPlot(Cat.r, rug=TRUE, xcenter="midnight")

Marten.r <- pred.full$timerad[pred.full$scientificName %in% c('Martes', 'Martes foina')]
densityPlot(Marten.r, rug=TRUE, xcenter="midnight")

Mustela.r <- pred.full$timerad[pred.full$scientificName == 'Mustela putorius']
densityPlot(Mustela.r, rug=TRUE, xcenter="midnight")

Rat.r <- pred.full$timerad[pred.full$scientificName %in% c('Rattus norvegicus', 'Rodentia')]
densityPlot(Rat.r, rug=TRUE, xcenter="midnight")

# Prettier plots:
densityPlot(Badger.r, extend=NULL, lwd=5, xcenter = "m")
densityPlot(Fox.r, add=TRUE, lwd=5, rug=TRUE, col='red', xcenter="m")
densityPlot(Mustela.r, add=TRUE, lwd=5, rug=TRUE, col='darkgreen', xcenter="m")
densityPlot(Marten.r, add=TRUE, lwd=5, rug=TRUE, col='blue', xcenter="m")
densityPlot(Cat.r, add=TRUE, lwd=5, rug=TRUE, col='orange', xcenter="m")
densityPlot(Rat.r, add=TRUE, lwd=5, rug=TRUE, col='purple', xcenter="m")

legend('topleft', c("Badger", "Fox","Polecat", "Marten", "Cat", "Rat"), lty=1, lwd=5, col=c('black', 'red', 'darkgreen', 'blue', 'orange', 'purple'), bg='white')
# Add vertical dotted lines to mark sunrise (say 05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.14,6.48, (18+38/60) - 24), lty=3)
abline(v=c(5.14,6.48, (21+50/60) - 24), lty=3)

#### signficance testing of overlap

##############################################################################################################
## overlap plots voor elke combinatie van dieren
## plus overlap estimates
## plus bootstrap en confidence interval
#############################################################################################################

## badger & fox
overlapPlot(Fox.r, Badger.r, xcenter = "midnight", 
            linetype = c(1, 1), linecol = c("red", "blue"), linewidth = c(2, 2))
legend('topright', c("Fox", "Badger"), lty=c(1,1), col=c("red", "blue"), bty='n')
foxbadest <- overlapEst(Fox.r, Badger.r, type="Dhat4")
foxbadest

foxbad.b <- bootstrap(Fox.r, Badger.r, 10000, type="Dhat4") # takes a few seconds
mean(foxbad.b)

bootCI(foxbadest, foxbad.b, conf=0.95)

## badger & polecat

overlapPlot(Mustela.r, Badger.r, xcenter = "midnight", 
            linetype = c(1, 1), linecol = c("red", "blue"), linewidth = c(2, 2))
legend('topright', c("Polecat", "Badger"), lty=c(1,1), col=c("red", "blue"), bty='n')
polebadest <- overlapEst(Mustela.r, Badger.r, type="Dhat4")
polebadest

polebad.b <- bootstrap(Mustela.r, Badger.r, 10000, type="Dhat4") # takes a few seconds
mean(polebad.b)

bootCI(polebadest, polebad.b, conf=0.95)

## badger & marten

overlapPlot(Badger.r, Marten.r, xcenter="midnight",
            linetype = c(1, 1), linecol = c("blue", "orange"), linewidth = c(2, 2))
legend('topright', c("Badger", "Marten"), lty=c(1,1), col=c("blue", "orange"), bty='n')
badmarest <- overlapEst(Badger.r, Marten.r, type="Dhat4")
badmarest

badmar.b <- bootstrap(Badger.r, Marten.r, 10000, type="Dhat4") # takes a few seconds
mean(badmar.b)

bootCI(badmarest, badmar.b)

## badger en cat
overlapPlot(Badger.r, Cat.r, xcenter="midnight",
            linetype = c(1, 1), linecol = c("blue", "dark green"), linewidth = c(2, 2))
legend('topright', c("Badger", "Cat"), lty=c(1,1), col=c("blue", "dark green"), bty='n')
badcatest <- overlapEst(Badger.r, Cat.r, type="Dhat4")
badcatest

badcat.b <- bootstrap(Badger.r, Cat.r, 10000, type="Dhat4") # takes a few seconds
mean(badcat.b)

bootCI(badcatest, badcat.b)

### badger & rat

overlapPlot(Badger.r, Rat.r, xcenter="midnight",
            linetype = c(1, 1), linecol = c("blue", "dark green"), linewidth = c(2, 2))
legend('topright', c("Badger", "Rat"), lty=c(1,1), col=c("blue", "dark green"), bty='n')
badratest <- overlapEst(Badger.r, Rat.r, type="Dhat4")
badratest

badrat.b <- bootstrap(Badger.r, Rat.r, 10000, type="Dhat4") # takes a few seconds
mean(badrat.b)

bootCI(badratest, badrat.b)

###################################################################################
## fox en cat
overlapPlot(Fox.r, Cat.r, xcenter="midnight", 
            linetype = c(1, 1), linecol = c("red", "dark green"),linewidth = c(2, 2))
legend('topright', c("Fox", "Cat"), lty=c(1,1), col=c("red", "dark green"), bty='n')
foxcatest <- overlapEst(Fox.r, Cat.r, type="Dhat4")
foxcatest

foxcat.b <- bootstrap(Fox.r, Cat.r, 10000, type="Dhat4") # takes a few seconds
mean(foxcat.b)

bootCI(foxcatest, foxcat.b)    

######################################################################################
## fox en marten
overlapPlot(Fox.r, Marten.r, xcenter="midnight",
            linetype = c(1, 1), linecol = c("red", "orange"),linewidth = c(2, 2))
legend('topright', c("Fox", "Marten"), lty=c(1,1), col=c("red", "orange"), bty='n')
foxmarest <- overlapEst(Fox.r, Marten.r, type="Dhat4")
foxmarest

foxmar.b <- bootstrap(Fox.r, Marten.r, 10000, type="Dhat4") # takes a few seconds
mean(foxmar.b)

bootCI(foxmarest, foxmar.b)

#####################################

## fox en polecat
overlapPlot(Fox.r, Mustela.r, xcenter="midnight",
            linetype = c(1, 1), linecol = c("red", "orange"),linewidth = c(2, 2))
legend('topright', c("Fox", "Polecat"), lty=c(1,1), col=c("red", "orange"), bty='n')
foxmusest <- overlapEst(Fox.r, Mustela.r, type="Dhat4")
foxmusest

foxmus.b <- bootstrap(Fox.r, Mustela.r, 10000, type="Dhat4") # takes a few seconds
mean(foxmus.b)

bootCI(foxmusest, foxmus.b)

######## fox & rat

overlapPlot(Fox.r, Rat.r, xcenter="midnight", 
            linetype = c(1, 1), linecol = c("red", "dark green"),linewidth = c(2, 2))
legend('topright', c("Fox", "Rat"), lty=c(1,1), col=c("red", "dark green"), bty='n')
foxratest <- overlapEst(Fox.r, Rat.r, type="Dhat4")
foxratest

foxrat.b <- bootstrap(Fox.r, Rat.r, 10000, type="Dhat4") # takes a few seconds
mean(foxrat.b)

bootCI(foxratest, foxrat.b)    

######### polecat & marten
overlapPlot(Mustela.r, Marten.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Polecat", "Marten"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
polecatmarest <- overlapEst(Mustela.r, Marten.r, type="Dhat4")
polecatmarest

polecatmar.b <- bootstrap(Mustela.r, Marten.r, 10000, type="Dhat4") # takes a few seconds
mean(polecatmar.b)

bootCI(polecatmarest, polecatmar.b)

## polecat en cat
overlapPlot(Cat.r, Mustela.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Cat", "Polecat"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
catpoleest <- overlapEst(Cat.r, Mustela.r, type="Dhat4")
catpoleest

catpole.b <- bootstrap(Cat.r, Mustela.r, 10000, type="Dhat4") # takes a few seconds
mean(catpole.b)

bootCI(catpoleest, catpole.b)

## polecat en rat
overlapPlot(Rat.r, Mustela.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Rat", "Polecat"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
ratpoleest <- overlapEst(Rat.r, Mustela.r, type="Dhat4")
ratpoleest

ratpole.b <- bootstrap(Rat.r, Mustela.r, 10000, type="Dhat4") # takes a few seconds
mean(ratpole.b)

bootCI(ratpoleest, ratpole.b)

##################################################################################
## cat en marten
overlapPlot(Cat.r, Marten.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Cat", "Marten"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
catmarest <- overlapEst(Cat.r, Marten.r, type="Dhat4")
catmarest

catmar.b <- bootstrap(Cat.r, Marten.r, 10000, type="Dhat4") # takes a few seconds
mean(catmar.b)

bootCI(catmarest, catmar.b)

## rat en marten
overlapPlot(Rat.r, Marten.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Rat", "Marten"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
ratmarest <- overlapEst(Rat.r, Marten.r, type="Dhat4")
ratmarest

ratmar.b <- bootstrap(Rat.r, Marten.r, 10000, type="Dhat4") # takes a few seconds
mean(ratmar.b)

bootCI(ratmarest, ratmar.b)

#### cat & rat

overlapPlot(Cat.r, Rat.r, xcenter= "midnight",
            linetype = c(1, 1), linecol = c("dark green", "orange"), linewidth = c(2, 2))
legend('topright', c("Cat", "Rat"), lty=c(1,1), col=c("dark green", "orange"), bty='n')
catratest <- overlapEst(Cat.r, Rat.r, type="Dhat4")
catratest

catrat.b <- bootstrap(Cat.r, Rat.r, 10000, type="Dhat4") # takes a few seconds
mean(catrat.b)

bootCI(catratest, catrat.b)

####### Compare activity level estimates
# Uses a Wald test to ask whether the difference between estimates a1 and a2 is significantly different
# from 0: statistic W = (a1-a2)^2 / (SE1^2+SE2^2) tested on chi-sq distribution with 1 degree of
# freedom.



Fox.f <- fitact(Fox.r, sample="data", reps=10)
Badger.f <- fitact(Badger.r, sample="data", reps=10)
Mustela.f <- fitact(Mustela.r, sample="data", reps=1000)
Marten.f<- fitact(Marten.r, sample="data", reps=1000)
Cat.f<- fitact(Cat.r, sample="data", reps=1000)
Rat.f<- fitact(Rat.r, sample="data", reps=1000)

Fox.f@act
Badger.f@act
Mustela.f@act
Marten.f@act
Cat.f@act
Rat.f@act

compareAct(list(Fox.f,Badger.f, Mustela.f, Marten.f, Cat.f, Rat.f))

### I should probably do more reps

### get significance estimates of overlap

compareCkern(Fox.f,Badger.f,reps=10)
compareCkern(Badger.f,Mustela.f,reps=10)
compareCkern(Badger.f,Cat.f,reps=10)
compareCkern(Badger.f,Rat.f,reps=10)
compareCkern(Fox.f,Mustela.f,reps=10)
compareCkern(Fox.f,Marten.f,reps=10)
compareCkern(Fox.f,Cat.f,reps=10)
compareCkern(Fox.f,Rat.f,reps=10)
compareCkern(Mustela.f,Marten.f,reps=10)
compareCkern(Mustela.f,Cat.f,reps=10)
compareCkern(Mustela.f,Rat.f,reps=10)
compareCkern(Marten.f,Cat.f,reps=10)
compareCkern(Marten.f,Rat.f,reps=10)
compareCkern(Cat.f,Rat.f,reps=10)

### all p=0.0000
## This randomised distribution is then used to define an empirical probability distribution against
## which the probability that the observed overlap arose by chance is judged.

### compare times when species active
## cannot really compare species directly

plot(Fox.f)
compareTimes(Fox.f, c(5.5,6,0.5,1))
compareTimes(Badger.f, c(5.5,6,0.5,1))
plot(Mustela.f)
compareTimes(Mustela.f, c(5.5,6,0.5,1))

### whatson-wheeler test

library(circular)

### example

x1 <- circular(c(35, 45, 50, 55, 60, 70, 85, 95, 105, 120),
               units="degrees", template="geographics")
x2 <- circular(c(75, 80, 90, 100, 110, 130, 135, 140, 150, 160, 165),
               units="degrees", template="geographics")

watson.wheeler.test(list(x1, x2))

#### seems to work with the radians distributions
### but get warning messages

watson.wheeler.test(list(Fox.r, Badger.r))
watson.wheeler.test(list(Fox.r, Mustela.r)) #NS
watson.wheeler.test(list(Fox.r, Marten.r)) #NS
watson.wheeler.test(list(Fox.r, Cat.r))
watson.wheeler.test(list(Fox.r, Rat.r))
watson.wheeler.test(list(Badger.r, Mustela.r))
watson.wheeler.test(list(Badger.r, Marten.r))
watson.wheeler.test(list(Badger.r, Cat.r))
watson.wheeler.test(list(Badger.r, Rat.r))
watson.wheeler.test(list(Marten.r, Mustela.r)) #NS
watson.wheeler.test(list(Rat.r, Mustela.r)) #NS
watson.wheeler.test(list(Cat.r, Mustela.r))
watson.wheeler.test(list(Cat.r, Marten.r))
watson.wheeler.test(list(Rat.r, Marten.r)) #NS
watson.wheeler.test(list(Cat.r, Rat.r))
