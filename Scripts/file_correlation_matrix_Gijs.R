### correlation  matrix of predator species occurrence for summed dataset of all years

# R_file_6

remove(list=ls()) # clear everything in memory
library("readxl")

#set workdrive. update accordingly to name of your folder location
setwd("C:/data/food webs article 2024 V3")

total_data2 <- read_excel("data_predator_sum_21_22_23_updated.xlsx")
str(total_data2)
total_data<-subset(total_data2, (locationName!= "ZWF 031"))
total_data
str(total_data)

my_data <- total_data[, c(11,12,13,14,15,16)]
my_data

library(tidyverse)

a<-my_data %>% 
  rename(
    Red_fox= fractie_vos_pdag,
    Polecat= fractie_bunzing_pdag,
    Badger = fractie_das_pdag,
    Beech_marten = fractie_marter_pdag,
    Domestic_cat=fractie_kat_pdag,
    Brown_rat=fractie_rat_pdag
  )

a

res <- cor(a)
round(res, 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(res, method="number")

### more sophisticated
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(a, method = "kendall")
warnings()

### warnings about ties stay no matter what test I do

p.mat
res

corrplot(res, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05)

## alternative
library("PerformanceAnalytics")
chart.Correlation(my_data, histogram=TRUE, pch=19)