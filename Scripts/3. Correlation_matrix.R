#' Animal correlation plots
#' Start date: 7 may 2025
#' Author: Gijs van Male
#' Supervisors: Chris Smit, Rienk Fokkema, Pieter Otte

# Loading libraries ----
{
  library(overlap)
  library(corrplot)
}

#Function for correlation significance testing ----
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

# Correlation matrix SM ----
SM_cor_df <- SM_location |>
  dplyr::select(fraction_Felis_catus, 
                fraction_Martes_foina,
                fraction_Mustela_putorius,
                fraction_Vulpes_vulpes,
                fraction_Mustela_erminea)

res_SM <- cor(SM_cor_df)
colnames(res_SM) <- rownames(res_SM) <- species_labels

p.mat_SM <- cor.mtest(SM_cor_df, method = "kendall")
colnames(p.mat_SM) <- rownames(p.mat_SM) <- species_labels
res_SM <- res_SM[!rownames(res_SM) %in% "Red fox", !colnames(res_SM) %in% "Red fox"]

png("Figures/3.Correlation_matrix_SM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(res_SM, 
         type = "upper", 
         order = "alphabet", 
         p.mat = p.mat_SM, 
         sig.level = 0.05,
         tl.col = "black",
         tl.cex = 2.0,
         title = "Correlation Between Species in the Soarremoarre area (2023)",
         mar = c(0, 0, 2, 0),
         cex.main = 2,
         cl.cex =  2)
dev.off()

# Correlation matrix RM ----
RM_cor_df <- RM_location |>
  dplyr::select(fraction_Felis_catus, 
                fraction_Martes_foina,
                fraction_Mustela_putorius,
                fraction_Vulpes_vulpes,
                fraction_Mustela_erminea)

res_RM <- cor(RM_cor_df)
colnames(res_RM) <- rownames(res_RM) <- species_labels

p.mat_RM <- cor.mtest(RM_cor_df, method = "kendall")
colnames(p.mat_RM) <- rownames(p.mat_RM) <- species_labels

res_RM <- res_RM[!rownames(res_RM) %in% "Stoat", !colnames(res_RM) %in% "Stoat"]
png("Figures/3.Correlation_matrix_RM.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(res_RM, 
         type = "upper", 
         order = "alphabet", 
         p.mat = p.mat_RM, 
         sig.level = 0.05,
         tl.col = "black",
         tl.cex = 2,
         title = "Correlation Between Species in the Reitdiep midden area (2023)",
         mar = c(0, 0, 2, 0),
         cex.main = 2,
         cl.cex =  2)
dev.off()

# Correlation matrix SW ----
SW_cor_df <- SW_location |>
  dplyr::select(fraction_Felis_catus, 
                fraction_Martes_foina,
                fraction_Mustela_putorius,
                fraction_Vulpes_vulpes,
                fraction_Mustela_erminea)

res_SW <- cor(SW_cor_df)
colnames(res_SW) <- rownames(res_SW) <- species_labels

p.mat_SW <- cor.mtest(SW_cor_df, method = "kendall")
colnames(p.mat_SW) <- rownames(p.mat_SW) <- species_labels

#png("Figures/3.Correlation_matrix_SW.png", width = 1920, height = 1080) #TURN ON WHEN SAVING
corrplot(res_SW, 
         type = "upper", 
         order = "alphabet", 
         p.mat = p.mat_RM, 
         sig.level = 0.05,
         tl.col = "black",
         tl.cex = 0.9,
         title = "Correlation Between Species in the South-West area (2023)",
         mar = c(0, 0, 2, 0))
dev.off()
