#' --- 
#' title: Spatial Models Homework
#' author: Ian Rolfe
#' date: 2/27/2023 
#' output: html_document
#' ---
#' 

#'
#' ## Loading in the data and packages
#'

library(vegan)
library(nlme)
data(BCI)
## UTM Coordinates (in metres)
BCI_xy = data.frame(x = rep(seq(625754, 626654, by=100), each=5), 
                    y = rep(seq(1011569,  1011969, by=100), len=50))

#'
#' Quick overview of the data
#'
#' The Plots below show that there are significantly more common species than there are rare species
#'
#' Log transforming the data makes the distribution easier to visualize
#'

plot(BCI_xy)
cs = colSums(BCI)
hist(cs)
hist(log(cs))

col_brks <- hist(log(cs), plot=F)$breaks
col_indices <- as.numeric(cut(log(cs), col_brks))
cols <- rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col=cols[col_indices])

#'
#' ## Question 1 -- Is there evidence of spatial dependence in a rare and a common species in the BCI tree dataset?
#'

#'
#' Identifying species that I would consider rare and common for this analysis
#'  

  # Taking column sums for each species (species abundance; proxy for 'rareness') and summarizing that vector
summary(cs)

  # Use species with abundance of 7 for rare (25% quantile), and abundance of 82 for common (75% quantile)
sp_rare = "Tetrathylacium.johansenii"
sp_common = "Trichilia.pallida"

#'
#' Looking at the spatial dependence of this common and rare species
#'


  # Calculating Euclidean distances 
rare_dist = dist(BCI$Tetrathylacium.johansenii, method = "euclidean")
common_dist = dist(BCI$Trichilia.pallida, method = "euclidean")
xy_dist = dist(BCI_xy, method = "euclidean")

max_dist <- max(BCI_xy) / 2

  # Plotting these distances and linear models, then calculated correlations
plot(xy_dist, rare_dist)
abline(lm(rare_dist ~ xy_dist), lwd=3, col='red')
lines(lowess(xy_dist, rare_dist), lwd=3, col='pink')
abline(v = max_dist, col='red', lwd=3, lty=2)

rare_cor = cor(xy_dist, rare_dist)
rare_cor

plot(xy_dist, common_dist)
abline(lm(common_dist ~ xy_dist), lwd=3, col='red')
lines(lowess(xy_dist, common_dist), lwd=3, col='pink')
abline(v = max_dist, col='red', lwd=3, lty=2)

common_cor = cor(xy_dist, common_dist)
common_cor

  # Performing permutation tests to compare these correlations to a null distribution
rare_mantel = mantel(xy_dist, rare_dist)
rare_mantel

common_mantel = mantel(xy_dist, common_dist)
common_mantel

#'
#' From these figures, correlations, and permutation tests I would say that there is no evidence for spatial dependence in these rare and common species.
#' 
#' The figures both show small, negative relationships, and the correlations confirm that these are negative relationships with correlation coefficients less than 0.2 (-0.16 for rare, -0.09 for common).
#' 
#' To test the significance of those correlations, I ran permutation tests which resulted in p-vales of 1 (rare) and 0.936 (common).
#' 
#' So there is no evidence for spatial dependence in either the rare or common species I chose (T.johansenii, and T. pallida)
#'



#'
#' ## Question 2
#' 
#' General Least Squares to predict abundance of the species Drypetes standleyi using abundance of other tree species
#'

#' 
#' #### Model 1: Abundance of Picramnia latifolia as a predictor for Drypetes standleyi abundance
#' 

mod1_BCI = data.frame(BCI$Picramnia.latifolia, BCI$Drypetes.standleyi, BCI_xy)
  # Changing column names for simplicity (sp_a = P. latifolia, sp_b = D. standleyi)
colnames(mod1_BCI) = c("sp_a", "sp_b","x","y")

single_sp_mod = gls(sp_b ~ sp_a, data = mod1_BCI)
summary(single_sp_mod)

plot(sp_b ~ sp_a, data = mod1_BCI)
abline(gls(sp_b ~ sp_a, data = mod1_BCI), lwd=3, col='red')

#'
#' From the summary of this model, 
#' we can see that the abundance of D. standleyi does seem to correlate with P. latifolia abundance 
#' (p = 0.0007).
#'

#' 
#' Now we are going to use Variogram to examine the spatial dependence of the residuals.
#'

  # This plot shows a positive autocorrelation 
vario_mod1 = Variogram(single_sp_mod, form= ~ x + y, resType = 'response')
plot(vario_mod1)

res <- residuals(single_sp_mod)
res_var <- dist(res)^2 * 0.5
plot(dist(mod1_BCI[, c('x', 'y')]), res_var)
lines(lowess(dist(mod1_BCI[, c('x', 'y')]), res_var), col='red', lwd=2)
abline(v = max_dist, col='red', lwd=3, lty=2)

  # Testing AR1 -- Made the relationship worse (p = 0.5786), and doesn't fit model well
single_sp_exp <- update(single_sp_mod, corr=corExp(form=~x + y))
summary(single_sp_exp)
plot(Variogram(single_sp_exp, maxDist = max_dist))

  # Testing AR1 with nugget included -- Made relationship similar to the initial gls (p = 0.0007), but semivariogram line is flat and doesn't fit model well
single_sp_exp_nug <- update(single_sp_exp, corr=corExp(c(0.00000001, 0.00001),form=~x + y, nugget=T))
summary(single_sp_exp_nug)
plot(Variogram(single_sp_exp_nug, maxDist = max_dist))

  # Testing Rational Quadratic Error -- Made relationship worse (p = 0.5036), and doesn't fit the model well
single_sp_rat_nug<-update(single_sp_mod, corr=corRatio(form=~x + y, nugget=T))
summary(single_sp_rat_nug)
plot(Variogram(single_sp_rat_nug, maxDist = max_dist))

  # Comparing the models
anova(single_sp_mod, single_sp_exp, single_sp_exp_nug, single_sp_rat_nug)

#'
#' Based off of the p-values for each model, the best models were the gls, and the model with exponential error which accounted for the non-zero y-intercept (both had p-values of 0.0007).
#' 
#' When comparing the models with the anova() function, the models with the lowest AIC's were the exponential error and rational quadratic error with nugget included (308.9 and 306.6, respectively).
#' 
#' Including the spatial error terms did improve the fits of these models, but showed that there was no relationship between the abundance of P. latifolia and D. standleyi.
#'


#'
#' #### Model 2: Abundance of all species below as predictors for D. standleyi abundance
#'

sp_ids = c("Cordia.lasiocalyx", "Hirtella.triandra",
           "Picramnia.latifolia", "Quassia.amara",
           "Tabernaemontana.arborea", "Trattinnickia.aspera", 
           "Xylopia.macrantha")

mod2_BCI = data.frame(BCI$Cordia.lasiocalyx, BCI$Hirtella.triandra, BCI$Picramnia.latifolia, 
                      BCI$Quassia.amara, BCI$Tabernaemontana.arborea, BCI$Trattinnickia.aspera,
                      BCI$Xylopia.macrantha,
                      BCI$Drypetes.standleyi, BCI_xy)
colnames(mod2_BCI) = c("sp_c", "sp_d",
                       "sp_a", "sp_e",
                       "sp_f", "sp_g", 
                       "sp_h", "sp_b", "x", "y")

all_sp_mod = gls(sp_b ~ sp_a + sp_c + sp_d + sp_e + sp_f + sp_g + sp_h, data = mod2_BCI)
  # Signif in sp_c and sp_h
summary(all_sp_mod)

#'
#' Adding spatial error terms and comparing them to the original gls model
#'

  # This plot shows a relatively good fit of the model
vario_mod2 = Variogram(all_sp_mod, form= ~ x + y, resType = 'response')
plot(vario_mod2)

res <- residuals(all_sp_mod)
res_var <- dist(res)^2 * 0.5
plot(dist(mod2_BCI[, c('x', 'y')]), res_var)
lines(lowess(dist(mod2_BCI[, c('x', 'y')]), res_var), col='red', lwd=2)
abline(v = max_dist, col='red', lwd=3, lty=2)

  # Testing AR1 -- Signif in sp_g and sp_h, not a terrible fit graphically
all_sp_exp <- update(all_sp_mod, corr=corExp(form=~x + y))
summary(all_sp_exp)
plot(Variogram(all_sp_exp, maxDist = max_dist))

  # Testing AR1 with nugget included  -- Signif in sp_c and sp_h, flat line on graph and bad fit
all_sp_exp_nug <- update(all_sp_exp, corr=corExp(c(0.1, 0.3),form=~x + y, nugget=T))
summary(all_sp_exp_nug)
plot(Variogram(all_sp_exp_nug, maxDist = max_dist))

  # Testing Rational Quadratic Error -- Signif in sp_g and sp_h, bad fit at high distances
all_sp_rat_nug<-update(all_sp_mod, corr=corRatio(form=~x + y, nugget=T))
summary(all_sp_rat_nug)
plot(Variogram(all_sp_rat_nug, maxDist = max_dist))

  # Comparing the models
anova(all_sp_mod, all_sp_exp, all_sp_exp_nug, all_sp_rat_nug)

#'
#' Based off the p-values for each individual model, there were significant relationships between certain species and D. standleyi (including C. lasiocalyx, T. aspera, and X. macrantha).
#' 
#' When comparing the models with the anova() function, the models with the lowest AIC were the exponential error and rational quadratic error with nugget included (301.6 and 303.1, respectively).
#' 
#' The addition of spatial error terms did improve the fit of the model, and showed that species sp_g (T. aspera) and sp_h (X. macrantha) have significant impacts on the abundance of D. standleyi.
#'

#' 
#' I would say that adding the spatial error terms significantly impacted the fit of the model in the single species analysis, while in the model with all species it only slight impacted the model fit (decreasing the AIC by 10 units).
#' 
#' The single species model before adding the spatial error terms have a positive autocorrelation, and therefore the relationship was likely being impacted by the spatial relationship of the species.
#' 
#' We saw less of a shift in fit in the all species model because the original fit was quite good, and did not show signs of a positive autocorrelation.
#' 

