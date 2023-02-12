#' --- 
#' title: Univariate Models Homework
#' author: Ian Rolfe
#' date: 1/31/2023 
#' output: html_document
#' ---
#' 

#'
#' ## Loading in the data and packages
#' 

# read in directly from website: 
trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')
# or download and import locally
#trees <- read.csv('./treedata_subset.csv')

## Importing Car to get the Anova() function which allows for comparison of continuous
  # and discrete explanatory variables
## Will estimate partial effect sizes, variance explained, and p-values for each explanatory 
  # variable included in the model
library(car)


# #Helpful function that shows the column names, their class, 
  # and a couple of the values from the column
str(trees)


#'
#' ## Question 1:
#' 
#' To start, I wanted to visualize the data to look for obvious relationships between the cover and the other explanatory variables
#' 



## First restructuring the data to better ask questions about generalists (Acer r.)
  # and specialists (Abies f. -- Frasier Fir)

# we wish to model species cover across all sampled plots
# create site x sp matrix for two species 
sp_cov = with(trees, tapply(cover, list(plotID, spcode), 
                            function(x) round(mean(x))))
sp_cov = ifelse(is.na(sp_cov), 0, sp_cov)
sp_cov = data.frame(plotID = row.names(sp_cov), sp_cov)
# create environmental matrix
cols_to_select = c('elev', 'tci', 'streamdist', 'disturb', 'beers')
env = aggregate(trees[ , cols_to_select], by = list(trees$plotID), 
                function(x) x[1])
names(env)[1] = 'plotID'
# merge species and enviornmental matrices
site_dat = merge(sp_cov, env, by='plotID')
# subset species of interest
abies = site_dat[ , c('ABIEFRA', cols_to_select)]
acer  = site_dat[ , c('ACERRUB', cols_to_select)]
names(abies)[1] = 'cover'
names(acer)[1] = 'cover'



## Visualizing the data

# New way to visualize by adding smoothing lines and correlation coeff to pairs()
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ cover + elev + tci + streamdist + beers, data = abies, lower.panel = panel.cor, upper.panel = panel.smooth)
  # Elevation has the highest correlation (0.45), and the next is stream dist (0.22)
pairs(~ cover + elev + tci + streamdist + beers, data = acer, lower.panel = panel.cor, upper.panel = panel.smooth)
  # Elevation has the highest correlation (0.39), and the next is beers (0.14)


## How disturbance impacts cover

boxplot(cover ~ disturb, data = abies)
  # Not a great graph due to large amount of 0 in the cover in abies
boxplot(cover ~ disturb, data = acer)
  # Better graph, but doesn't like like disturbance significantly affects cover in acer

## Looking for interaction between elevation and disturbance

# Abies f. -- Lines not parallel, likely an interaction
plot(cover ~ elev, data = abies, type = 'n')
points(cover ~ elev, data = abies, subset = disturb == "CORPLOG",
       pch = 1, col = 'red')
lines(lowess(abies$elev[abies$disturb == 'CORPLOG'],
             abies$cover[abies$disturb == 'CORPLOG']),
      lty = 1, col = 'red')
points(cover ~ elev, data = abies, subset = disturb == "VIRGIN",
       pch = 2, col = 'blue')
lines(lowess(abies$elev[abies$disturb == 'VIRGIN'],
             abies$cover[abies$disturb == 'VIRGIN']),
      lty = 2, col = 'blue')
points(cover ~ elev, data = abies, subset = disturb == "LT-SEL",
       pch = 1, col = 'green')
lines(lowess(abies$elev[abies$disturb == 'LT-SEL'],
             abies$cover[abies$disturb == 'LT-SEL']),
      lty = 1, col = 'green')
legend('topleft', c('CORPLOG', 'VIRGIN', 'LT-SEL'), col = c('red', 'blue', 'green'), 
       pch = c(1, 2, 1), lty = c(1, 2, 1), bty = 'n')

# Acer r. -- Lines not parallel, likely an interaction
plot(cover ~ elev, data = acer, type = 'n')
points(cover ~ elev, data = acer, subset = disturb == "CORPLOG",
       pch = 1, col = 'red')
lines(lowess(acer$elev[acer$disturb == 'CORPLOG'],
             acer$cover[acer$disturb == 'CORPLOG']),
      lty = 1, col = 'red')
points(cover ~ elev, data = acer, subset = disturb == "VIRGIN",
       pch = 2, col = 'blue')
lines(lowess(acer$elev[acer$disturb == 'VIRGIN'],
             acer$cover[acer$disturb == 'VIRGIN']),
      lty = 2, col = 'blue')
points(cover ~ elev, data = acer, subset = disturb == "LT-SEL",
       pch = 1, col = 'green')
lines(lowess(acer$elev[acer$disturb == 'LT-SEL'],
             acer$cover[acer$disturb == 'LT-SEL']),
      lty = 1, col = 'green')
legend('topleft', c('CORPLOG', 'VIRGIN', 'LT-SEL'), col = c('red', 'blue', 'green'), 
       pch = c(1, 2, 1), lty = c(1, 2, 1), bty = 'n')


#' 
#' Now I know that elevation should have a large effect on both Abies and Acer, I need to make some models
#' 


abies_model = lm(cover ~ ., data = abies)
abies_mod_esd = lm(cover ~ elev + streamdist + disturb, data = abies)
abies_mod_es = lm(cover ~ elev + streamdist, data = abies)
abies_mod_e = lm(cover ~ elev, data = abies)
abies_int = lm(cover ~ elev*streamdist*disturb, data = abies)
abies_int_2 = lm(cover ~ elev*streamdist, data = abies)

AIC(abies_model) # AIC = 1970, good model, but essentially the same as abies_mod_esd
AIC(abies_mod_esd) # AIC = 1971, good model; about the same as abies_model
AIC(abies_mod_es) # AIC = 2000, worse model
AIC(abies_mod_e) # AIC = 2002, worse model
AIC(abies_int) # AIC = 1262 BEST MODEL
AIC(abies_int_2) # AIC = 1880, worse model 

summary(abies_model)
summary(abies_int)
  # Adjusted R^2 = 0.7138; Doesn't do a great job of showing us how disturbance affects cover, because it separates out the individual disturbance levels

Anova(abies_int, type = 3)
  # Shows us that the interaction between streamdist and disturbance, and the interaction between 
    # streamdist, disturbance, and elevation significantly impact cover in Abies 
  #We want to use the Anova() function instead of anova(), because the Type 3 ANOVA looks at these variable independently rather than in a sequential order
    # which gives us a better idea of what variables actually have affects, rather then which variables have effects after others (which is what happens in the sequential test)


acer_model = lm(cover ~ ., data = acer)
acer_mod_etb = lm(cover ~ elev + tci + beers, data = acer)
acer_mod_et = lm(cover ~ elev + tci, data = acer)
acer_mod_e = lm(cover ~ elev, data = acer)
acer_int = lm(cover ~ elev*tci*beers, data = acer)
acer_int_2 = lm(cover ~ elev*tci, data = acer)


AIC(acer_model) # AIC = 3438
AIC(acer_mod_etb) # AIC = 3439; essentially the same as acer_model, but more simple, so use this
AIC(acer_mod_et) # AIC = 3445, worse model
AIC(acer_mod_e) # AIC = 3453, worse model
AIC(acer_int) # AIC = 3429, BEST MODEL
AIC(acer_int_2) # AIC = 3440

summary(acer_int)
  # Adjusted R^2 = 0.1829; model does not fit the data well

Anova(acer_model, type = 3)
Anova(acer_int, type = 3)
  # Shows us that elevation, tci, and beers have an impact on cover


#' 
#' I have some models that I think will work, now I need to run diagnostics to make sure they are fitting the assumptions of least squares regression
#' 


plot(abies_int)
  # Model does not meet the assumptions of equal variance of residuals (homoscedasticity), normal distribution
plot(acer_int)
  # This model looks better but still wouldn't feel confident in homoscedasticity



#' 
#' ### What we've learned from these models:
#' 
#'  **Abies fraseri**
#'  
#'  The exploratory model seems to show that elevation, distance from a stream, and disturbance are important variable impacting the cover of Abies fraseri.
#'  
#'  The most important variable explaining cover seems to be the interactions between the variables above
#'  
#'  Model diagnostics indicate that the "abies_int" model does violate the assumptions of a normal distribution and homoscedasticity
#'
#'  **Acer rubrum**
#'  
#'  The exploratory model shows that elevation, topographic convergence index (water potential), and the transformed slope aspect (heat load index) are important variables impacting cover of Acer rubrum
#'  
#'  The most important variable explaining cover seems to be the interaction between water potential and heat load index
#'  
#'  Model diagnostics indicate that the "acer_int" model does violate the assumptions of a normal distribution and homoscedasticity
#'  
#'  **Based on the Adjuested R-squared values from the models, we are able to explain the variance in cover in Abies f. better than we are in Acer r.**
#'  
#'  **This is likely because Abies f. is a specialist, i.e., requires a more specialized set of environmental conditions, while Acer r. is a generalist and can have a broader range/cover.**
#'  
#'  
#'  And it is clear that the lack of cover at the low elevation in Abies f., and lack of cover at high elevations in Acer r.,  is affecting the residuals, so we might want to try a different model than linear
#' 
 

#'  
#' ## Question 2
#' Because we know that the data for Abies f. and Acer r. doesn't work with a linear model, we want to look at different relationships
#' 
#' The Poisson distribution might be a good fit, because our 'cover' variable is integers from 0-10, so let's try that
#' 

## R-squared is not calculated in GLM, so here is a function to calculate a "Pseudo R-squared" value.
pseudo_r2 = function(glm_mod) {
  1 -  glm_mod$deviance / glm_mod$null.deviance
}

  # Using Poisson distribution with a Generalized Linear Model on Abies f. 
    # (variables included are based on Anova()'s that I ran above)

abies_glm = glm(cover ~ elev * streamdist * disturb, data = abies, family = 'poisson')
summary(abies_glm) # AIC = 202
pseudo_r2(abies_glm) # R-squared = 0.914, better fit than the linear model
plot(abies_glm)
  # Still not meeting the assumptions of homoscedasticity


  # Using Poisson distribution with a Generalized Linear Model on Acer r.
    # (variables included are based on Anova()'s that I ran above)

acer_glm = glm(cover ~ elev * tci * beers, data = acer, family = 'poisson')
acer_glm_et = glm(cover ~ elev * tci, data = acer, family = 'poisson')
acer_glm_noint = glm(cover ~ elev + tci + beers, data = acer, family = 'poisson')

summary(acer_glm) # AIC = 3625.9

pseudo_r2(acer_glm) # R-squared = 0.148, worse fit than the linear model
pseudo_r2(acer_glm_et) # R-squared = 0.130, even worse fit then previous model
pseudo_r2(acer_glm_noint) # R-squared = 0.127

plot (acer_glm)
  # Still not meeting the assumptions of homoscedasticity, and LOTS of leverage from a few points

#' 
#' Based on my qualitative assessment of the variables with comparisons to both linear and poisson models, I was correct in assuming that elevation was an important factor in both Abies f. and Acer r.
#' 
#' Though realistically elevation itself was not as important than its interactions with other variables (such as with disturbance and stream distance in Abies f. and with water potential and heat load index in Acer r.
#' 
#' Changing the error distribution from linear to Poisson did help in explaining the variance in Abies f. but not in Acer r. 
#' 
#' In Abies f. the explanation of variance actually increased (from ~ 70% to ~90%), while in Acer r. it decreased from ~18% to ~14%
#' 
#' 



#' 
#' ## Question 3
#' 
#' What we learned about the distribution, or more specifically the cover, of these two species was that their ecological characteristics of being a specialist or generalist plays an important role.
#' 
#' We were best able to explain the distribution of Abies fraseri due to it being a specialist and requiring certain environmental conditions in order to grow.
#' From the data, we might reasonably assume that the distribution Abies f. can be explained by its elevation and location relative to a water source.
#' Its distribution is also clearly impacted by human disturbance, with Abies f. having higher cover in regions without human interaction.
#' 
#' Due to Acer rubrum being a habitat generalist, it is more difficult to identify specific factors affecting its distribution.
#' Initially, I believed that the distribution of Acer r. could be explained in part by elevation, which is true to the point that it cannot grow at high elevations,
#' but it is clear that other factors such as water potential and heat load index play a small role in determining their distribution when interacting with elevation.
#' Overall it is hard to predict the distribution of Acer r. because of their generalist ecology, which makes them more tolerant to a slew of environmental conditions.
#' 
 


#' 
#' ## Question 4 (Optional)
#' 
#' Using StepAIC() to test which model has the lowest AIC
#' 

library(MASS)

# Found this to be the best model
stepAIC(abies_int)

# Found (cover ~ elev + tci + beers + elev:tci + elev:beers) to be the best model
  # Most important interactions were between elevation and tci, and elevation and beers
stepAIC(acer_int)

# Found (cover ~ elev + streamdist + disturb + elev:disturb + streamdist:disturb) to be the best model
  # Most important interactions were elev:disturb and streamdist:disturb
stepAIC(abies_glm)

# Found (cover ~ elev + tci + beers + elev:tci + elev:beers) to be the best model
  # similar to linear model, the most important interactions were elev:tci and elev:beers
stepAIC(acer_glm)

#'
#' ## Question 5 (Optional)
#' 
#' I will have to try this one later
#'

