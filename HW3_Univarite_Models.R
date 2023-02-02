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
trees <- read.csv('./data/treedata_subset.csv')

# Importing Car to get the Anova() function which allows for comparison of continuous
  # and discrete explanatory variables
# Will estimate partial effect sizes, variance explained, and p-values for each explanatory 
  # variable included in the model
library(car)


# Helpful function that shows the column names, their class, 
  # and a couple of the values from the column
str(trees)


#'
#' ## Question 1:
#' 
#' To start, I wanted to visualize the data to look for obvious relationships between the cover and the other explanatory variables



# First restructuring the data to better ask questions about generalists (Acer r.)
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



# Visualizing the data

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



# How disturbance impacts cover
boxplot(cover ~ disturb, data = abies)
  # Not a great graph due to large amount of 0 in the cover in abies
boxplot(cover ~ disturb, data = acer)
  # Better graph, but doesn't like like disturbance significantly affects cover in acer



#' Now I know that elevation should have a large effect, I need to make some models
#' 

abies_model = lm(cover ~ ., data = abies)
summary(abies_model)
  # Doesn't do a great job of showing us how disturbance affects cover, because it seperates out the individual disturbance levels
Anova(abies_model, type = 3)
  # Shows us that elevation, streamdist, and disturbance impact cover.
    # We want to use the Anova() function instead of anova(), because the Type 3 ANOVA looks at these variable independently rather than in a sequential order
    # which gives us a better idea of what variables actually have affects, rather then which variables have effects after others (which is what happens in the sequential test)

acer_model = lm(cover ~ ., data = acer)
summary(acer_model)
Anova(acer_model, type = 3)
  # Shows us that elevation, tci, and beers have an impact on cover

#' I have some models that I think will work, now I need to run diagnostics to make sure they are fitting the assumptions of least squares regression
#' 

plot(abies_model)
  # Model does not meet the assumptions of equal variance of residuals (homoscedasticity), normal distribution
plot(acer_model)
  # This model looks better but still wouldn't feel confident in homoscedasticity, or normal distribution

#' And it is clear that the lack of cover at the low elevation in Abies f. is affecting the residuals, so we might want to try a different model than linear
#' 

#' ## Question 2
#' Because we know that the data for Abies f. and Acer r. doesn't work with a linear model, we want to look at different relationships
#' 
#' The Poisson distribution might be a good fit, so let's try that



