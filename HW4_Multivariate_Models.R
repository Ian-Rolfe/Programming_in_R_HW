#' --- 
#' title: Multivariate Models Homework
#' author: Ian Rolfe
#' date: 2/16/2023 
#' output: html_document
#' ---
#' 

#'
#' ## Loading in the data and packages
#' 

library(vegan)
  # Dataframe/Matrix containing # of observations of different species at 20 unique sites
data(dune)
  # Dataframe/Matrix containing environmental data for the 20 unique sites
data(dune.env)
?dune

#'
#' ## Question 1: Indirect Ordination using NMDS
#' 

# Making sure everything looks good -- We can see
sp_sum <- apply(dune, 2, sum)
site_sum <- apply(dune, 1, sum)
par(mfrow=c(2,2))
hist(sp_sum)
col <- colorRamp(c('red', 'orange', 'blue'))
plot(sp_sum[order(sp_sum, decreasing=T)], type='o', col='red', lwd=2,
     xlab='Sp Rank', ylab='Sum Cover')
hist(site_sum)
plot(site_sum[order(site_sum, decreasing=T)], type='o', col='red', lwd=2,
     xlab='Site Rank', ylab='Sum Cover')


# Steps: Run NMDS, add environmental data (in figure)
dune_mds = metaMDS(dune)

par(mfrow=c(1,1))
plot(dune_mds, type='n')
text(dune_mds, 'sp', cex=.5)
# generate vector of colors 
color_vect = rev(terrain.colors(6))[-1]
points(dune_mds, 'sites', pch=19, 
       col=color_vect[dune.env$Moisture])
legend('topright', paste("Moisture =", 1:5, sep=''), 
       col=color_vect, pch=19)

#'
#' The way to interpret this graph is to look at the spatial orientation of the different species.
#' It is important to remember that you should focus on the orientations as they pertain to the X and Y axes, as the X axis is explaining the most variation and the Y is explaining the second most.
#' 
#' The goal of creating these plots is to find groupings between extensive datasets, and to minimize the number of variables which explain variation between those groupings.
#' 
#' From this figure, you can see that certain species (e.g., Comapalu, Eoleopalu, Juncarti) are grouped closer to the sites with the highest moisture.
#' You can also see that some species (e.g., Anthodor, Vicilath, Planlanc) are grouped closest to sites with low moisture.
#' This analysis shows that some dune species require (or rather exist near) sites which have higher or lower moisture contents.
#'


#'
#' ## Question 2
#'

#'
#' #### Testing all constrained axes of the model (entire model)
#'

dune_cca <- cca(dune ~ ., data=dune.env)
RsquareAdj(dune_cca, 100)

anova(dune_cca, permutations=999)
anova(dune_cca, by="margin", permutations=999)

plot(dune_cca, type='n', scaling=1)
orditorp(dune_cca, display='sp', cex=0.5, scaling=1, col='blue')
text(dune_cca, display='bp', col='red')

#'
#' #### Testing only moisture and management
#' 
#' I wanted to test these variables because they look to be important in reference to the X axis in the above graph
#'

dune_cca_moist <- cca(dune ~ Moisture+Manure, data=dune.env)
RsquareAdj(dune_cca_moist, 100)

anova(dune_cca_moist, permutations=999)

plot(dune_cca_moist, type='n', scaling=1)
orditorp(dune_cca_moist, display='sp', cex=0.5, scaling=1, col='blue')
text(dune_cca_moist, display='bp', col='red')


#'
#' ## Question 3
#'
#' The analyses that I conducted agree with each other, and suggest a similar take home message.
#' That is that both Moisture and Manure play an important role in determining the types of species located at a particular site.
#' 
#' I personally find both to be useful, but I think it is important to refine your analysis after viewing the entire model.
#' 
#' When I was doing the initial exploratory analysis, I found that moisture played a role in determining presence or absence of certain species (this makes ecological sense).
#' By plotting the entire model, I was able to learn more about the impacts of certain explanatory factors, and then the final model was the most refined, with Manure and Moisture being the variables included.
#' 
#' It is important to use models which explain variation while still being simple, so refining to the last model I tested was an important step, and helped raise the Adjusted R-squared value.
#' 
#' 
#'



