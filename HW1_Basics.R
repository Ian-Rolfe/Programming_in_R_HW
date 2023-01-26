#' --- 
#' title: R Basics Homework
#' author: Ian Rolfe
#' date: 1/17/2023 
#' output: html_document
#' ---
#' 
#' ## Reading in the Data


tgpp <- read.csv("./tgpp.csv", header = TRUE)

tgpp <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/tgpp.csv', header = T)

#' ## Vieiwng Data

head(tgpp)
summary(tgpp)

#' ## 1. What are the names of the columns in this dataset?
#' 
#' *The column names are "plot", "year", "record_id", "corner", "scale", "richness", "easting", "northing", "slope", "ph", "yrsslb"*
#' *We know this from "names(tgpp)*

names(tgpp)

#' ## 2. How many rows and columns does this data file have?
#' 
#' *This file has 4080 rows and 11 columns*
#' *We know this from dim(tgpp)*

dim(tgpp)

#' ## 3. What kind of object is each data column?
#' 
#' *Using sapply() to apply the class() function to all columns in "tgpp". This finds the class of each object in "tgpp"*
#' 

sapply(tgpp, class)

#' ## 4. What are the values of the datafile for the rows 1,5,8 at columns 3,7,10?
#' 
#' *We'll use the brackets[] to find these values below*
#' *rows always are input before columns, so we will use the code tgpp[c(1,5,8), c(3,7,10)]*
#' *Creates a 3x3 matrix of data*

tgpp[c(1,5,8), c(3,7,10)]

#' ## 5. Create a pdf of the relationship between "scale" and "richness".
#' 
#' *We'll create this by using the plot function, with "scale" as our X and "richness" as our Y*
#' *Then we'll add nice colors using "col = "blue""*

#' pdf(./R_Homework_fig.pdf) to save the figure as a pdf file
plot(richness ~ scale, data = tgpp, xlab = "Scale (m^2)", ylab = "Richness", col = "blue")

#' ## 6. What happens to your plot when you set the plot argument log equal to 'xy'?
#' 
#' *By setting the plot argument log = 'xy', you are telling R to have both the x and y axis be logarithmic*

plot(richness ~ scale, data = tgpp, xlab = "Scale (m^2)", ylab = "Richness", col = "blue", log = "xy")
