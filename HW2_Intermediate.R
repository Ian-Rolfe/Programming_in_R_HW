#' --- 
#' title: R Intermediate Homework
#' author: Ian Rolfe
#' date: 1/19/2023 
#' output: html_document
#' ---
#' 

#' ## Loading in the data
data(iris)
head(iris)

#' ## Questions 1 and 2
#'  Annotated code -- This code chunk utilizes for loops to calculate the average of the columns for each given species


# Creates species list within the iris dataset
sp_ids <- unique(iris$Species)

# Creating Output as a table, and filling the table with species list and columns from iris
output <- matrix(NA, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(output) <- sp_ids
colnames(output) <- names(iris[ , -ncol(iris)])

# Looping through the species
for(i in seq_along(sp_ids)) {
  # Subsetting the iris dataset for the species 'i', and removing the species column
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  # Looping through the generated iris_sp data.frame
  for(j in 1:(ncol(iris_sp))) {
    # Creating 'x' and 'y' variables to hold values later
    x <- 0
    y <- 0
    # Deciding to perform a function, IF the number of rows is greater than 0
    if (nrow(iris_sp) > 0) {
      # Looping through the rows in iris_sp
      for(k in 1:nrow(iris_sp)) {
        # Adding the values of the generated 'x' variable with the value found in subsequent rows
        x <- x + iris_sp[k, j]
        # Adding 1 to the value of 'y', each time the code loops through
        y <- y + 1
      }
      # Filling the Output table with values of 'x' divided by 'y'
      # This effectively finds the average of the rows in this for loop
      output[i, j] <- x / y 
    }
  }
}
# Output now contains the average of each column for each species
output


#' ## Question 3
#' Output could be named 'trait_avgs', x could be named 'trait_sum', y could be named 'N'



#' 
#' ## Question 4
#' New way to calculate the averages by decreasing the number of for loops

# Creates species list within the iris dataset
sp_ids <- unique(iris$Species)
# Creating Output as a table, and filling the table with species list and columns from iris
trait_avgs <- matrix(NA, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(trait_avgs) <- sp_ids
colnames(trait_avgs) <- names(iris[ , -ncol(iris)])

# Looping through the species
for(i in seq_along(sp_ids)) {
  # Subsetting the iris dataset for the species 'i', and removing the species column
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  # Looping through the columns in iris_sp
  for(j in 1:(ncol(iris_sp))) {
    # Calculating the mean of the column j and storing it in trait_avgs at point [i,j]
    trait_avgs[i,j] <- mean(iris_sp[ ,j])
  }
}
# Output now contains the average of each column for each species
trait_avgs
  

#' ## Question 5
#' Creating vector x and a for loop that adds the values of x together to create vector y
 
x <- 1:10
y <- NA

for(i in 1:length(x)) {
  y[i] <- sum(x[1]:x[i])
}
y

#' ## Question 6
#' Adding an if statement that says if any y value is greater than 10, it should be an NA
for(i in 1:length(x)) {
  y[i] <- sum(x[1]:x[i])
  if (y[i] > 10) {
    y[i] <- NA
  }
}
y

#' ## Question 7
#' Putting the loops above into a function, so that it can be applied to any object in R
#' 
#' I didn't like the y[i] > 10 = NA, so I removed that from my function

# Should realistically add some defensive code that would prevent errors that don't make sense
  # Prevent people from putting non-vectors or character strings
prev_list_sum = function(x){
  for(i in 1:length(x)) {
    y[i] <- sum(x[1]:x[i])
  }
  print(y)
}
  
prev_list_sum(x)

#' ## Question 8 (Optional)
#' Creating Fibonacci numbers with a for loop
#' 

# Changed question -- Make a function that takes a numeric argument that gives that number of numbers from the fibonacci sequence
# Look at solutions for help with the code
fibonacci_sum = function(x){
  for(i in 1:x){
    y = c(0,1,1,2,3)
    f = 0
    if (i < 3){
      f[i] = y[i]
    }
    else if (i > 5){
      f[i] = y[(i-1)] + y[(i-2)]
    }
    else {
      f[i] = f[i-1] + f[i-2]
    }
  }
  print(f)
}

fibonacci_sum(5)

# I couldn't get this to work. It seems like it should, but maybe you can't use 'i' like I was trying to.
# Not really sure what the fix is for this
