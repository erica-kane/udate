#### DATA IMPUTATION ####

# Read in the linked dataset and check its format.
linked = read.csv('linked.csv')
head(linked)

# Load the mice library and check where the missing values are.
library(mice)
md.pattern(linked)

# Visual representation of where the missing values are.
library(VIM)
aggr_plot <- aggr(linked, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(linked), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Check what methods are available for imputing data.
methods(mice)

# Multiple imputation: 5 datasets, 50 iterations.
tempData <- mice(linked,m=5,maxit=50,method = "cart", seed=500)
summary(tempData)
# Note: I tried using several methods to impute the data, but because some variables are highly
# correlated (linear combinations of each other) we do not get an invertible matrix. I was getting
# this error: "system is computationally singular: reciprocal condition number = 9.79766e-17".
# This is a problem since a lot of the default imputation methods use regression models to predict 
# the missing values. Therefore we use the Classification And Regression Tree (CART) method. Takes 
# a long time to compute but yields excellent results.

# Saved the mids object to a file.
save(tempData, file = "imputed.rda")

# Plot the imputed data to check against observed values.
densityplot(tempData)
# The magenta points (imputed) matches the shape of the blue ones (observed).

########## THIS IS ROUGH WORK ##########################################

#### STANDARDISATION OF DATA ####

# tempData$data$index <- tempData$data$ 
# install.packages("miceadds")
install.packages("TAM")
library(TAM)
library(miceadds)
tempData[[, vars]] <- sapply(tempData[[, vars]], as.numeric)

# convert into datlist
datlist <- miceadds::mids2datlist(tempData)

# select the numeric variables we want to standardise
vars <- c('Crimerate', 'hhSocialRented', 'JobSeekers', 'Noqual', 'nonwhite', 'NotEnglishspeaking', 'Malelifeexpectancy')

# standardise the chosen variables 
sdatlist <- miceadds::scale_datlist( datlist=datlist, orig_var = vars, trafo_var=paste0("z",vars))

# reconvert into mids object
imp2 <- miceadds::datlist2mids(sdatlist)


#### LINEAR REGRESSION MODEL ####

modelfit1 <- with(imp2, lm(zMalelifeexpectancy~ zCrimerate + zhhSocialRented + zJobSeekers +
                                zNoqual + znonwhite + zNotEnglishspeaking + Wardcode))
summary(pool(modelfit1))
