#### DATA IMPUTATION ####

# Read in the linked dataset and check its format.
linked = read.csv('cleaned.csv')
head(linked)

# Load the mice library and check where the missing values are.
library(mice)
md.pattern(linked)

# Visual representation of where the missing values are.
library(VIM)
aggr_plot <- aggr(linked, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(linked), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Number of NA values in each row:
na_rows = rowSums(is.na(linked))

# Find which row has the most na values
which.max(na_rows)
max(na_rows)
# Row 574 has most NAs.

# Delete row 574 and the categorical columns
# as we don't want to include dummy variables
# (linear combinations of each other)
newlinked <- linked[-574, -c(1,2,3,4)]

# Check what methods are available for imputing data.
methods(mice)

# Multiple imputation: 5 datasets, 30 iterations.
imp <- mice(newlinked, m=5, maxit=30, seed=500)
#imp$loggedEvents
summary(imp)

# Saved the mids object to a file.
save(imp, file = "imp.rda")

# Uncomment the following to load the imputed datasets
# imp <- load('imp.rda')

# Plot the imputed data to check against observed values.
densityplot(imp)
# The magenta points (imputed) matches the shape of the blue ones (observed).

########## THIS IS ROUGH WORK ##########################################

#### STANDARDISATION OF DATA ####

# Check that the imputed data actually has no missing values
md.pattern(complete(imp))
# apply(complete(imp), 2, function(x) any(is.na(x)))
# which(is.na(as.numeric(as.character(imp))))


# Insert the socioeconomic index
long <- mice::complete(imp, "long", include = TRUE)
long$socioecindex <- with(long, (Noqual + hhSocialRented + JobSeekers) / 3)
impnew <- as.mids(long)

# Check the new column is also complete
md.pattern(complete(impnew))

# install.packages("miceadds")
# install.packages("TAM")
library(TAM)
library(miceadds)

# convert into datlist
datlist <- miceadds::mids2datlist(impnew)

# select the numeric variables we want to standardise
vars <- c('Crimerate', 'hhSocialRented', 'JobSeekers', 
          'Noqual', 'nonwhite', 'NotEnglishspeaking', 
          'Openspace', 'Greaterthan65', 'NotBorninUK',
          'Children', 'LifeExpectancy', 'socioecindex')

# standardise the chosen variables 
sdatlist <- miceadds::scale_datlist( datlist=datlist, orig_var = vars, trafo_var=paste0("z",vars))

# reconvert into mids object
imp2 <- miceadds::datlist2mids(sdatlist)

#### LINEAR REGRESSION MODEL ####


# Model 1 is simple linear regression with the variables we stadardised.
# I excluded some i didn't think were relevant
modelfit1 <- with(imp2, lm(zLifeExpectancy~ zCrimerate + zhhSocialRented + zJobSeekers +
                           zNoqual + znonwhite + zNotEnglishspeaking + 
                           zGreaterthan65 + zNotBorninUK + zChildren))


# Model 2 contains the socioeconomic index we calculated earlier.
modelfit2 <- with(imp2, lm(zLifeExpectancy~ zCrimerate  + zNotEnglishspeaking + 
                             zOpenspace + zGreaterthan65 + zNotBorninUK +zsocioecindex))

summary(pool(modelfit1))
summary(pool(modelfit2))

