#### May 2020 - CM ####
### ADHD Med Challenge - Extract Subject-Level Exponential Gaussian Components from RT Data ###
## This script takes a vector of RTs (for correct 'go' trials) for each subject, calculates exponential Gaussian components (mu/sigma/tau), and saves them in an output csv file ##

rm(list=ls())

library(retimes)  # needed to extract ex-Gauss components
# The retimes package has 2 functions that estimate exponentiall gaussian components:
  # mexgauss estimates mu, sigma, and tau using the method of moments, and only outputs these 3 values.
  # timefit bootstraps the data, then uses a gaussian kernel estimator to compute mu and sigma, and then estimates tau from the bootstrapped values using maximum likelihood. It also outputs an Akaike Information Criterion (AIC) value, which estimates "out-of-sample prediction error and thereby relative quality of the statistical models for a given set of data"

# Here, we use the timefit function because the method of moments is a parametric estimator, which means that any deviation from the assumed distributions will lead to bias in the resulting estimates. The bootstrap estimate is more robust to these violations
# Specifically, here we use a bootstrapping method with 5000 iterations with replacement


### The ex-Gaussian model decomposes the RT distribution into a normal and an expontential distribution. We pull out the mean and standard deviation of the normal distribution (mu and sigma respectively), and a composite measure of the mean and standard deviation of the exponential distribution (tau). We also pull out the Akaike Information Criterion - a measure of model fit ##


# Read in the csv file containing the RT data for each task for each session for each subject #
data_rt <- read.csv("ADHD MedChallenge Response Times.csv", sep = ",")
data_rt$Group <- as.character(data_rt$Group)
data_rt$Task <- as.character(data_rt$Task)
data_rt$Session_order..A...drug.first. <- as.character(data_rt$Session_order..A...drug.first.)


# Create an empty data frame that will hold all of the information required for analyses and will be filled in by the loop further down the script #
data_exGauss <- data.frame(subjectID = NA,
                           group = NA,
                           task = NA,
                           ses = NA,
                           ses_order = NA,
                           mu = NA,
                           sigma = NA,
                           tau = NA,
                           aic = NA)


# Create a loop that gets all of the RT data for each task for each session for each subject and calculates the corresponding ex-Gauss components #
for (row in 1:nrow(data_rt)) {
  
  temp_data <- data.frame(subjectID = NA,
                          group = NA,
                          task = NA,
                          ses = NA,
                          ses_order = NA,
                          mu = NA,
                          sigma = NA,
                          tau = NA,
                          aic = NA)
  
  
  temp_subID <- data_rt[row,][1]
  temp_group <- data_rt[row,][2]
  temp_task <- data_rt[row,][3]
  temp_ses <- data_rt[row,][4]
  temp_ses_order <- data_rt[row,][5]
  
  reactionTimes <- c(data_rt[row, 6:193])
  reactionTimes <- as.numeric(unlist(reactionTimes))
  #exGauss_components <- mexgauss(na.omit(reactionTimes))
  exGauss_components <- timefit(na.omit(reactionTimes), iter = 5000, replace = TRUE)
  
  # The output of timefit is an S4 object, so use @ instead of $ to pull out components #
  temp_mu <- exGauss_components@par["mu"]
  temp_sigma <- exGauss_components@par["sigma"]
  temp_tau <- exGauss_components@par["tau"]
  temp_AIC <- exGauss_components@AIC
  
  temp_data$subjectID <- temp_subID
  temp_data$group <- temp_group
  temp_data$task <- temp_task
  temp_data$ses <- temp_ses
  temp_data$ses_order <- temp_ses_order
  temp_data$mu <- temp_mu
  temp_data$sigma <- temp_sigma
  temp_data$tau <- temp_tau
  temp_data$aic <- temp_AIC
  
  data_exGauss <- rbind(data_exGauss, temp_data)
  
}


## Save the data_exGauss data frame as a csv file that will be used for analyses ##
# Convert each column from 'list' to 'character'
data_exGauss$subjectID <- vapply(data_exGauss$subjectID, paste, collapse = ',', character(1L))
data_exGauss$group <- vapply(data_exGauss$group, paste, collapse = ',', character(1L))
data_exGauss$task <- vapply(data_exGauss$task, paste, collapse = ',', character(1L))
data_exGauss$ses <- vapply(data_exGauss$ses, paste, collapse = ',', character(1L))
data_exGauss$ses_order <- vapply(data_exGauss$ses_order, paste, collapse = ',', character(1L))
data_exGauss$mu <- vapply(data_exGauss$mu, paste, collapse = ',', character(1L))
data_exGauss$sigma <- vapply(data_exGauss$sigma, paste, collapse = ',', character(1L))
data_exGauss$tau <- vapply(data_exGauss$tau, paste, collapse = ',', character(1L))
data_exGauss$aic <- vapply(data_exGauss$aic, paste, collapse = ',', character(1L))

write.csv(data_exGauss, file = "medchal_exGauss_data.csv", row.names = FALSE)
