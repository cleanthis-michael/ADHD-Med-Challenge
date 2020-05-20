#### May 2020 - CM ####
### ADHD Med Challenge - Combine the Summary Statistics extracted from the "newCode_final.py" script with the exponential gaussian data extracted from the "extract_exGauss_components.R" script

rm(list=ls())
library(stringr)
library(dplyr)

### Read in the csv files containing the summary statistics and the ex-Gauss data
data_all <- read.csv("ADHD MedChallenge Results Summary.csv", sep = ',', header = TRUE)

data_exGauss <- read.csv("medchal_exGauss_data.csv", sep = ',', stringsAsFactors = FALSE)

# Rename columns of data_exGauss to be consistent with data_all
data_exGauss$subjectID <- paste('sub-', data_exGauss$subjectID, sep = '')
data_exGauss$ses <- paste('Ses', data_exGauss$ses, sep = '')
data_exGauss$task <- replace(data_exGauss$task, data_exGauss$task == 'GNGgoonly', 'GoOnly')
data_exGauss$task <- replace(data_exGauss$task, data_exGauss$task == 'GNGregular', 'GNGReg')
data_exGauss$task <- replace(data_exGauss$task, data_exGauss$task == 'GNGreward', 'GNGRew')
names(data_exGauss)[names(data_exGauss) == 'mu'] <- 'Mu'
names(data_exGauss)[names(data_exGauss) == 'sigma'] <- 'Sigma'
names(data_exGauss)[names(data_exGauss) == 'tau'] <- 'Tau'
names(data_exGauss)[names(data_exGauss) == 'aic'] <- 'AIC'


#### Loop through each subject inside data_all. For each subject, the following loop will get the subject's mu, sigma, tau, and AIC from data_exGauss, and write them in the corresponding cell in the data_all ####
for (row in 1:nrow(data_all)) {
  
  subid <- data_all[row,1]; subid
  temp_data_exGauss <- filter(data_exGauss, subjectID == subid)  # get each subject's data one at a time
  
  # For each subject, loop through each task for each session #
  for (i in 1:nrow(temp_data_exGauss)) {
    
    # Create strings of the column names that will be accessed #
    cell_template <- paste(temp_data_exGauss[i,]$ses, '_', temp_data_exGauss[i,]$task, '_')
    cell_mu <- paste(cell_template, 'Mu')
    cell_sigma <- paste(cell_template, 'Sigma')
    cell_tau <- paste(cell_template, 'Tau')
    cell_aic <- paste(cell_template, 'AIC')
    
    # The 'paste' function introduces spaces between each part of the string => remove them #
    cell_mu <- str_replace_all(cell_mu, pattern = ' ', repl = '')
    cell_sigma <- str_replace_all(cell_sigma, pattern = ' ', repl = '')
    cell_tau <- str_replace_all(cell_tau, pattern = ' ', repl = '')
    cell_aic <- str_replace_all(cell_aic, pattern = ' ', repl = '')
    
    # Get the mu, sigma, tau, and AIC value for each subject for each task for each session #
    mu <- temp_data_exGauss[i,]$Mu
    sigma <- temp_data_exGauss[i,]$Sigma
    tau <- temp_data_exGauss[i,]$Tau
    aic <- temp_data_exGauss[i,]$AIC
    
    # Write the mu, sigma, tau, and AIC values extracted from data_exGauss into the corresponding cell in data_all #
    data_all[row,cell_mu] <- mu
    data_all[row,cell_sigma] <- sigma
    data_all[row,cell_tau] <- tau
    data_all[row, cell_aic] <- aic
    
  }
  
}

## Save the final csv file containing all of the data ##
write.csv(data_all, file = "medchal_GNG_alldata.csv", row.names = FALSE)

