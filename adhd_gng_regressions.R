####################################################################################
####################################################################################
###############  Exploration of the Distribution of ADHD Symptoms   ################
#########  Relationship between ADHD Symptom Severity and GNG Performance  #########
#######  Relationship between ADHD Symptom Severity and Scanner Head Motion  #######
####################################################################################
####################################################################################

library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(retimes)


setwd('~/Desktop/Cleanthis/University of North Carolina at Chapel Hill/1. ADHD MedChallenge Study/6. ADHD Symptoms')
adhd_symptoms <- read_excel('ADHD Symptom Summary.xlsx')


############################################################################################
####################  Exploration of the Distribution of ADHD Symptoms  ####################
############################################################################################

### Splitting ADHD Symptom Columns by Group (ADHD vs. TD)
adhd_symptoms$conners_p_in_raw_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_p_inattention_raw, NA)
adhd_symptoms$conners_p_in_raw_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_p_inattention_raw, NA)
adhd_symptoms$conners_p_hyp_raw_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_p_hyperactivity_raw, NA)
adhd_symptoms$conners_p_hyp_raw_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_p_hyperactivity_raw, NA)

adhd_symptoms$conners_p_in_t_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_p_inattention_t, NA)
adhd_symptoms$conners_p_in_t_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_p_inattention_t, NA)
adhd_symptoms$conners_p_hyp_t_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_p_hyperactivity_t, NA)
adhd_symptoms$conners_p_hyp_t_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_p_hyperactivity_t, NA)

adhd_symptoms$conners_t_in_raw_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_t_inattention_raw, NA)
adhd_symptoms$conners_t_in_raw_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_t_inattention_raw, NA)
adhd_symptoms$conners_t_hyp_raw_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_t_hyperactivity_raw, NA)
adhd_symptoms$conners_t_hyp_raw_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_t_hyperactivity_raw, NA)

adhd_symptoms$conners_t_in_t_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_t_inattention_t, NA)
adhd_symptoms$conners_t_in_t_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_t_inattention_t, NA)
adhd_symptoms$conners_t_hyp_t_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$conners_t_hyperactivity_t, NA)
adhd_symptoms$conners_t_hyp_t_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$conners_t_hyperactivity_t, NA)

adhd_symptoms$snap_p_in_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$snap_p_inattention, NA)
adhd_symptoms$snap_p_in_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$snap_p_inattention, NA)
adhd_symptoms$snap_p_hyp_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$snap_p_hyperactivity, NA)
adhd_symptoms$snap_p_hyp_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$snap_p_hyperactivity, NA)

adhd_symptoms$snap_t_in_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$snap_t_inattention, NA)
adhd_symptoms$snap_t_in_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$snap_t_inattention, NA)
adhd_symptoms$snap_t_hyp_adhd <- ifelse(adhd_symptoms$group == 'ADHD', adhd_symptoms$snap_t_hyperactivity, NA)
adhd_symptoms$snap_t_hyp_td <- ifelse(adhd_symptoms$group == 'TD', adhd_symptoms$snap_t_hyperactivity, NA)



### Drawing Exploratory Plots ###
## Conners Scale - Parent-Reports
plot(adhd_symptoms$subject_id, adhd_symptoms$conners_p_in_raw_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Parent-Reported Inattention (Raw)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_p_inattention_raw)), 
              max(na.omit(adhd_symptoms$conners_p_inattention_raw))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_p_in_raw_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_p_in_t_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Parent-Reported Inattention (t)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_p_inattention_t)), 
              max(na.omit(adhd_symptoms$conners_p_inattention_t))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_p_in_t_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_p_hyp_raw_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Parent-Reported Hyperactivity (Raw)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_p_hyperactivity_raw)), 
              max(na.omit(adhd_symptoms$conners_p_hyperactivity_raw))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_p_hyp_raw_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_p_hyp_t_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Parent-Reported Hyperactivity (t)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_p_hyperactivity_t)), 
              max(na.omit(adhd_symptoms$conners_p_hyperactivity_t))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_p_hyp_t_td, 'p', col = 'blue')



## Conners Scale - Teacher-Reports
plot(adhd_symptoms$subject_id, adhd_symptoms$conners_t_in_raw_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Teacher-Reported Inattention (Raw)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_t_inattention_raw)), 
              max(na.omit(adhd_symptoms$conners_t_inattention_raw))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_t_in_raw_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_t_in_t_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Teacher-Reported Inattention (t)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_t_inattention_t)), 
              max(na.omit(adhd_symptoms$conners_t_inattention_t))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_t_in_t_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_t_hyp_raw_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Teacher-Reported Hyperactivity (Raw)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_t_hyperactivity_raw)), 
              max(na.omit(adhd_symptoms$conners_t_hyperactivity_raw))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_t_hyp_raw_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$conners_t_hyp_t_adhd, 'p',
     xlab = 'Subject ID', ylab = 'Conners Teacher-Reported Hyperactivity (t)', 
     ylim = c(min(na.omit(adhd_symptoms$conners_t_hyperactivity_t)), 
              max(na.omit(adhd_symptoms$conners_t_hyperactivity_t))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$conners_t_hyp_t_td, 'p', col = 'blue')



## SNAP-IV - Parent-Reports
plot(adhd_symptoms$subject_id, adhd_symptoms$snap_p_in_adhd, 'p',
     xlab = 'Subject ID', ylab = 'SNAP-IV Parent-Reported Inattention', 
     ylim = c(min(na.omit(adhd_symptoms$snap_p_inattention)), 
              max(na.omit(adhd_symptoms$snap_p_inattention))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$snap_p_in_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$snap_p_hyp_adhd, 'p',
     xlab = 'Subject ID', ylab = 'SNAP-IV Parent-Reported Hyperactivity', 
     ylim = c(min(na.omit(adhd_symptoms$snap_p_hyperactivity)), 
              max(na.omit(adhd_symptoms$snap_p_hyperactivity))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$snap_p_hyp_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$snap_t_in_adhd, 'p',
     xlab = 'Subject ID', ylab = 'SNAP-IV Teacher-Reported Inattention', 
     ylim = c(min(na.omit(adhd_symptoms$snap_t_inattention)), 
              max(na.omit(adhd_symptoms$snap_t_inattention))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$snap_t_in_td, 'p', col = 'blue')


plot(adhd_symptoms$subject_id, adhd_symptoms$snap_t_hyp_adhd, 'p',
     xlab = 'Subject ID', ylab = 'SNAP-IV Teacher-Reported Hyperactivity', 
     ylim = c(min(na.omit(adhd_symptoms$snap_t_hyperactivity)), 
              max(na.omit(adhd_symptoms$snap_t_hyperactivity))),
     col = 'red')
lines(adhd_symptoms$subject_id, adhd_symptoms$snap_t_hyp_td, 'p', col = 'blue')




############################################################################################
#############  Relationship between ADHD Symptom Severity and GNG Performance  #############
###############  The following analyses consider both ADHD and TD children  ################
############################################################################################

### Combining ADHD Symptom and GNG Data Frames ###
gng_data <- read_excel('ADHD MedChallenge Results Summary_deblinded.xlsx')
gng_exGauss <- read_excel('ADHD MedChallenge Ex-Gaussian Parameters.xlsx') 
medchal_data <- merge(adhd_symptoms, gng_data, by = 'subject_id')
medchal_data <- merge(medchal_data, gng_exGauss, by = 'subject_id')

adhd_symptoms_csv <- read.csv('ADHD Symptom Summary.csv')
gng_data_csv <- read.csv('ADHD MedChallenge Results Summary_deblinded.csv')
gng_exGauss_csv <- read.csv('ADHD MedChallenge Ex-Gaussian Parameters.csv') 
medchal_data <- merge(adhd_symptoms_csv, gng_data_csv, by = 'subject_id')
medchal_data <- merge(medchal_data, gng_exGauss, by = 'subject_id')



### Behavioral Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReg_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReg_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReg_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Tau*group, data = medchal_data))



## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRew_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRew_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRew_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Tau*group, data = medchal_data))



### Placebo Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReg_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReg_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReg_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Tau*group, data = medchal_data))




## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRew_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRew_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRew_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Tau*group, data = medchal_data))



### Drug Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReg_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReg_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGReg_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReg_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Tau*group, data = medchal_data))


## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRew_dprim*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRew_dprim*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGRew_dprim*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRew_dprim*group, data = medchal_data))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Mu*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Mu*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Mu*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Mu*group, data = medchal_data))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Sigma*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Sigma*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Sigma*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Sigma*group, data = medchal_data))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Tau*group, data = medchal_data))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Tau*group, data = medchal_data))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Tau*group, data = medchal_data))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Tau*group, data = medchal_data))







#full_model <- lm(snap_p_inattention ~ group + Run3_GNGReward_Tau, data = medchal_data)
#null_model <- lm(snap_p_inattention ~ 1, data = medchal_data)
#step(null_model, scope = list(upper = full_model))
        


############################################################################################
#############  Relationship between ADHD Symptom Severity and GNG Performance  #############
###################  The following analyses consider ADHD children only ####################
############################################################################################

medchal_data_ADHD <- filter(medchal_data, group == 'ADHD')

### Behavioral Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReg_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRegular_Tau, data = medchal_data_ADHD))



## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGRew_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run1_GNGReward_Tau, data = medchal_data_ADHD))



### Placebo Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReg_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRegular_Tau, data = medchal_data_ADHD))




## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGRew_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run2_GNGReward_Tau, data = medchal_data_ADHD))



### Drug Day
## GNG Regular
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReg_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRegular_Tau, data = medchal_data_ADHD))


## GNG Reward
# d prime as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGRew_dprim, data = medchal_data_ADHD))


# Mu as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Mu, data = medchal_data_ADHD))


# Sigma as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Sigma, data = medchal_data_ADHD))


# Tau as predictor, Conners/Snap Inattention/Hyperactivity as response
summary(lm(conners_p_inattention_raw ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_inattention_t ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_p_hyperactivity_t ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(conners_t_inattention_raw ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_inattention_t ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(conners_t_hyperactivity_t ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))

summary(lm(snap_p_inattention ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_p_hyperactivity ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_inattention ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))
summary(lm(snap_t_hyperactivity ~ Run3_GNGReward_Tau, data = medchal_data_ADHD))

