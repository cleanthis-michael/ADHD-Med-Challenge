####################################################################################
####################################################################################
###############  Correlations between Teacher- and Parent-Reported  ################
#################################  ADHD Symptoms  ##################################
####################################################################################
####################################################################################

library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(retimes)


### Comparing Parent-Reported ADHD Symptoms between SNAP-IV and Conners ###
setwd('~/Desktop/Cleanthis/University of North Carolina at Chapel Hill/1. ADHD MedChallenge Study/6. ADHD Symptoms')
conners_parent <- read_excel('Conners 3 Rating Scale/Conners ADHD Symptoms - Parent Ratings.xlsx')
snap_parent <- read_excel('SNAP-IV/SNAP-IV ADHD Symptoms - Parent Ratings.xlsx')


## ADHD Inattention Symptoms
# Conners Raw Scores
fit_inattention_parent_raw <- lm(snap_parent$snap_inattention ~ conners_parent$conners_inattention_raw)
plot(conners_parent$conners_inattention_raw, snap_parent$snap_inattention, xlab = 'Conners Parent-Reported Inattention (Raw)', ylab = 'SNAP-IV Parent-Reported Inattention', col = 'blue')
abline(fit_inattention_parent_raw, col = 'red')

cor(conners_parent$conners_inattention_raw, snap_parent$snap_inattention, use = 'pairwise.complete.obs')

# Conners t-Scores
fit_inattention_parent_t <- lm(snap_parent$snap_inattention ~ conners_parent$conners_inattention_t)
plot(conners_parent$conners_inattention_t, snap_parent$snap_inattention, xlab = 'Conners Parent-Reported Inattention (t)', ylab = 'SNAP-IV Parent-Reported Inattention', col = 'blue')
abline(fit_inattention_parent_t, col = 'red')

cor(conners_parent$conners_inattention_t, snap_parent$snap_inattention, use = 'pairwise.complete.obs')


## ADHD Hyperactivity Symptoms
# Conners Raw Scores
fit_hyperactivity_parent_raw <- lm(snap_parent$snap_hyperactivity ~ conners_parent$conners_hyperactivity_raw)
plot(conners_parent$conners_hyperactivity_raw, snap_parent$snap_hyperactivity, xlab = 'Conners Parent-Reported Hyperactivity (Raw)', ylab = 'SNAP-IV Parent-Reported Hyperactivity', col = 'blue')
abline(fit_hyperactivity_parent_raw, col = 'red')

cor(conners_parent$conners_hyperactivity_raw, snap_parent$snap_hyperactivity, use = 'pairwise.complete.obs')

# Conners t-Scores
fit_hyperactivity_parent_t <- lm(snap_parent$snap_hyperactivity ~ conners_parent$conners_hyperactivity_t)
plot(conners_parent$conners_hyperactivity_t, snap_parent$snap_hyperactivity, xlab = 'Conners Parent-Reported Hyperactivity (t)', ylab = 'SNAP-IV Parent-Reported Hyperactivity', col = 'blue')
abline(fit_hyperactivity_parent_t, col = 'red')

cor(conners_parent$conners_hyperactivity_t, snap_parent$snap_hyperactivity, use = 'pairwise.complete.obs')



### Comparing Teacher-Reported ADHD Symptoms between SNAP-IV and Conners ###
conners_teacher <- read_excel('Conners 3 Rating Scale/Conners ADHD Symptoms - Teacher Ratings.xlsx')
snap_teacher <- read_excel('SNAP-IV/SNAP-IV ADHD Symptoms - Teacher Ratings.xlsx')


## ADHD Inattention Symptoms
# Conners Raw Scores
fit_inattention_teacher_raw <- lm(snap_teacher$snap_inattention ~ conners_teacher$conners_inattention_raw)
plot(conners_teacher$conners_inattention_raw, snap_teacher$snap_inattention, xlab = 'Conners Teacher-Reported Inattention (Raw)', ylab = 'SNAP-IV Teacher-Reported Inattention', col = 'blue')
abline(fit_inattention_teacher_raw, col = 'red')

cor(conners_teacher$conners_inattention_raw, snap_teacher$snap_inattention, use = 'pairwise.complete.obs')

# Conners t-Scores
fit_inattention_teacher_t <- lm(snap_teacher$snap_inattention ~ conners_teacher$conners_inattention_t)
plot(conners_teacher$conners_inattention_t, snap_teacher$snap_inattention, xlab = 'Conners Teacher-Reported Inattention (t)', ylab = 'SNAP-IV Teacher-Reported Inattention', col = 'blue')
abline(fit_inattention_teacher_t, col = 'red')

cor(conners_teacher$conners_inattention_t, snap_teacher$snap_inattention, use = 'pairwise.complete.obs')


## ADHD Hyperactivity Symptoms
# Conners Raw Scores
fit_hyperactivity_teacher_raw <- lm(snap_teacher$snap_hyperactivity ~ conners_teacher$conners_hyperactivity_raw)
plot(conners_teacher$conners_hyperactivity_raw, snap_teacher$snap_hyperactivity, xlab = 'Conners Teacher-Reported Hyperactivity (Raw)', ylab = 'SNAP-IV Teacher-Reported Hyperactivity', col = 'blue')
abline(fit_hyperactivity_teacher_raw, col = 'red')

cor(conners_teacher$conners_hyperactivity_raw, snap_teacher$snap_hyperactivity, use = 'pairwise.complete.obs')


# Conners t-Scores
fit_hyperactivity_teacher_t <- lm(snap_teacher$snap_hyperactivity ~ conners_teacher$conners_hyperactivity_t)
plot(conners_teacher$conners_hyperactivity_t, snap_teacher$snap_hyperactivity, xlab = 'Conners Teacher-Reported Hyperactivity (t)', ylab = 'SNAP-IV Teacher-Reported Hyperactivity', col = 'blue')
abline(fit_hyperactivity_teacher_t, col = 'red')

cor(conners_teacher$conners_hyperactivity_t, snap_teacher$snap_hyperactivity, use = 'pairwise.complete.obs')



### Comparing Parent-Reported ADHD Symptoms and Teacher-Reported ADHD Symptoms ###

## Conners Scale
# ADHD Inattention Symptoms - Conners Raw Scores
fit_con_inattention_pt_raw <- lm(conners_parent$conners_inattention_raw ~ conners_teacher$conners_inattention_raw)
plot(conners_parent$conners_inattention_raw, conners_teacher$conners_inattention_raw, xlab = 'Conners Parent-Reported Inattention (Raw)', ylab = 'Conners Teacher-Reported Inattention (Raw)', col = 'blue')
abline(fit_con_inattention_pt_raw, col = 'red')

cor(conners_parent$conners_inattention_raw, conners_teacher$conners_inattention_raw, use = 'pairwise.complete.obs')

# ADHD Inattention Symptoms - Conners t-Scores
fit_con_inattention_pt_t <- lm(conners_parent$conners_inattention_t ~ conners_teacher$conners_inattention_t)
plot(conners_parent$conners_inattention_t, conners_teacher$conners_inattention_t, xlab = 'Conners Parent-Reported Inattention (t)', ylab = 'Conners Teacher-Reported Inattention (t)', col = 'blue')
abline(fit_con_inattention_pt_t, col = 'red')

cor(conners_parent$conners_inattention_t, conners_teacher$conners_inattention_t, use = 'pairwise.complete.obs')


# ADHD Hyperactivity Symptoms - Conners Raw Scores
fit_con_hyperactivity_pt_raw <- lm(conners_parent$conners_hyperactivity_raw ~ conners_teacher$conners_hyperactivity_raw)
plot(conners_parent$conners_hyperactivity_raw, conners_teacher$conners_hyperactivity_raw, xlab = 'Conners Parent-Reported Hyperactivity (Raw)', ylab = 'Conners Teacher-Reported Hyperactivity (Raw)', col = 'blue')
abline(fit_con_hyperactivity_pt_raw, col = 'red')

cor(conners_parent$conners_hyperactivity_raw, conners_teacher$conners_hyperactivity_raw, use = 'pairwise.complete.obs')

# ADHD Hyperactivity Symptoms - Conners t-Scores
fit_con_hyperactivity_pt_t <- lm(conners_parent$conners_hyperactivity_t ~ conners_teacher$conners_hyperactivity_t)
plot(conners_parent$conners_hyperactivity_t, conners_teacher$conners_hyperactivity_t, xlab = 'Conners Parent-Reported Hyperactivity (t)', ylab = 'Conners Teacher-Reported Hyperactivity (t)', col = 'blue')
abline(fit_con_hyperactivity_pt_t, col = 'red')

cor(conners_parent$conners_hyperactivity_t, conners_teacher$conners_hyperactivity_t, use = 'pairwise.complete.obs')


# ADHD Inattention Symptoms - SNAP-IV
fit_snap_inattention_pt <- lm(snap_parent$snap_inattention ~ snap_teacher$snap_inattention)
plot(snap_parent$snap_inattention, snap_teacher$snap_inattention, xlab = 'SNAP-IV Parent-Reported Inattention', ylab = 'SNAP-IV Teacher-Reported Inattention', col = 'blue')
abline(fit_snap_inattention_pt, col = 'red')

cor(snap_parent$snap_inattention, snap_teacher$snap_inattention, use = 'pairwise.complete.obs')


# ADHD Hyperactivity Symptoms - SNAP-IV
fit_snap_hyperactivity_pt <- lm(snap_parent$snap_hyperactivity ~ snap_teacher$snap_hyperactivity)
plot(snap_parent$snap_hyperactivity, snap_teacher$snap_hyperactivity, xlab = 'SNAP-IV Parent-Reported Hyperactivity', ylab = 'SNAP-IV Teacher-Reported Hyperactivity', col = 'blue')
abline(fit_snap_hyperactivity_pt, col = 'red')

cor(snap_parent$snap_hyperactivity, snap_teacher$snap_hyperactivity, use = 'pairwise.complete.obs')
