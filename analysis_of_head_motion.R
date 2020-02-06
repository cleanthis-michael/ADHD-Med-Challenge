##########################################################################################
##########################################################################################
############################  Analysis of Head Motion Data   #############################
##########################################################################################
##########################################################################################

library(tidyverse)
library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(retimes)


head_motion_data <- read.csv('FDdata.csv')
head_motion_data$'condition' <- str_c(head_motion_data$'ses', head_motion_data$'task', sep = ', ')
head_motion_data$'condition' <- str_c(head_motion_data$'condition', head_motion_data$'run', sep = ', ')


##########################################################################################
#######################  Exploring Head Motion across Sessions   #########################
##########################################################################################


### TD Children -------------------------------------------------------------------------
# MRI Session 1 -------------------------------------------------------------------------
ses1_rest_run1_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-rest',
                              run == 'run-01')
ses1_rest_run2_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-rest',
                              run == 'run-02')


ses1_gngreg_run1_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gng',
                              run == 'run-01')
ses1_gngreg_run2_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gng',
                              run == 'run-02')

ses1_gngrew_run1_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gngreward',
                              run == 'run-01')
ses1_gngrew_run2_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gngreward',
                              run == 'run-02')
ses1_gngrew_run3_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gngreward',
                              run == 'run-03')
ses1_gngrew_run4_fd <- filter(head_motion_data, ses == 'ses-01', task == 'task-gngreward',
                              run == 'run-04')


# MRI Session 2 -------------------------------------------------------------------------
ses2_rest_run1_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-rest',
                            run == 'run-01')
ses2_rest_run2_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-rest',
                            run == 'run-02')


ses2_gngreg_run1_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gng',
                              run == 'run-01')
ses2_gngreg_run2_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gng',
                              run == 'run-02')

ses2_gngrew_run1_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gngreward',
                              run == 'run-01')
ses2_gngrew_run2_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gngreward',
                              run == 'run-02')
ses2_gngrew_run3_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gngreward',
                              run == 'run-03')
ses2_gngrew_run4_fd <- filter(head_motion_data, ses == 'ses-02', task == 'task-gngreward',
                              run == 'run-04')


### ADHD Children -----------------------------------------------------------------------
# Placebo Session -----------------------------------------------------------------------
pl_rest_run1_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-rest',
                            run == 'run-01')
pl_rest_run2_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-rest',
                            run == 'run-02')


pl_gngreg_run1_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gng',
                              run == 'run-01')
pl_gngreg_run2_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gng',
                              run == 'run-02')

pl_gngrew_run1_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gngreward',
                              run == 'run-01')
pl_gngrew_run2_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gngreward',
                              run == 'run-02')
pl_gngrew_run3_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gngreward',
                              run == 'run-03')
pl_gngrew_run4_fd <- filter(head_motion_data, ses == 'Placebo', task == 'task-gngreward',
                              run == 'run-04')


# Drug Session --------------------------------------------------------------------------
dr_rest_run1_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-rest',
                          run == 'run-01')
dr_rest_run2_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-rest',
                          run == 'run-02')


dr_gngreg_run1_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gng',
                            run == 'run-01')
dr_gngreg_run2_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gng',
                            run == 'run-02')

dr_gngrew_run1_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gngreward',
                            run == 'run-01')
dr_gngrew_run2_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gngreward',
                            run == 'run-02')
dr_gngrew_run3_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gngreward',
                            run == 'run-03')
dr_gngrew_run4_fd <- filter(head_motion_data, ses == 'Drug', task == 'task-gngreward',
                            run == 'run-04')




### Drawing Boxplots --------------------------------------------------------------------
## TD Children --------------------------------------------------------------------------
# MRI Session 1 -------------------------------------------------------------------------
TD_day_1 <- filter(head_motion_data, ses == 'ses-01', Group == 'TD')
ggplot(TD_day_1, mapping = aes(x = condition, y = mean_fd)) + geom_boxplot()
TD_day_1$condition <- fct_recode(TD_day_1$condition, 'gng_reg_1' = 'ses-01, task-gng, run-01',
                                 'gng_reg_2' = 'ses-01, task-gng, run-02',
                                 'gng_rew_1' = 'ses-01, task-gngreward, run-01',
                                 'gng_rew_2' = 'ses-01, task-gngreward, run-02',
                                 'gng_rew_3' = 'ses-01, task-gngreward, run-03',
                                 'gng_rew_4' = 'ses-01, task-gngreward, run-04',
                                 'gng_rest_1' = 'ses-01, task-rest, run-01',
                                 'gng_rest_2' = 'ses-01, task-rest, run-02')

TD_day_1$condition <- factor(TD_day_1$condition, levels = c('gng_rest_1', 'gng_rest_2',
                                                            'gng_reg_1', 'gng_reg_2',
                                                            'gng_rew_1', 'gng_rew_2',
                                                            'gng_rew_3', 'gng_rew_4'))
ggplot(TD_day_1, mapping = aes(x = condition, y = mean_fd)) + xlab('TD MRI Session 1') + ylab('Mean FD') + geom_boxplot()


# MRI Session 2 -------------------------------------------------------------------------
TD_day_2 <- filter(head_motion_data, ses == 'ses-02', Group == 'TD')
ggplot(TD_day_2, mapping = aes(x = condition, y = mean_fd)) + geom_boxplot()
TD_day_2$condition <- fct_recode(TD_day_2$condition, 'gng_reg_1' = 'ses-02, task-gng, run-01',
                                 'gng_reg_2' = 'ses-02, task-gng, run-02',
                                 'gng_rew_1' = 'ses-02, task-gngreward, run-01',
                                 'gng_rew_2' = 'ses-02, task-gngreward, run-02',
                                 'gng_rew_3' = 'ses-02, task-gngreward, run-03',
                                 'gng_rew_4' = 'ses-02, task-gngreward, run-04',
                                 'gng_rest_1' = 'ses-02, task-rest, run-01',
                                 'gng_rest_2' = 'ses-02, task-rest, run-02')

TD_day_2$condition <- factor(TD_day_2$condition, levels = c('gng_rest_1', 'gng_rest_2',
                                                            'gng_reg_1', 'gng_reg_2',
                                                            'gng_rew_1', 'gng_rew_2',
                                                            'gng_rew_3', 'gng_rew_4', 'NA'))
ggplot(TD_day_2, mapping = aes(x = condition, y = mean_fd)) + xlab('TD MRI Session 2') + ylab('Mean FD') + geom_boxplot()


## ADHD Children ------------------------------------------------------------------------
# MRI Placebo Session -------------------------------------------------------------------
ADHD_placebo <- filter(head_motion_data, ses == 'Placebo', Group == 'ADHD')
ggplot(ADHD_placebo, mapping = aes(x = condition, y = mean_fd)) + geom_boxplot()
ADHD_placebo$condition <- fct_recode(ADHD_placebo$condition, 'gng_reg_1' = 'Placebo, task-gng, run-01',
                                 'gng_reg_2' = 'Placebo, task-gng, run-02',
                                 'gng_rew_1' = 'Placebo, task-gngreward, run-01',
                                 'gng_rew_2' = 'Placebo, task-gngreward, run-02',
                                 'gng_rew_3' = 'Placebo, task-gngreward, run-03',
                                 'gng_rew_4' = 'Placebo, task-gngreward, run-04',
                                 'gng_rest_1' = 'Placebo, task-rest, run-01',
                                 'gng_rest_2' = 'Placebo, task-rest, run-02')

ADHD_placebo$condition <- factor(ADHD_placebo$condition, levels = c('gng_rest_1', 'gng_rest_2',
                                                            'gng_reg_1', 'gng_reg_2',
                                                            'gng_rew_1', 'gng_rew_2',
                                                            'gng_rew_3', 'gng_rew_4'))
ggplot(ADHD_placebo, mapping = aes(x = condition, y = mean_fd)) + xlab('ADHD MRI Placebo Session') + ylab('Mean FD') + geom_boxplot()


# MRI Drug Session ----------------------------------------------------------------------
ADHD_drug <- filter(head_motion_data, ses == 'Drug', Group == 'ADHD')
ggplot(ADHD_drug, mapping = aes(x = condition, y = mean_fd)) + geom_boxplot()
ADHD_drug$condition <- fct_recode(ADHD_drug$condition, 'gng_reg_1' = 'Drug, task-gng, run-01',
                                     'gng_reg_2' = 'Drug, task-gng, run-02',
                                     'gng_rew_1' = 'Drug, task-gngreward, run-01',
                                     'gng_rew_2' = 'Drug, task-gngreward, run-02',
                                     'gng_rew_3' = 'Drug, task-gngreward, run-03',
                                     'gng_rew_4' = 'Drug, task-gngreward, run-04',
                                     'gng_rest_1' = 'Drug, task-rest, run-01',
                                     'gng_rest_2' = 'Drug, task-rest, run-02')

ADHD_drug$condition <- factor(ADHD_drug$condition, levels = c('gng_rest_1', 'gng_rest_2',
                                                                    'gng_reg_1', 'gng_reg_2',
                                                                    'gng_rew_1', 'gng_rew_2',
                                                                    'gng_rew_3', 'gng_rew_4'))
ggplot(ADHD_drug, mapping = aes(x = condition, y = mean_fd)) + xlab('ADHD MRI Drug Session') + ylab('Mean FD') + geom_boxplot()




##########################################################################################
##################  Regression Analyses between ADHD Symptom Severity  ###################
###############################      and Head Motion       ###############################
##########################################################################################

adhd_symptoms <- read.csv('ADHD Symptoms/ADHD Symptom Summary.csv')
head_motion_data$subid <- str_sub(head_motion_data$subid, 5, 8)
symptoms_by_head_motion <- data.frame(tapply(head_motion_data$mean_fd, head_motion_data$subid, mean))
colnames(symptoms_by_head_motion)[colnames(symptoms_by_head_motion) == 'tapply.head_motion_data.mean_fd..head_motion_data.subid..mean.'] <- 'mean_fd'

subids_with_motion_data <- c(head_motion_data$subid)
adhd_symptoms_filtered <- filter(adhd_symptoms, subject_id %in% subids_with_motion_data)  # contains ADHD symptom severity only for subjects with head motion data

symptoms_by_head_motion$sex <- adhd_symptoms_filtered$sex
symptoms_by_head_motion$age <- adhd_symptoms_filtered$age
symptoms_by_head_motion$group <- adhd_symptoms_filtered$group
symptoms_by_head_motion$conners_p_inattention_raw <- adhd_symptoms_filtered$conners_p_inattention_raw
symptoms_by_head_motion$conners_p_hyperactivity_raw <- adhd_symptoms_filtered$conners_p_hyperactivity_raw
symptoms_by_head_motion$conners_p_inattention_t <- adhd_symptoms_filtered$conners_p_inattention_t
symptoms_by_head_motion$conners_p_hyperactivity_t <- adhd_symptoms_filtered$conners_p_hyperactivity_t
symptoms_by_head_motion$conners_t_inattention_raw <- adhd_symptoms_filtered$conners_t_inattention_raw
symptoms_by_head_motion$conners_t_hyperactivity_raw <- adhd_symptoms_filtered$conners_t_hyperactivity_raw
symptoms_by_head_motion$conners_t_inattention_t <- adhd_symptoms_filtered$conners_t_inattention_t
symptoms_by_head_motion$conners_t_hyperactivity_t <- adhd_symptoms_filtered$conners_t_hyperactivity_t
symptoms_by_head_motion$snap_p_inattention <- adhd_symptoms_filtered$snap_p_inattention
symptoms_by_head_motion$snap_p_hyperactivity <- adhd_symptoms_filtered$snap_p_hyperactivity
symptoms_by_head_motion$snap_t_inattention <- adhd_symptoms_filtered$snap_t_inattention
symptoms_by_head_motion$snap_t_hyperactivity <- adhd_symptoms_filtered$snap_t_hyperactivity


############################################################################################
###############  Relationship between ADHD Symptom Severity and Head Motion  ###############
###############  The following analyses consider both ADHD and TD children  ################
############################################################################################

summary(lm(conners_p_inattention_raw ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_p_hyperactivity_raw ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_p_inattention_t ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_p_hyperactivity_t ~ mean_fd*group, data = symptoms_by_head_motion))

summary(lm(conners_t_inattention_raw ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_t_hyperactivity_raw ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_t_inattention_t ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(conners_t_hyperactivity_t ~ mean_fd*group, data = symptoms_by_head_motion))

summary(lm(snap_p_inattention ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(snap_p_hyperactivity ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(snap_t_inattention ~ mean_fd*group, data = symptoms_by_head_motion))
summary(lm(snap_t_hyperactivity ~ mean_fd*group, data = symptoms_by_head_motion))



############################################################################################
###############  Relationship between ADHD Symptom Severity and Head Motion  ###############
##################  The following analyses consider ADHD children only  ####################
############################################################################################

symptoms_by_head_motion_ADHD <- filter(symptoms_by_head_motion, group == 'ADHD')

summary(lm(conners_p_inattention_raw ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_p_hyperactivity_raw ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_p_inattention_t ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_p_hyperactivity_t ~ mean_fd, data = symptoms_by_head_motion_ADHD))

summary(lm(conners_t_inattention_raw ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_t_hyperactivity_raw ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_t_inattention_t ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(conners_t_hyperactivity_t ~ mean_fd, data = symptoms_by_head_motion_ADHD))

summary(lm(snap_p_inattention ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(snap_p_hyperactivity ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(snap_t_inattention ~ mean_fd, data = symptoms_by_head_motion_ADHD))
summary(lm(snap_t_hyperactivity ~ mean_fd, data = symptoms_by_head_motion_ADHD))


t.test(symptoms_by_head_motion$mean_fd ~ symptoms_by_head_motion$group, var.equal = TRUE)

# ADHD Placebo vs. ADHD Drug
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-03',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-03',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-04',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'Drug'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-04',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE, paired = TRUE)


# ADHD Placebo vs. TD (Session 1)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-03',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-03',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-04',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-04',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)


# ADHD Placebo vs. TD (Session 2)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-03',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-03',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Placebo' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-04',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-04',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)


# ADHD Drug vs. TD (Session 1)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-03',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-03',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-04',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-01'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-04',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)


# ADHD Drug vs. TD (Session 2)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-rest' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-rest' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gng' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gng' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-01',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-01',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-02',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-02',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-03',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-03',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)
t.test(ifelse(head_motion_data$ses == 'Drug' & head_motion_data$task == 'task-gngreward' & head_motion_data$run == 'run-04',
              head_motion_data$mean_fd, NA), ifelse(head_motion_data$ses == 'ses-02'
                                                    & head_motion_data$task == 'task-gngreward' 
                                                    & head_motion_data$run == 'run-04',
                                                    head_motion_data$mean_fd, NA), var.equal = TRUE)



















