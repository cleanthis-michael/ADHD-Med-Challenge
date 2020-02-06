### Setup ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)


### Crone Data
pds_df <- read_excel('data_crone/BrainTime_PDS_Testosterone.xlsx')
pds_df$PDS_boys <- as.numeric(pds_df$PDS_boys)

pds_df$testos_girls <- ifelse(pds_df$`Sex 1=boys` == '0', pds_df$Testos_4pmol, NA)
pds_df$testos_boys <- ifelse(pds_df$`Sex 1=boys` == '1', pds_df$Testos_4pmol, NA)
pds_df$log_testos_girls <- ifelse(pds_df$`Sex 1=boys` == '0', pds_df$log_testos_4pmol, NA)
pds_df$log_testos_boys <- ifelse(pds_df$`Sex 1=boys` == '1', pds_df$log_testos_4pmol, NA)


### Timepoint 1 -------------------------------------------------------------------------
pds_tp1 <- filter(pds_df, Timepoint == '1')

cor(pds_tp1$Age, pds_tp1$PDS_girls, use = "pairwise.complete.obs")
cor(pds_tp1$Age, pds_tp1$PDS_boys, use = "pairwise.complete.obs")
cor(pds_tp1$testos_girls, pds_tp1$PDS_girls, use = "pairwise.complete.obs")
cor(pds_tp1$testos_boys, pds_tp1$PDS_boys, use = "pairwise.complete.obs")
cor(pds_tp1$log_testos_girls, pds_tp1$PDS_girls, use = "pairwise.complete.obs")
cor(pds_tp1$log_testos_boys, pds_tp1$PDS_boys, use = "pairwise.complete.obs")

summary(lm(Age ~ PDS_girls, data = pds_tp1))
summary(lm(Age ~ PDS_boys, data = pds_tp1))
summary(lm(testos_girls ~ PDS_girls, data = pds_tp1))
summary(lm(testos_boys ~ PDS_boys, data = pds_tp1))
summary(lm(log_testos_girls ~ PDS_girls, data = pds_tp1))
summary(lm(log_testos_boys ~ PDS_boys, data = pds_tp1))


ggplot(data = pds_tp1) +
  geom_point(aes(x = PDS_boys, y = Age, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = Age, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = Age, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = Age, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale Score') + ylab('Age')

ggplot(data = pds_tp1) +
  geom_point(aes(x = PDS_boys, y = testos_boys, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = testos_boys, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = testos_girls, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = testos_girls, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale') + ylab('Testosterone Levels')

ggplot(data = pds_tp1) +
  geom_point(aes(x = PDS_boys, y = log_testos_boys, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = log_testos_boys, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = log_testos_girls, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = log_testos_girls, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale') + ylab('Log Testosterone Levels')



### All 3 Timepoints --------------------------------------------------------------------
cor(pds_df$Age, pds_df$PDS_girls, use = "pairwise.complete.obs")
cor(pds_df$Age, pds_df$PDS_boys, use = "pairwise.complete.obs")
cor(pds_df$testos_girls, pds_df$PDS_girls, use = "pairwise.complete.obs")
cor(pds_df$testos_boys, pds_df$PDS_boys, use = "pairwise.complete.obs")
cor(pds_df$log_testos_girls, pds_df$PDS_girls, use = "pairwise.complete.obs")
cor(pds_df$log_testos_boys, pds_df$PDS_boys, use = "pairwise.complete.obs")

summary(lm(Age ~ PDS_girls, data = pds_df))
summary(lm(Age ~ PDS_boys, data = pds_df))
summary(lm(testos_girls ~ PDS_girls, data = pds_df))
summary(lm(testos_boys ~ PDS_boys, data = pds_df))
summary(lm(log_testos_girls ~ PDS_girls, data = pds_df))
summary(lm(log_testos_boys ~ PDS_boys, data = pds_df))


ggplot(data = pds_df) +
  geom_point(aes(x = PDS_boys, y = Age, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = Age, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = Age, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = Age, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale Score') + ylab('Age')

ggplot(data = pds_df) +
  geom_point(aes(x = PDS_boys, y = testos_boys, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = testos_boys, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = testos_girls, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = testos_girls, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale') + ylab('Testosterone Levels')

ggplot(data = pds_df) +
  geom_point(aes(x = PDS_boys, y = log_testos_boys, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = PDS_boys, y = log_testos_boys, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = PDS_girls, y = log_testos_girls, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = PDS_girls, y = log_testos_girls, colour = 'Girls'), method = 'lm') + 
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale') + ylab('Log Testosterone Levels')



### ADHD Med Challenge Data -------------------------------------------------------------
pds_medchal <- read_excel('data_medchal/medchal_data.xlsx')
pds_medchal <- filter(pds_medchal, redcap_event_name == 'parent_arm_2' 
                      | redcap_event_name == 'child_arm_3')

### Make changes to the Dummy Codes -----------------------------------------------------
# Girls coded as '0', boys coded as '1'
pds_medchal$pds_sex_p[pds_medchal$pds_sex_p == 2] <- 0
pds_medchal$pds_sex_c[pds_medchal$pds_sex_c == 2] <- 0

# Options 0, 1, 2, 3 need to become 1, 2, 3, 4
pds_medchal$pds_1_p[pds_medchal$pds_1_p == 4] <- NA
pds_medchal$pds_1_p[pds_medchal$pds_1_p == 3] <- 4
pds_medchal$pds_1_p[pds_medchal$pds_1_p == 2] <- 3
pds_medchal$pds_1_p[pds_medchal$pds_1_p == 1] <- 2
pds_medchal$pds_1_p[pds_medchal$pds_1_p == 0] <- 1

pds_medchal$pds_1_c[pds_medchal$pds_1_c == 4] <- NA
pds_medchal$pds_1_c[pds_medchal$pds_1_c == 3] <- 4
pds_medchal$pds_1_c[pds_medchal$pds_1_c == 2] <- 3
pds_medchal$pds_1_c[pds_medchal$pds_1_c == 1] <- 2
pds_medchal$pds_1_c[pds_medchal$pds_1_c == 0] <- 1


pds_medchal$pds_2_p[pds_medchal$pds_2_p == 4] <- NA
pds_medchal$pds_2_p[pds_medchal$pds_2_p == 3] <- 4
pds_medchal$pds_2_p[pds_medchal$pds_2_p == 2] <- 3
pds_medchal$pds_2_p[pds_medchal$pds_2_p == 1] <- 2
pds_medchal$pds_2_p[pds_medchal$pds_2_p == 0] <- 1

pds_medchal$pds_2_c[pds_medchal$pds_2_c == 4] <- NA
pds_medchal$pds_2_c[pds_medchal$pds_2_c == 3] <- 4
pds_medchal$pds_2_c[pds_medchal$pds_2_c == 2] <- 3
pds_medchal$pds_2_c[pds_medchal$pds_2_c == 1] <- 2
pds_medchal$pds_2_c[pds_medchal$pds_2_c == 0] <- 1


pds_medchal$pds_3_p[pds_medchal$pds_3_p == 4] <- NA
pds_medchal$pds_3_p[pds_medchal$pds_3_p == 3] <- 4
pds_medchal$pds_3_p[pds_medchal$pds_3_p == 2] <- 3
pds_medchal$pds_3_p[pds_medchal$pds_3_p == 1] <- 2
pds_medchal$pds_3_p[pds_medchal$pds_3_p == 0] <- 1

pds_medchal$pds_3_c[pds_medchal$pds_3_c == 4] <- NA
pds_medchal$pds_3_c[pds_medchal$pds_3_c == 3] <- 4
pds_medchal$pds_3_c[pds_medchal$pds_3_c == 2] <- 3
pds_medchal$pds_3_c[pds_medchal$pds_3_c == 1] <- 2
pds_medchal$pds_3_c[pds_medchal$pds_3_c == 0] <- 1


pds_medchal$pds_4f_p[pds_medchal$pds_4f_p == 4] <- NA
pds_medchal$pds_4f_p[pds_medchal$pds_4f_p == 3] <- 4
pds_medchal$pds_4f_p[pds_medchal$pds_4f_p == 2] <- 3
pds_medchal$pds_4f_p[pds_medchal$pds_4f_p == 1] <- 2
pds_medchal$pds_4f_p[pds_medchal$pds_4f_p == 0] <- 1

pds_medchal$pds_4f_c[pds_medchal$pds_4f_c == 4] <- NA
pds_medchal$pds_4f_c[pds_medchal$pds_4f_c == 3] <- 4
pds_medchal$pds_4f_c[pds_medchal$pds_4f_c == 2] <- 3
pds_medchal$pds_4f_c[pds_medchal$pds_4f_c == 1] <- 2
pds_medchal$pds_4f_c[pds_medchal$pds_4f_c == 0] <- 1


pds_medchal$pds_4m_p[pds_medchal$pds_4m_p == 4] <- NA
pds_medchal$pds_4m_p[pds_medchal$pds_4m_p == 3] <- 4
pds_medchal$pds_4m_p[pds_medchal$pds_4m_p == 2] <- 3
pds_medchal$pds_4m_p[pds_medchal$pds_4m_p == 1] <- 2
pds_medchal$pds_4m_p[pds_medchal$pds_4m_p == 0] <- 1

pds_medchal$pds_4m_c[pds_medchal$pds_4m_c == 4] <- NA
pds_medchal$pds_4m_c[pds_medchal$pds_4m_c == 3] <- 4
pds_medchal$pds_4m_c[pds_medchal$pds_4m_c == 2] <- 3
pds_medchal$pds_4m_c[pds_medchal$pds_4m_c == 1] <- 2
pds_medchal$pds_4m_c[pds_medchal$pds_4m_c == 0] <- 1


pds_medchal$pds_5m_p[pds_medchal$pds_5m_p == 4] <- NA
pds_medchal$pds_5m_p[pds_medchal$pds_5m_p == 3] <- 4
pds_medchal$pds_5m_p[pds_medchal$pds_5m_p == 2] <- 3
pds_medchal$pds_5m_p[pds_medchal$pds_5m_p == 1] <- 2
pds_medchal$pds_5m_p[pds_medchal$pds_5m_p == 0] <- 1

pds_medchal$pds_5m_c[pds_medchal$pds_5m_c == 4] <- NA
pds_medchal$pds_5m_c[pds_medchal$pds_5m_c == 3] <- 4
pds_medchal$pds_5m_c[pds_medchal$pds_5m_c == 2] <- 3
pds_medchal$pds_5m_c[pds_medchal$pds_5m_c == 1] <- 2
pds_medchal$pds_5m_c[pds_medchal$pds_5m_c == 0] <- 1


# Menstruation codes of 0, 1 need to become 4, 1
pds_medchal$pds_5f_p[pds_medchal$pds_5f_p == 1] <- 4
pds_medchal$pds_5f_p[pds_medchal$pds_5f_p == 0] <- 1
pds_medchal$pds_5f_c[pds_medchal$pds_5f_c == 1] <- 4
pds_medchal$pds_5f_c[pds_medchal$pds_5f_c == 0] <- 1


### Scoring Calculations  --------------------------------------------------------------
# Female PDS: average of all 5 items = mean(growth, bodyhair, skin, breast, period)
# Female Puberty Category Scale: summation of body hair (pds_2) and breast development (pds_4f) = sum(bodyhair, breast)
# Female Puberty Category:
#   Prepubertal = score of 2 and no menarche
#   Early Pubertal = score of 3 and no menarche
#   Midpubertal = score >3 and no menarche
#   Late Pubertal = score <=7 and menarche
#   Postpubertal = score of 8 and menarche

# Male PDS: average of all 5 items = mean(growth, bodyhair, skin, facial hair, voice)
# Male Puberty Category Scale = summation of body hair (pds_2), voice change (pds_4m), and facial hair growth (pds_5m) = sum(body hair, voice, facial hair)
# Male Puberty Category:
#   Prepubertal = score of 3
#   Early Pubertal = score of 4 or 5 and no 3-pt responses
#   Midpubertal = score of 6-8 and no 4-pt responses
#   Late Pubertal = score of 9-11
#   Postpubertal = score of 12


# To resolve discrepancies (i.e., when a male participant receives a puberty category score of 4 or 5 but does have a 3-pt response):
#   (1) Look at the three questions involved in creating the puberty category score. If none of those three questions disqualify them for the category, keep at the same category.
#   (2) If one of the three questions do disqualify them, look at the age of the participant and the other questions to evaluate. Participants are not automatically bumped up to the next category level - males tend to over estimate puberty progression. [adapted from Crockett, 1988, unpublished manuscript]


### PDS Scores: sums and averages  -----------------------------------------------------
pds_medchal$basepds_p <- rowSums(pds_medchal[c('pds_1_p', 'pds_2_p', 'pds_3_p')], FALSE)
pds_medchal$basepds_c <- rowSums(pds_medchal[c('pds_1_c', 'pds_2_c', 'pds_3_c')], FALSE)  

pds_medchal$malepds_p <- rowSums(pds_medchal[c('basepds_p', 'pds_4m_p', 'pds_5m_p')], FALSE)
pds_medchal$femalepds_p <- rowSums(pds_medchal[c('basepds_p', 'pds_4f_p', 'pds_5f_p')], FALSE)
pds_medchal$malepds_c <- rowSums(pds_medchal[c('basepds_c', 'pds_4m_c', 'pds_5m_c')], FALSE)
pds_medchal$femalepds_c <- rowSums(pds_medchal[c('basepds_c', 'pds_4f_c', 'pds_5f_c')], FALSE)

require(dplyr)

pds_medchal <- pds_medchal %>%
  mutate(malepds_p = ifelse(is.na(malepds_p), '0', pds_medchal$malepds_p)) %>%
  mutate(malepds_c = ifelse(is.na(malepds_c), '0', pds_medchal$malepds_c)) %>%
  mutate(femalepds_p = ifelse(is.na(femalepds_p), '0', pds_medchal$femalepds_p)) %>%
  mutate(femalepds_c = ifelse(is.na(femalepds_c), '0', pds_medchal$femalepds_c))
  
pds_medchal$malepds_p <- as.numeric(pds_medchal$malepds_p)
pds_medchal$femalepds_p <- as.numeric(pds_medchal$femalepds_p)
pds_medchal$malepds_c <- as.numeric(pds_medchal$malepds_c)
pds_medchal$femalepds_c <- as.numeric(pds_medchal$femalepds_c)

pds_medchal$pdssum_p <- (pds_medchal$malepds_p + pds_medchal$femalepds_p)
pds_medchal$childpds_p <- (pds_medchal$pdssum_p / 5)  # this column contains the PDS score for each individual
pds_medchal$pdssum_c <- (pds_medchal$malepds_c + pds_medchal$femalepds_c)
pds_medchal$childpds_c <- (pds_medchal$pdssum_c / 5)  # this column contains the PDS score for each individual


### Puberty Category Score  ------------------------------------------------------------
pds_medchal$male_pubcat_score_p <- rowSums(pds_medchal[c('pds_2_p', 'pds_4m_p', 'pds_5m_p')], FALSE) 
pds_medchal$male_pubcat_score_c <- rowSums(pds_medchal[c('pds_2_c', 'pds_4m_c', 'pds_5m_c')], FALSE) 
pds_medchal$female_pubcat_score_p <- rowSums(pds_medchal[c('pds_2_p', 'pds_4f_p')], FALSE) 
pds_medchal$female_pubcat_score_c <- rowSums(pds_medchal[c('pds_2_c', 'pds_4f_c')], FALSE) 

pds_medchal <- pds_medchal %>%
  mutate(male_pubcat_score_p = ifelse(is.na(male_pubcat_score_p), '0', pds_medchal$male_pubcat_score_p)) %>%
  mutate(male_pubcat_score_c = ifelse(is.na(male_pubcat_score_c), '0', pds_medchal$male_pubcat_score_c)) %>%
  mutate(female_pubcat_score_p = ifelse(is.na(female_pubcat_score_p), '0', pds_medchal$female_pubcat_score_p)) %>%
  mutate(female_pubcat_score_c = ifelse(is.na(female_pubcat_score_c), '0', pds_medchal$female_pubcat_score_c))
  
pds_medchal$male_pubcat_score_p <- as.numeric(pds_medchal$male_pubcat_score_p)
pds_medchal$female_pubcat_score_p <- as.numeric(pds_medchal$female_pubcat_score_p)
pds_medchal$child_pubcat_score_p <- (pds_medchal$male_pubcat_score_p + pds_medchal$female_pubcat_score_p)

pds_medchal$male_pubcat_score_c <- as.numeric(pds_medchal$male_pubcat_score_c)
pds_medchal$female_pubcat_score_c <- as.numeric(pds_medchal$female_pubcat_score_c)
pds_medchal$child_pubcat_score_c <- (pds_medchal$male_pubcat_score_c + pds_medchal$female_pubcat_score_c)


### Assign Female Puberty Category Labels ----------------------------------------------
#   Prepubertal = score of 2 and no menarche
#   Early Pubertal = score of 3 and no menarche
#   Midpubertal = score >3 and no menarche
#   Late Pubertal = score <=7 and menarche
#   Postpubertal = score of 8 and menarche

pds_medchal$pubcat_female_p[pds_medchal$pds_sex_p == '0' & pds_medchal$child_pubcat_score_p == '2' & pds_medchal$pds_5f_p == '1'] <- "Prepubertal"
pds_medchal$pubcat_female_p[pds_medchal$pds_sex_p == '0' & pds_medchal$child_pubcat_score_p == '3' & pds_medchal$pds_5f_p == '1'] <- "Early Pubertal"
pds_medchal$pubcat_female_p[pds_medchal$pds_sex_p == '0' & pds_medchal$child_pubcat_score_p > '3' & pds_medchal$pds_5f_p == '1'] <- "Midpubertal"
pds_medchal$pubcat_female_p[pds_medchal$pds_sex_p == '0' & pds_medchal$child_pubcat_score_p <= '7' & pds_medchal$pds_5f_p == '4'] <- "Late Pubertal"
pds_medchal$pubcat_female_p[pds_medchal$pds_sex_p == '0' & pds_medchal$child_pubcat_score_p == '8' & pds_medchal$pds_5f_p == '4'] <- "Postpubertal"

pds_medchal$pubcat_female_c[pds_medchal$pds_sex_c == '0' & pds_medchal$child_pubcat_score_c == '2' & pds_medchal$pds_5f_c == '1'] <- "Prepubertal"
pds_medchal$pubcat_female_c[pds_medchal$pds_sex_c == '0' & pds_medchal$child_pubcat_score_c == '3' & pds_medchal$pds_5f_c == '1'] <- "Early Pubertal"
pds_medchal$pubcat_female_c[pds_medchal$pds_sex_c == '0' & pds_medchal$child_pubcat_score_c > '3' & pds_medchal$pds_5f_c == '1'] <- "Midpubertal"
pds_medchal$pubcat_female_c[pds_medchal$pds_sex_c == '0' & pds_medchal$child_pubcat_score_c <= '7' & pds_medchal$pds_5f_c == '4'] <- "Late Pubertal"
pds_medchal$pubcat_female_c[pds_medchal$pds_sex_c == '0' & pds_medchal$child_pubcat_score_c == '8' & pds_medchal$pds_5f_c == '4'] <- "Postpubertal"


### Assign Male Puberty Category Labels ------------------------------------------------
#   Prepubertal = score of 3
#   Early Pubertal = score of 4 or 5 and no 3-pt responses
#   Midpubertal = score of 6-8 and no 4-pt responses
#   Late Pubertal = score of 9-11
#   Postpubertal = score of 12
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p <= '3' & pds_medchal$child_pubcat_score_p != '0' & ! is.na(pds_medchal$child_pubcat_score_p)] <- "Prepubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '4' & pds_medchal$pds_2_p != '3' & pds_medchal$pds_4m_p != '3' & ! pds_medchal$pds_5m_p != '3'] <- "Early Pubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '5' & pds_medchal$pds_2_p != '3' & pds_medchal$pds_4m_p != '3' & ! pds_medchal$pds_5m_p != '3'] <- "Early Pubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '6' & pds_medchal$pds_2_p != '4' & pds_medchal$pds_4m_p != '4' & ! pds_medchal$pds_5m_p != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '7' & pds_medchal$pds_2_p != '4' & pds_medchal$pds_4m_p != '4' & ! pds_medchal$pds_5m_p != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '8' & pds_medchal$pds_2_p != '4' & pds_medchal$pds_4m_p != '4' & ! pds_medchal$pds_5m_p != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '9'] <- "Late Pubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '10'] <- "Late Pubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '11'] <- "Late Pubertal"
pds_medchal$pubcat_male_p[pds_medchal$pds_sex_p == '1' & pds_medchal$child_pubcat_score_p == '12'] <- "Postpubertal"

pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c <= '3' & pds_medchal$child_pubcat_score_c != '0' & ! is.na(pds_medchal$child_pubcat_score_c)] <- "Prepubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '4' & pds_medchal$pds_2_c != '3' & pds_medchal$pds_4m_c != '3' & ! pds_medchal$pds_5m_c != '3'] <- "Early Pubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '5' & pds_medchal$pds_2_c != '3' & pds_medchal$pds_4m_c != '3' & ! pds_medchal$pds_5m_c != '3'] <- "Early Pubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '6' & pds_medchal$pds_2_c != '4' & pds_medchal$pds_4m_c != '4' & ! pds_medchal$pds_5m_c != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '7' & pds_medchal$pds_2_c != '4' & pds_medchal$pds_4m_c != '4' & ! pds_medchal$pds_5m_c != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '8' & pds_medchal$pds_2_c != '4' & pds_medchal$pds_4m_c != '4' & ! pds_medchal$pds_5m_c != '4'] <- "Midpubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '9'] <- "Late Pubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '10'] <- "Late Pubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '11'] <- "Late Pubertal"
pds_medchal$pubcat_male_c[pds_medchal$pds_sex_c == '1' & pds_medchal$child_pubcat_score_c == '12'] <- "Postpubertal"

write.csv(pds_medchal, 'data_medchal/ADHD Med Challenge PDS-Tanner Summary.csv')

### Finally, assign a label to discrepant participants according to the following instructions:
# To resolve discrepancies (i.e., when a male participant receives a puberty category score of 4 or 5 but does have a 3-pt response):
#   (1) Look at the three questions involved in creating the puberty category score (bodyhair, voice, facialhair). If none of those three questions disqualify them for the category, keep at the same category.
#   (2) If one of the three questions do disqualify them, look at the age of the participant and the other questions to evaluate. Participants are not automatically bumped up to the next category level - males tend to over estimate puberty progression. [adapted from Crockett, 1988, unpublished manuscript]




### Analyses of ADHD Med Challenge Data ------------------------------------------------
pds_mcfinal <- read_excel('data_medchal/ADHD Med Challenge PDS-Tanner Summary - Final.xlsx')

pds_mcfinal$pubcat_p <- paste(pds_mcfinal$pubcat_female_p, pds_mcfinal$pubcat_male_p)
pds_mcfinal$pubcat_p[pds_mcfinal$pubcat_p == 'NA NA'] <- NA
pds_mcfinal$pubcat_p <- str_remove(pds_mcfinal$pubcat_p, ' NA')
pds_mcfinal$pubcat_p <- str_remove(pds_mcfinal$pubcat_p, 'NA ')


pds_mcfinal$pubcat_c <- paste(pds_mcfinal$pubcat_female_c, pds_mcfinal$pubcat_male_c)
pds_mcfinal$pubcat_c[pds_mcfinal$pubcat_c == 'NA NA'] <- NA
pds_mcfinal$pubcat_c <- str_remove(pds_mcfinal$pubcat_c, ' NA')
pds_mcfinal$pubcat_c <- str_remove(pds_mcfinal$pubcat_c, 'NA ')


#group_means_p <- pds_mcfinal %>%
#  group_by(pubcat_p, pds_sex_p) %>%
#  summarise(mean_pds = mean(childpds_p))


pds_mcfinal$pubcat_p <- factor(pds_mcfinal$pubcat_p, levels = c('Prepubertal', 'Early Pubertal',
                                                                'Midpubertal', 'Late Pubertal'))

pds_mcfinal$pubcat_c <- factor(pds_mcfinal$pubcat_c, levels = c('Prepubertal', 'Early Pubertal',
                                                                'Midpubertal', 'Late Pubertal'))


### Plots ------------------------------------------------------------------------------
pds_mcfinal$female_pds_score_p <- ifelse(pds_mcfinal$pds_sex_p == '0', pds_mcfinal$childpds_p, NA)
pds_mcfinal$male_pds_score_p <- ifelse(pds_mcfinal$pds_sex_p == '1', pds_mcfinal$childpds_p, NA)
pds_mcfinal$female_pds_score_c <- ifelse(pds_mcfinal$pds_sex_c == '0', pds_mcfinal$childpds_c, NA)
pds_mcfinal$male_pds_score_c <- ifelse(pds_mcfinal$pds_sex_c == '1', pds_mcfinal$childpds_c, NA)

cor(pds_mcfinal$age, pds_mcfinal$female_pds_score_p, use = "pairwise.complete.obs")
cor(pds_mcfinal$age, pds_mcfinal$male_pds_score_p, use = "pairwise.complete.obs")
cor(pds_mcfinal$age, pds_mcfinal$female_pds_score_c, use = "pairwise.complete.obs")
cor(pds_mcfinal$age, pds_mcfinal$male_pds_score_c, use = "pairwise.complete.obs")

summary(lm(age ~ female_pds_score_p, data = pds_mcfinal))
summary(lm(age ~ male_pds_score_p, data = pds_mcfinal))
summary(lm(age ~ female_pds_score_c, data = pds_mcfinal))
summary(lm(age ~ male_pds_score_c, data = pds_mcfinal))


ggplot(data = pds_mcfinal) +
  geom_point(aes(x = male_pds_score_p, y = age, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = male_pds_score_p, y = age, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = female_pds_score_p, y = age, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = female_pds_score_p, y = age, colour = 'Girls'), method = 'lm') +
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale Score (Parent-Report)') + ylab('Age') + theme_bw()

ggplot(data = subset(pds_mcfinal, !is.na(pubcat_p)), aes(x = pubcat_p, y = childpds_p, fill = pds_sex_p)) +
  geom_boxplot() +
  xlab('Pubertal Category (Parent-Report)') + ylab('PDS Score') +
  scale_fill_discrete(name = 'Sex') + theme_bw()


ggplot(data = pds_mcfinal) +
  geom_point(aes(x = male_pds_score_c, y = age, colour = 'Boys'), size = 0.8) +
  geom_smooth(aes(x = male_pds_score_c, y = age, colour = 'Boys'), method = 'lm') +
  geom_point(aes(x = female_pds_score_c, y = age, colour = 'Girls'), size = 0.8) +
  geom_smooth(aes(x = female_pds_score_c, y = age, colour = 'Girls'), method = 'lm') +
  scale_colour_manual(name = 'Sex', values = c(Boys = 'blue', Girls = 'red')) +
  xlab('Pubertal Development Scale Score (Child-Report)') + ylab('Age') + theme_bw()

ggplot(data = subset(pds_mcfinal, !is.na(pubcat_c)), aes(x = pubcat_c, y = childpds_c, fill = pds_sex_c)) +
  geom_boxplot() +
  xlab('Pubertal Category (Child-Report)') + ylab('PDS Score') +
  scale_fill_discrete(name = 'Sex') + theme_bw()


### t-tests ----------------------------------------------------------------------------
## Separating PDS Scores According to Pubertal Stage -----------------------------------
# Both boys and girls ------------------------------------------------------------------
pds_mcfinal$child_pds_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal', pds_mcfinal$childpds_p, NA)
pds_mcfinal$child_pds_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal', pds_mcfinal$childpds_p, NA)
pds_mcfinal$child_pds_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal', pds_mcfinal$childpds_p, NA)
pds_mcfinal$child_pds_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal', pds_mcfinal$childpds_p, NA)

pds_mcfinal$child_pds_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal', pds_mcfinal$childpds_c, NA)
pds_mcfinal$child_pds_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal', pds_mcfinal$childpds_c, NA)
pds_mcfinal$child_pds_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal', pds_mcfinal$childpds_c, NA)
pds_mcfinal$child_pds_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal', pds_mcfinal$childpds_c, NA)

# Boys Only  ---------------------------------------------------------------------------
pds_mcfinal$boys_pds_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal', pds_mcfinal$male_pds_score_p, NA)
pds_mcfinal$boys_pds_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal', pds_mcfinal$male_pds_score_p, NA)
pds_mcfinal$boys_pds_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal', pds_mcfinal$male_pds_score_p, NA)
pds_mcfinal$boys_pds_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal', pds_mcfinal$male_pds_score_p, NA)

pds_mcfinal$boys_pds_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal', pds_mcfinal$male_pds_score_c, NA)
pds_mcfinal$boys_pds_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal', pds_mcfinal$male_pds_score_c, NA)
pds_mcfinal$boys_pds_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal', pds_mcfinal$male_pds_score_c, NA)
pds_mcfinal$boys_pds_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal', pds_mcfinal$male_pds_score_c, NA)

# Girls Only ---------------------------------------------------------------------------
pds_mcfinal$girls_pds_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal', pds_mcfinal$female_pds_score_p, NA)
pds_mcfinal$girls_pds_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal', pds_mcfinal$female_pds_score_p, NA)
pds_mcfinal$girls_pds_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal', pds_mcfinal$female_pds_score_p, NA)
pds_mcfinal$girls_pds_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal', pds_mcfinal$female_pds_score_p, NA)

pds_mcfinal$girls_pds_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal', pds_mcfinal$female_pds_score_c, NA)
pds_mcfinal$girls_pds_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal', pds_mcfinal$female_pds_score_c, NA)
pds_mcfinal$girls_pds_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal', pds_mcfinal$female_pds_score_c, NA)
pds_mcfinal$girls_pds_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal', pds_mcfinal$female_pds_score_c, NA)


## Separating Age According to Pubertal Stage ------------------------------------------
# Both boys and girls ------------------------------------------------------------------
pds_mcfinal$child_age_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal', pds_mcfinal$age, NA)

pds_mcfinal$child_age_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal', pds_mcfinal$age, NA)
pds_mcfinal$child_age_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal', pds_mcfinal$age, NA)

# Boys Only ----------------------------------------------------------------------------
pds_mcfinal$boys_age_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal' & pds_mcfinal$pds_sex_p == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal' & pds_mcfinal$pds_sex_p == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal' & pds_mcfinal$pds_sex_p == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal' & pds_mcfinal$pds_sex_p == '1', pds_mcfinal$age, NA)

pds_mcfinal$boys_age_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal' & pds_mcfinal$pds_sex_c == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal' & pds_mcfinal$pds_sex_c == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal' & pds_mcfinal$pds_sex_c == '1', pds_mcfinal$age, NA)
pds_mcfinal$boys_age_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal' & pds_mcfinal$pds_sex_c == '1', pds_mcfinal$age, NA)

# Girls Only ---------------------------------------------------------------------------
pds_mcfinal$girls_age_stage1_p <- ifelse(pds_mcfinal$pubcat_p == 'Prepubertal' & pds_mcfinal$pds_sex_p == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage2_p <- ifelse(pds_mcfinal$pubcat_p == 'Early Pubertal' & pds_mcfinal$pds_sex_p == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage3_p <- ifelse(pds_mcfinal$pubcat_p == 'Midpubertal' & pds_mcfinal$pds_sex_p == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage4_p <- ifelse(pds_mcfinal$pubcat_p == 'Late Pubertal' & pds_mcfinal$pds_sex_p == '0', pds_mcfinal$age, NA)

pds_mcfinal$girls_age_stage1_c <- ifelse(pds_mcfinal$pubcat_c == 'Prepubertal' & pds_mcfinal$pds_sex_c == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage2_c <- ifelse(pds_mcfinal$pubcat_c == 'Early Pubertal' & pds_mcfinal$pds_sex_c == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage3_c <- ifelse(pds_mcfinal$pubcat_c == 'Midpubertal' & pds_mcfinal$pds_sex_c == '0', pds_mcfinal$age, NA)
pds_mcfinal$girls_age_stage4_c <- ifelse(pds_mcfinal$pubcat_c == 'Late Pubertal' & pds_mcfinal$pds_sex_c == '0', pds_mcfinal$age, NA)


### Running the t-tests ----------------------------------------------------------------
## Comparison of PDS Score across Pubertal Stages --------------------------------------
# Both boys and girls ------------------------------------------------------------------
t.test(pds_mcfinal$child_pds_stage1_p, pds_mcfinal$child_pds_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_pds_stage2_p, pds_mcfinal$child_pds_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_pds_stage3_p, pds_mcfinal$child_pds_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$child_pds_stage1_c, pds_mcfinal$child_pds_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_pds_stage2_c, pds_mcfinal$child_pds_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_pds_stage3_c, pds_mcfinal$child_pds_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)


# Boys only ----------------------------------------------------------------------------
t.test(pds_mcfinal$boys_pds_stage1_p, pds_mcfinal$boys_pds_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$boys_pds_stage2_p, pds_mcfinal$boys_pds_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
#t.test(pds_mcfinal$boys_pds_stage3_p, pds_mcfinal$boys_pds_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$boys_pds_stage1_c, pds_mcfinal$boys_pds_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$boys_pds_stage2_c, pds_mcfinal$boys_pds_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
#t.test(pds_mcfinal$boys_pds_stage3_c, pds_mcfinal$boys_pds_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)


# Girls only ----------------------------------------------------------------------------
t.test(pds_mcfinal$girls_pds_stage1_p, pds_mcfinal$girls_pds_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_pds_stage2_p, pds_mcfinal$girls_pds_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_pds_stage3_p, pds_mcfinal$girls_pds_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$girls_pds_stage1_c, pds_mcfinal$girls_pds_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_pds_stage2_c, pds_mcfinal$girls_pds_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_pds_stage3_c, pds_mcfinal$girls_pds_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)


## Comparison of Age across Pubertal Stages --------------------------------------------
# Both boys and girls ------------------------------------------------------------------
t.test(pds_mcfinal$child_age_stage1_p, pds_mcfinal$child_age_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_age_stage2_p, pds_mcfinal$child_age_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_age_stage3_p, pds_mcfinal$child_age_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$child_age_stage1_c, pds_mcfinal$child_age_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_age_stage2_c, pds_mcfinal$child_age_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$child_age_stage3_c, pds_mcfinal$child_age_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)


# Boys only ----------------------------------------------------------------------------
t.test(pds_mcfinal$boys_age_stage1_p, pds_mcfinal$boys_age_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$boys_age_stage2_p, pds_mcfinal$boys_age_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
#t.test(pds_mcfinal$boys_age_stage3_p, pds_mcfinal$boys_age_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$boys_age_stage1_c, pds_mcfinal$boys_age_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$boys_age_stage2_c, pds_mcfinal$boys_age_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
#t.test(pds_mcfinal$boys_age_stage3_c, pds_mcfinal$boys_age_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)


# Girls only ----------------------------------------------------------------------------
t.test(pds_mcfinal$girls_age_stage1_p, pds_mcfinal$girls_age_stage2_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_age_stage2_p, pds_mcfinal$girls_age_stage3_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_age_stage3_p, pds_mcfinal$girls_age_stage4_p, var.equal = TRUE, paired = FALSE, na.action = na.omit)

t.test(pds_mcfinal$girls_age_stage1_c, pds_mcfinal$girls_age_stage2_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_age_stage2_c, pds_mcfinal$girls_age_stage3_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)
t.test(pds_mcfinal$girls_age_stage3_c, pds_mcfinal$girls_age_stage4_c, var.equal = TRUE, paired = FALSE, na.action = na.omit)

