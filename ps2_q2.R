# Stats 506, Fall 2019
# Problem Set 2, Question 2
#
# This script contains curve measurements for a new data set KH2017_raw.
#
# Author: Cali Li
# Uniqname: lwenjing
# Date: 10/11/2019

#80: ---------------------------------------------------------------------------

# load packages: ---------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(mousetrap)

# read or load data: -----------------------------------------------------------

 
# Data:
kh2017 <- KH2017_raw
# Question 2.a: ----------------------------------------------------------------
source('ps2_q2_funcs.R')

# Question 2.b: ----------------------------------------------------------------
trans = function(dat) {
  dat = as_tibble(dat)
  dat = select(dat,subject_nr,count_trial,xpos_get_response, 
               ypos_get_response, timestamps_get_response)
  x = as.list(as.character(dat$xpos_get_response))
  x = str_sub(x,start = 2, end = (str_length(x)-1))
  x = str_split(x,', ')
  x = lapply(x, FUN = as.numeric)
  #similar for y and t
  y = as.list(as.character(dat$ypos_get_response))
  y = str_sub(y,start = 2, end = (str_length(y)-1))
  y = str_split(y,', ')
  y = lapply(y, FUN = as.numeric)
  t = as.list(as.character(dat$timestamps_get_response))
  t = str_sub(t,start = 2, end = (str_length(t)-1))
  t = str_split(t,', ')
  t = lapply(t, FUN = as.numeric)
  dattran = transmute(dat,subject_nr,count_trial,x,y,t)
  return(dattran)
}
recs = trans(kh2017)

# Question 2.c: ----------------------------------------------------------------
recsq2c <- filter(kh2017,correct == 1)
recsq2ctrans = trans(recsq2c)
n = nrow(recsq2c)
result1 = matrix(data = NA, nrow=n, ncol = 1)
result2 = matrix(data = NA, nrow=n, ncol = 1)
result3 = matrix(data = NA, nrow=n, ncol = 1)
result4 = matrix(data = NA, nrow=n, ncol = 1)

for (i in 1:n) {
  mat = cbind(unlist(recsq2ctrans$x[i]),
              unlist(recsq2ctrans$y[i]),
              unlist(recsq2ctrans$t[i]))
  mat = normalize_traj(mat)
  result1[i] = comp_dist(mat[,1],mat[,2])
  result2[i] = max( abs(mat[, 2]) )
  result3[i] = mean( abs(mat[, 2]) )
  result4[i] = comp_auc(mat[,1],mat[,2])
}
result4q2c = transmute(recsq2ctrans,subject_nr,count_trial, 
                  Condition = recsq2c$Condition,
                  Exemplar = recsq2c$Exemplar,
                  tot_dist = result1,
                  max_abs_dev = result2,
                  avg_abs_dev = result3,
                  AUC = result4)

cap = '**Table 3.** 
      *Measures of curvature computed from the KH2017_raw data set.*'
knitr::kable(result4q2c, digits = 1, caption = cap)

# Question 2.d: ----------------------------------------------------------------
(fit0 = lmer(log(tot_dist) ~ Condition + (1 | subject_nr) + (1 | Exemplar), 
             data = result4q2c))
summary0 <- summary(fit0)
s0 = summary0$coefficients
summaryintotal0 = c(estimate = s0[,1],lower = s0[,1] - 2 * s0[,2],
                    upper = s0[,1] + 2 * s0[,2])
summaryintotal0 = matrix(summaryintotal0, ncol = 3, nrow = 2,
                         dimnames = list(c('Intercept','Condition'),
                                       c('Estimate_tot_dist', 'Lower_tot_dist',
                                         'Upper_tot_dist')))


(fit1 = lmer(log(max_abs_dev) ~ Condition + (1 | subject_nr) + (1 | Exemplar),
             data = result4q2c))
summary1 <- summary(fit1)
s1 = summary1$coefficients
summaryintotal1 = c(estimate = s1[,1],lower = s1[,1] - 2 * s1[,2],
                    upper = s1[,1] + 2 * s1[,2])
summaryintotal1 = matrix(summaryintotal1,ncol = 3, nrow = 2,
                      dimnames = list(c('Intercept','Condition'),
                                      c('Estimate_max_abs_dev',
                                        'Lower_max_abs_dev',
                                        'Upper_max_abs_dev')))

(fit2 = lmer(log(avg_abs_dev) ~ Condition + (1 | subject_nr) + (1 | Exemplar), 
             data = result4q2c))
summary2 <- summary(fit2)
s2 = summary2$coefficients
summaryintotal2 = c(estimate = s2[,1],lower = s2[,1] - 2 * s2[,2],
                    upper = s2[,1] + 2 * s2[,2])
summaryintotal2 = matrix(summaryintotal2, ncol = 3, nrow = 2,
                      dimnames = list(c('Intercept','Condition'),
                                      c('Estimate_avg_abs_dev',
                                        'Lower_avg_abs_dev',
                                        'Upper_avg_abs_dev')))

(fit3 = lmer(log(AUC) ~ Condition + (1 | subject_nr) + (1 | Exemplar), 
             data = result4q2c))
summary3 <- summary(fit3)
s3 = summary3$coefficients
summaryintotal3 = c(estimate = s3[,1], lower = s3[,1] - 2 * s3[,2],
                    upper = s3[,1] + 2 * s3[,2])
summaryintotal3 = matrix(summaryintotal3, ncol = 3, nrow = 2,
                      dimnames = list(c('Intercept','Condition'),
                                      c('Estimate_AUC','Lower_AUC','Upper_AUC')))

sumary <- cbind(summaryintotal0, summaryintotal1, 
                summaryintotal2, summaryintotal3)
cap = '**Table 4.** 
      *Fitness of different models per curvature measure with "Condition".*'
knitr::kable(sumary, digits = 1, caption = cap)

# Question for 2.d: ------------------------------------------------------------
# Which one does condition have the largest (relative) effect?
# From the summary above, we can find that 
# avg_abs_dev has the largest relative effect.
# Since the coefficient of Condition in that model has largest abs value.
# The larger departure from 0 means has bigger effect.
# The same for confidence intervial.
# The CI for the third model has the largest departure from 0.