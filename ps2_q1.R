# Stats 506, Fall 2019
# Problem Set 2, Question 1
#
# This script contains point estimate and the confidence intervial estimate
# for temperature in RECS survey data. 
#
# Author: Cali Li
# Uniqname: lwenjing
# Date: 10/11/2019

#80: ---------------------------------------------------------------------------

# load packages: -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)

# read or load data: -----------------------------------------------------------

recs = readr::read_delim('C:/Users/wenji/Downloads/recs2015_public_v4.csv',
                         delim=',')

# Question 1.a: ----------------------------------------------------------------
# the national average home temperature at night in winter
# (among homes that use space heating).
# variable in data: TEMPHOME == 1 ---> use space heating
#                   TEMPNITE != -2 ---> Winter temperature at night

# Mean by final & replicate weights: -------------------------------------------
mean_replicate = recs %>%
  filter(HEATHOME == 1 & TEMPNITE != -2) %>%
  gather(key = 'replicate', value = 'BRRWT', BRRWT1:BRRWT96) %>%
  select(replicate, BRRWT, TEMPNITE, NWEIGHT) %>%
  group_by(replicate) %>%
  mutate( means_rep = TEMPNITE * BRRWT / sum(BRRWT),
          means_tot = TEMPNITE * NWEIGHT / sum(NWEIGHT)) %>%
  ungroup() %>%
  summarize( std_err = sqrt( sum( {means_rep - means_tot}^2 ) *4/96 ),
             lwr = sum(means_tot) - qnorm(.975)*std_err,
             upr = sum(means_tot) + qnorm(.975)*std_err)

# Question 1.b: ----------------------------------------------------------------
# Variable in data: DIVISION ---> Census Division
#                  UATYP10 ---> Census 2010 Urban Type
#                  TEMPHOME == 1 ---> use space heating
#                  FUELHEAT ---> Main space heating fuel
# Define function:

## division
decode_division = function(x){
  # Decodes numeric codes to division labels
  #
  # Args: 
  #   x: a single numeric code for a DIVISION
  #
  # Returns: The division label
  
  # Throw an error if x isn't numeric
  if(!is.numeric(x) | x < 1) stop('decode_division expects numeric input 
                                  indexed from 1!')
  
  switch(x,
         "NewEng", "MidAtlan", "ENCen",
         "WNCentral", "SoutAtlan", "ESCen",
         "WSCentral", "MountNth", "MountSth",
         "Pacific"
  )
}
decode_all_division = function(x){
  # Vectorizes decode_division above
  #
  # Args: 
  #  x: a vector of integer-valued division
  #
  # Returns: A vector of division labes or a "list" if some are unmatched.
  sapply(x, decode_division)
}

## urban type
decode_uatyp = function(x){
  
  if(x != "U" & x!= "C" & x != "R") stop('Invaild input for decode_uatyp!')
  
  switch(x,
         "U" = "UrbanArea",
         "C" = "UrbanCluster",
         "R" = "Rural"
  )
}
decode_all_uatyp = function(x){
  # Vectorizes decode_uatyp above
  #
  # Args: 
  #  x: a vector of integer-valued uatyp
  #
  # Returns: A vector of uatyp labes or a "list" if some are unmatched.
  sapply(x, decode_uatyp)
}

## fuelheat
decode_fuel = function(x){
  if(!is.numeric(x)) stop('decode_fuel expects numeric input!')
  x = as.character(x)
  switch(x,
         "1" = "NaturalGas",
         "2" = "Propane",
         "3" = "OilKerosene",
         "5" = "Electricity",
         "7" = "Wood",
         "21" = "OtherFuel",
         "-2" = "NotApplicable"
  )
}
decode_all_fuel = function(x){
  # Vectorizes decode_fuel above
  #
  # Args: 
  #  x: a vector of integer-valued heatfuel
  #
  # Returns: A vector of fuel labes or a "list" if some are unmatched.
  sapply(x, decode_fuel)
}

# Using the decode functions above get a table in one-line:
fuel_pro = recs %>% 
  filter(HEATHOME == 1) %>%
  gather(key = 'replicate', value = 'BRRWT', BRRWT1:BRRWT96) %>%
  mutate(division = decode_all_division(DIVISION), 
         uatyp = decode_all_uatyp(UATYP10), 
         fuel = decode_all_fuel(FUELHEAT)) %>%
  select(division, uatyp, fuel, heathome = HEATHOME, 
         weight = NWEIGHT, replicate, BRRWT) %>%
  replace_na(list(weight = 0)) %>%
  group_by(division, uatyp, fuel, replicate) %>%
  summarize(repweight = sum(BRRWT), fuelpro = sum(weight)) %>%
  group_by(division, uatyp, replicate) %>%
  mutate(reppro = 100 * repweight / sum(repweight), 
         pro = 100 * fuelpro / sum(fuelpro)) %>%
  group_by(division, uatyp, fuel)  %>%
  summarize(pro = pro[1],
            std_err =  sqrt(mean({reppro - pro}^2)*4/96)) %>%
  mutate(lwr = pro - qnorm(.975)*std_err,
         upr = pro + qnorm(.975)*std_err,
         pro = sprintf("(%f , %f) ; %f",lwr,upr,pro) ) %>%
  select(-std_err, -lwr, -upr) %>%
  tidyr::pivot_wider(names_from = fuel, 
                     values_from = pro) %>%
  knitr::kable(digits = 1, caption = paste('**Table 1.** 
                                           *Proprtion of fuel types by division
                                              & urban type.*',
                                           'Each row shows the fuel type 
                                           distribution for one or more division 
                                           & urban types.'))

# Question 1.c:
# ------------------------------------------------------------------------------
# variable in data: TEMPNITE != -2 ---> Winter temperature at night
#                  TEMPGONE != -2 ---> Winter temperature when no one 
#                                      is home during the day
#                  TEMPHOME != -2 ---> Winter temperature when someone 
#                                      is home during the day

# Input data:
temp_nweights = recs %>% 
  filter(TEMPNITE >= 0 , TEMPGONE >= 0 , TEMPHOME >= 0) %>%
  mutate(division = decode_all_division(DIVISION), 
         uatyp = decode_all_uatyp(UATYP10)) %>%
  select(division, uatyp, tempnite = TEMPNITE, tempgone = TEMPGONE, 
         temphome = TEMPHOME, weight = NWEIGHT) %>%
  group_by(division, uatyp) %>%
  summarize( w4tempnite = sum(tempnite*weight) / sum(weight),
             w4tempgone = sum(tempgone*weight) / sum(weight),
             w4temphome = sum(temphome*weight) / sum(weight)
  )

# Mean by replicate weights & plot:
temp_replicate = recs %>%
  filter(TEMPNITE >= 0 , TEMPGONE >= 0 , TEMPHOME >= 0) %>%
  tidyr::pivot_longer(cols = starts_with('BRRWT'),
                      names_to = 'replicate',
                      values_to = 'BRRWT') %>%
  mutate(division = decode_all_division(DIVISION), 
         uatyp = decode_all_uatyp(UATYP10)) %>%
  select(division, uatyp, tempnite = TEMPNITE, tempgone = TEMPGONE, 
         temphome = TEMPHOME, weight = NWEIGHT,replicate, BRRWT) %>%
  replace_na(list(weight = 0)) %>%
  group_by(division, uatyp,replicate) %>%
  summarize(tempnite_rep = sum(tempnite * BRRWT) / sum(BRRWT),
            tempgone_rep = sum(tempgone * BRRWT) / sum(BRRWT),
            temphome_rep = sum(temphome * BRRWT) / sum(BRRWT)
  ) %>%
  ungroup() %>%
  left_join(temp_nweights, by = c('division', 'uatyp')) %>%
  group_by(division, uatyp) %>%
  summarize(w4tempnite = w4tempnite[1],                      # Compute se.
            w4tempgone = w4tempgone[1],
            w4temphome = w4temphome[1],
            std_err4tempnite = sqrt( sum( {tempnite_rep - w4tempnite}^2 )*4/96 ),
            std_err4tempgone = sqrt( sum( {tempgone_rep - w4tempgone}^2 )*4/96 ),
            std_err4temphome = sqrt( sum( {temphome_rep - w4temphome}^2 )*4/96 )
  ) %>%
  mutate(ci4tempnite = qnorm(.975)*std_err4tempnite,
         ci4tempgone = qnorm(.975)*std_err4tempgone,
         ci4temphome = qnorm(.975)*std_err4temphome
  ) %>%
  select(-std_err4tempnite, -std_err4temphome, -std_err4tempgone) %>%
  tidyr::pivot_longer( cols = starts_with('w4'),
                       names_to = 'temp_type',
                       values_to = 'mean_temp',
  ) %>%
  mutate(ci = case_when(temp_type == 'w4tempnite' ~ ci4tempnite,
                        temp_type == 'w4tempgone' ~ ci4tempgone,
                        temp_type == 'w4temphome' ~ ci4temphome)) %>%
  ggplot(aes(x = uatyp, y = mean_temp, fill = temp_type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_errorbar(aes(ymin = mean_temp - ci, ymax = mean_temp + ci), 
                width = 0.5, position = position_dodge(0.9)) +
  facet_wrap(~ division, nrow = 2) + xlab('Urban Type') + 
  ylab('Temperature') + 
  ggtitle('Average Winter Home Temperatures') +
  theme_light()

# Question 1.d: ----------------------------------------------------------------
# Variable for data: EQUIPMUSE ---> Main heating equipment household behavior 
#                                 (1,9) -9 = NA
#                   TEMPNITE != -2 ---> Winter temperature at night
#                   TEMPHOME != -2 ---> Winter temperature when someone 
#                                       is at home during the day
#                   NWEIGHT ---> Final sample weight

## behavior type
decode_equipmuse = function(x) {
  if(!as.numeric(x)) stop('Invaild input for decode_equipmuse!')
  x = as.character(x)
  switch(x,
         "1" = "Set one temperature and leave it there most of the time",
         "2" = "Manually adjust the temperature at night 
                or when no one is at home",
         "3" = "Program the thermostat to automatically adjust the temperature 
                during the day and night at certain times",
         "4" = "Turn equipment on or off as needed",
         "5" = "Our household does not have control over the equipment",
         "9" = "Other"
  )
}
decode_all_equipmuse = function(x) {
  # Vectorizes decode_equipmuse above
  #
  # Args: 
  #  x: a vector of integer-valued equipmuse
  #
  # Returns: A vector of equipmuse labes or a "list" if some are unmatched.
  sapply(x, decode_equipmuse)
}

## Function median:
my_median = function(x,y) {
  x = sort(x)
  i = 1
  cumu = 0
  while (i <= length(x)) {
    cumu = cumu + x[i]
    if (cumu/sum(x) >= 0.5) {
      return(y[i])
    }
    else {
      i = i + 1
    }
  }
}

# Median for weighted temp difference:
temp_median = recs %>% 
  filter(TEMPNITE >= 0 , EQUIPMUSE >= 0 , TEMPHOME >= 0) %>%
  mutate(equipuse = decode_all_equipmuse(EQUIPMUSE)) %>%
  select(TEMPHOME, TEMPNITE, weight = NWEIGHT, equipuse) %>%
  group_by(equipuse) %>%
  summarize(mean_median = my_median(weight, (TEMPHOME - TEMPNITE)))

repl_median = recs %>% 
  filter(TEMPNITE >= 0 , EQUIPMUSE >= 0 , TEMPHOME >= 0) %>%
  gather(key = 'replicate', value = 'BRRWT', BRRWT1:BRRWT96) %>%
  mutate(equipuse = decode_all_equipmuse(EQUIPMUSE)) %>%
  select(TEMPHOME, TEMPNITE, BRRWT, replicate, equipuse) %>%
  group_by(equipuse,replicate) %>%
  summarize(rep_median = my_median(BRRWT, (TEMPHOME - TEMPNITE))) %>%
  ungroup() %>%
  left_join(temp_median, by = c('equipuse')) %>%
  group_by(equipuse) %>%
  summarize( mean_median = mean_median[1],
             std_err =  sqrt( mean( {rep_median - mean_median}^2 )*4/96 )) %>%
  mutate( lwr = mean_median - qnorm(.975)*std_err,
          upr = mean_median + qnorm(.975)*std_err
  ) %>%
  select(-std_err) %>%
  knitr::kable(digits = 1, caption = paste('**Table 2.** 
                                    * Median difference between the daytime 
                                      (with someone home) 
                                      and nighttime temperatures for 
                                      each level of"main heating equipment 
                                      household behavior".*'), 
               col.names = c('Main Heating Equipment Household Behavior', 
                             'the Mean Difference between the daytime 
                             and the nighttime temperatures', 
                             'Lower Bound', 'Upper Bound'))
