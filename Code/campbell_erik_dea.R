title: "Lab3 Data Exploration"
author: "erik campbell"
date: "`r format(Sys.time(), '%d %B, %Y')`"

library(tidyverse)
library(vtable)
library(purrr)
library(lubridate)
library(fixest)

# RESEACH QUESTION: "Among colleges that predominantly grant bachelor's degrees, did the release of Score Card shift student interest to high-earning
# colleges relative to low-earnings ones?

# Create file path for map function
TrendsUptoFileData <- list.files(path = '../DataExplorationAssignment/Lab3_Rawdata', 
                                 pattern = 'trends_up_', full.names = TRUE)

# Read in google trends data 
TrendsUptoFileData <- map_df(TrendsUptoFileData, read_csv) 

# Transform monthweek and standardize indices relative to themselves. 
TrendsUptoFileData <- TrendsUptoFileData %>%
  mutate(monthorweek = str_sub(TrendsUptoFileData$monthorweek, end=10), monthorweek = ymd(monthorweek), monthorweek = as_date(monthorweek)) %>%
  mutate(monthorweek = floor_date(monthorweek,"month")) %>%   # warning, changed to monthly-level here
  group_by(schname, keyword) %>%
  mutate(standardized_index = (index - mean(index)) /sd(index)) # standardized index, now a one-unit change in standardized index can be understood and interpreted = 1 std deviation change in search interest


# Read in scorecard file                                                 
ScoreCard <- read_csv('../DataExplorationAssignment/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv') %>%
  rename(unitid = UNITID, opeid = OPEID) # 7804 obs

# Read in id_name_link
idNameLink <- read_csv('../DataExplorationAssignment/Lab3_Rawdata//id_name_link.csv')

# Dropping university that share exact name by & calculate n = each school name appears in in id_name_link
idNameLinkNew <- idNameLink %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n==1)

# Join Google trends data to id_name_link data using schname, then join Scorecard using unitid & opoeid
MergedData <- TrendsUptoFileData %>%                        
  inner_join(idNameLinkNew, by = "schname") %>%
  inner_join(ScoreCard, by = c('unitid', 'opeid')) %>%
  na.omit(MergedData)                                                                        

# Create base data set and transform variables of interest
WorkingData <- MergedData %>%
  select('unitid', 'opeid','keynum', 'schname', 'index','standardized_index', 'monthorweek', 'md_earn_wne_p10-REPORTED-EARNINGS', 'CONTROL', 'SAT_AVG', 'LOCALE',
         'SAT_AVG_ALL', 'PREDDEG', 'PCTFLOAN', 'GRAD_DEBT_MDN_SUPP') %>%
  rename(college_name = schname, median_earnings_10 = 'md_earn_wne_p10-REPORTED-EARNINGS', month = monthorweek, sat_avg = SAT_AVG, sat_avg_all= SAT_AVG_ALL,
         school_geography = LOCALE, predominant_degree = PREDDEG, Percent_fed_loan = PCTFLOAN, median_debt_10yrs = GRAD_DEBT_MDN_SUPP, 
         public_private = CONTROL) %>%
  filter(predominant_degree == 3) %>%             #filtering to colleges that primarily give out BS/BA degrees
  filter(median_earnings_10 != 'NULL') %>%
  filter(median_earnings_10 != 'PrivacySuppressed') %>%
  mutate(median_earnings_10 = as.numeric(median_earnings_10))%>%
  mutate(sat_avg = as.numeric(sat_avg)) %>%
  mutate(sat_avg_all = as.numeric(sat_avg_all)) %>%
  mutate(Percent_fed_loan = as.numeric(Percent_fed_loan)) %>%
  mutate(median_debt_10yrs = as.numeric(median_debt_10yrs)) %>%
  mutate(public_private = as.character(public_private)) %>%
  mutate(public_private = case_when(
    public_private == '1' ~ 'public',
    public_private == '2' ~ 'private_non_profit',
    public_private == '3' ~ 'private_for_profit')) %>%
  mutate(school_geography = case_when(
    school_geography == '11' ~ 'city',
    school_geography == '12' ~ 'city',
    school_geography == '13' ~ 'city',
    school_geography == '21' ~ 'Suburb',
    school_geography == '22' ~ 'Suburb',
    school_geography == '23' ~ 'Suburb',
    school_geography == '31' ~ 'town',
    school_geography == '32' ~ 'town',
    school_geography == '33' ~ 'town',
    school_geography == '41' ~ 'rural',
    school_geography == '42' ~ 'rural',
    school_geography == '43' ~ 'rural'))

# Determine low-earning vs. high-earning colleges by graphically.#NOTE: NORMAL W/SLIGHT skewness right and a few outliers
ggplot(WorkingData, aes(x = median_earnings_10)) + 
  geom_histogram(color = 4, fill = 'white', bins = 45) +
  labs(x = 'median_earnings', title = 'HISTOGRAM OF MEDIAN SALARY 10 YEARS POST GRAD')  

# This is the line dividing high earning vs. low earning(median == 42,000). This is the line dividing high earning vs. low earning
summary(WorkingData$median_earnings_10)
median_earnings_threshold <- median(WorkingData$median_earnings_10)  # $42,000
mean_earnings_threshold <- mean(WorkingData$median_earnings_10)   # $43,799.20
sd_earnings_threshold <- sd(WorkingData$median_earnings_10)  # sd $11,607

# Add "high-earning = TRUE" & "low-earning = FALSE" variable Working Data
WorkingData <- WorkingData %>%
  mutate(high_earning_school = median_earnings_10 > median_earnings_threshold)
  
# Break data set into 2 parts, pre-9/1/2015 & post 9/1/2015
PreScoreCardWorking <-WorkingData %>%
  filter(month <= '2015-08-31')

PostScoreCardWorking <- WorkingData %>%
  filter(month >= '2015-09-01')


# Select variables for models comparing same variables both before and after Scorecard go-live
PRE_SC_Model <- PreScoreCardWorking %>%
  select('college_name', month, standardized_index, median_earnings_10, median_debt_10yrs, 'public_private', high_earning_school, sat_avg, Percent_fed_loan, 'school_geography')

POST_SC_MODEL <- PostScoreCardWorking %>%
  select('college_name', month, standardized_index, median_earnings_10, median_debt_10yrs, 'public_private', high_earning_school, sat_avg, Percent_fed_loan, 'school_geography')

################################################################# ANALYSIS ####################################################################################################

# Run Base Binary OLS PRE-Score Card AND plot
basemodel_preScorecard <- lm(median_earnings_10 ~ standardized_index, data = PRE_SC_Model)
basemodel_preScorecard_feols <- feols(median_earnings_10 ~ standardized_index, data = PRE_SC_Model)

ggplot(data= basemodel_preScorecard, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings",
       title = "Basemodel- Student Interest vs. Median Earnings BEFORE ScoreCard-SEP2015") +
  geom_abline(slope = 0, color = "blue") +
  coord_cartesian(xlim = c(-5,13))

ggplot(basemodel_preScorecard) +              # Check for heteroskedasticity
  geom_point(aes(fitted(basemodel_preScorecard), resid(basemodel_preScorecard))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'BASEMODEL BEFORE SCOREDCARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot (data = basemodel_preScorecard) +    # Evaluating the standardized residuals
  geom_histogram(mapping = aes(x= rstandard(basemodel_preScorecard)), bins = 200,) +
  labs(title = "BASEMODEL BEFORE SCOREDCARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))

# run Base Binary OLS POST-Score Card AND plot
basemodel_postScorecard <-lm(median_earnings_10 ~ standardized_index, data = POST_SC_MODEL)
basemodel_postScorecard_feols <-feols(median_earnings_10 ~ standardized_index, data = POST_SC_MODEL)

ggplot(data= basemodel_postScorecard, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings",
       title = "Basemodel- Student Interest vs. Median Earnings AFTER ScoreCard-SEP2015") +
  geom_abline(slope = 0, color = "blue") +
  coord_cartesian(xlim = c(-5,13))

ggplot(basemodel_postScorecard) +              # Check for heteroskedasticity
  geom_point(aes(fitted(basemodel_postScorecard), resid(basemodel_postScorecard))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'BASEMODEL AFTER-SCOREDCARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot (data = basemodel_postScorecard) +    # Evaluating the standardized residuals
  geom_histogram(mapping = aes(x= rstandard(basemodel_postScorecard)), bins = 200,) +
  labs(title = "BASEMODEL AFTER SCOREDCARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))

# Add high_earning_school AND public_private variable for model 1 (pre-scorecard)AND plot

m1 <- lm(median_earnings_10 ~ standardized_index + high_earning_school + public_private, data = PRE_SC_Model)
m1_feols <- feols(median_earnings_10 ~ standardized_index + high_earning_school + public_private, data = PRE_SC_Model, se = "hetero")

ggplot(data= m1, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings", title = "M1- Student Interest vs. Median Earnings Before ScoreCard-SEP2015:") +
  geom_abline(slope = 0, color = "blue") +
  coord_cartesian(xlim = c(-5,13))

ggplot(m1) + # Check for heteroskedasticity
  geom_point(aes(fitted(m1), resid(m1))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'M1 PRESCOREDCARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot(data = m1) +        # Evaluating the standardized residuals
  geom_histogram(mapping = aes(x= rstandard(m1)), bins = 200,) +
  labs(title = "M1 PRESCOREDCARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))


# Add high_earning_school AND public_private variable for model 2 (post-scorecard) AND plot

m2 <- lm(median_earnings_10 ~ standardized_index + high_earning_school + public_private, data = POST_SC_MODEL)
m2_feols <- feols(median_earnings_10 ~ standardized_index + high_earning_school + public_private, data = POST_SC_MODEL, se = "hetero")

wald(m2_feols, c('public_private'))
# Our F-test stat is 87.4 additional explanatory power scaled by 140,260 degrees of freedom and p-value of 2.2e-16.

ggplot(data= m2, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings", title = "M2- Student Interest vs. Median Earnings: AFTER ScoreCard-SEP2015") +
  coord_cartesian(xlim = c(-5,13))

ggplot(m2) + # Check for heteroskedasticity
  geom_point(aes(fitted(m2), resid(m2))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'M2 POSTSCORECARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot (data = m2) +
  geom_histogram(mapping = aes(x= rstandard(m2)), bins = 200,) +
  labs(title = "M2 POSTSCORECARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))

# Add school_geography to model 3 (pre-scorecard) AND plot

m3 <- lm(median_earnings_10 ~ standardized_index + high_earning_school + public_private + school_geography, data = PRE_SC_Model)
m3_feols <- feols(median_earnings_10 ~ standardized_index + high_earning_school + public_private + school_geography, data = PRE_SC_Model, se = "hetero")

wald(m3_feols, c('school_geography'))
# Our F-test stat is 1,520.7 additional explanatory power scaled by 601,325 degrees of freedom and p-value of 2.2e-16.


ggplot(data= m3, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings", title = "M3- Student Interest vs. Median Earnings: Before ScoreCard-SEP2015") +
  coord_cartesian(xlim = c(-5,13))

ggplot(m3) + # Check for heteroskedasticity
  geom_point(aes(fitted(m3), resid(m3))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'M3 PRESCOREDCARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot (data = m3) +
  geom_histogram(mapping = aes(x= rstandard(m3)), bins = 200,) +
  labs(title = "M3 PRESCOREDCARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))


# Add school_geography to model 4 (post-scorecard) AND plot
m4 <- lm(median_earnings_10 ~ standardized_index + high_earning_school + public_private + school_geography, data = POST_SC_MODEL)
m4_feols <- feols(median_earnings_10 ~ standardized_index + high_earning_school + public_private + school_geography, data = POST_SC_MODEL, se = "hetero")

wald(m4_feols, c('school_geography'))
# Our F-test stat is 348.9 additional explanatory power scaled by 140,257 degrees of freedom and p-value of 2.2e-16.

ggplot(m4, aes(x= standardized_index, y = median_earnings_10)) +
  geom_smooth() +
  labs(x = "Standardized Index", y = "Median Earnings", title = "M4- Student Interest vs. Median Earnings: AFTER ScoreCard-SEP2015 ") +
  coord_cartesian(xlim = c(-5,13))

ggplot(m4) + # Check for heteroskedasticity
  geom_point(aes(fitted(m4), resid(m4))) +
  labs(x = 'Fitted', y = 'Residuals', title = 'M4 POSTSCORECARD- HETEROSCKEDASTICITY') +
  geom_abline(slope = 0, color = "blue")

ggplot (data = m4) +
  geom_histogram(mapping = aes(x= rstandard(m4)), bins = 200,) +
  labs(title = "M4 POSTSCORECARD- Histogram of standardized residuals", x = "standardized Residuals", y = "count") +
  coord_cartesian(xlim = c(-3,3))

# review models

etable(basemodel_preScorecard_feols,basemodel_postScorecard_feols,m1_feols,m2_feols,m3_feols, m4_feols)


