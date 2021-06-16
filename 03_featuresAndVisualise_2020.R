# This is the new ONE
# The COVID-19 lockdown one

# feature engineering

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)


# User defined functions --------------------------------------------------

#' preProc1
#' Initial data pre-processing, that's cyclic encoding of dates and reducing factorial levels
#' @param df 
#' The input dataframe
#' @return the pre-processed dataframe
preProc01_OLD <- function(df) {
  if ('country_destination' %in% names(df)) {
    df1 <- df %>%
      mutate(across(where(is.factor) & !'country_destination', ~ fct_lump(.x, prop=0.05)))
  }
  else {
    df1 <- df %>%
      mutate(across(where(is.factor), ~ fct_lump(.x, prop=0.1)))
  }
  df2 <- df1 %>%
    bind_cols(
      cyclic_encoding(df1$date_account_created, 'month') %>% as_tibble() %>%
        rename(create_month_sin = sin.month, create_month_cos = cos.month)) %>%
    bind_cols(
      cyclic_encoding(df1$date_first_active, 'month') %>% as_tibble() %>%
        rename(active_month_sin = sin.month, active_month_cos = cos.month))
  df3 <- df2 %>%
    select(-date_account_created, -date_first_active)
  return(df3)
}


#' preProc1
#' Initial data pre-processing, that's cyclic encoding of dates and reducing factorial levels
#' @param df 
#' The input dataframe
#' @return the pre-processed dataframe
#' @todo This function is not good as it combines the test & train data together
#' I did this mainly because of mismatch between factor levels in train & test data
preProc01 <- function(train_df, test_df) {
  df_list <- list(train=train_df, test=test_df)
  df_combined <- bind_rows(df_list, .id = 'data_set')
  df1 <- df_combined %>%
    mutate(across(where(is.factor) & !'country_destination', ~ fct_lump(.x, prop=0.05))) %>%
    bind_cols(
      cyclic_encoding(df_combined$date_account_created, 'month') %>% as_tibble() %>%
        rename(create_month_sin = sin.month, create_month_cos = cos.month)) %>%
    bind_cols(
      cyclic_encoding(df_combined$date_first_active, 'month') %>% as_tibble() %>%
        rename(active_month_sin = sin.month, active_month_cos = cos.month))

  df3 <- df1 %>%
    select(-date_account_created, -date_first_booking)
  df_splitted <- split(df3, df3$data_set)
  df_splitted$train <- select(df_splitted$train, -data_set)
  df_splitted$test <- select(df_splitted$test, -data_set)
  return(df_splitted)
}


# Load data ---------------------------------------------------------------

load('Scenario_01_with_sessions_train.RData')
load('Scenario_01_with_sessions_test.RData')


# Feature Engineering -----------------------------------------------------

# trying preprocessing scenario 01
# that's 
#    a. reducing factor levels, and
#    b. cycling encoding for the dates
kaggleDataSet <- preProc01(kaggleTrain, kaggleTest)
kaggleTrain01 <- kaggleDataSet[['train']]
kaggleTest01 <- kaggleDataSet[['test']]


skim(kaggleTrain01) 
skim(kaggleTest01) 

save(kaggleTrain01, file = 'KaggleTrain_PreProcSc01_WithSessions.RData')
save(kaggleTest01, file = 'KaggleTest_PreProcSc01_WithSessions.RData')

# Visualization -----------------------------------------------------------

kaggleAll <- bind_rows('Train' = kaggleTrain, 'Test' = kaggleTest, .id = 'dataset') %>%
  mutate(dataset = as.factor(dataset),
         days_to_booking = difftime(date_first_booking, date_account_created, units = 'days'),
         days_to_active = difftime(date_account_created, date_first_active, units = 'days'))
  

# destination country per year-day
kaggleTrain %>%
  mutate(year_account_created = format(date_account_created, "%Y"),
         yearday_account_created = as.numeric(format(date_account_created, "%j"))) %>%
  ggplot(aes(x=yearday_account_created)) +
  geom_area(aes(fill=country_destination), stat='count', position='stack') +
  facet_grid(year_account_created ~ .)

# same but as percentage instead of count
kaggleTrain %>%
  mutate(year_account_created = format(date_account_created, "%Y"),
         yearday_account_created = as.numeric(format(date_account_created, "%j"))) %>%
  filter(country_destination != 'NDF') %>%
  ggplot(aes(x=yearday_account_created)) +
  geom_area(aes(fill=country_destination), stat='count', position='fill') +
  facet_grid(year_account_created ~ .)

kaggleTrain %>%
  filter(country_destination != 'NDF') %>%
  mutate(year_booking = format(date_first_booking, "%Y"),
         yearday_booking = as.numeric(format(date_first_booking, "%j"))) %>%
  ggplot(aes(x=yearday_booking)) +
  geom_area(aes(fill=country_destination), stat='count', position='stack') +
  facet_grid(year_booking ~ .) + 
  scale_fill_brewer(palette = "Paired")

kaggleAll %>%
  ggplot(aes(x=date_first_booking)) +
  geom_area(aes(fill=dataset), stat='count', position='stack')

kaggleAll %>%
  ggplot(aes(x=date_first_active)) +
  geom_area(aes(fill=country_destination), stat='count', position='stack')

ggplot(kaggleAll, aes(as.numeric(days_to_booking))) +
  geom_area(stat = 'bin', aes(fill = dataset)) +
  scale_x_log10()


# Uni-variate plots -------------------------------------------------------

kaggleAll %>% 
  select(id, dataset, is.numeric) %>% 
  select(id, dataset, contains("device_type")) %>% 
  pivot_longer(is.numeric, names_to = 'Feature', values_to = 'Value') %>% 
  filter (Value != 0) %>% 
  ggplot(aes(x = Value, fill = dataset)) +
  facet_wrap(Feature ~ ., scales = "free") +
  geom_histogram(position = "dodge") +
  scale_x_log10()


