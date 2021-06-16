# This is the new ONE
# The COVID-19 lockdown one

# a 2nd version of reading the files
# this is the working one


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(anytime)
library(skimr)

dataPath <- "/home/hnagaty/dataNAS/airbnb/"
dataPath <- '/home/hnagaty/MEGA/myDataScience/airbnb/data/'



# User defined functions ---------------------------------------------------

readData <- function(usersFile, sessionsFile = NULL) {
  users <- read_csv(usersFile,
                    col_types = cols(id = col_character(),
                                     date_account_created = col_date(format = "%Y-%m-%d"),
                                     timestamp_first_active = col_character(),
                                     date_first_booking = col_date(format = "%Y-%m-%d"),
                                     gender = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     age = col_number(),
                                     signup_method = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     signup_flow = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     language = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     affiliate_channel = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     affiliate_provider = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     first_affiliate_tracked = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                                     signup_app = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     first_device_type = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     first_browser = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                                     country_destination = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE)),
                    na=c("null","empty"))
  users <- users %>%
    mutate(date_first_active = anydate(timestamp_first_active)) %>%
    select(-timestamp_first_active)
  
  if (!is.null(sessionsFile)) {
    sessions <- read_csv(sessionsFile,
                         col_types = cols(
                           user_id = col_character(),
                           action = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                           action_type = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                           action_detail = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                           device_type = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                           secs_elapsed = col_double()),
                         na=c("null","empty"))
    sessionsA <- sessions %>%
      group_by(user_id,action_detail) %>%
      summarise(Cnt=n()) %>%
      spread(key=action_detail,value=Cnt,fill=0,sep="-")
    sessionsB <- sessions %>%
      group_by(user_id,device_type) %>%
      summarise(Cnt=n()) %>%
      spread(key=device_type,value=Cnt,fill=0,sep="-")
    sessionsC <- sessions %>%
      group_by(user_id) %>%
      summarise(avgSecs=mean(secs_elapsed,na.rm = TRUE),
                sumSecs=sum(secs_elapsed,na.rm=TRUE),
                cntActions=n())
    usersDf <- users %>%
      inner_join(sessionsA, by=c('id'='user_id')) %>%
      inner_join(sessionsB, by=c('id'='user_id')) %>%
      inner_join(sessionsC, by=c('id'='user_id'))
    return(usersDf)
  }
  else return(users)
}


# Load & then save the results --------------------------------------------

kaggleTrain <- readData(paste0(dataPath, "train_users_2.csv"), paste0(dataPath, "sessions.csv"))
kaggleTest <- readData(paste0(dataPath, "test_users.csv"), paste0(dataPath, "sessions.csv"))

skim(kaggleTrain)
skim(kaggleTest)

countries <- read_csv("countries.csv")

save(kaggleTrain, file = 'Scenario_01_with_sessions_train.RData', compress = 'bzip2')
save(kaggleTest, file = 'Scenario_01_with_sessions_test.RData', compress = 'bzip2')


