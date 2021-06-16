# a redo of reading & basic data munging
# a redo on Jul-2020, during the COVID-19 lockdown

# this is basic exploration of files
# the actual reading is included in the script 03b_features.R

# this file is superceded by 01b_readData_2020.R

library(tidyverse)
library(anytime)

dataPath <- "/home/hnagaty/dataNAS/airbnb/"
dataPath <- '~/data/'

# Reading the users file --------------------------------------------------
trainUsersRaw <- read_csv(paste0(dataPath, "train_users_2.csv"),
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

glimpse(trainUsersRaw)
summary(trainUsersRaw) # %>% write.csv('trainUsersSummary.csv')
colSums(is.na(trainUsersRaw)) # %>% write.csv('caCounts.csv')

# Some levels do exist in train data but not in the test data
# I'm saving the levels here, for use with the train data
## DELAY THIS FOR NOW


# Countries file ----------------------------------------------------------
countries <- read_csv(paste0(dataPath, "countries.csv"),
                      col_types = cols(
                        country_destination = col_factor(levels=levels(trainUsersRaw$country_destination),ordered = FALSE, include_na = FALSE),
                        lat_destination = col_double(),
                        lng_destination = col_double(),
                        distance_km = col_double(),
                        destination_km2 = col_double(),
                        destination_language = col_factor(levels=levels(trainUsersRaw$language),ordered = FALSE, include_na = FALSE),
                        #destination_language = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                        language_levenshtein_distance = col_double()))
glimpse(countries)
summary(countries)
countries

# Age Buckets -------------------------------------------------------------
ageBuckets <- read_csv(paste0(dataPath,"age_gender_bkts.csv"),
                       col_types = cols(
                         age_bucket = col_character(),
                         country_destination = col_factor(levels = levels(trainUsersRaw$country_destination)),
                         gender = col_factor(levels = tolower(levels(trainUsersRaw$gender))),
                         population_in_thousands = col_number(),
                         year = col_skip() # it's alawys 2015, so I skipped it
                       ))
glimpse(ageBuckets)
summary(ageBuckets)
ageBuckets

# Sessions ----------------------------------------------------------------
sessions <- read_csv(paste0(dataPath,"sessions.csv"),
                     col_types = cols(
                       user_id = col_character(),
                       action = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                       action_type = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                       action_detail = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                       device_type = col_factor(levels = NULL, ordered = FALSE, include_na = TRUE),
                       secs_elapsed = col_double()),
                     na=c("null","empty"))

glimpse(sessions)
summary(sessions)
str(sessions)
head(sessions)

sessions <- sessions %>%
  filter(user_id!="") %>%
  select(-action_type,-action)
str(sessions)

#summarize values
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
sessionsD <- sessions %>%
  group_by(user_id,device_type) %>%
  summarise(deviceCnt=n()) %>%
  top_n(1,deviceCnt) %>%
  rename(topDevice=device_type)

sessionPerUser <- sessionsA %>%
  inner_join(sessionsB) %>%
  inner_join(sessionsC) 

