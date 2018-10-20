library(readr)
library(dplyr)
library(tidyr)
#library(reshape2)

# Train Users  --------------------------------------------------------
trainUsersRaw <- read_csv("data/train_users_2.csv",
                       col_types = cols(
                         id = col_character(),
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
summary(trainUsersRaw)
trainUsersRaw$dataset <- "train"


# Test Users  --------------------------------------------------------

# Some levels do exist in train data but not in the test data
# I'm adding in them here by hand

levels(trainUsersRaw$language) <- append(levels(trainUsersRaw$language),"-unknown-")
levels(trainUsersRaw$first_browser) <- append(levels(trainUsersRaw$first_browser),c("IBrowse","Nintendo Browser","UC Browser"))
levels(trainUsersRaw$signup_method) <- append(levels(trainUsersRaw$signup_method),"weibo")
levels(trainUsersRaw$signup_flow) <- append(levels(trainUsersRaw$language),c("14","0","25","8","23","12","21"))

testUsersRaw <- read_csv("data/test_users.csv",
                          col_types = cols(
                            id = col_character(),
                            date_account_created = col_date(format = "%Y-%m-%d"),
                            timestamp_first_active = col_character(),
                            date_first_booking = col_date(format = "%Y-%m-%d"),
                            gender = col_factor(levels = levels(trainUsersRaw$gender), ordered = FALSE, include_na = FALSE),
                            age = col_number(),
                            signup_method = col_factor(levels = levels(trainUsersRaw$signup_method), ordered = FALSE, include_na = FALSE),
                            signup_flow = col_factor(levels(trainUsersRaw$signup_flow), ordered = FALSE, include_na = FALSE),
                            language = col_factor(levels = levels(trainUsersRaw$language), ordered = FALSE, include_na = FALSE),
                            affiliate_channel = col_factor(levels = levels(trainUsersRaw$affiliate_channel), ordered = FALSE, include_na = FALSE),
                            affiliate_provider = col_factor(levels = levels(trainUsersRaw$affiliate_provider), ordered = FALSE, include_na = FALSE),
                            first_affiliate_tracked = col_factor(levels = levels(trainUsersRaw$first_affiliate_tracked), ordered = FALSE, include_na = TRUE),
                            signup_app = col_factor(levels = levels(trainUsersRaw$signup_app), ordered = FALSE, include_na = FALSE),
                            first_device_type = col_factor(levels = levels(trainUsersRaw$first_device_type), ordered = FALSE, include_na = FALSE),
                            first_browser = col_factor(levels = levels(trainUsersRaw$first_browser), ordered = FALSE, include_na = FALSE)),
                          na=c("null","empty"))
glimpse(testUsersRaw)
summary(testUsersRaw)
testUsersRaw$dataset <- "test"

# Countries ---------------------------------------------------------------

# I edited the language in countries.csv by hand, to match the same naming of training data
countries <- read_csv("data/countries.csv",
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
ageBuckets <- read_csv("data/age_gender_bkts.csv",
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

sessions <- read_csv("data/sessions.csv",
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


# explore sessions --------------------------------------------------------

count(sessions,user_id,sort=TRUE)
count(sessions,action) %>% arrange(desc(n))
testSession <- filter(sessions,user_id=="mxqbh3ykxl") #sample user
count(sessions,action_type,sort=TRUE)
count(sessions,action_detail,sort=TRUE)


actions <- sessions %>% select(action_type,action_detail,action) %>% distinct()
n_distinct(sessions$action)
n_distinct(sessions$action_type)
n_distinct(sessions$action_detail)

cha <- table(actions$action_detail,actions$action_type) # ==> not mutually exclusive
ch <- as.data.frame(table(actions$action_detail,actions$action))

sum(sessions$user_id=="") # there are some rows with no user_id
sum(is.na(sessions$user_id)) # no NA in user_id


# by visual inspection of above listings, and with fair knowledge of the session actions, I decided to include only the action_detail column.
# Including all columns would be very computationaly expensive. This column alone seems to give adeuqate information.


# summarize & reshape sessions --------------------------------------------

sessions <- sessions %>%
  filter(user_id!="") %>%
  select(-action_type,-action)
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
# I will not use sessionsA & sessionB, because they will yield a huge sparse dataframe ==> computaionally expensive
summary(sessionsC)
summary(sessionsD)

rm(testSession,sessions,sessionsA,sessionsB)
rm(ch,cha)
rm(actions)
