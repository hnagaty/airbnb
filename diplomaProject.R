library(tidyverse)
library(mice)

dataPath <- "~/data/airbnb/"
dataPath <- "data/"

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
trainUsersRaw$dataset <- "train"
glimpse(trainUsersRaw)
summary(trainUsersRaw)


# Test Users  --------------------------------------------------------

# Some levels do exist in train data but not in the test data
# I'm adding in them here by hand

levels(trainUsersRaw$language) <- append(levels(trainUsersRaw$language),"-unknown-")
levels(trainUsersRaw$first_browser) <- append(levels(trainUsersRaw$first_browser),c("IBrowse","Nintendo Browser","UC Browser"))
levels(trainUsersRaw$signup_method) <- append(levels(trainUsersRaw$signup_method),"weibo")
levels(trainUsersRaw$signup_flow) <- append(levels(trainUsersRaw$language),c("14","0","25","8","23","12","21"))

testUsersRaw <- read_csv(paste0(dataPath, "test_users.csv"),
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
testUsersRaw$dataset <- "test"
glimpse(testUsersRaw)
summary(testUsersRaw)

# Data munging
# data cleansing

trainUsers <- trainUsersRaw


# It seems that some users typed their birth year instead of the age. The fraction to the total is small.
# Correct the age for values between 2014 & 1924 (assume the reference year is 2014)
trainUsers <- mutate(trainUsers,age=if_else(age>=1924,2014-age,age))

# deal with unreasonable age values
trainUsers$age[trainUsers$age>100] <- NA
trainUsers$age[trainUsers$age<15] <- NA
summary(trainUsers$age)

# in gender, make -unknown- to be NA
summary(trainUsers$gender)
trainUsers$gender[trainUsers$gender=="-unknown-"] <- NA
trainUsers$gender <- droplevels(trainUsers$gender)
summary(trainUsers$gender)

ggplot(trainUsers,aes(x=age)) +
  geom_histogram(aes(y=..density..),col="black",fill="cyan",binwidth = 5) +
  scale_x_continuous(limits = c(15,100)) +
  geom_density(col="red") +
  labs(title="Age Density Distribution",subtitle="After cleansing", y="Density",x="Age") +
  scale_y_continuous(labels = scales::percent)

ggplot(trainUsers,aes(x=age)) +
  geom_histogram(col="white",fill="cyan",binwidth = 5) +
  scale_x_continuous(limits = c(15,100)) +
  labs(title="Age Density Histogram",subtitle="After cleansing & before NA imputation", y="Count",x="Age")

md.pattern(trainUsers, rotate.names = TRUE)

# NA Imputations
# Simple method
# impute missing values in age
ageNACount <- sum(is.na(trainUsers$age))
ageSampleSpace <- trainUsers$age[!is.na(trainUsers$age)]
sampleAge <- sample(ageSampleSpace, size = ageNACount, replace = TRUE)
hist(sampleAge)

trainUsers$age[is.na(trainUsers$age)] <- sampleAge
summary(trainUsers$age)

ggplot(trainUsers,aes(x=age)) +
  geom_histogram(col="white",fill="cyan",binwidth = 5) +
  scale_x_continuous(limits = c(15,100)) +
  labs(title="Age Density Histogram",subtitle="After NA Imputations", y="Count",x="Age")

# impute missing values in gender
genderNACount <- sum(is.na(trainUsers$gender))
genderSampleSpace <- trainUsers$gender[!is.na(trainUsers$gender)]
sampleGender <- sample(genderSampleSpace, size = genderNACount, replace = TRUE)

trainUsers$gender[is.na(trainUsers$gender)] <- sampleGender
summary(trainUsers$gender)

write_csv(trainUsers, "trainUsersAfterSimpleNAImputation.csv")


# Impute with a more sophisticated method

md.pattern(trainUsers, rotate.names = TRUE)
#trainsImputed <- mice(trainUsers, method = "mean") 


#md.pattern(trainsImputed, rotate.names = TRUE)

#pMatrix <- quickpred(trainUsers, exclude = "date_first_booking")

impMethod = c("", "", "", "", "", "pmm", "", "", "", "", "", "", "", "", "", "", "")
impMethod = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm")
imputed <- mice(trainUsers, method=impMethod, m=1, maxit=5)

trainImputed <- complete(imputed)
summary(trainUsers$age)
summary(trainImputed$age)
md.pattern(trainImputed, rotate.names = TRUE)
