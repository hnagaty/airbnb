library(anytime) # to extract date from timestamp
library(hydroTSM) # I use this to extract the season from the date
library(dplyr)
library(ggplot2)

allUsers <- bind_rows(trainUsersRaw,testUsersRaw)
#allUsers <- trainUsersRaw

# gender level clean-up & others
levels(allUsers$gender) <- tolower(levels(allUsers$gender)) #to match the names in the bucket file
levels(allUsers$gender)[levels(allUsers$gender) == "-unknown-"] <- "unknown" #this one looks better :)
# make NA in "first_affiliate_tracked" as a separate category
levels(allUsers$first_affiliate_tracked)[is.na(levels(allUsers$first_affiliate_tracked))] <- "unknown"
levels(allUsers$first_browser)[levels(allUsers$first_browser) == "-unknown-"] <- "unknown" #this one looks better :)



# New features and transformations ----------------------------------------

# age
allUsers <- mutate(allUsers,age=if_else(age>=1924,2014-age,age))
allUsers$age[allUsers$age>104] <- NA
allUsers$age[allUsers$age<15] <- NA
allUsers$ageRange <- cut(allUsers$age,breaks=seq(15,105,5),right=FALSE) #same cuts as in the age buckets file
levels(allUsers$ageRange) <- append(levels(allUsers$ageRange),"unknown")
allUsers$ageRange[is.na(allUsers$ageRange)] <- "unknown"

# Dates
nowDate<- as.Date.character("2016-01-01")
allUsers <- allUsers %>%
  mutate(date_first_active = anydate(timestamp_first_active))
# not sure if below is a good idea
allUsers <- allUsers %>%
  mutate(date_first_booking=if_else(is.na(date_first_booking),nowDate,date_first_booking))
allUsers <- allUsers %>%
  mutate(days_signup_active = as.integer(difftime(date_account_created,date_first_active,units="days")),
         days_book_signup = as.integer(difftime(date_first_booking,date_account_created,units="days")))
allUsers <- allUsers %>%
  mutate(book_month=as.factor(months(date_first_booking)),
         book_season=as.factor(time2season(date_first_booking,out.fmt = "seasons")),
         signup_month=as.factor(months(date_account_created)),
         signup_season=as.factor(time2season(date_account_created,out.fmt = "seasons")))
allUsers <- allUsers %>%
  select(-date_first_active,-timestamp_first_active,-date_account_created,-date_first_booking)
allUsers <- select(allUsers,-days_signup_active)

# Others
allUsers<- allUsers %>%
  mutate(made_booking=as.factor(country_destination!="NDF"))
levels(allUsers$made_booking) <- c("NO","YES")

glimpse(allUsers)
summary(allUsers)

table(allUsers$made_booking,allUsers$dataset,useNA = "ifany") # ==> all NAs are from the test set. Normal

allUsersExpanded <- allUsers %>%
  left_join(sessionsC,by=c("id" = "user_id")) %>%
  left_join(sessionsD,by=c("id" = "user_id")) %>%
  left_join(countries) %>%
  select(-language_levenshtein_distance,-destination_km2,-lng_destination)
summary(allUsersExpanded)
allUsersExpanded <- allUsersExpanded %>%
  mutate(secsPerAction=sumSecs/cntActions)

table(allUsersExpanded$topDevice,allUsersExpanded$dataset,useNA = "ifany") # ==> the session cover part of train data and almost all train data
# convert NA to "uknown" level
levels(allUsersExpanded$topDevice) <- append(levels(allUsersExpanded$topDevice),"unknown")
allUsersExpanded$topDevice[is.na(allUsersExpanded$topDevice)] <- "unknown"
allUsersExpanded$destination_language[is.na(allUsersExpanded$destination_language)] <- "-unknown-"
summary(allUsersExpanded)


# Cleanup -----------------------------------------------------------------
rm(sessionsC,sessionsD,testUsersRaw,trainUsersRaw)


# Reduce levels -----------------------------------------------------------
# to speed up the predictions & consume less memory
str(allUsers)
summary(allUsers)
factCols <- c("gender","signup_method","signup_flow","language","affiliate_channel",
              "affiliate_provider","first_affiliate_tracked","signup_app",
              "first_device_type","first_browser",
              'book_season','signup_season',"topDevice")
#             "ageRange",'signup_month','book_month')
numericCols <- c("age","days_book_signup")

for (x in factCols) {
  if (!"other" %in% levels(allUsersExpanded[[x]])) { # add "other" to the levels if not there
    levels(allUsersExpanded[[x]]) <- append(levels(allUsersExpanded[[x]]),"other")
  }
  asData <- count(allUsersExpanded,!!as.name(x)) %>%
    top_n(5,n)
  allUsersExpanded[!allUsersExpanded[[x]] %in% asData[[1]],x] <- "other"
  allUsersExpanded[[x]] <- droplevels(allUsersExpanded[[x]])
}
rm(asData,x)
summary(allUsersExpanded)

#### Same code for allUsers df
factCols <- c("gender","signup_method","signup_flow","language","affiliate_channel",
              "affiliate_provider","first_affiliate_tracked","signup_app",
              "first_device_type","first_browser",
              'book_season','signup_season')
#             "ageRange",'signup_month','book_month') # I chose not to reduce the levels of those features

for (x in factCols) {
  if (!"other" %in% levels(allUsers[[x]])) { # add "other" to the levels if not there
    levels(allUsers[[x]]) <- append(levels(allUsers[[x]]),"other")
  }
  asData <- count(allUsers,!!as.name(x)) %>%
    top_n(5,n)
  allUsers[!allUsers[[x]] %in% asData[[1]],x] <- "other"
  allUsers[[x]] <- droplevels(allUsers[[x]])
}
rm(asData,x)
summary(allUsers)
