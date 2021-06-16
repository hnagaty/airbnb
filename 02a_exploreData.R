library(anytime)
library(dplyr)
library(ggplot2)
library(hydroTSM) # I use this to extract the season from the date

trainUsers <- trainUsersRaw


# Checking the age -----------------------------------------------------------------

ageCheck <- trainUsers %>%
  count(age) %>%
  mutate(percentage=n/sum(n)*100)%>%
  arrange(desc(age))


# It seems that some users typed their birth year instead of the age. The fraction to the total is small.
# Correct the age for values between 2014 & 1924 (assume the reference year is 2014)
trainUsers <- mutate(trainUsers,age=if_else(age>=1924,2014-age,age))
ageCheck <- trainUsers %>%
  count(age) %>%
  mutate(percentage=n/sum(n)*100)%>%
  arrange(desc(age))
# now, deal with age > 105 and less than 15
trainUsers$age[trainUsers$age>104] <- NA
trainUsers$age[trainUsers$age<15] <- NA
# create agerange variabl
trainUsers$ageRange <- cut(trainUsers$age,breaks=seq(15,105,5),right=FALSE) #same cuts as in the age buckets file
levels(trainUsers$ageRange) <- append(levels(trainUsers$ageRange),"unknown")
trainUsers$ageRange[is.na(trainUsers$ageRange)] <- "unknown"
count(trainUsers,ageRange,sort=TRUE) %>%
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt))
# the age group is concentrated from 30~34, then from 24~29


ggplot(trainUsers,aes(x=age)) +
  geom_histogram(aes(y=..density..),col="black",fill="cyan",binwidth = 5) +
  scale_x_continuous(limits = c(15,105)) +
  geom_density(col="red") +
  labs(title="Age Density Distribution",subtitle="After cleansing", y="Density",x="Age") +
  scale_y_continuous(labels = scales::percent)
# reasonable distribution of age
rm(ageCheck)

# The timestamp & dates -----------------------------------------------------------
nowDate<- as.Date.character("2016-01-01")
trainUsers <- trainUsers %>%
  mutate(date_first_active = anydate(timestamp_first_active))
# not sure if below is a good idea
trainUsers <- trainUsers %>%
  mutate(date_first_booking=if_else(is.na(date_first_booking),nowDate,date_first_booking))
  
trainUsers <- trainUsers %>%
  mutate(days_signup_active = as.integer(difftime(date_account_created,date_first_active,units="days")),
         days_book_signup = as.integer(difftime(date_first_booking,date_account_created,units="days")))
trainUsers <- trainUsers %>%
  mutate(book_month=as.factor(months(date_first_booking)),
         book_season=as.factor(time2season(date_first_booking,out.fmt = "seasons")),
         signup_month=as.factor(months(date_account_created)),
         signup_season=as.factor(time2season(date_account_created,out.fmt = "seasons")))
trainUsers <- trainUsers %>%
  select(-date_first_active,-timestamp_first_active,-date_account_created,-date_first_booking)

summary(trainUsers$days_book_signup)
summary(trainUsers$days_signup_active)

count(trainUsers,days_signup_active,sort=TRUE) %>% 
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt)) # 99.9% of users made signup on the same date of 1st activity. Will remove this column
trainUsers <- select(trainUsers,-days_signup_active)

fivenum(trainUsers$days_book_signup) # Of users who book a destination, 75% make the booking not later than 29 days
quantile(trainUsers$days_book_signup,probs=c(0.5,0.75,0.9,0.95,0.99),na.rm=TRUE)
trainUsers %>%
  filter(!is.na(days_book_signup)) %>%
  count(days_book_signup,sort=TRUE) %>% 
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt)) %>%
  ggplot(aes(x=days_book_signup,y=pcnt)) +
  geom_col(fill="lightblue") +
  scale_x_continuous(limits=c(0,50)) +
  labs(title="Days between signup & booking",y="Percentage",x="Days")
summary(trainUsers$days_book_signup)
ggplot(trainUsers,aes(x=days_book_signup)) +
  geom_histogram(aes(y=..density..),col="grey",fill="cyan",binwidth = 1) +
  scale_x_continuous(limits = c(0,30),breaks=seq(0,30,2))

trainUsers %>%
  count(ageRange,sort=TRUE) %>% 
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt))

sum(trainUsers$days_book_signup<0,na.rm=TRUE) # few (only 29) observations are less than 0

13.4 + 12.7 + 8.9
# Others ------------------------------------------------------------------

trainUsers<- trainUsers %>%
  mutate(made_booking=as.factor(country_destination!="NDF"))
levels(trainUsers$made_booking) <- c("NO","YES")

ggplot(trainUsersPlot,aes(x=age,fill=made_booking)) +
  geom_histogram(position="fill",binwidth = 4) +
  labs(title="Age distribution vs booking status",x="Age",y="Percentage",fill="Booking Status") +
  scale_y_continuous(labels = scales::percent)

count(trainUsersPlot,book_month,sort=TRUE) %>% 
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt)) 

ggplot(trainUsersPlot,aes(x=secsPerAction,fill=gender)) +
  geom_histogram() +
  labs(title="Histogram of seconds/action",x="Seconds",y="Count") + 
  scale_x_continuous(limits=c(0,100000))
