library(ggplot2)
library(dplyr)
library(corrplot)
library(DescTools)

# Plot functions ----------------------------------------------------------
plotBar <- function(x) {
  title <- paste(x,"distribution vs booked (yes/no)")
  ggplot(trainUsersPlot,aes_string(x="made_booking",fill=x)) +
    geom_bar(position = "fill") +
    ggtitle(title) +
    labs(y="Count")
}

plotHist <- function(x) {
  title <- paste("Histogram of",x)
  ggplot(trainUsersPlot,aes_string(x=x)) +
    geom_histogram() +
    ggtitle(title)
}


plotCol <- function(x) {
  title <- paste("Distribution of",x)
  p <- count(trainUsersPlot,!!as.name(x),sort=TRUE) %>%
    mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt))
  ggplot(p,aes_string(x=x,y="n")) + geom_col() + ggtitle(title) + labs(y="Count")
}




factCols <- c("gender","ageRange","signup_method","signup_flow","language","affiliate_channel",
              "affiliate_provider","first_affiliate_tracked","signup_app",
              "first_device_type","first_browser",
              'book_month','book_season','topDevice')
numericCols <- c("age","days_book_signup")

lapply(factCols,plotCol)
lapply(factCols,plotBar)

# reduce no. of categories, only for plotting and exploration. but not for the modelling
# display only the top 5, the others mark as othere
# in a new data frame




count(trainUsers,signup_method,sort=TRUE) %>%
  ggplot(aes(x=signup_method,y=n)) + geom_col()

count(trainUsers,first_browser) %>%
  mutate(pcnt=n/sum(n)*100,cumpcnt=cumsum(pcnt)) %>%
  top_n(5,n)

reduceLevels <- function (x) { #something wrong in the fct, probably related to scoping!
  if (!"other" %in% levels(allUsersComp[[x]])) { # add "other" to the levels if not there
    levels(allUsersComp[[x]]) <- append(levels(allUsersComp[[x]]),"other")
  }
  asData <- count(allUsersComp,!!as.name(x)) %>%
    top_n(5,n)
  allUsersComp[!allUsersComp[[x]] %in% asData[[1]],x] <- "other"
}

#lapply(factCols,reduceLevels)

# redo above with for loop
for (x in factCols) {
  if (!"other" %in% levels(trainUsersPlot[[x]])) { # add "other" to the levels if not there
    levels(trainUsersPlot[[x]]) <- append(levels(trainUsersPlot[[x]]),"other")
  }
  asData <- count(trainUsersPlot,!!as.name(x)) %>%
    top_n(5,n)
  trainUsersPlot[!trainUsersPlot[[x]] %in% asData[[1]],x] <- "other"
}  

lapply(factCols,plotCol)
lapply(factCols,plotBar)

lapply(factCols,plotBar)
lapply(numericCols, plotHist)

myTable <- table(trainUsersPlot$ageRange,trainUsersPlot$made_booking)
myTable
plotBar("ageRange")
chisq <- chisq.test(myTable)
chisq
round(chisq$residuals, 2)
corrplot(chisq$residuals, is.cor = FALSE)
CramerV(myTable)

title <- paste(x,"distribution vs booked (yes/no)")
trainUsersPlot %>%
  filter(topDevice!="unknown") %>%
  ggplot(aes(x=made_booking,fill=topDevice)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Device type against booking status",x="Made Booking",y="Percentage",fill="Device")

# Some plots after joining sessions ---------------------------------------
ggplot(allUsersExpanded,aes(x=sumSecs,fill=made_booking)) +
  geom_histogram(aes(y=..density..),position="dodge") +
  scale_x_continuous(limits = c(0,1e+7))
# users who spend less time on the site are less likely to make a booking
ggplot(allUsersExpanded,aes(x=made_booking,y=sumSecs)) +
  geom_jitter(alpha=0.3) +
  scale_y_log10()
summary(allUsersExpanded$sumSecs)
ggplot(allUsersExpanded,aes(x=log(sumSecs))) +
  geom_histogram()

ggplot(allUsersExpanded,aes(x=made_booking,fill=topDevice)) +
  geom_bar(position="fill")
ggplot(allUsersComp,aes(x=topDevice,y=avgSecs)) +
  geom_bar(stat = "summary", fun.y = "mean")

