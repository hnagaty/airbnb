library(tidyverse)
library(tidymodels)
library(lubridate)
library(themis)






kaggle_train_data <- kaggleTrain
kaggle_test_data <- kaggleTest




  
# returns
# fit time
# model parameters
# the fitted model object
# the polt
# confusion martix





# predict train & test


ggplot(testDataWithPred, aes(x=id, y=country_destination, col=pred)) +
  geom_point(size = 0.7, position='jitter', alpha = 0.6) +
  scale_x_discrete(labels=NULL, breaks=NULL)



autoplot(confusion_matrix, type = 'heatmap')

GetTopPred(train_preds)

s <- data_test %>%
  mutate(pred = GetTopPred(test_preds)) %>% 
  select(id, country_destination, pred) %>%
  gather(pred, country_destination, key = 'type', value = 'destination', factor_key = TRUE) %>% 
  mutate(destination = factor(destination, levels = dest_levels))
