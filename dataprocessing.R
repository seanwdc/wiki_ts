# Creating the initial dataset
rm(list = ls())
library(pageviews)
library(lubridate)

# Params
start = as.POSIXct("2015/07/01") 
cutoff = as.POSIXct("2022/03/01") - 60*60
get_hourly = FALSE

# Pulling Daily Data
daily_data = project_pageviews(project = "en.wikipedia", platform = "all", user_type = "all", granularity = "daily", start = pageview_timestamps(start), end = pageview_timestamps(cutoff), reformat = TRUE)%>% select(date, views)
daily_data = data.frame(views = daily_data$views, date = daily_data$date)

# Pulling Hourly Data
if (get_hourly) {

hourly_data = project_pageviews(project = "en.wikipedia", platform = "all", user_type = "all", granularity = "hourly", start = pageview_timestamps(start), end = pageview_timestamps(cutoff), reformat = TRUE)
start = start + 5000*60*60
while (start < cutoff) {
  # print(start)
  temp = project_pageviews(project = "en.wikipedia", platform = "all", user_type = "all", granularity = "hourly", start = pageview_timestamps(start), end = cutoff, reformat = TRUE)
  hourly_data = rbind(hourly_data, temp) 
  start = start + 5000*60*60
}

write_csv(hourly_data, 'hourlydata.csv')
}

daily_data$views = daily_data$views / 1000000
train_size = length(daily_data$date[year(daily_data$date) < 2021])
test_size = ceiling(length(daily_data$date[year(daily_data$date) >= 2021]) / 2)
train_val_df = slice_head(daily_data, n = dim(daily_data)[1] - test_size)
test_df = slice_tail(daily_data, n = test_size)

setwd("/Users/sean/Desktop/code/timeseries/project")
write_csv(train_val_df, "trainvaldf.csv")
write_csv(test_df, "testdf.csv")

# Processing Data for Recurrent Neural Networks
get_sequential_data = function (input_size = 7, dataset = NULL) {
  print("SHIFT THE NN Processing HERE LATER, can keep the code clean")
}

get_train_df = function (train_val_df) {
  train_size = length(train_val_df$date[year(daily_data$date) < 2021])
  val_size = dim(train_val_df)[1] - train_size
  train_df = slice_head(train_val_df, n = train_size)
  
  return (train_df)
}

get_val_df = function (train_val_df) {
  train_size = length(train_val_df$date[year(daily_data$date) < 2021])
  val_size = dim(train_val_df)[1] - train_size
  val_df = slice_tail(train_val_df, n = val_size)
  return (val_df)
}