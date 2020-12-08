flatten_forecast <- function(forecast_mdl) {
  forecast_mdl %>% gather(Store, value, -Date, convert = TRUE)
}

# Update forecasts to the test dataframe
update_forecast <- function(test_pred_df, dept_preds, dept) {
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_pred_df %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_pred_df$Dept == dept
  pred.d <- test_pred_df[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  test_pred_df$Weekly_Pred[pred.d.idx] <- pred.d$value
  
  return(test_pred_df)
}

##### Model Building #####
tslm_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # tslm forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    myfit = tslm(store_ts ~ season + trend)
    test_ts[, j] <- forecast(myfit, h = num_forecasts)$mean
  }
  
  test_ts
}

shift <- function(test, threshold=1.1, shift=1){
  
  idx <- 5:9
  holiday <- test[idx, 2:46] 
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=TRUE)) 
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=TRUE)) 
  holiday[is.na(holiday)] <- 0
  
  if(is.finite(surge/baseline) & surge/baseline > threshold){
    shifted.sales <- ((7-shift)/7) * holiday 
    shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift/7) * holiday[1:4, ]
    shifted.sales[1, ] <- holiday[1, ]
    test[idx, 2:46] <- shifted.sales
  }
  
  return(test)
}

##### Prediction function #####

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  test_pred_df <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  test_pred_df$Weekly_Pred <- 0
  test_dates <- unique(test_pred_df$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_pred_df$Store)
  num_stores <- length(all_stores)
  
  # Not all departments need predictions
  test_depts <- unique(test_pred_df$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {

    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)
    
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)
    
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    # Fit model and predict forecast
    tslm_tst <- tslm_model(train_dept_ts, test_dept_ts)
    
    # Shift predictions by a week for the fold 5 due to the holiday shift
    if (t==5){
      tslm_tst <- shift(tslm_tst)
    }
    
    test_pred_df <- update_forecast(test_pred_df, tslm_tst, dept)
  }
  
  return (test_pred_df)
  
}