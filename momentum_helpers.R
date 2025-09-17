horizon_total <- 10
postSelBase <- readRDS('post_sel_models_base.rds')

postSelPca <- readRDS('post_sel_models_pca.rds')

pcaVarRets <- readRDS('pcaVarRets.rds')
year_month_extract = function(date_series, data){
  
  data$observation_year <- lubridate::year(date_series)
  data$observation_month <- lubridate::month(date_series)
  data$observation_day <- lubridate::day(date_series)
  return(data)
}

ticker_get = function(ticker,
                      start_date = '1996-01-01'){
  if(grepl('^',ticker)){
    
    tickerObj <- yahoofinancer::Index$new(ticker)
    
  }
  else{
    
    tickerObj <- yahoofinancer::Ticker$new(ticker)
  }
  
  ticker_hist<- tickerObj$get_history(interval = '1d',
                                      start = as.Date(start_date))
  ticker_price <- data.frame(date =ticker_hist$date,
                             close_price = ticker_hist$close,
                             volume = ticker_hist$volume
  )
  
  ticker_price$date <- sapply(ticker_price$date,
                              function(date){
                                strsplit(as.character(date),
                                         split = ' ')[[1]][[1]]
                              })
  
  date_col_ind <- which(grepl('date',colnames(ticker_price),
                              ignore.case = TRUE) )
  # ticker_price$date <- as.Date(ticker_price$date)
  ticker_price <- year_month_extract(ticker_price[,date_col_ind],
                                     ticker_price) %>%
    rename(Date=date)
  ticker_price$dailyGrowth <- daily_growth_calc(ticker_price,
                                                'close_price')
  ticker_price$yoyGrowth <-  yoy_growth_calc(ticker_price,
                                             'close_price')
  tagged_inds <- which(grepl('Growth|volume|close',
                             colnames(ticker_price)))
  colnames(ticker_price)[tagged_inds] <- paste(gsub('\\^','',
                                              ticker),
                                              colnames(ticker_price)[tagged_inds],
                                               sep = '_')
  ticker_price
}

yoy_growth_calc = function(data, col){
  as.numeric(sapply(1:nrow(data),
                    function(index){
                      year_used <- data$observation_year[index]
                      month_used <- data$observation_month[index]
                      day_used <- data$observation_day[index]
                      current_value <- as.numeric(data[,col][index])
                      past_values <- dplyr::filter(data,
                                                   observation_year == (year_used-1),
                                                   observation_month == month_used,
                                                   observation_day %in% c(day_used)
                      )[,col]
                      if(length(past_values)==0){
                        past_values <- dplyr::filter(data,
                                                     observation_year == (year_used-1),
                                                     observation_month == month_used,
                                                     observation_day %in% c(day_used-2,
                                                                            day_used-1,
                                                                            day_used))[,col]
                      }
                      
                      
                      past_value <- tail(na.omit(as.numeric(past_values)),1)
                      round(100*((current_value/past_value)-1),2)
                    }))
  
}

daily_growth_calc = function(data, col){
  round(100*((data[,col]/dplyr::lag(data[,col],1))-1),
        2) 
}



xgboost_pred =  function(outcome,
                         horizon_total,
                         data_used = sp500_charData){
  outcome_preds <- foreach(horizon_used = 1:horizon_total,
                           .combine = 'c') %do% {
                             tuning_grid_file = read.csv(sprintf( '%s_covar_ret_par_testing.csv',
                                                                  outcome),
                                                         row.names = 'X')
                             horizon_range <- unique(tuning_grid_file$horizon)
                             horizon_used <- min(horizon_used,
                                                 max(horizon_range))
                             tune_params = row.names(dplyr::filter(tuning_grid_file,
                                                                   horizon==horizon_used)
                                                     )[1]
                             fixed_used = regmatches(tune_params,
                                                     regexpr("(?<=fixed_).*(?=_covars)",
                                                             tune_params, 
                                                             perl = TRUE))
                             covars_used = regmatches(tune_params,
                                                      regexpr("(?<=covars_).*(?=$)",
                                                              tune_params, 
                                                              perl = TRUE))
                             
                             file_name = list.files('xgboost_models')[grepl(paste(outcome,
                                                                                  horizon_used,
                                                                                  fixed_used,
                                                                                  covars_used,
                                                                                  sep='_'),
                                                                            list.files('xgboost_models')
                                                                            )
                                                                      ][[1]]                            
                             xgboost_model <- xgboost::xgb.load(sprintf('xgboost_models\\%s',
                                                                        file_name))
                             if(grepl('post',
                                      covars_used)){
                               ret_vars <- names(get(covars_used)[[outcome]]$coefficients)
                             }
                             else{
                               ret_vars <- get(covars_used)
                             }
                             data_used$target_lag1 <- dplyr::lag(data_used[,outcome],
                                                                 1)
                             
                             data_used <- data_used[!is.na(data_used$target_lag1),]
                             xgb_preds_apply <-  as.numeric(predict(xgboost_model, 
                                                                    data.matrix(tail(data_used[,c(ret_vars,
                                                                                                  'target_lag1',
                                                                                                  outcome)],1))
                                                                    
                             ))
                             xgb_preds_apply
                           }
  
  write.csv(outcome_preds,
            sprintf('%s_preds.csv',
                    outcome
            ))
  outcome_preds
}
