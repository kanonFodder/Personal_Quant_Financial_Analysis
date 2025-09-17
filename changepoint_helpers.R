get_ticker_data = function(ticker_used='^GSPC',
                           start_date = sprintf('%s-01-01',
                                                lubridate::year(Sys.time()
                                                )
                           )){
  if(grepl('^',ticker_used)){
    
    ticker <- yahoofinancer::Index$new(ticker_used)
    
  }
  else{
    
    ticker <- yahoofinancer::Ticker$new(ticker_used)
  }
  
  ticker_history <- ticker$get_history(interval = '1d',
                                       start = start_date)
  close_col_index <- which(grepl('close',colnames(ticker_history)))[1]
  
  ticker_history$close_price_return <- 100*(
    (ticker_history[,close_col_index]/
       dplyr::lag(ticker_history[,close_col_index],1))-1)
  
  colnames(ticker_history) <- paste(gsub('^','', ticker_used),
                                    colnames(ticker_history),
                                    sep = '_')
  ticker_history
}

merge_ticker_data = function(ticker1_hist,
                             ticker2_hist){
  max_rows <- min(nrow(ticker1_hist),
                  nrow(ticker2_hist)
  )
  merged_finance_data <- cbind(tail(ticker1_hist,max_rows),
                               tail(ticker2_hist, max_rows))
  merged_finance_data <- na.omit(merged_finance_data)
  date_col_ind <- which(grepl('date',colnames(merged_finance_data),
                              ignore.case = TRUE) )
  merged_finance_data$year <- lubridate::year(merged_finance_data[,date_col_ind[1]
  ]
  )
  merged_finance_data
  
}



ts_feat_gen = function(series){
  feat_vals <- tsfeatures(series,
                          features = c('stl_features',
                                       'acf_features',
                                       'pacf_features',
                                       'autocorr_features',
                                       'dist_features',
                                       'arch_stat',
                                       'unitroot_kpss',
                                       'hurst',
                                       'entropy',
                                       'nonlinearity',
                                       'stability',
                                       'lumpiness'
                                       )
                          )
  return(feat_vals)
}


cosine_sim = function(series1, series2){
  
  
  sum(series1*series2)/
    ((sqrt(sum(series1^2)))*(sqrt(sum(series2^2))) )
}
cosine_val_gen = function(lag_used, data = merged_finance_data){
  
  price_return_inds <- which(grepl('close_price_return',
                                   colnames(data)))
  
  names_suffix <- paste(unique(sapply(colnames(data),
                                      function(x){
                                        unlist(strsplit(x,
                                                        split='_')
                                               )[[1]]
                                        }
                                      )
                               
                               )[1:2],
                        collapse = '_')
  merged_data_slices <-  lapply(lag_used:nrow(data),
                                function(index){
                                  (index-lag_used):index
                                })
  dist_name <- sprintf('cosine_dist_series_%s_%s.rds',
                       lag_used,
                       names_suffix)    
  
  doFuture::registerDoFuture()
  plan(multisession)
  
  if(dist_name %in% list.files()){
    cosine_dist_series <- readRDS(dist_name)
    
    len_used <- length(cosine_dist_series)-(lag_used-1)
    
    if(len_used<length(merged_data_slices)){
      cosine_sim_add <- foreach(slice = merged_data_slices[-c(1:len_used)],
                                .combine = 'c') %do% {
                                  ticker1_slice = data[,price_return_inds[1]][slice]
                                  ticker2_slice = data[,price_return_inds[2]][slice]
                                  ticker1_feat = na.omit(ts_feat_gen(ticker1_slice))
                                  ticker2_feat = na.omit(ts_feat_gen(ticker2_slice))
                                  
                                  cosine_sim(ticker1_feat,
                                             ticker2_feat)
                                }
      cosine_dist_add <- 1- cosine_sim_add
      
      cosine_dist_series <- append(cosine_dist_series,
                                   cosine_dist_add)
      readr::write_rds(cosine_dist_series,
                       dist_name)
    }
    
  }
  else {
    cosine_sim_series <- foreach(slice = merged_data_slices) %do% {
      ticker1_slice = data[,price_return_inds[1]][slice]
      ticker2_slice = data[,price_return_inds[2]][slice]
      ticker1_feat = na.omit(ts_feat_gen(ticker1_slice))
      ticker2_feat = na.omit(ts_feat_gen(ticker2_slice))
      
      
      cosine_sim(ticker1_feat,
                 ticker2_feat)
    }
    cosine_sim_series <- unlist(cosine_sim_series)
    cosine_sim_series <- c(rep(NA,(lag_used-1)),
                           cosine_sim_series)
    cosine_dist_series <- 1- cosine_sim_series
    # readr::write_rds(cosine_sim_series,
    #                  sim_name
    # )
    readr::write_rds(cosine_dist_series,
                     dist_name
    )
  }
}


discount_seq_gen_default= function(index, alpha = 0.05){
  0.07720838*(log(max(index,2))/(index*exp(sqrt(log(index)))
  )
  )
}

e_rlond_test = function(alpha, data, randomization = TRUE ){
  discoveries <- 0
  length_used <- length(data)
  discount_vals <- sapply(1:length_used,
                          discount_seq_gen_default,
                          alpha)
  results <- sapply(1:length_used,
                    function(index){
                      # print(index)
                      if(is.na(data[index])){
                        return(FALSE)
                      }
                      test_level <- alpha*discount_vals[index]*(discoveries+1)
                      if(randomization==TRUE){
                        test_level= max(alpha,test_level)/runif(1)  
                      }
                      
                      
                      if(data[index]>(1/(test_level))
                      ){
                        discoveries <<- discoveries+1
                        return(TRUE)
                        
                      }
                      return(FALSE)
                    }
  )
  
  which(results)
}

e_cusum = function(alpha, data, cutoff = 0){
  threshold <- 1/alpha
  length_used <- length(data)
  
  sd_used <- sd(data, na.rm = TRUE)
  discovery_ind <- c()
  past_value <- c()
  results <- sapply(1:length_used,
                    function(index){
                      if(index %in% 1:(lag_used)){
                        result <- abs(data[index]-cutoff)
                        if(is.na(result)){
                          past_value <<- append(past_value,0)
                          result <- 0
                        }
                        past_value <<- result
                        
                        if(result>threshold){
                          past_value <<- 0
                          return(TRUE)
                        }
                        return(FALSE)
                      }
                      else{
                        result <- (abs(data[index]-cutoff))*max(1, past_value,na.rm = TRUE)
                        past_value <<- result
                        
                        if(result>threshold){
                          past_value <<- 0
                          return(TRUE)
                        }
                        return(FALSE)
                      }
                    })
  which(results)
  
}

e_sr = function(alpha, data, cutoff = 0){
  
  threshold <- 1/alpha
  length_used <- length(data)
  
  sd_used <- sd(data, na.rm = TRUE)
  discovery_ind <- c()
  past_value <- c()
  results <- sapply(1:length_used,
                    function(index){
                      # message(index)
                      if((is.na(data[index]))|(index==1)){
                        # message(index)
                        result <- abs(data[index]-cutoff)
                        if(is.na(result)){
                          past_value <<- append(past_value,
                                                0)
                          result <- 0
                        }
                        past_value <<- result
                       
                        if(result>threshold){
                          past_value <<-0
                          result <- 0
                          return(TRUE)
                        }
                        return(FALSE)
                      }
                      else{
                        result <- (abs(data[index]-cutoff))*(1+past_value)
                        
                        past_value <<- result
                        
                        if(result>threshold){
                          past_value <<- 0
                          result <- 0
                          return(TRUE)
                        }
                        return(FALSE)
                      }
                    })
  which(results)
  
}

e_mix <- function(alpha, data, cutoff = 0,
                  method_used = 'SR',
                  min_change = 0.1){
  e_detector_mix <- stcpR6::Stcp$new(method = method_used,#'SR',
                                     family = 'Normal',
                                     alternative = 'two.sided',
                                     threshold = log(1/alpha),
                                     m_pre = cutoff,
                                     delta_lower = min_change
                                     
                                     
  )
  discoveries <- c(0)
  data[which(is.na(data))] <- 1
  length_used <- length(data)
  stop_time <- 0
  while(tail(discoveries,1)<length_used){
    if(tail(discoveries,1)==0){
      e_detector_mix$updateLogValuesUntilStop(data[1:length_used])
      stop_time <- e_detector_mix$getTime()
      
      discoveries <- c(discoveries,
                       stop_time)
    }
    else{
      e_detector_mix$updateLogValuesUntilStop(data[tail(discoveries,1):length_used])
      stop_time <- e_detector_mix$getTime()
      
      
      discoveries <- c(discoveries,
                       (tail(discoveries,1)+stop_time))
    }
    e_detector_mix$reset()
    
    
  }
  return(discoveries[-c(1, length(discoveries))])
  
}



year_month_extract = function(date_series, data){
  
  data$observation_year <- lubridate::year(date_series)
  data$observation_month <- lubridate::month(date_series)
  data$observation_day <- lubridate::day(date_series)
  return(data)
}
