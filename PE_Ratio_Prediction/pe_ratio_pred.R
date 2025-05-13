---
title: "pe-rates"
output: html_document
date: "2025-04-06"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

library(dplyr)
library(tidyr)
year_month_extract = function(date_series, data){
  
  data$observation_year <- lubridate::year(date_series)
  data$observation_month <- lubridate::month(date_series)
  data$observation_day <- lubridate::day(date_series)
  return(data)
}

SPY <- yahoofinancer::Index$new('SPY')
SPY_1996_on<- SPY$get_history(interval = '1d',
                              start = '1996-05-09')
SPY_price <- data.frame(date =SPY_1996_on$date,
                        close_price = SPY_1996_on$close)
SPY_price$date <- sapply(SPY_price$date,
                         function(date){
                           strsplit(as.character(date),
                                    split = ' ')[[1]][[1]]
                         })

SPY_price <- year_month_extract(SPY_price$date,SPY_price) %>%
              rename(Date=date)


write.csv(SPY_price,
          'SPY_price.csv')
#https://fred.stlouisfed.org/series/DGS10
current_date <- lubridate::date(Sys.time())
tbill10_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?mode=fred&recession_bars=on&id=DGS10&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=%s&revision_date=2025-05-06&nd=1996-12-31', current_date)

download.file(tbill10_link,
              'TBILL10.csv')
treasury10yrYield <- read.csv('TBILL10.csv') 

treasury10yrYield <- year_month_extract(treasury10yrYield$observation_date,
                                        treasury10yrYield)


treasury10yrYield <- rename(treasury10yrYield,
                            treasury10Yield=DGS10
                            )


treasury10yrYield$treasury10Yield_yoyGrowth <- as.numeric(sapply(1:nrow(treasury10yrYield),
                                               function(index){
         year_used <- treasury10yrYield$observation_year[index]
         month_used <- treasury10yrYield$observation_month[index]
         day_used <- treasury10yrYield$observation_day[index]
         current_value <- as.numeric(treasury10yrYield$treasury10Yield[index])
         
         past_values <- dplyr::filter(treasury10yrYield,
                       observation_year == (year_used-1),
                       observation_month == month_used,
                       observation_day %in% c((day_used-1),
                                              day_used,
                                              (day_used+1))
                       )$treasury10Yield
         
         past_value <- mean(as.numeric(past_values),na.rm=TRUE)
         round(100*((current_value/past_value)-1),2)
       }))

junkbond_link  <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?mode=fred&recession_bars=on&id=BAMLH0A0HYM2&fml=a&fam=avg&vintage_date=%s&revision_date=2025-05-07&nd=1996-12-31',
                          current_date)
#'https://fred.stlouisfed.org/series/BAMLH0A0HYM2'

download.file(junkbond_link,
              'highYieldSpread.csv')
junkBondYield <- read.csv('highYieldSpread.csv')

junkBondYield <- year_month_extract(junkBondYield$observation_date,
                                        junkBondYield)



junkBondYield <- rename(junkBondYield,
                            junkBondSpread=BAMLH0A0HYM2
                            ) 
junkBondYield$junkBondSpread_yoyGrowth <- as.numeric(sapply(1:nrow(junkBondYield),
                                               function(index){
         year_used <- junkBondYield$observation_year[index]
         month_used <- junkBondYield$observation_month[index]
         day_used <- junkBondYield$observation_day[index]
         current_value <- as.numeric(junkBondYield$junkBondSpread[index])
         past_values <- dplyr::filter(junkBondYield,
                       observation_year == (year_used-1),
                       observation_month == month_used,
                       observation_day %in% c((day_used-1),
                                              day_used,
                                              (day_used+1))
                       )$junkBondSpread
         past_value <- mean(as.numeric(past_values),na.rm=TRUE)

         round(100*((current_value/past_value)-1),2)
       }))

fedFunds_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS&vintage_date=%s&revision_date=2025-05-07&nd=1996-12-31',
                         current_date)
download.file(fedFunds_link,
              'FEDFUNDS.csv'
              )
fedFunds <- read.csv("FEDFUNDS.csv") %>%
  mutate(fedfund_yoyGrowth = round(100*((FEDFUNDS/dplyr::lag(FEDFUNDS,12))-1),2)
                         )

fedFunds <- year_month_extract(fedFunds$observation_date,
                   fedFunds)
fedFunds$observation_date <- NULL
fedFunds$observation_day <- NULL




sp500_peRatio <- readxl::read_excel("S&P 500 PE Ratio 2025-04-10 19_49_50.xlsx",
                                    skip = 4) %>%
                 rename(peRatio=Value,
                        peRatio_yoy_perc = `YOY (%)`)

sp500_peRatio <- year_month_extract(sp500_peRatio$Date,
                                    sp500_peRatio)
sp500_peForwardRatio <- readxl::read_excel("S&P 500 PE Ratio with Forward Estimate 2025-04-10 19_53_57.xlsx",
                                    skip = 4)  %>%
                 rename(peForwardRatio=Value,
                        peForwardRatio_yoy_perc = `YOY (%)`)



sp500_peForwardRatio <- year_month_extract(sp500_peForwardRatio$Date,
                                           sp500_peForwardRatio)

sp500_opEPS <- readxl::read_excel("S&P 500 Operating EPS with Estimate (TTM) 2025-04-10 19_55_35.xlsx",
                                    skip = 4) %>%
                 rename(opEps=Value,
                        opEps_yoy_perc = `YOY (%)`)

sp500_opEPS <- year_month_extract(sp500_opEPS$Date,
                                  sp500_opEPS)
sp500_opEPS$Date <- NULL
sp500_opMargin <- readxl::read_excel("S&P 500 Operating Margin 2025-04-10 19_55_21.xlsx",
                                    skip = 4) %>%
                  rename(opMargin=Value,
                         opMargin_yoy_perc = `YOY (%)`)
sp500_opMargin <- year_month_extract(sp500_opMargin$Date,
                                     sp500_opMargin)

sp500_opMargin$Date <- NULL


```


```{r}
pe_join_keys = c('Date',
                    'observation_year',
                    'observation_month',
                 'observation_day')

join_keys1 = c('Date'='observation_date',
                    'observation_year',
                    'observation_month',
               'observation_day')


join_keys2 = c(
                   'observation_year',
                    'observation_month')

sp500_charData <- left_join(sp500_peRatio,
                            SPY_price, by = pe_join_keys) %>%
                  left_join(treasury10yrYield, by = join_keys1) %>%
                  left_join(junkBondYield, by = join_keys1 ) %>%
                  # left_join(sp500_opEPS, by = join_keys2) %>%
                  # left_join(sp500_opMargin, by = join_keys2) %>%
                  left_join(fedFunds, by = join_keys2)# %>%
                  # fill(opEps,
                  #      opEps_yoy_perc,
                  #      opMargin,
                  #      opMargin_yoy_perc,
                  #      .direction = 'down'
                  #      )  %>%
                  # mutate(forwardEarnings = close_price*(1/peForwardRatio),
                  #        forwardEarningsGrowth = (
                  #          (forwardEarnings/dplyr::lead(forwardEarnings,1))-1))
          

sp500_charData_apply <- left_join(SPY_price, 
                                  treasury10yrYield, 
                                  by = join_keys1) %>%
                        left_join(junkBondYield, by = join_keys1 ) %>%
                  # left_join(sp500_opEPS, by = join_keys2) %>%
                  # left_join(sp500_opMargin, by = join_keys2) %>%
                  left_join(fedFunds, by = join_keys2) %>%
                  tidyr::fill(treasury10Yield,
                              treasury10Yield_yoyGrowth,
                              junkBondSpread,
                              junkBondSpread_yoyGrowth,
                              FEDFUNDS,
                              fedfund_yoyGrowth,
                              .direction = 'down')
sp500_charData[,grepl('_day', 
                      colnames(sp500_charData))] <- NULL
sp500_charData <- na.omit(sp500_charData)
sp500_charData <- sp500_charData[order(sp500_charData$Date),]

sp500_charData <- dplyr::filter(sp500_charData,peRatio<50)

      
sp500_charData$observation_month <- factor(sp500_charData$observation_month)
sp500_charData$observation_year <- factor(sp500_charData$observation_year)

write.csv(sp500_charData,
          'sp500_charData.csv')


sp500_charData_apply[,grepl('_day', 
                      colnames(sp500_charData_apply))] <- NULL
sp500_charData_apply <- na.omit(sp500_charData_apply)
sp500_charData_apply <- sp500_charData_apply[order(sp500_charData_apply$Date),]

sp500_charData_apply <- dplyr::filter(sp500_charData_apply,peRatio<50)

      
sp500_charData_apply$observation_month <- factor(sp500_charData_apply$observation_month)
sp500_charData_apply$observation_year <- factor(sp500_charData_apply$observation_year)

write.csv(sp500_charData_apply,
          'sp500_charData_apply')

```

```{r}
#variable selection using a lasso model
sp500_charData <- read.csv('sp500_charData.csv')

covars <- c(#'peForwardRatio',
            # 'peForwardRatio_yoy_perc',
           # 'forwardEarnings',
            # 'forwardEarningsGrowth',
             'treasury10Yield',
           'treasury10Yield_yoyGrowth',
            'junkBondSpread',
            'junkBondSpread_yoyGrowth',
           # 'opEps',
           # 'opEps_yoy_perc',
            # 'opMargin',
            # 'opMargin_yoy_perc',
          'FEDFUNDS',
            'fedfund_yoyGrowth'
            # 'peRatio_yoy_perc'
            )

lasso_peOPforwardE_model <-glmnet::glmnet(sp500_charData[,covars],
               sp500_charData$peRatio,
               alpha = 1,
               nlambda = 500,
               intercept = FALSE
               )
lasso_residuals <- sp500_charData$peRatio - 
                    predict(lasso_peOPforwardE_model,
                            data.matrix(sp500_charData[,covars]),
                            s = tail(lasso_peOPforwardE_model$lambda,1))

1-(sum(lasso_residuals^2)/sum((sp500_charData$peRatio-
                             mean(sp500_charData$peRatio))^2))
plot(sp500_charData$peRatio,lasso_residuals)

lasso_coefs <- lasso_peOPforwardE_model$beta[,ncol(lasso_peOPforwardE_model$beta)]

covar_ret <- names(lasso_coefs[which(lasso_coefs!=0)])
summary(as.numeric(lasso_residuals))
covar_ret
year_dummies <- model.matrix(~-1+factor(observation_year), 
                             data = sp500_charData)
rows_used <- nrow(sp500_charData)

```


```{r}
#training xgboost model
#want to add automated hyper parameter selection in the future
xgboost_peOpForwardE_model <- xgboost::xgboost(data = data.matrix(sp500_charData[(rows_used-720):(rows_used),covar_ret]),
                                        label = data.matrix(sp500_charData$peRatio[(rows_used-720):(rows_used)]),
                                        nrounds = 50,
                                        early_stopping_rounds = 3,
                                        params = list(eta=0.1                                                      
                                                      ))

xgb_preds <- predict(xgboost_peOpForwardE_model, 
                     data.matrix(sp500_charData[(rows_used-720):rows_used,covar_ret]))
write.csv(xgb_preds,'xgb_preds.csv')
xgb_residuals <- sp500_charData$peRatio[(rows_used-720):rows_used] - xgb_preds

#R-squared value
1-(sum(xgb_residuals^2)/sum((sp500_charData$peRatio[(rows_used-720):rows_used]-
                             mean(sp500_charData$peRatio[(rows_used-720):rows_used]))^2))
summary(xgb_residuals)

plot(sp500_charData$peRatio[(rows_used-720):rows_used],
     xgb_residuals)
ggsave('xgb_residuals_vs_peRatio.jpg')
plot(sp500_charData$peRatio[(rows_used-720):rows_used],
     xgb_preds)
ggsave('xgb_preds_vs_peRatio.jpg')
ggplot()+
  geom_point(aes(x =as.Date(sp500_charData$Date[(rows_used-720):rows_used]),
                 y = xgb_preds,
                 col = 'orange'))+
  geom_point(aes(x =as.Date(sp500_charData$Date[(rows_used-720):rows_used]),
                 y = sp500_charData$peRatio[(rows_used-720):rows_used], 
                 col = 'dodgerblue4'))+
  scale_color_manual(values = c('orange',
                                'dodgerblue4'),
                     breaks = c('orange',
                                'dodgerblue4'),
                     labels =c('XGB Preds',
                               'Real Values'),
                     name = 'Model')
  labs(x = 'Date',
       y = 'Predicted P/E Ratio')
  
ggsave('xgb_preds_real_pe.jpg')
xgboost::xgb.save(xgboost_peOpForwardE_model,'xgboostModel.rds')
readr::write_rds(xgboost::xgb.importance(model = xgboost_peOpForwardE_model),
         'xgboostModelImportance.rds')

```

```{r}

#applying above trained model to all dates
apply_rows_used <- nrow(sp500_charData_apply)

xgb_preds_apply <- predict(xgboost_peOpForwardE_model, 
                     data.matrix(sp500_charData_apply[,covar_ret]))
#writing P/E Ratio preds for last 3 years' worth of trading session to csv file
write.csv(data.frame(date = as.Date(sp500_charData_apply$Date[(apply_rows_used-720):apply_rows_used]),
           predPE = xgb_preds_apply),
          'xgb_pred_apply.csv')

ggplot()+
  geom_point(aes(x = as.Date(sp500_charData_apply$Date[(apply_rows_used-720):apply_rows_used]),
                 y = xgb_preds_apply)
     )+
  labs(x = 'Date',
       y = 'Predicted P/E Ratio')

ggsave('predPEPlot.jpg')
```




```{r}
#cross validation of xgboost model architecture across time
sp500_timeSplits <- caret::createTimeSlices(y = sp500_charData$peRatio,
                                            
                                     initialWindow = 240*3,
                                     horizon = 1,
                                     fixedWindow =FALSE,
                                     skip = 5)


library(doFuture)
registerDoFuture()
plan(multisession)
library(doRNG)
covar_ret_model_rmses <-unlist(foreach(x = 1:length(sp500_timeSplits$train),
                                 .combine = 'rbind') %dorng%{
         train_indexes_used <- sp500_timeSplits$train[[x]]
         test_indexes_used <- sp500_timeSplits$test[[x]]
 
         
         model_used <- xgboost::xgboost(data = data.matrix(sp500_charData[train_indexes_used,
                                                              covar_ret]),
                                        label = data.matrix(sp500_charData$peRatio[train_indexes_used]),
                                        nrounds = 50,
                                        early_stopping_rounds = 3,
                                        params = list(eta=0.5

                                                      ),
                                        verbose = FALSE)

         preds_used <- predict(model_used, 
                               data.matrix(
                               sp500_charData[test_indexes_used,covar_ret])
         )
         
     
         residuals <- as.numeric(unlist(sp500_charData[test_indexes_used,
                                                           'peRatio'])) - preds_used
        
         
         return(residuals)
                                 })

summary(unlist(covar_ret_model_rmses))
covar_ret_model_rmses <- unlist(na.omit(covar_ret_model_rmses))
         
write.csv(data.frame(min_RMSE = min(covar_ret_model_rmses),
                     one_perc_quant_RMSE = quantile(covar_ret_model_rmses,0.01),
                     first_quart_quant_RMSE = quantile(covar_ret_model_rmses,0.25),
                     mean_RMSE = mean(covar_ret_model_rmses),
                     median_RMSE = median(covar_ret_model_rmses),
                     third_quart_quant_RMSE = quantile(covar_ret_model_rmses,0.75),
                    ninety_perc_quant_RMSE = quantile(covar_ret_model_rmses,0.9),
                    ninety_nine_perc_quant_RMSE = quantile(covar_ret_model_rmses,0.99),
                     max_RMSE = max(covar_ret_model_rmses),
                    expected_residual_sd = sd(covar_ret_model_rmses)),
                  file= 'xgb_shift.csv')

```
