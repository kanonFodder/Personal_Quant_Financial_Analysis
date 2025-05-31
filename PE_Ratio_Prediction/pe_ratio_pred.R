library(dplyr)
library(tidyr)
year_month_extract = function(date_series, data){
  
  data$observation_year <- lubridate::year(date_series)
  data$observation_month <- lubridate::month(date_series)
  data$observation_day <- lubridate::day(date_series)
  return(data)
}



current_date <- lubridate::date(Sys.time())
start_date <- '2024-01-01'
SPY <- yahoofinancer::Index$new('SPY')
SPY_1996_on<- SPY$get_history(interval = '1d',
                              start = start_date)
SPY_price <- data.frame(date =SPY_1996_on$date,
                        SPY_close_price = SPY_1996_on$close)
SPY_price$date <- sapply(SPY_price$date,
                         function(date){
                           strsplit(as.character(date),
                                    split = ' ')[[1]][[1]]
                         })

SPY_price <- year_month_extract(SPY_price$date,SPY_price) %>%
  rename(Date=date)


write.csv(SPY_price,
          'SPY_price.csv')

VIX <- yahoofinancer::Index$new('^VIX')
VIX_1996_on<- VIX$get_history(interval = '1d',
                              start = start_date)
VIX_price <- data.frame(date =VIX_1996_on$date,
                        VIX_close_price = VIX_1996_on$close)
VIX_price$date <- sapply(VIX_price$date,
                         function(date){
                           strsplit(as.character(date),
                                    split = ' ')[[1]][[1]]
                         })
VIX_price <- year_month_extract(VIX_price$date,VIX_price) %>%
  rename(Date=date)

VIX_price$VIX_dailyGrowth <- VIX_price$VIX_close_price/dplyr::lag(VIX_price$VIX_close_price,
                                                                  1)
VIX_price$VIX_yoyGrowth <-  as.numeric(sapply(1:nrow(VIX_price),
                                              function(index){
                                                year_used <- VIX_price$observation_year[index]
                                                month_used <- VIX_price$observation_month[index]
                                                day_used <- VIX_price$observation_day[index]
                                                current_value <- as.numeric(VIX_price$VIX_close_price[index])
                                                
                                                past_values <- dplyr::filter(VIX_price,
                                                                             observation_year == (year_used-1),
                                                                             observation_month == month_used,
                                                                             observation_day %in% c(
                                                                               day_used)
                                                )$VIX_close_price
                                                
                                                past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                round(100*((current_value/past_value)-1),2)
                                              }))

write.csv(VIX_price,
          'VIX_price.csv')
#https://fred.stlouisfed.org/series/DGS10



tbill10_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=DGS10&fml=a&vintage_date=%s&revision_date=%s&nd=%s', current_date,current_date, start_date)

download.file(tbill10_link,
              'TBILL10_recent.csv')
treasury10yrYield <- read.csv('TBILL10_recent.csv') 

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
                                                                                                observation_day %in% c(
                                                                                                  day_used)
                                                                   )$treasury10Yield
                                                                   
                                                                   past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                                   round(100*((current_value/past_value)-1),2)
                                                                 }))
treasury10yrYield$treasury10Yield_dailyGrowth <- round(100*((treasury10yrYield$treasury10Yield)/
                                                              dplyr::lag(treasury10yrYield$treasury10Yield,1)-1),
                                                       2) 

junkbond_link  <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=BAMLH0A0HYM2&fml=a&vintage_date=%s&revision_date=%s&nd=%s',
                          current_date,current_date,start_date)
#'https://fred.stlouisfed.org/series/BAMLH0A0HYM2'

download.file(junkbond_link,
              'highYieldSpread_recent.csv')
junkBondYield <- read.csv('highYieldSpread_recent.csv')

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
                                                                                           observation_day %in% c(
                                                                                             day_used)
                                                              )$junkBondSpread
                                                              past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                              
                                                              round(100*((current_value/past_value)-1),2)
                                                            }))
junkBondYield$junkBondSpread_dailyGrowth <- round(100*((junkBondYield$junkBondSpread)/
                                                         dplyr::lag(junkBondYield$junkBondSpread,1)-1),
                                                  2) 

# fedFunds_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS&vintage_date=%s&revision_date=%s&nd=1996-12-31',
#                          current_date,current_date)
# download.file(fedFunds_link,
#               'FEDFUNDS.csv'
#               )
# fedFunds <- read.csv("FEDFUNDS.csv") %>%
#   mutate(fedfund_yoyGrowth = round(100*((FEDFUNDS/dplyr::lag(FEDFUNDS,12))-1),2)
#                          )
# 
# fedFunds <- year_month_extract(fedFunds$observation_date,
#                    fedFunds)
# fedFunds$observation_date <- NULL
# fedFunds$observation_day <- NULL

#Economic Policy Uncertainty Index for United States
epu_link <-sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=USEPUINDXD&vintage_date=%s&revision_date=%s&nd=%s',
                   
                   current_date,current_date,start_date )

#Equity Market-related Economic Uncertainty Index
equity_epu_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=WLEMUINDXD&vintage_date=%s&revision_date=%s&nd=%s',
                           current_date,current_date,start_date)

#Equity Market Volatility: Infectious Disease Tracker
infect_epu_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=INFECTDISEMVTRACKD&vintage_date=%s&revision_date=%s&nd=%s',
                           current_date,current_date,start_date)

download.file(epu_link,
              'epu_link_recent.csv')
download.file(equity_epu_link,
              'equity_epu_link_recent.csv')
download.file(infect_epu_link,
              'infect_epu_link_recent.csv'  )


epu<- read.csv('epu_link_recent.csv') %>%
  rename(epu_val = USEPUINDXD) 
epu <- year_month_extract(epu$observation_date,
                          epu)
epu$epu_val_yoy_3dsmoothed <-round(as.numeric(sapply(1:nrow(epu),
                                                     function(index){
                                                       year_used <- epu$observation_year[index]
                                                       month_used <- epu$observation_month[index]
                                                       day_used <- epu$observation_day[index]
                                                       current_value <- as.numeric(epu$epu_val[index])
                                                       past_values <- dplyr::filter(epu,
                                                                                    observation_year == (year_used-1),
                                                                                    observation_month == month_used,
                                                                                    observation_day %in% c(
                                                                                      day_used)
                                                       )$epu_val
                                                       past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                       
                                                       round(100*((current_value/past_value)-1),2)
                                                     })),2)
epu$epu_val_dailyGrowth <- round(100*((epu$epu_val)/
                                        dplyr::lag(epu$epu_val,1)-1),
                                 2) 
equity_epu <- read.csv('equity_epu_link_recent.csv') %>%
  rename(equity_epu_val = WLEMUINDXD)

equity_epu <- year_month_extract(equity_epu$observation_date,
                                 equity_epu)
equity_epu$equity_epu_val_yoy_3dsmoothed <-round(as.numeric(sapply(1:nrow(equity_epu),
                                                                   function(index){
                                                                     year_used <- equity_epu$observation_year[index]
                                                                     month_used <- equity_epu$observation_month[index]
                                                                     day_used <- equity_epu$observation_day[index]
                                                                     current_value <- as.numeric(equity_epu$equity_epu_val[index])
                                                                     past_values <- dplyr::filter(equity_epu,
                                                                                                  observation_year == (year_used-1),
                                                                                                  observation_month == month_used,
                                                                                                  observation_day %in% c(
                                                                                                    day_used)
                                                                     )$equity_epu_val
                                                                     past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                                     
                                                                     round(100*((current_value/past_value)-1),2)
                                                                   })),2)

equity_epu$equity_epu_val_dailyGrowth <- round(100*((epu$epu_val)/
                                                      dplyr::lag(epu$epu_val,1)-1),
                                               2) 
infect_epu <- read.csv('infect_epu_link_recent.csv') %>%
  rename(infect_epu_val=INFECTDISEMVTRACKD)
infect_epu <- year_month_extract(infect_epu$observation_date,
                                 infect_epu)

infect_epu$infect_epu_val_yoy_3dsmoothed <-round(as.numeric(sapply(1:nrow(infect_epu),
                                                                   function(index){
                                                                     year_used <- infect_epu$observation_year[index]
                                                                     month_used <- infect_epu$observation_month[index]
                                                                     day_used <- infect_epu$observation_day[index]
                                                                     current_value <- as.numeric(infect_epu$infect_epu_val[index])
                                                                     past_values <- dplyr::filter(infect_epu,
                                                                                                  observation_year == (year_used-1),
                                                                                                  observation_month == month_used,
                                                                                                  observation_day %in% c(
                                                                                                    day_used
                                                                                                  )
                                                                     )$infect_epu_val
                                                                     past_value <- mean(as.numeric(past_values),na.rm=TRUE)
                                                                     
                                                                     round(100*((current_value/past_value)-1),2)
                                                                   })),2)


infect_epu$infect_epu_val_dailyGrowth <- round(100*((infect_epu$infect_epu_val)/
                                                      dplyr::lag(infect_epu$infect_epu_val,1)-1),
                                               2)

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


sp500_charData_apply <- left_join(SPY_price, 
                                  VIX_price, 
                                  by = pe_join_keys) %>%
  left_join(treasury10yrYield, by = join_keys1) %>%
  left_join(junkBondYield, by = join_keys1 ) %>%
  left_join(epu, by = join_keys1) %>%
  left_join(equity_epu, by = join_keys1) %>%
  left_join(infect_epu, by = join_keys1) %>%
  
  # left_join(sp500_opEPS, by = join_keys2) %>%
  # left_join(sp500_opMargin, by = join_keys2) %>%
  # left_join(fedFunds, by = join_keys2) %>%
  tidyr::fill(VIX_close_price,
              VIX_yoyGrowth,
              VIX_dailyGrowth,
              treasury10Yield,
              treasury10Yield_yoyGrowth,
              treasury10Yield_dailyGrowth,
              junkBondSpread,
              junkBondSpread_yoyGrowth,
              junkBondSpread_dailyGrowth,
              epu_val,
              epu_val_yoy_3dsmoothed,
              epu_val_dailyGrowth,
              equity_epu_val,
              equity_epu_val_yoy_3dsmoothed,
              equity_epu_val_dailyGrowth,
              infect_epu_val,
              infect_epu_val_yoy_3dsmoothed,
              infect_epu_val_dailyGrowth,
              # FEDFUNDS,
              # fedfund_yoyGrowth,
              .direction = 'down')


sp500_charData_apply[,grepl('_day', 
                            colnames(sp500_charData_apply))] <- NULL
sp500_charData_apply <- na.omit(sp500_charData_apply)
sp500_charData_apply <- sp500_charData_apply[order(sp500_charData_apply$Date),]




sp500_charData_apply$observation_month <- factor(sp500_charData_apply$observation_month)
sp500_charData_apply$observation_year <- factor(sp500_charData_apply$observation_year)

write.csv(sp500_charData_apply,
          'sp500_charData_apply_recent.csv')

sp500_charData_apply <- read.csv('sp500_charData_apply_recent.csv',
                                 row.names = 'X')
apply_rows_used <- nrow(sp500_charData_apply)
xgboost_peOpForwardE_model <- xgboost::xgb.load('xgboostModelVix2.rds')
xgb_preds_apply <- predict(xgboost_peOpForwardE_model, 
                           data.matrix(sp500_charData_apply[,
                                                            covar_ret]))

xgb_preds_apply <- data.frame(date = as.Date(sp500_charData_apply$Date),
                              predPE = xgb_preds_apply)
tail(xgb_preds_apply,10)
write.csv(xgb_preds_apply[order(xgb_preds_apply$date,
                                decreasing = TRUE),],
          'xgb_pred_apply.csv')

ggplot()+
  geom_point(aes(x = xgb_preds_apply$date,
                 y = xgb_preds_apply$predPE)
  )+
  labs(x = 'Date',
       y = 'Predicted P/E Ratio')

ggsave('predPEPlot.jpg')
xgboost::xgb.importance(covar_ret, model = xgboost_peOpForwardE_model)
