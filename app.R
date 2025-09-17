library(yahoofinancer)
library(doRNG)
library(doFuture)
library(plotly)
library(tsfeatures)
library(DT)
library(stringi)
library(readxl)
library(tidyverse)
library(shiny)
# library(rgdal)
library(readr)
library(shinydashboard)
library(listviewer)
library(gridExtra)
library(dplyr)
library(tidyr)
source('changepoint_helpers.R')
source('momentum_helpers.R')

outcomes = c('SPY_close_price',
             'SPY_daily_upInd',
             'SPY_daily_downInd',
             'SPY_daily_sigUpInd',
             'SPY_daily_sigDownInd',
             'SPY_daily_sigInd',
             'SPY_daily_sigIndCat'
             # 'SPY_daily_sigUpInd50',
             # 'SPY_daily_sigDownInd50',
             # 'SPY_daily_sigInd50',
             # 'SPY_daily_sigIndCat50'
             
             )

outcomeList = list(outcomes)
names(outcomes) <- outcomes
current_date = lubridate::ymd(as.Date(Sys.time()))

postSelBase <- readRDS('post_sel_models_base.rds')

postSelPca <- readRDS('post_sel_models_pca.rds')

pcaVarRets <- readRDS('pcaVarRets.rds')
old_date <- c()
ui <- fluidPage(titlePanel(strong(h3('Ouyang Family Equity Analysis Tools'))),
                navbarPage(strong('Quantitative Tools'),
                           tabPanel('Changepoint Analysis',
                                    class = 'leftAlign',
                                    fluidPage(
                                      fluidRow(column(4,textInput("ticker_name1", 
                                                         h4("Ticker 1 Search"),
                                                         '^GSPC'
                                                         )
                                                      ),
                                               column(4, textInput('ticker_name2',
                                                                   h4('Ticker 2 Search'),
                                                                   '^N225'
                                                                   )
                                                      )),
                                      fluidRow(column(4,numericInput('lag_used',
                                                                     'Lag Used',
                                                                     60,
                                                                     10,
                                                                     240,
                                                                     10)
                                                      ),
                                               column(4,numericInput('arl_used',
                                                                     'ARL Used',
                                                                     20,
                                                                     10,
                                                                     100,
                                                                     5)
                                                      ),
                                               column(4,dateInput('start_used',
                                                                     'Start Date Used',
                                                                  sprintf('%s-01-01',
                                                                          lubridate::year(Sys.time()
                                                                          )),
                                                                  min = '1996-01-01',
                                                                  max = sprintf('%s-01-01',
                                                                                lubridate::year(Sys.time()
                                                                                ))
                                                                     )
                                               )),
                                            
                                      br(),
                                      textOutput('error_message'),
                                      plotOutput('ticker_change_plot',
                                                 width ='100%',
                                                    height = '800px'),
                                      br(),
                                      fluidRow(column(6, 
                                                      downloadButton("downloadDetectionData",
                                                                     "Download Change Point Dates")),
                                               column(6, 
                                                      downloadButton("downloadChangePointPlot",
                                                                     "Download Change Point Plot")
                                                      )),
                                      br(),
                                        
                                        p("Made with",
                                          a("Shiny",
                                            href = "http://shiny.rstudio.com"),
                                          ".")
                                        )
                                      ),
                           tabPanel('SPY Forecasting',
                                    fluidPage(
                                      
                                      fluidRow(
                                        column(2, textInput("ticker_fore", 
                                                            "Ticker Search",
                                                            'SPY'
                                                            )),
                                        column(2,selectInput('outcome_used',
                                                             'Outcome',
                                                             outcomes
                                                            )),
                                        column(2,numericInput('horizon_used',
                                                                'Forecast Horizon',
                                                                  10,
                                                                1,
                                                                10,
                                                                1)),
                                        column(2,radioButtons('uncertainty_used',
                                                            'Calculate 99% Confidence Intervals?',
                                                           c('Yes'=TRUE,
                                                             'No'=FALSE))
                                             ),
                                        
                                        column(2,dateInput('start_used_fore',
                                                         'Start Date Used',
                                                         as.Date(lubridate::ymd(as.Date(Sys.time())
                                                         )-months(6)),
                                                         min = '1996-01-01',
                                                         max =as.Date(lubridate::ymd(as.Date(Sys.time())
                                                                       )-days(1)))
                                               )),
                                      plotOutput('momentum_forecast_plot')
                                    ))
                                    )
                           )

server <- function(input, output, session) {
  
  dataChangePlot = reactive({
    ticker1_hist <- get_ticker_data(input$ticker_name1,
                                    start_date = lubridate::ymd(input$start_used
                                                         )-days(input$lag_used)
                                    )
    ticker2_hist <- get_ticker_data(input$ticker_name2,
                                    start_date = lubridate::ymd(input$start_used
                                    )-days(input$lag_used))
    
    merged_finance_data <- merge_ticker_data(ticker1_hist,
                                             ticker2_hist)
    merged_finance_data
  })
  
  dataMomentumPlot = reactive({
    start_date =as.Date(input$start_used_fore)-lubridate::years(1)
    ticker_data <- ticker_get(input$ticker_fore,
                              start_date)
    
    ticker_data$SPY_log_volume <- log(ticker_data[,grepl('volume', 
                                                         colnames(ticker_data))])
    ticker_data$SPY_daily_upInd <- ticker_data$SPY_close_price>dplyr::lag(ticker_data$SPY_close_price,1)
    ticker_data$SPY_daily_downInd <-!ticker_data$SPY_daily_upInd
    ticker_data$SPY_daily_sigUpInd <- ticker_data$SPY_dailyGrowth>1
    ticker_data$SPY_daily_sigDownInd <- ticker_data$SPY_dailyGrowth<(-1)
    ticker_data$SPY_daily_sigInd <- abs(ticker_data$SPY_dailyGrowth)>1
    
    ticker_data$SPY_daily_sigIndCat <- 1
    ticker_data$SPY_daily_sigIndCat[which(ticker_data$SPY_daily_sigUpInd)] <- 2
    ticker_data$SPY_daily_sigIndCat[which(ticker_data$SPY_daily_sigDownInd)] <- 0
    
    ticker_data$SPY_daily_sigIndCat <- as.factor(ticker_data$SPY_daily_sigIndCat)
    
    ticker_data$SPY_volume <- NULL
    
    vix_data <- ticker_get('^VIX',
                           start_date)
    tbill10_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=DGS10&cosd=%s&coed=%s',
                            start_date,
                            current_date
                            )
    download.file(tbill10_link,
                  'TBILL10.csv')
    treasury10yrYield <- read.csv('TBILL10.csv') 
    treasury10yrYield <- year_month_extract(treasury10yrYield$observation_date,
                                            treasury10yrYield)
    treasury10yrYield <- rename(treasury10yrYield,
                                treasury10Yield=DGS10
                                )
    treasury10yrYield$treasury10Yield_yoyGrowth <- yoy_growth_calc(treasury10yrYield,
                                                                   'treasury10Yield')
    treasury10yrYield$treasury10Yield_dailyGrowth <- daily_growth_calc(treasury10yrYield,
                                                                       'treasury10Yield')
    
    junkbond_link  <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=BAMLH0A0HYM2&cosd=%s&coed=%s',
                              start_date,
                              current_date
                              )
    download.file(junkbond_link,
                  'highYieldSpread.csv')
    junkBondYield <- read.csv('highYieldSpread.csv')
    
    junkBondYield <- year_month_extract(junkBondYield$observation_date,
                                        junkBondYield)
    
    junkBondYield <- rename(junkBondYield,
                            junkBondSpread=BAMLH0A0HYM2
    ) 
    junkBondYield$junkBondSpread_yoyGrowth <-yoy_growth_calc(junkBondYield,
                                                             'junkBondSpread')
    junkBondYield$junkBondSpread_dailyGrowth <- daily_growth_calc(junkBondYield,
                                                                  'junkBondSpread')
    fedFunds_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS&cosd=%s&coed=%s',
                             start_date,
                             current_date
                             )
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
    
    #Economic Policy Uncertainty Index for United States
    epu_link <-  sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=USEPUINDXD&cosd=%s&coed=%s',
                         start_date,
                         current_date
                         )
    #Equity Market-related Economic Uncertainty Index
    equity_epu_link <- sprintf('https://fred.stlouisfed.org/graph/fredgraph.csv?id=WLEMUINDXD&cosd=%s&coed=%s',
                               start_date,
                               current_date
                               )
    download.file(epu_link,
                  'epu_link.csv')
    download.file(equity_epu_link,
                  'equity_epu_links.csv')
    epu<- read.csv('epu_link.csv') %>%
      rename(epu_val = USEPUINDXD) 
    epu <- year_month_extract(epu$observation_date,
                              epu)
    epu$epu_val_yoy <-yoy_growth_calc(epu,
                                      'epu_val')
    epu$epu_val_dailyGrowth <- daily_growth_calc(epu, 'epu_val')
    equity_epu <- read.csv('equity_epu_link.csv') %>%
      rename(equity_epu_val = WLEMUINDXD)
    equity_epu <- year_month_extract(equity_epu$observation_date,
                                     equity_epu)
    equity_epu$equity_epu_val_yoy<- yoy_growth_calc(equity_epu,
                                                    'equity_epu_val')
    equity_epu$equity_epu_val_dailyGrowth <- daily_growth_calc(equity_epu,
                                                               'equity_epu_val')
    
    market_join_keys = c('Date',
                         'observation_year',
                         'observation_month',
                         'observation_day')
    
    cov_join_keys = c('Date'='observation_date',
                      'observation_year',
                      'observation_month',
                      'observation_day')
    
    fedFund_join_keys2 = c(
      'observation_year',
      'observation_month')
    sp500_charData <- left_join(ticker_data, 
                                vix_data, 
                                by = market_join_keys) %>%
      left_join(treasury10yrYield, by = cov_join_keys) %>%
      left_join(junkBondYield, by = cov_join_keys ) %>%
      left_join(epu, by = cov_join_keys) %>%
      left_join(equity_epu, by = cov_join_keys) %>%
      # left_join(infect_epu, by = cov_join_keys) %>%
      left_join(fedFunds, by = fedFund_join_keys2) %>%
      tidyr::fill(everything(),
                  .direction = 'down')
    
    sp500_charData[,grepl('_day+', 
                          colnames(sp500_charData))] <- NULL
    sp500_charData <- na.omit(sp500_charData)
    
    sp500_charData <- sp500_charData[order(sp500_charData$Date),]
    index_vars = c('Date',
                   'observation_year',
                   'observation_month',
                   'observation_day'
    )
    sp500_charData[,outcomes] <- apply(sp500_charData[,outcomes],
                                       2,
                                       as.factor)
    sp500_charData$observation_month <- factor(sp500_charData$observation_month)
    sp500_charData$observation_year <- factor(sp500_charData$observation_year)
    write.csv(sp500_charData,
              sprintf('%s\\asset_charData_%s.csv',
                      getwd(),
                      current_date
                      ))
    sp500_charData
    
    })

  sr_detections <- c()
  sr_changePlot <- c()
  merged_finance_data <- c()
  plot_previous <- c()
  output$ticker_change_plot <- renderPlot({
    alpha_used <- 1/input$arl_used
    lag_used <- input$lag_used
    
    merged_finance_data <<- tryCatch({dataChangePlot()},
                                     error =function(cond){
                                       cond
                                       
                                     })
    if('error' %in% class(merged_finance_data)){
      
      return(sr_changePlot)
    }
    
    cosine_vals_used <-cosine_val_gen(input$lag_used,
                                       merged_finance_data)
    cosine_dist_series <- unlist(readRDS(sprintf('cosine_dist_series_%s_%s.rds',
                                                input$lag_used,
                                                paste(input$ticker_name1,
                                                      input$ticker_name2,
                                                      sep = '_')
                                                )
                                         )
                                 )
    cosine_dist_changes <- tail(cosine_dist_series/dplyr::lag(cosine_dist_series,1),
                                nrow(merged_finance_data))
    counter <- 0
    past_val <- 0
    merged_finance_data$cosine_dist_change <- cosine_dist_changes
    merged_finance_data$changepoint_metric <- sapply(cosine_dist_changes,
                                                     function(val){
                                                       if(is.na(val)){
                                                         return(0)
                                                       }
                                                       result <- val*(1+past_val)
                                                       if(result>=input$arl_used){
                                                         past_val <<- 0
                                                       }
                                                       else{
                                                         past_val <<- result
                                                       }
                                                       return(result)
                                                     })
    
    e_sr_detections <- e_sr(alpha_used, merged_finance_data$cosine_dist_change,
                            cutoff = 0)
    
    merged_finance_data$detections <- 0
    merged_finance_data$detections[e_sr_detections] <-1
    date_col_ind <- which(grepl('date$',
                          colnames(merged_finance_data),
                          ignore.case = TRUE))
    merged_finance_data <- merged_finance_data[as.Date(merged_finance_data[,date_col_ind[1]])>input$start_used,]
    
    # print(head(merged_finance_data[,date_col_ind[1]]))
    dates_used <- merged_finance_data[,
                            colnames(merged_finance_data)[date_col_ind][1]]
    
    detections_sr <- as.Date(dates_used)[merged_finance_data$detections==1]
    detections_sr <- detections_sr[order(detections_sr, decreasing = TRUE)]
    sr_detections <<- detections_sr[detections_sr>input$start_used]
    

    ticker1_ind <- colnames(merged_finance_data)[grepl(paste(paste('\\',
                                                                   input$ticker_name1,
                                                                   sep=''),
                                                             'close$',
                                                             sep = '_'),
                                                       colnames(merged_finance_data)
                                                       )]
    ticker2_ind <- colnames(merged_finance_data)[grepl(paste(paste('\\',
                                                                   input$ticker_name2,
                                                                   sep=''),
                                                             'close$',
                                                             sep = '_'),
                                                       colnames(merged_finance_data))]
    #function to create graphics
    changePoint_plot = function(dates = dates_used,
                                data = merged_finance_data,
                                col_used = ticker1_ind,
                                ticker = input$ticker_name1,
                                color_used = 'dodgerblue4',
                                y_label = 'Close Price'){
      changePlot <- ggplot()+
        geom_point(aes(x =as.Date(dates),
                       y = unlist(data[,col_used]),
                       colour = color_used),alpha = 0.5)+
        scale_x_date(date_breaks = '1 months',
                     date_labels ='%b-%Y' )+
        
        geom_vline(xintercept  =sr_detections,
                   col = 'grey8')+
        scale_color_manual(name = 'Index Close Price',
                           label = c(ticker),
                           values = color_used,
                           breaks = color_used)+
        labs(y = y_label,
             x = 'Date',
             title = sprintf('%s  Close Prices\nwith E-SR Changepoints as Vert. Lines, FDR of %s\nTechnical Features calculated with %s Lags',
                             ticker,
                             alpha_used,
                             lag_used))+ 
        theme(legend.position = 'none',
              axis.text.x = element_text(angle = 30,size = 12,
                                         vjust = 0.5))
      max_col_val <- max(unlist(data[,col_used]),na.rm=TRUE)
      min_col_val <- min(unlist(data[,col_used]),na.rm=TRUE)
      
      if((max_col_val>input$arl_used) & (min_col_val<input$arl_used)){
        changePlot <- changePlot +
          geom_hline(yintercept = input$arl_used)
      }
      changePlot
      
    }
    
    
    ticker1_plot = changePoint_plot(dates_used,
                                    merged_finance_data,
                                    ticker1_ind,
                                    input$ticker_name1,
                                    'dodgerblue4',
                                    'Close Price')
    ticker2_plot = changePoint_plot(dates_used,
                                    merged_finance_data,
                                    ticker2_ind,
                                    input$ticker_name2,
                                    'darkred',
                                    'Close Price')
    ticker_div_plot = changePoint_plot(dates_used,
                                    merged_finance_data,
                                    'changepoint_metric',
                                    paste(paste(input$ticker_name1,
                                          input$ticker_name2,sep=' And '),
                                          'Changepoint Evidence Value'),
                                    'purple4',
                                    'Evidence Value')
  
    ticker_cosine_plot = changePoint_plot(dates_used,
                                       merged_finance_data,
                                       'cosine_dist_change',
                                       paste(paste(input$ticker_name1,
                                                   input$ticker_name2,sep=' And '),
                                             'Cosine Distance Change'),
                                       'darkorange3',
                                       'Cosine Dist Change Ratio')
    
    #plotting generated graphics
    changePlot <-tryCatch({ do.call(grid.arrange, c(grobs = list(ticker1_plot,
                                                       ticker2_plot,
                                                       ticker_div_plot,
                                                       ticker_cosine_plot), 
                                         
                                          ncol = 2))},
                          error =function(cond){
                            cond
                            
                          })
    
    if('error' %in% class(changePlot)){
      return(sr_changePlot)
    }
    sr_changePlot <<- changePlot
    changePlot
  }
  )
  
  output$downloadDetectionData <- downloadHandler(
    filename = function() {
      paste(input$ticker_name1,
             input$ticker_name2,
             input$lag_used,
             input$alpha_used,
            
             "_srDetection.csv",
            sep = '')
    },
    content = function(file) {
      
      write.csv(sr_detections, file)
      
    }
  )
  output$downloadChangePointPlot <- downloadHandler(
    filename = function() {
      paste(input$ticker_name1,
            input$ticker_name2,
            'lag',
            input$lag_used,
            'arl',
            input$arl_used,
            
            "_changepointPlot.jpg",
            sep = '')
    },
    content = function(file) {
      
      ggsave(file, sr_changePlot,width = 10,height = 10, units = 'in'
            )
      }
  )
  asset_charData <- c()
  output$momentum_forecast_plot <- renderPlot({
    start_date =as.Date(input$start_used_fore)-lubridate::years(1)
    current_date = lubridate::ymd(as.Date(Sys.time()))
    
    if((sum(grepl(current_date,
             list.files()))>0)){
      asset_charData <<- read.csv(sprintf('%s\\asset_charData_%s.csv',
                                          getwd(),
                                          current_date
                                          ),
                                  row.names = 'X')
    }
    if((old_date!=start_date)){
      asset_charData <<- tryCatch({dataMomentumPlot()},
                                  error =function(cond){
                                    cond
                                  })
    }
    else{
      asset_charData <<- tryCatch({dataMomentumPlot()},
                                  error =function(cond){
                                    cond
                                  })
    } 
    old_date <<-start_date
    asset_charData <<- na.omit(asset_charData)
    preds_used <- xgboost_pred(input$outcome_used,
                               input$horizon_used,
                               data_used = asset_charData)
    if(input$uncertainty_used){
      
      residuals_used <- foreach(hor = 1:input$horizon_used) %do% {
        
        data_used <- asset_charData
        data_used$target_lag1 <- dplyr::lag(data_used[,input$outcome_used],
                                                  1)
        data_used$target <- dplyr::lead(data_used[,input$outcome_used],
                                                     hor)
        
        data_used <- na.omit(data_used
                                  )
        tuning_grid_file = read.csv(sprintf( '%s_covar_ret_par_testing.csv',
                                             input$outcome_used ),
                                    row.names = 'X')
        horizon_range <- unique(tuning_grid_file$horizon)
        
        hor = min(hor, max(horizon_range))
        
        tune_params = row.names(dplyr::filter(tuning_grid_file,
                                              horizon==hor))[1]
        fixed_used = regmatches(tune_params,
                                regexpr("(?<=fixed_).*(?=_covars)",
                                        tune_params, 
                                        perl = TRUE))
        covars_used = regmatches(tune_params,
                                 regexpr("(?<=covars_).*(?=$)",
                                         tune_params, 
                                         perl = TRUE))
        
        file_name = list.files('xgboost_models')[grepl(paste(input$outcome_used,
                                                             input$horizon_used,
                                                             fixed_used,
                                                             covars_used,
                                                             sep='_'),
                                                       list.files('xgboost_models'))][[1]]
        xgboost_model <- xgboost::xgb.load(sprintf('xgboost_models\\%s',
                                                   file_name))
        
        
        if(grepl('post',
                 covars_used)){
          ret_vars <- names(get(covars_used)[[input$outcome_used]]$coefficients)
        }
        else{
          ret_vars <- get(covars_used)
        }
        xgb_preds_apply <-  as.numeric(predict(xgboost_model, 
                                               data.matrix(data_used[,c(ret_vars,
                                                                             'target_lag1',
                                                                             input$outcome_used)],1)
                                               )
                                       )
        residuals_used <- data_used$target - xgb_preds_apply
        
        
      }
      blocklength <- 5
      blocks_used <- (nrow(asset_charData)-input$horizon_used%/%blocklength)+1
      # print(blocks_used)
      bootstrap_series <- lapply(1:blocks_used,
                                 function(block){
                                ((block-1)*blocklength):(block*blocklength)
                                 })
      set.seed(89898)
      resamples_used <- sample(bootstrap_series,
                               1000,
                               replace=TRUE)
      bootstrap_preds <- foreach(inds = resamples_used,
                                 .combine = 'rbind') %do% {
                                   preds_used+unlist(lapply(residuals_used,
                                                            function(res){
                                                              tail(res[unlist(inds)],1)
                                                            }))
                                 }
      min_quantiles <- apply(bootstrap_preds,
                                2,
                                function(preds){
                                  quantile(preds,0.005,
                                           na.rm=TRUE)
                                })
      
      max_quantiles <- apply(bootstrap_preds,
                                2,
                                function(preds){
                                  quantile(preds,0.995,
                                           na.rm=TRUE)
                                })
      if(input$outcome_used!=outcomes[1]){
        if(input$outcome_used==tail(outcomes,1)){
          min_quantiles[which(min_quantiles<0)] <- 0
          max_quantiles[which(max_quantiles>2)] <- 2
        }
        else{
          min_quantiles[which(min_quantiles<0)] <- 0
          max_quantiles[which(max_quantiles>1)] <- 1
        }
        
      }
      
    }
    
    plot_used <- ggplot()+
        geom_point(aes(x =as.Date(asset_charData$Date),
                       y = as.numeric(unlist(asset_charData[,input$outcome_used]))),
                   color = 'black')+
        geom_point(aes(x = as.Date(tail(asset_charData$Date,1))+days(1:input$horizon_used),
                       y =as.numeric(preds_used)), color = 'red')+
        scale_x_date(date_breaks = '1 months',
                     date_labels ='%b-%Y' )+
        labs(y = input$outcome_used,
             x = 'Date',
             title = sprintf('Forecasted %s (Red) Values (%s Day Horizon) on %s',
                             input$outcome_used,
                             input$horizon_used,
                             current_date))+ 
        theme(legend.position = 'none',
              axis.text.x = element_text(angle = 30,size = 12,
                                         vjust = 0.5))
    if(input$uncertainty_used){
      plot_used <- plot_used+
        geom_ribbon(aes(as.Date(tail(asset_charData$Date,1))+days(1:input$horizon_used),
                        ymin = min_quantiles,
                        ymax = max_quantiles),
                    color = 'coral',
                    alpha = 0.25,
                    fill='coral')
    }
    
    if(input$outcome_used!=outcomes[1]){
      if(input$outcome_used==tail(outcomes,1)){
        
        plot_used <- plot_used + scale_y_continuous(breaks = c(0,1,2),
                                                    minor_breaks = c(0.5,1.5),
                                                    labels = c('Sig. Down (100 Basis Pts)',
                                                               'No Sig. Movement',
                                                               'Sig. Up (100 Basis Pts)'
                                                    )
        )
      }
      else{
        plot_used <-  plot_used +  scale_y_continuous(breaks = c(0,1),
                                                      minor_breaks = c(0.25,
                                                                       0.5,
                                                                       0.75),
                                                      labels = c(FALSE,
                                                                 TRUE)
        )
        
      }
      
    }
    
    plot_used
      })

    
}


shinyApp(ui = ui, server = server)