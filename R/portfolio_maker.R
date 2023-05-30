portfolio_maker <- function(filtered_raw      = "raw"                                , # raw or filtered
                            weighted_simple   = "simple"                             , # simple or weighted
                            period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                            signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                            return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                            doublesort        = "dependent"                          , # "dependent" or "independent"
                            method            = "predictability"                     , # "predictability" or "cross sectionality"
                            portfolio_weight  = "simple"                             , # "simple" or "value"
                            signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                            signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                            horizon_signal1   = "1week"                              , # "1week" or "1day"
                            horizon_signal2   = "1week"                              , # "1week" or "1day"
                            splits_1          = 5                                    , # any integer or "custom"
                            splits_2          = 5                                    , # any integer or "custom"
                            custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                            custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                            splits_number     = 1                                    , # 1 or 2
                            scenarioID        = NULL
){
  #' compute portfolio returns based on characteristics
  #' 
  #' @param filtered_raw      specify if the options used for the signal should 
  #'                          include all available data (raw) or only those 
  #'                          satisfying the conditions used in Cremers and Weinbaum (2010)
  #' @param weighted_simple   specify if the implied volatility spread should be 
  #'                          computed as a simple average over maturity and strikes 
  #'                          or if it should be weighted with the sum of open interest
  #' @param period_signal     specify if the period relevant for estimating the signal 
  #'                          is across the entire week leading up to portfoloio 
  #'                          estimation or if it should be the last available trading day
  #' @param signal_weight_abs specify if the signal should be estimated with a 
  #'                          weight on open interest, volume traded or just a 
  #'                          simple average over the period of the signal
  #' @param return_period     specify if the returns are 1 week returns (open price 
  #'                          Monday to closing price Friday) or if it is a 4 week return
  #' @param doublesort        Should the portfolio sorting be independent or dependent.
  #' @param method            is the signal observed before forming the portfolios 
  #'                          (predictability) or during the return period 
  #'                          (cross sectional)
  #' @param portfolio_weight  Is the returns in the portfolios a simple average 
  #'                          or a value-weighted
  #' @param signal            What is the first signal used for the first sorting
  #'                          Either absolute impl. vol. spread or change in impl. vol. spread
  #' @param signal2           What is the second signal used for the sorting
  #' @param horizon_signal1   If the signal is the change, how long a period is 
  #'                          the change to be onserved over
  #' @param horizon_signal2   If the signal is the change, how long a period is 
  #'                          the change to be onserved over
  #' @param splits_1          the number of portfolios to make based on the 
  #'                          first signal (can also be custom)
  #' @param splits_2          the number of portfolios to make based on the 
  #'                          second signal (can also be custom)
  #' @param custom_split_1    If a custom split is chosen, then this specifies the 
  #'                          breakpoints of the signal.
  #' @param custom_split_2    If a custom split is chosen, then this specifies the 
  #'                          breakpoints of the signal.
  #' @param splits_number     Univariate or bivariate portfolio sort ? 
  #'                          Either 1 or 2.
  #'                          
  #' @returns "Success" or that the particular specification already exists. 
  #'          Also uploads the portfolio returns (both log and simple) in portfolios folder.
  #'          
  #' @author Marie Dyveke Olafsen, m.dyveke@@gmail.com : with inspiration from 
  #'                                                    Cremers and Weinbaum (2010)
  #'                                                    Shang (2016)
  #'                                                    Kjær (2022)
  
  source("/Users/mariedyveke/Documents/GitHub/thesis/R/Lib.R")
  
  ################################################################################
  ########################## Identification of metadata ##########################
  ################################################################################
  
  metadata <- tibble(
    "filtered_raw"      = filtered_raw,
    "weighted_simple"   = weighted_simple,
    "period_signal"     = period_signal,
    "signal_weight_abs" = signal_weight_abs,
    "return_period"     = return_period,
    "doublesort"        = doublesort,
    "method"            = method,
    "portfolio_weight"  = portfolio_weight,
    "signal"            = signal,
    "signal2"           = signal2,
    "horizon_signal1"   = horizon_signal1,
    "horizon_signal2"   = horizon_signal2,
    "splits_1"          = splits_1,
    "splits_2"          = splits_2,
    "custom_split_1"    = str(custom_split_1),
    "custom_split_2"    = str(custom_split_2),
    "splits_number"     = splits_number
  )
  
  if(is.null(scenarioID)){
    meta_data_files <- list.files(portfoliopath) %>% 
      as_tibble() %>% 
      dplyr::filter(grepl("metadata", value))
    
    for(file in meta_data_files$value){
      # file = meta_data_files$value[1]
      tmp_metadata <- read_csv(file = paste0(portfoliopath, "/",file),
                               show_col_types = FALSE) %>% 
        dplyr::select(colnames(metadata))
      if(all(tmp_metadata == metadata)){
        return("A metadata file equal to the one you have entered, already exists.")
      }
    }
    
    version_number <- nrow(meta_data_files) + 1
  } else{
    version_number <- scenarioID
  }
  
  metadata %>% 
    write.csv(file = paste0(portfoliopath, "/metadata_", version_number, ".csv"))
  
  ################################################################################
  ############################### Portfolio splits ###############################
  ################################################################################
  
  years = 1996:2021
  i = 1 # index for the plot
  v = 1 # index for the returns
  
  # Output of the following for loop
  plot_option_data = list()
  returns = list()
  returns_log = list()
  portfolios_signal_values = list()
  cols_of_returns = ifelse(splits_number==1, 
                           ifelse(splits_1 == "custom", 
                                  4+length(custom_split_1),
                                  2+splits_1),
                           ifelse(splits_1 == "custom" & splits_2 == "custom",
                                  2+(length(custom_split_1)+2)*(length(custom_split_2)+2),
                                  ifelse(splits_1 == "custom",
                                         2+(length(custom_split_1)+2)*splits_2,
                                         2+splits_1*splits_2)))
  
  if(splits_number==1){
    portfolio_names <- tibble(portfolios = 1:ifelse(splits_1 == "custom", 
                                1+length(custom_split_1),
                                splits_1))
  } else{
    port1 <- 1:ifelse(splits_1 == "custom", 
                      1+length(custom_split_1),
                      splits_1)
    port2 <- 1:ifelse(splits_2 == "custom", 
                      1+length(custom_split_2),
                      splits_2)
    
    portfolio_names <- tibble(port1v = sort(rep(port1, times = length(port2))), 
           port2v = rep(port2, times = length(port1))) %>% 
      mutate(portfolios = paste0(port1v, "_", port2v), .keep = "unused")
  }
  
  portf_merger <- as_tibble(matrix(data = NA, nrow = 1, ncol = (2+nrow(portfolio_names))))
  colnames(portf_merger) <- c("date", "week", portfolio_names$portfolios)
  
  for(year in years){
    # year = 1996
    # year = year + 1
    print(year)
    
    tmp_return_data <- read_csv(
      file = paste0(datapath1,"/CRSP_filteredstocks_weekly_raw_",year,".csv"), 
      show_col_types = FALSE)
    
    ##############################################################################
    ############# Importing outstanding shares, if value-weighted ################
    ##############################################################################
    
    dates = tmp_return_data %>% 
      group_by(date) %>%
      summarize(permnos = unique(permno), .groups = "keep")
    
    dates2 = tmp_return_data %>% 
      summarize(date = unique(date)) %>% 
      arrange(date)
    
    for(d in 1:length(dates2$date)){
      # date_ = ymd("1996-01-15")
      # d=1+d
      
      date_ <- ymd(dates2$date[[d]])
      tmp_return_date = tmp_return_data %>%
        dplyr::filter(date == date_)
      
      ############################################################################
      ################### Import correct signal period data ######################
      ############################################################################
      
      if(  month(date_) == 1 
           & day(date_) <= 7 
           & year(date_) != 1996
           & method == "predictability"){
        signal_year <-  year-1
      } else{
        signal_year <-  year
      }
      
      tmp_option_data_raw <- read_csv(file = paste0(datapath1,"/OPTIONM_filteredoptions_weekly_",filtered_raw,"_",signal_year,".csv"), show_col_types = FALSE)
      
      ############################################################################
      ################ Predictability or Cross sectional analysis ################
      ############################################################################
      
      if(method == "predictability"){
        start_date <- date_ - days(7)
        end_date <- date_ - days(1)
      } else if(method == "cross sectionality"){
        start_date <- date_
        end_date <- date_ + days(5)
      }
      
      tmp_option_data <- tmp_option_data_raw %>% 
        dplyr::filter((date <= end_date) & (date >= start_date)) %>% 
        group_by(PERMNO) %>% 
        fill(c(impl_vol_spread, weighted_impl_vol_spread)) %>% 
        ungroup()
      
      ############################################################################
      ################# Choosing simple or weighted impl. vol ####################
      ############################################################################
      
      if(weighted_simple == "simple"){
        tmp_option_data <-tmp_option_data %>%
          dplyr::select(PERMNO, 
                        date, 
                        impl_vol_spread, 
                        sum_open_interest, 
                        sum_volume)
      } else if(weighted_simple == "weighted"){
        tmp_option_data <- tmp_option_data %>%
          dplyr::select(PERMNO, 
                        date, 
                        impl_vol_spread = weighted_impl_vol_spread, 
                        sum_open_interest, 
                        sum_volume)
      }
      
      ############################################################################
      ################# Plotting the impl. vol. spread over t ####################
      ############################################################################
      
      plot_option_data[[i]] <- tmp_option_data %>%
        group_by(date) %>%
        summarize('0.05' = quantile(impl_vol_spread, c(0.05), na.rm = TRUE),
                  '0.10' = quantile(impl_vol_spread, c(0.10), na.rm = TRUE),
                  '0.25' = quantile(impl_vol_spread, c(0.25), na.rm = TRUE),
                  '0.50' = quantile(impl_vol_spread, c(0.50), na.rm = TRUE),
                  '0.75' = quantile(impl_vol_spread, c(0.75), na.rm = TRUE),
                  '0.90' = quantile(impl_vol_spread, c(0.90), na.rm = TRUE),
                  '0.95' = quantile(impl_vol_spread, c(0.95), na.rm = TRUE))
      
      ############################################################################
      ############### Identifying signal 1 and sorting of perms ##################
      ############################################################################
      
      if(signal == "absolute implied volatility spread"){
        
        if(period_signal == "last trading day"){
          signal_data <- tmp_option_data %>%
            group_by(PERMNO) %>%
            arrange(desc(date), .by_group = TRUE) %>%
            dplyr::filter(row_number()==1) %>% 
            dplyr::select(PERMNO, date, signal = impl_vol_spread)
          
        } else if(period_signal == "entire week"){
          
          if(signal_weight_abs == "open interest"){
            signal_data <- tmp_option_data %>%
              group_by(PERMNO) %>%
              summarize(date = max(date),
                        signal = weighted.mean(x=impl_vol_spread,w=sum_open_interest, na.rm=TRUE))
          } else if(signal_weight_abs == "volume"){
            signal_data <- tmp_option_data %>%
              group_by(PERMNO) %>%
              summarize(date = max(date),
                        signal = weighted.mean(x=impl_vol_spread,w=sum_volume, na.rm=TRUE))
          } else if(signal_weight_abs == "simple"){
            signal_data <- tmp_option_data %>%
              group_by(PERMNO) %>%
              summarize(date = max(date),
                        signal = mean(impl_vol_spread, na.rm=TRUE))
          } else{
            print("Please specify a weighting method for the signal.")
          }
        } else{
          print("No Correct definition of period_signal is given.")
        }
        
      } else if(signal == "change in implied volatility spread"){
        
        if(horizon_signal1 == "1week"){
          signal_data <- tmp_option_data %>%
            group_by(PERMNO) %>%
            arrange(desc(date), .by_group = TRUE) %>%
            dplyr::filter(row_number()==1 | row_number() == n()) %>%
            summarize(date = max(date),
                      signal = -diff(impl_vol_spread))
        } else if(horizon_signal1 == "1day"){
          signal_data <- tmp_option_data %>%
            group_by(PERMNO) %>%
            arrange(desc(date), .by_group = TRUE) %>%
            dplyr::filter(row_number()==1 | row_number() == 2) %>%
            summarize(date = max(date),
                      signal = -diff(impl_vol_spread))
        } else{
          print("Please specify a horizon for the first signal.")
        }
        
      } else{
        print("Please specify a correct signal for the first signal.")
      }
      
      ############################################################################
      ########################### Sort into portfolios ###########################
      ############################################################################
      
      signal_data_portfolios <- signal_data %>% 
        mutate(portfolios1 = FALSE,
               portfolios2 = FALSE) %>% 
        dplyr::rename(DATE = date, permno = PERMNO) %>% 
        dplyr::filter(!is.na(signal))
      
      available_permnos_for_portfolios <- tmp_return_date %>% 
        summarize(unique = unique(permno)) %>% 
        pull(unique)
      
      if(is.logical(custom_split_1) & is.numeric(splits_1)){
        quantiles <- seq(from = 0, to = 1, length.out = splits_1+1)
      } else {
        splits_1 <- length(custom_split_1)+1
        quantiles <- c(0, custom_split_1,1)
      }
      
      portfolio_cuts <- signal_data %>% 
        ungroup() %>% 
        dplyr::filter(PERMNO %in% available_permnos_for_portfolios) %>% 
        summarize(cuts = quantile(signal, quantiles, na.rm = TRUE)) %>% 
        cbind(quantiles %>% 
                as_tibble() %>% 
                dplyr::rename(quantiles = value)) %>% 
        as_tibble()
      
      for(k in 1:splits_1){
        lower_signal <- portfolio_cuts %>% 
          dplyr::filter(row_number() == k) %>% 
          pull(cuts)
        upper_signal <- portfolio_cuts %>% 
          dplyr::filter(row_number() == k+1) %>% 
          pull(cuts)
        
        signal_data_portfolios <- signal_data_portfolios %>% 
          mutate(portfolios1 = ifelse(signal >= lower_signal && signal < upper_signal, k, portfolios1))
      }
      
      if(any(is.na(signal_data_portfolios$portfolios1))){
        signal_data_portfolios["portfolios1"][is.na(signal_data_portfolios["portfolios1"])] <- max(0,signal_data_portfolios$portfolios1, na.rm = TRUE)
      }
      if(any(signal_data_portfolios$portfolios1 == 0)){
        signal_data_portfolios["portfolios1"][signal_data_portfolios["portfolios1"] == 0] <- max(0,signal_data_portfolios$portfolios1, na.rm = TRUE)
      }
      
      ############################################################################
      #################### Second portfolio: estimate signal #####################
      ############################################################################
      if(splits_number == 2 & nrow(tmp_option_data) != 0){
        if(signal2 == "absolute implied volatility spread"){
          
          if(period_signal == "last trading day"){
            signal_data_2 <- tmp_option_data %>%
              group_by(PERMNO) %>%
              arrange(desc(date), .by_group = TRUE) %>%
              dplyr::filter(row_number()==1) %>% 
              dplyr::select(PERMNO, date, signal = impl_vol_spread)
            
          } else if(period_signal == "entire week"){
            
            if(signal_weight_abs == "open interest"){
              signal_data_2 <- tmp_option_data %>%
                group_by(PERMNO) %>%
                summarize(date = max(date),
                          signal = weighted.mean(x=impl_vol_spread,w=sum_open_interest, na.rm=TRUE),
                          .groups = "keep")
            } else if(signal_weight_abs == "volume"){
              signal_data_2 <- tmp_option_data %>%
                group_by(PERMNO) %>%
                summarize(date = max(date),
                          signal = weighted.mean(x=impl_vol_spread,w=sum_volume, na.rm=TRUE),
                          .groups = "keep")
            } else if(signal_weight_abs == "simple"){
              signal_data_2 <- tmp_option_data %>%
                group_by(PERMNO) %>%
                summarize(date = max(date),
                          signal = mean(impl_vol_spread, na.rm=TRUE),
                          .groups = "keep")
            } else{
              print("Please specify a weighting method for the signal.")
            }
          } else{
            print("No Correct definition of period_signal is given.")
          }
          
        } else if(signal2 == "change in implied volatility spread"){
          
          if(horizon_signal2 == "1week"){
            signal_data_2 <- tmp_option_data %>%
              group_by(PERMNO) %>%
              arrange(desc(date), .by_group = TRUE) %>%
              dplyr::filter(row_number()==1 | row_number() == n()) %>%
              summarize(date = max(date),
                        signal = -diff(impl_vol_spread),
                        .groups = "keep")
          } else if(horizon_signal2 == "1day"){
            signal_data_2 <- tmp_option_data %>%
              group_by(PERMNO) %>%
              arrange(desc(date), .by_group = TRUE) %>%
              dplyr::filter(row_number()==1 | row_number() == 2) %>%
              summarize(date = max(date),
                        signal = -diff(impl_vol_spread),
                        .groups = "keep")
          } else{
            print("Please specify a horizon for the first signal.")
          }
          
        } else{
          print("Please specify a correct signal for the first signal.")
        }
        
        signal_data_portfolios <- signal_data_portfolios %>% 
          left_join(signal_data_2 %>% 
                      dplyr::rename(signal2=signal,permno=PERMNO,DATE=date)
                    , by=c("permno","DATE")) %>% 
          dplyr::filter(!is.na(signal2))
        
        number_of_portfolios <- signal_data_portfolios %>% 
          ungroup() %>% 
          summarize(portfolios = unique(portfolios1)) %>% 
          pull(portfolios)
      } else{
        number_of_portfolios <- splits_1
      }
      
      ############################################################################
      ####################### Second portfolio: sort into ########################
      ############################################################################
      if(splits_number == 2 & nrow(tmp_option_data) != 0 & length(number_of_portfolios) > 1){
        
        if(is.logical(custom_split_2) & is.numeric(splits_2)){
          quantiles2 <- seq(from = 0, to = 1, length.out = splits_2+1)
          quantiles2_all <- rep(seq(from = 0, to = 1, length.out = splits_2+1), 
                                each = length(unique(signal_data_portfolios$portfolios1)))
        } else {
          splits_2 <- length(custom_split_2)+1
          quantiles2 <- c(0, custom_split_2,1)
        }
        
        if(doublesort == "dependent"){
          quantiles2_fortibble <- quantiles2 %>% 
            #as_tibble() %>% 
            #dplyr::rename(quantiles = value) %>% 
            rep(., times = length(unique(signal_data_portfolios$portfolios1))) %>% 
            #matrix(nrow = length(quantiles2)) %>% 
            as_tibble() %>% #rbind.data.frame() %>% 
            #pivot_longer(cols = everything(), values_to = "quantiles") %>% 
            #arrange(name) %>% 
            dplyr::select(quantiles = value) #dplyr::select(quantiles)
          
          portfolio_cuts <- signal_data_portfolios %>%
            # left_join(signal_data_2 %>% dplyr::rename(signal_2 = signal, permno = PERMNO, DATE = date), by = c("permno", "DATE")) %>% 
            ungroup() %>% 
            group_by(portfolios1) %>% 
            summarize(cuts = quantile(signal2, quantiles2, na.rm = TRUE), .groups = "keep") %>%
            cbind(quantiles2_fortibble) %>% 
            spread(key = portfolios1, value = cuts) %>% 
            as_tibble()
          
          dependentsort_loop <- ncol(portfolio_cuts)-1#splits_2
          
        } else if(doublesort == "independent"){
          portfolio_cuts <- signal_data_2 %>% 
            ungroup() %>% 
            dplyr::filter(PERMNO %in% available_permnos_for_portfolios) %>% 
            summarize(cuts = quantile(signal, quantiles, na.rm = TRUE)) %>% 
            cbind(quantiles %>% as_tibble() %>% dplyr::rename(quantiles = value)) %>% 
            as_tibble()
          
          portfolio_cuts <- portfolio_cuts[,c(2,1)]
          
          dependentsort_loop <- 1
          
        } else {
          print("Please specify if the doublesorting should be dependent or independent.")
        }
        
        for(j in 1:dependentsort_loop){
          # j = 1 + j
          tmp_cuts <- portfolio_cuts %>% 
            dplyr::select(quantiles, (j+1))
          colnames(tmp_cuts) <- c("quantiles", "cuts")
          
          for(k in 1:splits_2){
            
            lower_signal <- tmp_cuts %>% 
              dplyr::filter(row_number() == k) %>% 
              pull(cuts)
            upper_signal <- tmp_cuts %>% 
              dplyr::filter(row_number() == k+1) %>% 
              pull(cuts)
            
            if(doublesort == "independent"){
              signal_data_portfolios <- signal_data_portfolios %>% 
                mutate(portfolios2 = ifelse(signal2 >= lower_signal && signal2 < upper_signal, k, portfolios2))
            } else {
              partportf <- as.numeric(colnames(portfolio_cuts)[1+j])
              signal_data_portfolios <- signal_data_portfolios %>% 
                mutate(portfolios2 = ifelse(signal2 >= lower_signal && signal2 < upper_signal && portfolios1 == partportf, k, portfolios2))
            }
          }
          
        }
        
        if(any(is.na(signal_data_portfolios$portfolios2))){
          signal_data_portfolios["portfolios2"][is.na(signal_data_portfolios["portfolios2"])] <- max(0,signal_data_portfolios$portfolios2, na.rm = TRUE)
        }
        if(any(signal_data_portfolios$portfolios2 == 0)){
          signal_data_portfolios["portfolios2"][signal_data_portfolios["portfolios2"] == 0] <- max(0,signal_data_portfolios$portfolios2, na.rm = TRUE)
        }
        
      }
      
      ############################################################################
      ##################### Calculate returns for portfolios #####################
      ############################################################################
      
      tmp_portfolio_returns <- tmp_return_date %>% 
        dplyr::left_join(signal_data_portfolios, by = "permno") %>%
        dplyr::rename(return = ret, 
               value  = marketvalue) %>% 
        dplyr::select(-open, -close, -numtrd, -bidaskspread, -vol, -returns1, -DATE, -signal) %>% 
        dplyr::filter(!is.na(portfolios1))
        
      tmp_portfolios_signal_values <- signal_data_portfolios %>% 
        group_by(portfolios1,portfolios2) %>% 
        summarize(date = min(DATE, na.rm = TRUE),
                  mean = mean(signal, na.rm = TRUE),
                  min = min(signal, na.rm = TRUE),
                  median = median(signal, na.rm = TRUE),
                  max = max(signal, na.rm = TRUE),
                  var = var(signal, na.rm = TRUE),
                  .groups = "keep")
      
      if(splits_number == 1){
        if(portfolio_weight == "simple"){
          tmp_returns <- tmp_portfolio_returns %>% 
            dplyr::select(permno, week, date, return, portfolios1) %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            group_by(portfolios1) %>% 
            summarize(date = min(date), week = min(week), return = mean(return, na.rm = TRUE)) %>% 
            spread(key = "portfolios1", value = "return")
          
          tmp_log_returns <- tmp_portfolio_returns %>% 
            dplyr::select(permno, week, date, return, portfolios1) %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            group_by(portfolios1) %>% 
            summarize(date = min(date), week = min(week), return = log(1+mean(return, na.rm = TRUE))) %>% 
            spread(key = "portfolios1", value = "return")
        } else if(portfolio_weight == "value"){
          tmp_returns <- tmp_portfolio_returns %>% 
            dplyr::select(permno, week, date, return, portfolios1,value) %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            group_by(portfolios1) %>% 
            summarize(date = min(date), week = min(week), return = weighted.mean(return,w=value, na.rm = TRUE)) %>% 
            spread(key = "portfolios1", value = "return")
          
          tmp_log_returns <- tmp_portfolio_returns %>% 
            dplyr::select(permno, week, date, return, portfolios1,value) %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            group_by(portfolios1) %>% 
            summarize(date = min(date), week = min(week), return = log(1+weighted.mean(return,w=value, na.rm = TRUE))) %>% 
            spread(key = "portfolios1", value = "return")
        }
      } else if(splits_number == 2){
        if(portfolio_weight == "simple"){
          tmp_returns <- tmp_portfolio_returns %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            mutate(portfolios12 = paste0(portfolios1, "_", portfolios2)) %>% 
            dplyr::select(permno, week, date, return, portfolios12) %>% 
            group_by(portfolios12) %>% 
            summarize(date = min(date), week = min(week), return = mean(return, na.rm = TRUE)) %>% 
            spread(key = "portfolios12", value = "return")
          
          tmp_log_returns <- tmp_portfolio_returns %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            mutate(portfolios12 = paste0(portfolios1, "_", portfolios2)) %>% 
            dplyr::select(permno, week, date, return, portfolios12) %>% 
            group_by(portfolios12) %>% 
            summarize(date = min(date), week = min(week), return = log(1+mean(return, na.rm = TRUE))) %>% 
            spread(key = "portfolios12", value = "return")
        } else if(portfolio_weight == "value"){
          tmp_returns <- tmp_portfolio_returns %>% 
            dplyr::filter(!is.na(portfolios1) & !is.na(portfolios2)) %>% 
            mutate(portfolios12 = paste0(portfolios1, "_", portfolios2)) %>% 
            dplyr::select(permno, week, date, return, portfolios12, value) %>% 
            group_by(portfolios12) %>% 
            summarize(date = min(date), week = min(week), return = weighted.mean(return,w=value, na.rm = TRUE)) %>% 
            spread(key = "portfolios12", value = "return")
          
          tmp_log_returns <- tmp_portfolio_returns %>% 
            dplyr::filter(!is.na(portfolios1)) %>% 
            mutate(portfolios12 = paste0(portfolios1, "_", portfolios2)) %>% 
            dplyr::select(permno, week, date, return, portfolios12, value) %>% 
            group_by(portfolios12) %>% 
            summarize(date = min(date), week = min(week), return = log(1+weighted.mean(return,w=value, na.rm = TRUE))) %>% 
            spread(key = "portfolios12", value = "return")
        }
      }
      
      #print(tmp_returns)
      
      if(nrow(tmp_returns) != 0){ # & ncol(tmp_returns) == cols_of_returns){ 
        returns[[v]] <- dplyr::bind_rows(portf_merger, tmp_returns) %>% 
          dplyr::filter(!is.na(date)) %>% 
          dplyr::select(colnames(portf_merger))
        #returns[[v]] <- tmp_returns
        returns_log[[v]] <- dplyr::bind_rows(portf_merger, tmp_log_returns) %>%
          dplyr::filter(!is.na(date)) %>% 
          dplyr::select(colnames(portf_merger))
        #returns_log[[v]] <- tmp_log_returns
        portfolios_signal_values[[v]] <- tmp_portfolios_signal_values
        v = v+1
      }
      
      i = i+1
    } # end date loop
    
  } # end year loop
  
  ################################################################################
  #################### Saving the impl. vol. spread over t #######################
  ################################################################################
  do.call(rbind.data.frame, plot_option_data) %>%
    ggplot(aes(x=date)) + 
    geom_line(aes(y=`0.05`, colour = "0.05")) +
    geom_line(aes(y=`0.50`, colour = "0.50")) +
    geom_line(aes(y=`0.95`, colour = "0.95")) +
    scale_x_date(limits = c(ymd(paste0(years[1],"-01-01")),ymd(paste0(tail(years,n=1)+1,"01-01"))), expand = c(0, 0)) +
    # scale_y_continuous(limits = c(min(`0.05`),max(`0.95`)), expand = c(0, 0)) +
    scale_colour_manual("", 
                        breaks = c("0.05", "0.50", "0.95"),
                        values = c("blue", "black", "blue")) +
    labs(title = paste0(
      ifelse(weighted_simple == "simple","Quantiles of the daily simple average of the Implied Volatility Spread,",
             "Quantiles of the daily weighted average of the Implied Volatility Spread,"),
      ifelse(filtered_raw == "raw", " all options", " filtered options")),
      x = "Date",
      y = "Implied Volatility Spread") +
    theme(legend.position="bottom",
          text=element_text(family="Times New Roman"),
          plot.title = element_text(hjust = 0.5))
  ggsave(filename = paste0("implvolspread_entireperiod_",filtered_raw,"_",weighted_simple,"_.png"),
         path = plotpath,
         width = 250, height = 110, units = "mm")
  
  do.call(rbind.data.frame, plot_option_data) %>%
    ggplot(aes(x=date)) + 
    geom_line(aes(y=`0.05`, colour = "0.05")) +
    geom_line(aes(y=`0.50`, colour = "0.50")) +
    geom_line(aes(y=`0.95`, colour = "0.95")) +
    scale_x_date(limits = c(ymd(paste0(years[1],"-01-01")),ymd(paste0(tail(years,n=1)+1,"01-01"))), expand = c(0, 0)) +
    # scale_y_continuous(limits = c(min(`0.05`),max(`0.95`)), expand = c(0, 0)) +
    scale_colour_manual("", 
                        breaks = c("0.05", "0.50", "0.95"),
                        values = c("blue", "black", "blue")) +
    labs(
      # title = paste0(
      # ifelse(weighted_simple == "simple","Quantiles of the daily simple average of the Implied Volatility Spread,",
      #        "Quantiles of the daily weighted average of the Implied Volatility Spread,"),
      # ifelse(filtered_raw == "raw", " all options", " filtered options")),
      x = "Date",
      y = "Implied Volatility Spread") +
    theme(legend.position="bottom",
          text=element_text(family="Times New Roman"),
          plot.title = element_text(hjust = 0.5))
  ggsave(filename = paste0("implvolspread_entireperiod_",filtered_raw,"_",weighted_simple,".png"),
         path = projectplotpath,
         width = 250, height = 110, units = "mm")
  
  do.call(rbind.data.frame, plot_option_data) %>% 
    write.csv(file = paste0(plotpath, "/distributionofsignal_", version_number, ".csv"))
  
  ################################################################################
  #################### Saving the returns and log returns ########################
  ################################################################################
  
  do.call(rbind.data.frame, returns_log) %>%
    write.csv(file = paste0(portfoliopath, "/logreturns_", version_number, ".csv"))
  
  do.call(rbind.data.frame, returns) %>% 
    write.csv(file = paste0(portfoliopath, "/returns_", version_number, ".csv"))
  
  do.call(rbind.data.frame, portfolios_signal_values) %>% 
    write.csv(file = paste0(portfoliopath, "/signalportfolios_", version_number, ".csv"))
  
}


