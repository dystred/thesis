results_test_table_maker <- function(scenarioID){
  source("/Users/mariedyveke/Documents/GitHub/thesis/R/Lib.R")
  
  #scenarioID <- 1
  returns_file <- paste0("returns_",scenarioID,".csv")
  returns_data <- read_csv(paste0(getwd(),"/portfolios/", returns_file),
                           show_col_types = FALSE,
                           #.name_repair = "unique_quiet",
                           col_select = -c(`...1`))
  
  cols  <- length(colnames(returns_data))-2
  iT    <- nrow(returns_data)
  pred <- TRUE
  
  ################################################################################
  ############################# import the FF factors ############################
  ################################################################################
  
  ff_data_files <- list.files(ffdatapath)
  
  ff3_data <- suppressWarnings({
    read_csv(paste0(ffdatapath,"/F-F_Research_Data_Factors_weekly.csv"), 
             skip = 3, 
             show_col_types = FALSE,
  #           .name_repair = "unique_quiet"
    ) %>% 
      dplyr::rename(date = `...1`) %>% 
      mutate(date = ymd(date)) %>% 
      dplyr::filter(lubridate::year(date) >= 1996 & lubridate::year(date) < 2023)
  })
  
  ff5_data <- read_csv(paste0(ffdatapath,"/F-F_Research_Data_5_Factors_2x3_daily.CSV"), 
                          skip = 3, 
                          show_col_types = FALSE
                       ) %>% 
    dplyr::rename(date = `...1`) %>% 
    mutate(date = ymd(date)) %>% 
    dplyr::filter(lubridate::year(date) >= 1996 & lubridate::year(date) < 2023) %>% 
    mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
    group_by(period_) %>% 
    mutate(across(c(where(is.numeric)), ~ .x/100 + 1)) %>% 
    mutate(across(c(where(is.numeric)), cumprod)) %>% 
    mutate(across(c(where(is.numeric)), ~ .x*100 - 100)) %>% 
    summarize(date = max(date),
              `Mkt-RF` = tail(`Mkt-RF`,n=1),
              SMB = tail(SMB, n=1),
              HML = tail(HML, n=1),
              RMW = tail(RMW, n=1),
              CMA = tail(CMA, n=1),
              RF = tail(RF, n=1)
              ) %>% 
    arrange(date)
  
  ################################################################################
  ############################# import the OA factors ############################
  ################################################################################
  
  # OA = OpenAssetPricing Factors
  # Long-Short portfolios of the following factors:
  
  oa_data_files <- list.files(oadatapath)
  oa_data <- read_csv(file = paste0(oadatapath, "/", oa_data_files[1]), 
                      show_col_types = FALSE
                      #.name_repair = "unique_quiet"
                      ) %>% 
    dplyr::select(date) %>% 
    dplyr::filter(lubridate::year(date) > 1995
                  & lubridate::year(date) < 2023)
  
  for(file in oa_data_files){
    # file <- oa_data_files[1]
    
    tmp_data <- read_csv(file = paste0(oadatapath, "/", file), 
                         show_col_types = FALSE
                         #.name_repair = "unique_quiet"
                         ) %>% 
      dplyr::select(date, portLS) %>% 
      dplyr::filter(lubridate::year(date) > 1995
                    & lubridate::year(date) < 2023)
    
    colnames(tmp_data) <- c("date", substring(file, 1, nchar(file)-4))
    
    oa_data <- oa_data %>%
      left_join(tmp_data)
  }
  
  ################################################################################
  ################### Patton&Timmermann test for Monotonicity ####################
  ################################################################################
  
  aver_returns <- returns_data %>% 
    dplyr::select(-date,-week) %>% 
    colMeans(na.rm = TRUE) %>% 
    t() %>% 
    as_tibble()
  
  if(grepl("_", paste(colnames(returns_data),collapse = ","))){
    doublesort = TRUE
    
    dimensions <- str_split_fixed(colnames(aver_returns),"_", n=2) %>% 
      as_tibble()
    
    tmp <- returns_data %>% 
      dplyr::select(-date,-week) %>% 
      colMeans(na.rm = TRUE)
    
    tmp_1 <- matrix(data = tmp, ncol = max(as.numeric(dimensions$V2)),
                  nrow = max(as.numeric(dimensions$V1)))
    
    tmp_1[is.na(tmp_1)] <- 1
    # First sorting is columns
    # Second sorting is rows
    
    diff_returns <- tmp_1 %>% 
      # Rowwise differences
      diff() %>%
      as_tibble() %>% 
      min() %>% 
      rbind(
        # Columnwise differences
        tmp_1 %>%
          t() %>% 
          diff() %>%
          as_tibble() %>% 
          min()
      )
    
    # Relevant returns
    # diagonal:
    cols_diag <- dimensions %>% 
      dplyr::filter(V1 == V2) %>% 
      mutate(x = paste0(V1,"_",V2), .keep = "none")
    
    rel_returns <- returns_data %>% 
      dplyr::select(-date,-week) %>% 
      dplyr::select(cols_diag$x)
    rel_returns[is.na(rel_returns)] <- 1
      
  } else{
    doublesort = FALSE
    
    diff_returns <- returns_data %>% 
      dplyr::select(-date,-week) %>% 
      colMeans(na.rm = TRUE) %>% 
      as_tibble() %>% 
      summarize(delta = diff(value))
    
    rel_returns <- returns_data %>% 
      dplyr::select(-date,-week)
  }
  
  min_diff_return <- diff_returns %>% 
    min()
  
  try({
    test_decreasing <- monoSummary(data          = rel_returns, 
                                   bootstrapRep  = 5000, 
                                   wolakRep      = 100,
                                   increasing    = FALSE, 
                                   difference    = FALSE, 
                                   plot          = FALSE, 
                                   block_length  = 6, 
                                   zero_treshold = 1e-10) %>% 
      tibble() %>% 
      dplyr::select(monoton_up = UP_pval, 
                    monoton_down = DOWN_pval, 
                    Wolak = Wolak_pval,
                    Bonferroni = Bonferroni_pval) %>% 
      mutate(category = "decreasing")
  })
    
  try({
    test_increasing <- monoSummary(data          = rel_returns, 
                                   bootstrapRep  = 5000, 
                                   wolakRep      = 100,
                                   increasing    = TRUE, 
                                   difference    = FALSE, 
                                   plot          = FALSE, 
                                   block_length  = 6, 
                                   zero_treshold = 1e-10) %>% 
      tibble() %>% 
      dplyr::select(monoton_up = UP_pval, 
                    monoton_down = DOWN_pval, 
                    Wolak = Wolak_pval,
                    Bonferroni = Bonferroni_pval) %>% 
      mutate(category = "increasing") %>% 
      rbind(test_decreasing) %>% 
      relocate(category)
  })
  
  table_formatter(x = test_increasing, 
                  filename = paste0("monotonicity_tests_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0),
                  row_names = FALSE)
  
  
  ################################################################################
  ############################### import the signal ##############################
  ################################################################################
  
  signals_file <- paste0("signalportfolios_",scenarioID,".csv")
  
  signal_data_raw <- read_csv(paste0(getwd(),"/portfolios/", signals_file),
                           show_col_types = FALSE,
                           #.name_repair = "minimal",
                           col_select = -c(`...1`))
  
  if(doublesort){
    signal_data <- signal_data_raw %>% 
      mutate(group = paste0(portfolios1,"_",portfolios2)) %>% 
      dplyr::select(-portfolios1, -portfolios2)
  } else{
    signal_data <- signal_data_raw %>% 
      mutate(group = portfolios1) %>% 
      dplyr::select(-portfolios1, -portfolios2)
  }
  
  signal_data_acf <- signal_data %>% 
    dplyr::select(group, mean) %>% 
    group_by(group) %>% 
    summarise(list_acf=list(acf(mean, plot=FALSE))) %>%
    mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
    dplyr::select(-list_acf) %>% 
    unnest(cols = c(acf_vals)) %>% 
    group_by(group) %>% 
    mutate(lag=row_number() - 1)
  
  df_ci <- signal_data_acf %>% 
    group_by(group) %>% 
    summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))
  
  ggplot(signal_data_acf, aes(x=lag, y=acf_vals)) +
    geom_bar(stat="identity", width=.15) +
    geom_hline(yintercept = 0) +
    geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
    geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
    labs(x="Lag", y="ACF") +
    facet_wrap(~group)
  ggsave(filename = paste0("ACF_entireperiod_simple_",scenarioID,".png"),
         path = projectplotpath,
         width = 250, height = 110, units = "mm")
  
  ################################################################################
  ############################ Plotting descriptives #############################
  ################################################################################
  
  # Cumulative returns
  
  returns_data %>% 
    arrange(date) %>%
    gather(key = "portfolio", value = "Return", -c(date,week)) %>% 
    dplyr::filter(!is.na(Return)) %>% 
    group_by(portfolio) %>% 
    mutate(Return = cumprod(Return)) %>% 
    ggplot(aes(x=date, y=Return)) +
    scale_x_date(limits = c(ymd(paste0(lubridate::year(min(returns_data$date)),"-01-01")),
                            ymd(paste0(lubridate::year(max(returns_data$date))+1,"01-01"))), 
                 expand = c(0, 0)) +
    geom_line(aes(color = portfolio)) +
    theme_light() +
    labs(color = "Portfolios:",
         y = "Simple Cumulative Returns") +
    theme(legend.position="bottom")
  ggsave(filename = paste0("cumulativereturns_entireperiod_simple_",scenarioID,".png"),
    path = projectplotpath,
    width = 250, height = 200, units = "mm")
  
  # Average returns for each portfolio -> 3d surface for bivariate
  
  if(doublesort){
    # first sorting is per column, and second is per row
    plots <- returns_data %>% 
      dplyr::select(-date, -week) %>% 
      colMeans(na.rm = TRUE) %>% 
      matrix(nrow = cols^0.5)
      
    rownames(plots) <- 1:cols^0.5
    colnames(plots) <- 1:cols^0.5
    
    plots <- melt(plots)
    colnames(plots) <- c("x","y","value")
    plots %>% 
      mutate(value = (value-1)*100) %>% 
      ggplot(aes(x=x, y=y, fill = value)) +
      geom_tile(linewidth = 1.5,
                linetype = 1) +
      coord_fixed() +
      guides(fill = guide_colourbar(title = "Mean Return",
                                    barheight = 10)) +
      labs(x = "Second Sorting Signal", 
           y = "First Sorting Signal") +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(face="bold", #color="#993333", 
                                       size=14 #, angle=45
                                       ),
            axis.text.y = element_text(face="bold", #color="#993333", 
                                       size=14 #, angle=45
                                       ),
            axis.ticks = element_blank())
    
    ggsave(filename = paste0("meanreturns_entireperiod_simple_",scenarioID,".png"),
           path = projectplotpath,
           width = 150, height = 100, units = "mm")
  } else{
    returns_data %>% 
      dplyr::select(-date, -week) %>% 
      colMeans(na.rm = TRUE) %>% 
      reshape::melt() %>% 
      mutate(value = (value-1)*100,
             x = rownames(.)) %>% 
      ggplot(aes(x = factor(x, levels = c(1:cols)), 
                 y = value)) +
      geom_bar(stat="identity") +
      labs(x = "Portfolios based on the First Sorting Signal",
           y = "Mean Simple Return, prc") +
      theme(axis.text.x = element_text(face="bold",
                                       size=14
            ))
    
    ggsave(filename = paste0("meanreturns_entireperiod_simple_",scenarioID,".png"),
           path = projectplotpath,
           width = 150, height = 150, units = "mm")
  }
  
  ################################################################################
  ############################# Tabling descriptives #############################
  ################################################################################
  
  tmp <- returns_data %>% 
    mutate(period_ = paste0(lubridate::year(date), "_",week)) %>% 
    left_join(ff3_data %>% 
                mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date)))
              , by = "period_") %>% 
    fill(RF)
  
  for(i in 1:cols){
    tmp[,2+i] <- (tmp[,2+i] - 1)*100 - tmp$RF
  }
  
  # Now we are looking at excess returns
  
  returns_data_excess <- tmp
  
  colnames(returns_data_excess) <- gsub("_",":",colnames(returns_data_excess))
  
  returns_data_excess <- returns_data_excess %>% 
    mutate(LS = .[[cols+2]]-.[[3]])
  
  if(doublesort){
    portfolios_number <- cols^0.5
    returns_data_excess <- returns_data_excess %>% 
      mutate(LSD = .[[3+portfolios_number*(portfolios_number-1)]] - .[[portfolios_number+2]])
  }
  
  descr_returns_data <- returns_data_excess %>% 
   summarise(across(c(where(is.numeric), -week), 
                    list(Mean = mean, 
                         Std = sd, 
                         Skew = moments::skewness, 
                         Kurt = moments::kurtosis), 
                    na.rm = TRUE)) %>% 
    gather(Letter, Val) %>% 
    separate(Letter, into = c("portfolios", "stat"), sep = "_") %>% 
    spread(portfolios, Val)
  
  colnames(descr_returns_data) <- gsub(":","_",colnames(descr_returns_data))
  
  descr_returns_data <- descr_returns_data %>% 
    rbind(c("SH", descr_returns_data %>% 
              dplyr::select(-stat) %>% 
              t() %>% 
              as_tibble() %>% 
              mutate(SH = .[[2]]/.[[4]]) %>% 
              pull(SH))
          ) %>% 
    mutate(across(c(everything(),-stat), as.numeric)) %>% 
    dplyr::select(stat, c(colnames(returns_data %>% dplyr::select(3:(cols+2))))) %>% 
    arrange(factor(stat, levels = c('Mean', 'Std', 'Skew', 'Kurt', 'SH'))) %>% 
    mutate(stat = ifelse(stat == "SH", "Sharpe Ratio", stat))
  table_formatter(x = descr_returns_data,
                  filename = paste0("descrstat_entireperiod_portfolios_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0))
  
  ################################################################################
  ############################### Regression Time ################################
  ################################################################################
  
  reg_data_pooled <- returns_data_excess
  colnames(reg_data_pooled) <- gsub(":","_",colnames(reg_data_pooled))
  
  returns_excess_ff5 <- reg_data_pooled %>% 
    mutate(period_ = paste0(lubridate::year(date.x), "_",week)) %>% 
    left_join(ff5_data %>% 
                dplyr::select(date, period_, RMW, CMA)#%>% 
                #mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date)))
              , by = "period_") %>% 
    fill(RF)
  
  ## Regressing the portfolios upon ff3 and ff5
  
  output_tibs <- tibble(.rows = 13)
  
  for(i in 1:cols){
    
    if(doublesort){
      reg_data <- returns_excess_ff5 %>% 
        dplyr::select(2+i, `Mkt-RF`, HML, SMB, CMA, RMW, LS, LSD)
      
      colnames(reg_data) <- c("Y", "MKT", "HML", "SMB", "CMA", "RMW", "LS", "LSD")
      
      lm.ff3 <- lm(Y ~ MKT + HML + SMB + LS + LSD,
                   data = reg_data)
      lm.ff5 <- lm(Y ~ MKT + HML + SMB + CMA + RMW + LS + LSD,
                   data = reg_data)
    } else {
      reg_data <- returns_excess_ff5 %>% 
        dplyr::select(2+i, `Mkt-RF`, HML, SMB, CMA, RMW, LS)
      
      colnames(reg_data) <- c("Y", "MKT", "HML", "SMB", "CMA", "RMW", "LS")
      
      lm.ff3 <- lm(Y ~ MKT + HML + SMB + LS,
                   data = reg_data)
      lm.ff5 <- lm(Y ~ MKT + HML + SMB + CMA + RMW + LS,
                   data = reg_data)
    }
    
    results_ff3 <- tibble(variables = rownames(t(t(summary(lm.ff3)$coefficients[,1]))), 
                      coefs = t(t(summary(lm.ff3)$coefficients[,1])),
                      stderr = t(t(summary(lm.ff3)$coefficients[,2]))) %>% 
      gather(key = version, value = Value, -variables) %>% 
      mutate(Variable = paste0(variables, "_", substring(version, 1,5)), .keep = "unused") %>% 
      relocate(Variable) %>% 
      arrange(Variable) %>% 
      rbind(tibble(Variable = "R_2", Value = summary(lm.ff3)$adj.r.squared))
    
    results_ff5 <- tibble(variables = rownames(t(t(summary(lm.ff5)$coefficients[,1]))), 
                          coefs = t(t(summary(lm.ff5)$coefficients[,1])),
                          stderr = t(t(summary(lm.ff5)$coefficients[,2]))) %>% 
      gather(key = version, value = Value, -variables) %>% 
      mutate(Variable = paste0(variables, "_", substring(version, 1,5)), .keep = "unused") %>% 
      relocate(Variable) %>% 
      arrange(Variable) %>% 
      rbind(tibble(Variable = "R_2", Value = summary(lm.ff5)$adj.r.squared))
    
    colnames(results_ff3) <- c("Variable", colnames(reg_data_pooled)[2+i])
    colnames(results_ff5) <- c("Variable", colnames(reg_data_pooled)[2+i])
    
    if(i == 1){
      output_tibs_ff3 <- results_ff3
      output_tibs_ff5 <- results_ff5
    } else{
      output_tibs_ff3 <- output_tibs_ff3 %>% 
        left_join(results_ff3, by = "Variable")
      output_tibs_ff5 <- output_tibs_ff5 %>% 
        left_join(results_ff5, by = "Variable")
    }
  }
  
  # Regressing the factors upon the ff3 and ff5
  
  if(doublesort){
    factors <- c("LS", "LSD")
  }else{
    factors <- c("LS")
  }
  
  for(i in factors){
    reg_data <- returns_excess_ff5 %>% 
      dplyr::select(all_of(i), `Mkt-RF`, HML, SMB, CMA, RMW)
    
    colnames(reg_data) <- c("Y", "MKT", "HML", "SMB", "CMA", "RMW")
    
    lm.ff3 <- lm(Y ~ MKT + HML + SMB,
                 data = reg_data)
    lm.ff5 <- lm(Y ~ MKT + HML + SMB + CMA + RMW,
                 data = reg_data)
    
    results_ff5 <- tibble(variables = rownames(t(t(summary(lm.ff5)$coefficients[,1]))), 
                          coefs = t(t(summary(lm.ff5)$coefficients[,1])),
                          stderr = t(t(summary(lm.ff5)$coefficients[,2]))) %>% 
      gather(key = version, value = Value, -variables) %>% 
      mutate(Variable = paste0(variables, "_", substring(version, 1,5)), .keep = "unused") %>% 
      relocate(Variable) %>% 
      arrange(Variable) %>% 
      rbind(tibble(Variable = "R_2", Value = summary(lm.ff5)$adj.r.squared))
    
    colnames(results_ff5) <- c("Variables", i)
    
    results_ff3 <- tibble(variables = rownames(t(t(summary(lm.ff3)$coefficients[,1]))), 
                          coefs = t(t(summary(lm.ff3)$coefficients[,1])),
                          stderr = t(t(summary(lm.ff3)$coefficients[,2]))) %>% 
      gather(key = version, value = Value, -variables) %>% 
      mutate(Variable = paste0(variables, "_", substring(version, 1,5)), .keep = "unused") %>% 
      relocate(Variable) %>% 
      arrange(Variable) %>% 
      rbind(tibble(Variable = "R_2", Value = summary(lm.ff3)$adj.r.squared))
    
    colnames(results_ff3) <- c("Variables", i)
    
    if(i == "LS"){
      output_tibs <- results_ff5 %>% 
        dplyr::rename(LS.ff5 = LS) %>% 
        left_join(results_ff3 %>% 
                    dplyr::rename(LS.ff3 = LS)
                  , by = "Variables") %>% 
        arrange(desc(nchar(Variables)), is.na(LS.ff3))
    } else{
      output_tibs <- output_tibs %>% 
        left_join(results_ff5 %>% 
          dplyr::rename(LSD.ff5 = LSD) %>% 
          left_join(results_ff3 %>% 
                      dplyr::rename(LSD.ff3 = LSD)
                    , by = "Variables")
          , by = "Variables")
    }
  }
  
  colnames(returns_data_excess) <- gsub(":","_",colnames(returns_data_excess))
  
  table_formatter(x = output_tibs,
                  filename = paste0("factorregression_FF3+5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0,nrow(output_tibs)-1),
                  row_names = FALSE)
  
  table_formatter(x = output_tibs_ff3,
                  filename = paste0("regression_portfolios_FF3_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0),
                  row_names = FALSE)
  
  table_formatter(x = output_tibs_ff5,
                  filename = paste0("regression_portfolios_FF5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0),
                  row_names = FALSE)
  
  colnames(returns_excess_ff5) <- gsub("_",":",colnames(returns_excess_ff5))
  
  returns_excess_ff5 <- returns_excess_ff5 %>% 
    mutate(LS = .[[cols+2]]-.[[3]])
  
  if(doublesort){
    portfolios_number <- cols^0.5
    returns_excess_ff5 <- returns_excess_ff5 %>% 
      mutate(LSD = .[[3+portfolios_number*(portfolios_number-1)]] - .[[portfolios_number+2]])
  }
  
  colnames(returns_data_excess) <- gsub(":","_",colnames(returns_data_excess))
  
  ################################################################################
  ############################### Competing Factors ##############################
  ################################################################################
  
  cf_data <- returns_excess_ff5 %>% 
    mutate(LS = .[[cols+2]] - .[[3]],
           period_ = paste0(lubridate::year(date.x), "_", week)) %>% 
    left_join(oa_data %>% 
                mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
                dplyr::select(-date),
              by = c("period_")) %>% 
    fill(c(colnames(oa_data %>% dplyr::select(-date))), .direction = "up") %>% 
    dplyr::filter(!is.na(date.x) & !is.na(date.y))
  
  cf_data[is.na(cf_data)] <- 0
  
  if(doublesort){
    cf_data <- cf_data %>% 
      mutate(LSD <- .[[2+cols^0.5]] - .[[(cols^0.5)*(cols^0.5-1)+3]]) %>% 
      dplyr::select(date=date.x, 
                    IMPVOL = LS,
                    IMPVOL_D = LSD,
                    MKT_RF = `Mkt-RF`,
                    SMB,
                    HML,
                    RMW,
                    CMA,
                    BetaLiquidityPS,
                    betaVIX,
                    CoskewACX,
                    Coskewness,
                    OptionVolume1,
                    OptionVolume2, 
                    skew1)
    
    factors <- c("IMPVOL", "IMPVOL_D", "MKT_RF", "SMB", "HML", "RMW", "CMA")
    factors_all <- c("IMPVOL", "IMPVOL_D", "MKT_RF", "SMB", "HML", "RMW", "CMA", "BetaLiquidityPS", "betaVIX", "CoskewACX", "Coskewness", "OptionVolume1", "OptionVolume2", "skew1")
    
    models <- lapply(paste(factors, ' ~ IMPVOL + IMPVOL_D + MKT_RF + SMB + HML + RMW + CMA-',factors),
                     function(f){ lm(as.formula(f), data = cf_data) %>% 
                         summary() %>%
                         "$"(coef) %>%
                         data.frame() %>%
                         dplyr::filter(rownames(.) == "(Intercept)") %>% 
                         dplyr::select(Estimate,`Pr...t..`)
                     }
    )
    
    models_all <- lapply(paste(factors_all, ' ~ IMPVOL + IMPVOL_D + MKT_RF + SMB + HML + RMW + CMA + BetaLiquidityPS + betaVIX + CoskewACX + Coskewness + OptionVolume1 + OptionVolume2 + skew1 -',factors_all),
                     function(f){ lm(as.formula(f), data = cf_data) %>% 
                         summary() %>%
                         "$"(coef) %>%
                         data.frame() %>%
                         dplyr::filter(rownames(.) == "(Intercept)") %>% 
                         dplyr::select(Estimate,`Pr...t..`)
                     }
    )
  } else{
    cf_data <- cf_data %>% 
      dplyr::select(date=date.x, 
                    IMPVOL = LS,
                    MKT_RF = `Mkt-RF`,
                    SMB,
                    HML,
                    RMW,
                    CMA,
                    BetaLiquidityPS,
                    betaVIX,
                    CoskewACX,
                    Coskewness,
                    OptionVolume1,
                    OptionVolume2, 
                    skew1)
    
    factors <- c("IMPVOL", "MKT_RF", "SMB", "HML", "RMW", "CMA")
    factors_all <- c("IMPVOL", "MKT_RF", "SMB", "HML", "RMW", "CMA", "BetaLiquidityPS", "betaVIX", "CoskewACX", "Coskewness", "OptionVolume1", "OptionVolume2", "skew1")
    
    models <- lapply(paste(factors, ' ~ IMPVOL + MKT_RF + SMB + HML + RMW + CMA-',factors),
                     function(f){ lm(as.formula(f), data = cf_data) %>% 
                         summary() %>%
                         "$"(coef) %>%
                         data.frame() %>%
                         dplyr::filter(rownames(.) == "(Intercept)") %>% 
                         dplyr::select(Estimate,`Pr...t..`)
                       }
    )
    
    models_all <- lapply(paste(factors_all, ' ~ IMPVOL + MKT_RF + SMB + HML + RMW + CMA + BetaLiquidityPS + betaVIX + CoskewACX + Coskewness + OptionVolume1 + OptionVolume2 + skew1 -',factors_all),
                         function(f){ lm(as.formula(f), data = cf_data) %>% 
                             summary() %>%
                             "$"(coef) %>%
                             data.frame() %>%
                             dplyr::filter(rownames(.) == "(Intercept)") %>% 
                             dplyr::select(Estimate,`Pr...t..`)
                         }
    )
  }
  
  results <- matrix(NA, nrow = length(factors), ncol = length(factors) + 1) 
  signif <- matrix(NA, nrow = length(factors), ncol = length(factors) + 1) 
  results_all <- matrix(NA, nrow = length(factors_all), ncol = length(factors_all) + 1) 
  signif_all <- matrix(NA, nrow = length(factors_all), ncol = length(factors_all) + 1) 
  
  for(j in 1:length(factors)){
    form <- paste(factors[j],
                  ' ~ ',paste(factors, collapse="+"),'-',factors[j])
    fit <- lm(form, data = cf_data) %>% 
      summary() 
    
    #fit_NW <- coeftest(fit, vcov.=NeweyWest(fit, lag=52, adjust=TRUE, verbose=TRUE)) %>% 
    #  summary()
    
    coef <- fit$coefficients[,1]
    p_val <- fit$coefficients[,4] 
    results[j,-(j+1)] <- coef 
    signif[j,-(j+1)] <- p_val
  }
  for(j in 1:length(factors_all)){
    form <- paste(factors_all[j],
                  ' ~ ',paste(factors_all, collapse="+"),'-',factors_all[j])
    fit <- lm(form, data = cf_data) %>% 
      summary() 
    
    coef <- fit$coefficients[,1]
    p_val <- fit$coefficients[,4] 
    results_all[j,-(j+1)] <- coef 
    signif_all[j,-(j+1)] <- p_val
  }
  
  ### FF5
  signif[is.na(signif)] <- 1
  results <- results %>% 
    round(2) %>% 
    data.frame() 
  results[signif<0.001] <- paste(results[signif<0.001]," (***)") 
  results[signif>0.001&signif<0.01] <- paste(results[signif>0.001&signif<0.01]," (**)") 
  results[signif>0.01&signif<0.05] <- paste(results[signif>0.01&signif<0.05]," (*)")
  results <- cbind(as.character(factors), results) 
  colnames(results) <- c("Dep. Variable","Intercept", factors)
  
  table_formatter(x = results %>% tibble() %>% mutate(across(everything(), as.character)),
                  filename = paste0("factorcompetition_FF5_", scenarioID, ".tex"),
                  hlines_after = c(0)
                  )
  
  ### ALL
  signif_all[is.na(signif_all)] <- 1
  results_all <- results_all %>% 
    round(2) %>% 
    data.frame() 
  results_all[signif_all<0.001] <- paste(results_all[signif_all<0.001]," (***)") 
  results_all[signif_all>0.001&signif_all<0.01] <- paste(results_all[signif_all>0.001&signif_all<0.01]," (**)") 
  results_all[signif_all>0.01&signif_all<0.05] <- paste(results_all[signif_all>0.01&signif_all<0.05]," (*)")
  results_all <- cbind(as.character(factors_all), results_all) 
  colnames(results_all) <- c("Dep. Variable","Intercept", factors_all)
  
  table_formatter(x = results_all %>% tibble() %>% mutate(across(everything(), as.character)),
                  filename = paste0("factorcompetition_ALL_", scenarioID, ".tex"),
                  hlines_after = c(0)
  )
  
  ################################################################################
  ################################# More Plotting ################################
  ################################################################################
  
  png(filename = paste0(projectplotpath,"/Heatmap_correlation_", scenarioID,".png"))
  heatmap(cor(returns_data_excess %>% 
                dplyr::select(-date.x, -week, -`period_`, -date.y, -`Mkt-RF`, -SMB, -HML, -RF),
              use="complete.obs"), 
          Colv = NA, 
          Rowv = NA, 
          scale="column", 
          xlab="Scaled by column", 
          ylab="Portfolios", 
          main="Correlation Heatmap",
          col = colorRampPalette(brewer.pal(8, "Blues"))(100))
  dev.off()
  
  ################################################################################
  ######################### Fama Macbeth - Time Invariant ########################
  ################################################################################
  
  if(doublesort){
    factors <- c("LS", "LSD")
  }else{
    factors <- c("LS")
  }
  
  fm_data_simple <- returns_data_excess %>% 
    dplyr::select(c(1:(cols+2))) %>% #, all_of(factors)) %>% 
    gather(key = "Portfolio", value = "Return", -c(date.x, week)) %>% 
    mutate(period = paste0(lubridate::year(date.x),"_",lubridate::week(date.x))) %>% 
    left_join(
      signal_data_raw %>% 
        mutate(Portfolio = ifelse(portfolios2, 
                                  paste0(portfolios1,"_",portfolios2),
                                  as.character(portfolios1)),
               period = paste0(lubridate::year(date), "_", lubridate::week(date) + ifelse(pred, 1, 0))) %>% 
        dplyr::select(Portfolio, period, median),
      by = c("Portfolio", "period")
    ) %>% 
    left_join(returns_data_excess %>% 
                mutate(period = paste0(lubridate::year(date.x),"_",lubridate::week(date.x))) %>% 
                dplyr::select(period, all_of(factors)),
              by = "period") %>% 
    dplyr::select(period, Portfolio, Return, median, all_of(factors)) %>% 
    group_by(Portfolio) %>% 
    dplyr::filter(!is.na(median))
  
  fm_data_simple[is.na(fm_data_simple)] <- 0
  
  # First stage regression
  
  accio <- as.formula(paste0("Return ~ median + ", paste(factors, collapse = " + ")))
  
  fm_first_stage_simple <- fm_data_simple %>% 
    group_map(~ broom::tidy(lm(accio, data = .x))) %>% 
    bind_rows() %>% 
    mutate(group = sort(rep(1:cols, times=(2+length(factors)))))
  
  # Second Stage
  
  fm_second_stage_simple <- fm_data_simple %>% 
    mutate(group = cur_group_id()) %>% 
    dplyr::select(-all_of(factors), -median) %>% 
    left_join(
      fm_first_stage_simple %>% 
        dplyr::filter(term %in% c("median", "LS", "LSD")) %>% 
        dplyr::select(group, term, estimate) %>% 
        spread(key = term, value = estimate)
      , by = "group") %>% 
    ungroup() %>% 
    #group_by(period) %>% 
    nest(data = -period) %>% 
    mutate(fit = map(data, ~ lm(accio, data = .x)), # S3 list-col
           tidied = map(fit, tidy)) %>% 
    unnest(tidied) %>% 
    #group_map(~ broom::tidy(lm(accio, data = .x))) %>% 
    #bind_rows() %>% 
    group_by(term) %>% 
    summarize(gamma_hat = mean(estimate),
              gamma_hat_se = var(estimate)^0.5) %>% 
    mutate(tstat = gamma_hat/(gamma_hat_se))
  table_formatter(x = fm_second_stage_simple,
                  filename = paste0("fm_results_timeinvariant_simple_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0))
  
  ################################################################################
  ########################## Fama Macbeth - Time Variant #########################
  ################################################################################
  
  window_length <- 52
  group_fm_second <- tibble()
  
  # First stage regression
  
  fm_first_stage_simple_time <- fm_data_simple %>% 
    dplyr::filter(!is.na(Return) & !is.na(median)) %>% 
    mutate(group = cur_group_id())
    
  for(i in unique(fm_first_stage_simple_time$group)){
    group_fm_first <- fm_first_stage_simple_time %>% 
      dplyr::filter(group == i)
    x_data <- group_fm_first %>% 
      ungroup() %>% 
      dplyr::select(median, all_of(factors))
    coefs1 <- roll_lm(x = as.matrix(x_data), 
                      y = group_fm_first$Return, 
                      width = window_length,
                      intercept = TRUE)$coefficients %>% 
      as_tibble()
    
    colnames(coefs1) <- paste0("beta_", colnames(coefs1))
    
    group_fm_second <- group_fm_first %>% 
      cbind(coefs1) %>% 
      rbind(group_fm_second)
    
  }
  
  colnames(group_fm_second) <- c("period", "Portfolio", "Return", "median", factors, "group", "beta_intercept", "beta_median", paste0("beta_", factors))
  if(doublesort){
    accio_beta <- as.formula("Return ~ beta_median + beta_LS + beta_LSD")
  }else{
    accio_beta <- as.formula("Return ~ beta_median + beta_LS")
  }
  
  # Second Stage
  
  fm_second_stage_simple_time <- group_fm_second %>% 
    dplyr::filter(!is.na(beta_median)) %>% 
    group_by(period) %>% 
    group_map(~ broom::tidy(lm(accio_beta, data = .x))) %>% 
    bind_rows() %>% 
    group_by(term) %>% 
    summarize(gamma_hat = mean(estimate),
              gamma_hat_se = var(estimate)^0.5) %>% 
    mutate(tstat = gamma_hat/(gamma_hat_se),
           term = str_remove(term, "beta_"))
  
  table_formatter(x = fm_second_stage_simple_time,
                  filename = paste0("fm_results_timevariant_simple_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0))
  
  ################################################################################
  ################ Fama Macbeth - Time Invariant + Other Factors #################
  ################################################################################
  
  fm_data_ff5 <- returns_data_excess %>% 
    dplyr::select(c(1:(cols+2))) %>% 
    gather(key = "Portfolio", value = "Return", -c(date.x, week)) %>% 
    mutate(period = paste0(lubridate::year(date.x),"_",lubridate::week(date.x))) %>% 
    left_join(
      signal_data_raw %>% mutate(Portfolio = ifelse(portfolios2, 
                                                    paste0(portfolios1,"_",portfolios2),
                                                    as.character(portfolios1)),
                                 period = paste0(lubridate::year(date), "_", lubridate::week(date) + ifelse(pred, 1, 0))) %>% 
        dplyr::select(Portfolio, period, median),
      by = c("Portfolio", "period")
    ) %>% 
    dplyr::select(period, Portfolio, Return, median) %>% 
    left_join(returns_excess_ff5 %>% 
                dplyr::select(`period:`, `Mkt-RF`, SMB, HML, RMW, CMA, RF),
              by = c("period" = "period:")) %>% 
    group_by(Portfolio) %>% 
    left_join(returns_data_excess %>% 
                mutate(period = paste0(lubridate::year(date.x),"_",lubridate::week(date.x))) %>% 
                dplyr::select(period, all_of(factors)),
              by = "period") 
  
  fm_data_ff5[is.na(fm_data_ff5)] <- 0
  
  # First stage regression
  
  accio_ff5 <- as.formula(paste0("Return ~ median + `Mkt-RF` + SMB + HML + RMW + CMA + ", paste(factors, collapse = " + ")))
  accio_ff5_beta <- as.formula(paste0("Return ~ median + MKT + SMB + HML + RMW + CMA + ", paste(factors, collapse = " + ")))
  
  fm_first_stage_ff5 <- fm_data_ff5 %>% 
    group_map(~ broom::tidy(lm(accio_ff5, data = .x))) %>% 
    bind_rows() %>% 
    mutate(group = sort(rep(1:cols, times=7+length(factors))))
  
  # Second Stage
  
  fm_second_stage_ff5 <- fm_data_ff5 %>% 
    mutate(group = cur_group_id()) %>% 
    dplyr::select(period, Portfolio, Return, group) %>% 
    left_join(
      fm_first_stage_ff5 %>% 
        dplyr::filter(term != "(Intercept)") %>% 
        dplyr::select(group, beta = estimate, variable = term) %>% 
        spread(key = variable, value = beta) %>% 
        dplyr::rename(MKT = `\`Mkt-RF\``)
      , by = "group") %>% 
    ungroup() %>% 
    group_by(period) %>% 
    group_map(~ broom::tidy(lm(accio_ff5_beta, data = .x))) %>% 
    bind_rows() %>% 
    group_by(term) %>% 
    summarize(gamma_hat = mean(estimate, na.rm = TRUE),
              gamma_hat_se = var(estimate, na.rm = TRUE)^0.5) %>% 
    mutate(tstat = gamma_hat/(gamma_hat_se))
  
  table_formatter(x = fm_second_stage_ff5,
                  filename = paste0("fm_results_timeinvariant_ff5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0))
  
  ################################################################################
  ################## Fama Macbeth - Time Variant + Other Factors #################
  ################################################################################
  
  window_length <- window_length
  group_fm_second <- tibble()
  
  # First stage regression
  
  fm_first_stage_ff5_time <- fm_data_ff5 %>% 
    dplyr::filter(!is.na(Return) & !is.na(median)) %>% 
    mutate(group = cur_group_id())
  
  for(i in unique(fm_first_stage_ff5_time$group)){
    # i = 1
    group_fm_first <- fm_first_stage_ff5_time %>% 
      dplyr::filter(group == i) %>% 
      dplyr::rename(MKT = 'Mkt-RF')
  
    x_data <- group_fm_first %>% 
      ungroup() %>% 
      dplyr::select(median, MKT, SMB, HML, RMW, CMA, all_of(factors))
    coefs1 <- roll_lm(x = as.matrix(x_data), 
                      y = group_fm_first$Return, 
                      width = window_length,
                      intercept = TRUE)$coefficients %>% 
      as_tibble()
    
    colnames(coefs1) <- paste0("beta_", colnames(coefs1))
    
    #coefs1 <- roll_regres(accio_ff5_beta,
    #                      data = group_fm_first,
    #                      width = window_length
    #                      )$coefs %>% 
    #  as_tibble() %>% 
    #  dplyr::select(-`(Intercept)`)
    
    #if(doublesort){
    #  colnames(coefs1) <- c("beta_median", "beta_MKT", "beta_SMB", "beta_HML", "beta_RMW", "beta_CMA", "beta_LS", "beta_LSD")
    #}else{
    #  colnames(coefs1) <- c("beta_median", "beta_MKT", "beta_SMB", "beta_HML", "beta_RMW", "beta_CMA", "beta_LS")
    #}
      
    group_fm_second <- group_fm_first %>% 
      cbind(coefs1) %>% 
      rbind(group_fm_second)
  }
  
  # Second Stage
  
  accio_ff5_beta_time <- as.formula(paste0("Return ~ beta_median + beta_MKT + beta_SMB + beta_HML + beta_RMW + beta_CMA + ", paste(paste0("beta_",factors), collapse = " + ")))
  
  fm_second_stage_ff5_time <- group_fm_second %>% 
    dplyr::filter(!is.na(beta_median)) %>% 
    ungroup() %>% 
    group_by(period) %>% 
    group_map(~ broom::tidy(lm(accio_ff5_beta_time, data = .x))) %>% 
    bind_rows() %>% 
    group_by(term) %>% 
    summarize(gamma_hat = mean(estimate, na.rm = TRUE),
              gamma_hat_se = var(estimate, na.rm = TRUE)^0.5) %>% 
    mutate(tstat = gamma_hat/(gamma_hat_se))
  
  table_formatter(x = fm_second_stage_ff5_time,
                  filename = paste0("fm_results_timevariant_ff5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0))
  
  ################################################################################
  ############################## Three-step Procedure ############################
  ################################################################################
  
  suppressWarnings({
    otherport_ff100 <- read_csv(paste0(ffdatapath, "/100_Portfolios_10x10_Daily.csv"),
      #paste0(ffdatapath, "/25_Portfolios_5x5_Daily.csv"),
      show_col_types = FALSE,
      skip = 18,
      col_names = TRUE) %>% 
      dplyr::rename(date = `...1`) %>% 
      mutate(date = ymd(date)) %>% 
      dplyr::filter(date >= ymd("1996-01-01")
                    & date <= ymd("2023-01-01")) %>%
      mutate(across(c(where(is.numeric)), ~ ifelse(.x == -999, NA, .x))) %>% 
      mutate(across(c(where(is.numeric)), ~ ifelse(.x == -99.99, NA, .x))) %>% 
      mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
      group_by(period_) %>% 
      mutate(across(c(where(is.numeric)), ~ .x/100 + 1)) %>% 
      mutate(across(c(where(is.numeric)), cumprod)) %>% 
      mutate(across(c(where(is.numeric)), ~ .x*100 - 100)) %>%
      arrange(desc(date)) %>% 
      dplyr::filter(row_number() == 1) %>% 
      arrange(date) %>% 
      dplyr::select(-date) %>% 
      left_join(ff5_data %>% 
                  dplyr::select(period_, RF),
                by = c("period_")) %>% 
      #dplyr::rename(`period:` = period_) %>% 
      mutate(across(c(where(is.numeric), -RF), ~ .x - RF)) %>% 
      dplyr::select(-RF) %>% 
      ungroup()
    
  })
  
  cols_orig <- cols
  cols_here <- cols+100
  
  # Taking the excess returns - normalizing them and lastly getting the correlationmatrix
  initial_data_tp <- returns_excess_ff5 %>%
    mutate(period_ = paste0(lubridate::year(date.x), "_", week)) %>% 
    relocate(period_) %>% 
    dplyr::select(1:(cols_orig+3)) %>% 
    left_join(otherport_ff100,
              by = c("period_")) %>% 
    dplyr::select(1:cols_here+3)
  
  initial_data_tp[is.na(initial_data_tp)] <- 0
  
  princompana <- initial_data_tp %>% 
    scale() %>% 
    cor(., use="complete.obs") %>% 
    princomp()
  
  tibble(pov = princompana$sdev^2/sum(princompana$sdev^2)) %>% 
    mutate(pc = row_number(),
           pov = round(pov,digits = 3)) %>% 
    dplyr::filter(pc<=25) %>% 
    ggplot(aes(x=pc,y=pov)) +
    geom_col(fill = "lightblue") +
    geom_point(size=2) +
    geom_text(aes(label = after_stat(y)), nudge_x = 0.4, nudge_y = 0.01) +
    geom_line() +
    theme_light() +
    labs(x = "Principal Components",
         y = "Proportion of Variance")
  ggsave(filename = paste0("principalcomponents_proportionvar_",scenarioID,".png"),
         path = projectplotpath,
         width = 250, height = 110, units = "mm")
  
  # Choosing the optimal amount of factors : robustness tests for more factors included.
  
  n_lags <- 12 # The paper chooses 3, we choose the equivalent in weeks (3*4 weeks per month app.)
  rt <- initial_data_tp %>% # returns_excess_ff5 %>% 
    #dplyr::select(1:cols+2) %>%
    t()
  rt[is.na(rt)] <- 0
  
  rtbar <- rt - matrix(rep(rowMeans(rt, na.rm = TRUE), times = dim(rt)[2]), ncol = dim(rt)[2], byrow = FALSE) %>% 
    as.matrix()
  #rtbar[is.na(rtbar)] <- 0
  
  rbar <- rowMeans(rt, na.rm = TRUE)
  
  gt <- returns_excess_ff5 %>%
    mutate(LS = ifelse(is.na(.[[cols_orig+2]]),0,.[[cols_orig+2]]) - ifelse(is.na(.[[3]]),0,.[[3]])) %>% 
    dplyr::select((cols_orig+5):ncol(.), -date) %>% 
    dplyr::rename(MKT_RF = `Mkt-RF`) %>% 
    dplyr::select(-RF)
  gt[is.na(gt)] <- 0
  
  gtbar <- gt - rowMeans(gt, na.rm = TRUE)
  d <- ncol(gtbar)
  
  mtr <- (as.matrix(rtbar))/sqrt(cols_here)/iT
  mtr[is.na(mtr)] <- 0
  
  S  <-  svd((mtr)) %>% 
    .$d
  
  tmp_eig <- S^2
  
  # Estimate objective function for all numbers
  obj <- tmp_eig[1:(cols_here-1)] + 
    0.5*1:(cols_here-1) * 
    (log(cols_here) + log(iT) ) * 
    (cols_here^(-0.5) + iT^(-0.5)) * median( tmp_eig[1:(cols_here-1)] )
  
  # Get optimal as estimate
  pHat <- which(obj==min(obj))-1
  pMax <- min(pHat + 2, cols_here)
  
  if(doublesort){
    gammahat <- matrix(data = NA, ncol = 10, nrow = 8)#cols)
  } else{
    gammahat <- matrix(data = NA, ncol = 10, nrow = 7)#cols)
  }
  #colnames(gammahat) <- c(1:10)
  alphahat <- matrix(data = NA, ncol = pMax, nrow = cols_here)
  avarhat <- matrix(NA, ncol = pMax, nrow = d+1)
  gthat <- array(rep(NA, d*dim(rt)[2]*pMax), dim = c(d, dim(rt)[2], pMax))
  What_obs <- matrix(NA, ncol = pMax, nrow = d)
  R2F <- matrix(data = NA, ncol = pMax, nrow = 1)
  R2G <- matrix(data = NA, ncol = pMax, nrow = d)
  
  # Eigenvectors :
  V  <-  svd(mtr) %>% 
    .$v
  
  for(p_hat in 1:pMax){  # at least one factor up to pmax
    #print(paste0("p_hat = ", p_hat))
    #p_hat=1 + p_hat
    vhat <- iT^0.5 * t(V[,1:p_hat]) # dims : phat * iT
    betahat <- iT^(-1) * rtbar %*% t(vhat) # dims: cols * phat
    
    Sigmavhat <- (vhat %*% t(vhat)) / iT # dims : phat * phat
    
    betahat_andintercept <- cbind(rep(1,times = cols_here), betahat) %>% 
      as.data.frame()
    
    # step 2: FM with PCs
    lumos <- as.formula(paste0("R ~ -1 +", paste(colnames(betahat_andintercept), collapse = " + ")))
    gammatilde <- lm(lumos, data = data.frame(R = rbar, betahat_andintercept)) %>% 
      .$coefficients # dims : phat + 1 * 1
    
    # step 3: TS Regression
    etahat <- t(gtbar) %*% t(vhat) %*% solve(vhat %*% t(vhat))
    what <- t(gtbar) - etahat %*% vhat
    
    # step 4: Combine TS and CS estimates
    gammahat[,p_hat] <- matrix(rbind(gammatilde[1], etahat %*% gammatilde[2:length(gammatilde)]))
    #gammahat[1:(nrow(etahat)+1),p_hat] <- matrix(rbind(gammatilde[1], etahat %*% gammatilde[2:length(gammatilde)]))
    
    # step 5: Goodness of Fit
    Miota <- diag(cols_here) - matrix(data = 1, nrow = cols_here, ncol = cols_here)/cols_here
    R2F[1,p_hat] <- t(rbar) %*% Miota %*% betahat %*% solve(t(betahat) %*% Miota %*% betahat) %*% t(betahat) %*% Miota %*% rbar / (t(rbar) %*% Miota %*% rbar)
    R2G[1,p_hat] <- 1/diag(t(gtbar) %*% as.matrix(gtbar)) %*% diag(etahat %*% (vhat %*% t(vhat)) %*% t(etahat))
    
    # step 6: Newey-West Estimation of Avar
    Pi11hat <- matrix(data = 0, nrow = d*p_hat, ncol = d*p_hat)
    Pi12hat <- matrix(data = 0, nrow = d*p_hat, ncol = p_hat)
    Pi22hat <- matrix(data = 0, nrow = p_hat, ncol = p_hat)
                                                                   
    for(t in 1:iT){
      # t <- 1
      # what : dim = d , iT. vhat : dim = phat , iT
      Pi11hat[1:(d*p_hat), 1:(d*p_hat)] <- Pi11hat[1:(d*p_hat), 1:(d*p_hat)] + as.vector(what[,t] %*% t(vhat[,t])) %*% t(as.vector(what[,t] %*% t(vhat[,t])))/iT
      Pi12hat[1:(d*p_hat), 1:p_hat]     <- Pi12hat[1:(d*p_hat),1:p_hat]      + as.vector(what[,t] %*% t(vhat[,t])) %*% t(vhat[,t])    /iT # OBS: small change
      Pi22hat[1:p_hat, 1:p_hat]         <- Pi22hat[1:p_hat,1:p_hat]          + vhat[,t] %*%           t(vhat[,t])                                 /iT
      
      for(s in min(1,t-1):min(t-1,n_lags)){
        # s <- 0
        Pi11hat[1:(d*p_hat), 1:(d*p_hat)] <- Pi11hat[1:(d*p_hat), 1:(d*p_hat)] + 1/iT*(1-s/(n_lags+1)) * (as.vector(what[,t] %*% t(vhat[,t])) %*% t(as.vector(what[,t-s] %*% t(vhat[,t-s]))) + as.vector(what[,t-s] %*% t(vhat[,t-s])) %*% t(as.vector(what[,t] %*% t(vhat[,t]))))
        Pi12hat[1:(d*p_hat), 1:p_hat]     <- Pi12hat[1:(d*p_hat), 1:p_hat] + 1/iT*(1-s/(n_lags+1)) * (as.vector(what[,t] %*% t(vhat[,t])) %*% t(vhat[,t]) + as.vector(what[,t] %*% t(vhat[,t])) %*% t(vhat[,t]))
        Pi22hat[1:p_hat, 1:p_hat]         <- Pi22hat[1:p_hat, 1:p_hat] + 1/iT*(1-s/(n_lags+1)) * (vhat[,t] %*% t(vhat[,t]) + vhat[,t] %*% t(vhat[,t]))
      }
    }
    
    alphahat[1:cols_here,p_hat] <- rbar - cbind(matrix(data = 1, nrow = cols_here, ncol = 1), betahat) %*% gammatilde
    
    # kron :: %x%
    
    avarhat[1:(d+1),p_hat] <- rbind(
      as.matrix((1-colMeans(betahat) %*% solve(t(betahat) %*% betahat / cols_here) %*% colMeans(betahat)^(-1) %*% var(alphahat[1:cols_here,p_hat]))/cols_here),
      as.matrix(diag(
        ( #obs: used to be as.matrix
            (t(gammatilde[2:(p_hat+1)]) %*% solve(Sigmavhat) ) %x% 
            diag(d) # dims : d x n
        ) %*% Pi11hat[1:(d*p_hat), 1:(d*p_hat)] %*% # problemchild
          ( (solve(Sigmavhat) %*% gammatilde[2:(p_hat+1)]) %x% 
             diag(d)
           ) / iT + # works : gives 6*6
          (
            (t(gammatilde[2:(p_hat+1)]) %*% solve(Sigmavhat)) %x% 
              diag(d) 
            ) %*% 
          Pi12hat %*% t(etahat) / iT + # works : gives 6*6
        t(
          (
            (
              t(gammatilde[2:(p_hat+1)]) %*% solve(Sigmavhat)
              ) %x% 
              diag(d) 
            ) %*% 
            Pi12hat %*% t(etahat)
          ) / iT + # works : gives 6*6
        as.matrix(etahat) %*% Pi22hat %*% t(etahat) / iT ) + # done so far, dim 1*6
        diag( 
          var(alphahat[1:cols,p_hat]) * as.matrix(etahat) %*% solve(t(betahat) %*% betahat/cols_here - as.matrix(colMeans(betahat)) %*% t(colMeans(betahat))) %*% t(etahat) ) / cols_here
        )
    )
    
    # step 7: recovery of gt
    gthat[1:d,1:dim(rt)[2],p_hat] <- etahat %*% vhat
    
    # Test of eta = 0 at phat
    
    for(j in 1:d){
      # j=1
      idx <- seq(from = 1, to = d*p_hat, by = d)
      What_obs[j,p_hat] <- iT * etahat[j,] %*% solve( solve(Sigmavhat) %*% Pi11hat[idx,idx] %*% solve(Sigmavhat) ) %*% as.matrix(etahat[j,])
    }
  }
  
  # Coefs for optimal P_hat
  optim_phat <- pHat
  
  tp_coefs <- gammahat[0:(d+1),1:pMax]
  tp_stderrors <- sqrt(avarhat[,1:pMax])
  tp_tstats <- tp_coefs/tp_stderrors
  tp_pval <- 1-pt(tp_tstats, df = cols_here*iT)
  
  ## Table of both coefs and standard errors
  variable = c("Intercept", colnames(gt))
  tp_coefs_se <- tp_coefs %>% 
    round(digits = 4) %>% 
    cbind(variable = paste0(variable, ":estimate")) %>% 
    rbind(
      tp_stderrors %>% 
        round(digits = 4) %>% 
        cbind(variable = paste0(variable, ":std.error"))
    )
  
  colnames(tp_coefs_se) <- c(1:pMax, "Factor")
  
  tp_coefs_se <- tp_coefs_se[, c("Factor", 1:pMax)]
  
  tp_coefs_ready <- tp_coefs_se[order(tp_coefs_se[, 1], decreasing=FALSE), ]
  
  table_formatter(x = tp_coefs_ready,
                  filename = paste0("threepass_coefs_se_FF5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0)
  )
  
  ## Table of only coefs with significance stars
  tp_pval_forsignif <- cbind(rep(1, times = nrow(tp_coefs)), tp_pval)
  tp_pval_forsignif[is.na(tp_pval_forsignif)] <- 1
  results <- tp_coefs %>% 
    round(3) %>% 
    data.frame() %>% 
    mutate(variable = c("Intercept", colnames(gt))) %>%
    relocate(variable)
  results[tp_pval_forsignif<0.001] <- paste(results[tp_pval_forsignif<0.001]," (***)") 
  results[tp_pval_forsignif>0.001&tp_pval_forsignif<0.01] <- paste(results[tp_pval_forsignif>0.001&tp_pval_forsignif<0.01]," (**)") 
  results[tp_pval_forsignif>0.01&tp_pval_forsignif<0.05] <- paste(results[tp_pval_forsignif>0.01&tp_pval_forsignif<0.05]," (*)")
  
  colnames(results) <- c("Factor", 1:pMax)
  
  table_formatter(x = results %>% tibble() %>% mutate(across(everything(), as.character)),
                  filename = paste0("threepass_coefs_signif_FF5_", scenarioID, ".tex"),
                  hlines_after = c(0)
  )
  
  # Strength of the factors:
  tp_strength <- round(R2G[1,],digits = 4)
  tp_wald <- round(1 - pchisq(What_obs,1:pMax), digits = 4)
  tp_strength_tabling <- cbind(c("R2_G"), t(tp_strength)) %>% 
    rbind(
      cbind(colnames(gt), tp_wald)
    ) %>% 
    data.frame()
  
  colnames(tp_strength_tabling) <- c("term", 1:pMax)
  
  table_formatter(x = tp_strength_tabling,
                  filename = paste0("threepass_strength_FF5_", scenarioID, ".tex"),
                  num_decimals = 4,
                  hlines_after = c(0,1)
  )
  
  cols <- cols_orig
  
  ################################################################################
  ################## Efficient Frontier and Optimal Portfolios ###################
  ################################################################################
  
  # dummies for portfolio number + dummy for doublesorting portfolio numbers (both directions)
  # dummies for post publication and post dataperiod
  # control factors (FF5 + lagged values of FF5)
  
  suppressWarnings({
    otherport_ff25 <- read_csv(#paste0(ffdatapath, "/100_Portfolios_10x10_Daily.csv"),
                                paste0(ffdatapath, "/25_Portfolios_5x5_Daily.csv"),
                                show_col_types = FALSE,
                                skip = 18,
                                col_names = TRUE) %>% 
      dplyr::rename(date = `...1`) %>% 
      mutate(date = ymd(date)) %>% 
      dplyr::filter(date >= ymd("1996-01-01")
                    & date <= ymd("2023-01-01")) %>%
      mutate(across(c(where(is.numeric)), ~ ifelse(.x == -999, NA, .x))) %>% 
      mutate(across(c(where(is.numeric)), ~ ifelse(.x == -99.99, NA, .x))) %>% 
      mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
      group_by(period_) %>% 
      mutate(across(c(where(is.numeric)), ~ .x/100 + 1)) %>% 
      mutate(across(c(where(is.numeric)), cumprod)) %>% 
      mutate(across(c(where(is.numeric)), ~ .x*100 - 100)) %>%
      arrange(desc(date)) %>% 
      dplyr::filter(row_number() == 1) %>% 
      arrange(date) %>% 
      dplyr::select(-date) %>% 
      left_join(ff5_data %>% 
                  dplyr::select(period_, RF),
                by = c("period_")) %>% 
      dplyr::rename(`period:` = period_) %>% 
      mutate(across(c(where(is.numeric), -RF), ~ .x - RF)) %>% 
      dplyr::select(-RF)
    
  })
  
  data_efficient_frontier <- returns_excess_ff5 %>% 
    dplyr::select(-date.y, -week, -`period:`, -date) %>% 
    dplyr::rename(date = date.x) %>% 
    dplyr::select(1:(cols+1)) %>% 
    mutate(period_ = paste0(lubridate::year(date), "_", lubridate::week(date)))
  
  colnames(data_efficient_frontier) <- gsub("_",":",colnames(data_efficient_frontier))
  
  data_meanstd_efficient_frontier <- data_efficient_frontier %>% 
      left_join(otherport_ff25,
              by = c("period:")) %>% 
    summarize(across(c(where(is.numeric)), 
                     list(Mean = mean, 
                          Std = sd),
                     na.rm = TRUE)) %>%
    gather(key = "measure", value = "value") %>% 
    mutate(portfolio = str_split_fixed(measure,"_",2)[,1],
           measure = str_split_fixed(measure,"_",2)[,2]) %>% 
    spread(key = measure, value = value) %>% 
    mutate(own = ifelse(str_length(portfolio)<4, TRUE, FALSE)) 
  
  ggplot(data_meanstd_efficient_frontier,
         aes(x = Std, y = Mean, color = own)) +
    geom_point() +
    labs(x = "Sample Standard Deviation",
         y = "Sample Mean Excess Return",
         color = "Impl. Vol. Spread Signal:") +
    xlim(0,NA) +
    theme_light() +
    theme(legend.position="bottom")
  ggsave(filename = paste0("meanstd_efffrontier_entireperiod_",scenarioID,".png"),
         path = projectplotpath,
         width = 250, height = 110, units = "mm")
  
  ### Calculating the efficient frontier
  data_efffrontier <- data_efficient_frontier %>% 
    left_join(otherport_ff25,
              by = c("period:")) %>% 
    dplyr::select(-date,-`period:`) %>% 
    mutate(across(c(where(is.numeric)), ~ ifelse(is.na(.x),0,.x)))
  
  eff <- eff.frontier(
    returns = data_efffrontier,
    short = "no",
    max.allocation = NULL,
    risk.premium.up = 0.6,
    risk.increment = 0.0005)
  
  eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
  
  # Graph the weights given different allocation constraints
  all_con <- seq(from = 0.05, to = 0.5, length = 50)
  weight_eff <- tibble(port = c(colnames(eff %>% 
                                           tibble() %>% 
                                           dplyr::select(-sharpe, -Std.Dev, -Exp.Return))))
  for(cons in all_con){
    print(cons)
    #cons = 0.2
  
    tmp_eff <- hush(eff.frontier(
      returns = data_efffrontier,
      short = "no",
      max.allocation = cons,
      risk.premium.up = 0.6,
      risk.increment = 0.0005))
  
    tmp_eff.optimal.point <- tmp_eff[tmp_eff$sharpe==max(tmp_eff$sharpe),]
    
    weight_tmp <- tmp_eff.optimal.point %>% 
      tibble() %>% 
      dplyr::select(-sharpe, -Std.Dev, -Exp.Return) %>% 
      t() %>% 
      tibble() %>% 
      mutate(port = colnames(eff.optimal.point %>% 
                               tibble() %>% 
                               dplyr::select(-sharpe, -Std.Dev, -Exp.Return))) %>% 
      arrange(desc(.[,1]))
    
    colnames(weight_tmp) <- c(cons, "port")
    
    weight_eff <- weight_eff %>% 
      left_join(weight_tmp,
                by = c("port"))
  }
  
  colnames(weight_eff) <- c("port", round(all_con,4))
  
  weight_eff %>% 
    mutate(own = ifelse(nchar(port)<4,"Impl.Vol","FF5.5")) %>% 
    relocate(own, .before = port) %>% 
    gather(key = Allocation_constraint,
           value = Weights,
           -own,-port) %>% 
    mutate(Allocation_constraint = as.numeric(Allocation_constraint),
           Weights = round(Weights,10)) %>% 
    #dplyr::filter(grepl("\\d", port)) %>% 
    ggplot(aes(fill = port, y = Weights, x = Allocation_constraint)) +
    #ggplot(aes(fill = own, y = Weights, x = Allocation_constraint)) +
    geom_bar(position="stack", stat="identity") +
    ylim(0,1.000001) +
    xlim(min(all_con)-0.005, max(all_con)+0.005) +
    theme(legend.position = "bottom",
          #legend.key.width=unit(3,"mm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    labs(x = "Allocation Constraint on Portfolio Weights",
         y = "Individual Portfolio Weights",
         fill = "Portfolios:")
  ggsave(filename = paste0("optimalallocation_SharpeRatio_entireperiod_",scenarioID,".png"),
         path = projectplotpath,
         width = 300, height = 300, units = "mm")
  
  # graph efficient frontier
  # Start with color scheme
  #ealred <- "#7D110C"
  #ealtan <- "#CDC4B6"
  #eallighttan <- "#F7F6F0"
  #ealdark <- "#423C30"
  
  ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + 
    geom_point(alpha=.1, color="blue") +#ealdark) +
    geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
               color="blue", size=5) +#ealred, size=5) +
    annotate(geom="text", x=eff.optimal.point$Std.Dev,
             y=eff.optimal.point$Exp.Return,
             label=paste("Risk: ",
                         round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                         round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                         round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
             hjust=1, vjust=1.5) +
             #hjust=0, vjust=1.2) +
    #geom_text(aes(label = after_stat(y)), nudge_x = 0.4, nudge_y = 0.01)
    #ggtitle("Efficient Frontier and Optimal Portfolio") +
    labs(x="Risk (standard deviation of portfolio)", 
         y="Return") +
    theme_light()
    #theme(panel.background=element_rect(fill=eallighttan),
    #      text=element_text(color=ealdark),
    #      plot.title=element_text(size=24, color=ealred))
  ggsave(filename = paste0("efficientfrontier_entireperiod_",scenarioID,".png"),
         path = projectplotpath,
         width = 250, height = 110, units = "mm")
  
  ################################################################################
  ####################### Pooled Cross Sectional Regression ######################
  ################################################################################
  
  crosssec_data <- returns_data_excess %>% 
    dplyr::select(1:(cols+3)) %>% 
    left_join(ff5_data %>% 
                dplyr::rename(MKT = `Mkt-RF`) %>% 
                dplyr::select(-date),
              by = c("period_")) %>% 
    left_join(oa_data %>% 
                mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
                dplyr::select(-date),
              by = c("period_")) %>% 
    fill(c(colnames(oa_data %>% dplyr::select(-date))), .direction = "up") %>% 
    relocate(`period_`, .before = date.x)
  
  crosssec_reg_data <- crosssec_data %>% 
    gather(key = "Portfolio", value = "Return", 4:(cols+3)) %>%
    relocate(Portfolio, Return, .after = week) %>% 
    dummy_cols(
      select_columns = "Portfolio",
      remove_first_dummy = TRUE,
      remove_most_frequent_dummy = FALSE,
      ignore_na = FALSE,
      split = NULL,
      remove_selected_columns = TRUE
    ) %>% 
    relocate(colnames(.)[grepl("Portfolio_", colnames(.), fixed = TRUE)], .after = Return)
  
  factors <- colnames(crosssec_reg_data %>% 
                        dplyr::select(-`period_`, -date.x, -week, -Return))
  
  alohomora_simple <- as.formula(paste("Return ~ ", paste(factors[1:(cols-1)], collapse="+")))
  alohomora_some <- as.formula(paste("Return ~ ", paste(factors[1:(cols+5)], collapse="+")))
  alohomora_all <- as.formula(paste("Return ~ ", paste(factors, collapse="+")))
  
  sum_simple <- crosssec_reg_data %>% 
    lm(formula = alohomora_simple) %>% 
    summary()
  
  sum_some <- crosssec_reg_data %>% 
    lm(formula = alohomora_some) %>% 
    summary()
  
  sum_all <- crosssec_reg_data %>% 
    lm(formula = alohomora_all) %>% 
    summary()
  
  summary_tmp <- tidy(sum_all) %>% 
    dplyr::select(term, estimate, std.error) %>%
    gather(key = vers, value = coefficient, -term) %>%
    mutate(Variable = paste0(term, ":", vers), .keep = "unused") %>% 
    relocate(Variable) %>% 
    dplyr::rename(coefficient_all = coefficient) %>% 
    left_join(
      tidy(sum_some) %>% 
        dplyr::select(term, estimate, std.error) %>%
        gather(key = vers, value = coefficient, -term) %>%
        mutate(Variable = paste0(term, ":", vers), .keep = "unused") %>% 
        dplyr::rename(coefficient_some = coefficient),
      by = "Variable"
    ) %>% 
    left_join(
      tidy(sum_simple) %>% 
        dplyr::select(term, estimate, std.error) %>%
        gather(key = vers, value = coefficient, -term) %>%
        mutate(Variable = paste0(term, ":", vers), .keep = "unused") %>% 
        dplyr::rename(coefficient_simple = coefficient),
      by = "Variable"
    ) %>% 
    arrange(is.na(coefficient_simple), is.na(coefficient_some), Variable) %>% 
    rbind(
      tibble(Variable = "Rsquared", 
             coefficient_all = sum_all$adj.r.squared,
             coefficient_some = sum_some$adj.r.squared,
             coefficient_simple = sum_simple$adj.r.squared,)
    )
  
  table_formatter(x = summary_tmp, 
                  filename = paste0("/crosssectional_regs_", scenarioID, ".tex"), 
                  num_decimals = 4,
                  row_names = FALSE,
                  hlines_after = c(0,
                                   nrow(sum_simple$coefficients)*2, 
                                   nrow(sum_some$coefficients)*2,
                                   nrow(sum_all$coefficients)*2)
                  )
  
  # Make smaller plot for with stars
  
  comprised_table <- tidy(sum_all) %>% 
    dplyr::select(term, estimate, p.value) %>%
    mutate(estimate = paste0(round(estimate, digits = 4), 
                             ifelse(p.value < 0.001, " (***)", 
                                    ifelse(p.value <0.01, " (**)",
                                           ifelse(p.value < 0.05, " (*)", ""))))) %>% 
    dplyr::select(-p.value) %>% 
    dplyr::rename(coefficient_all = estimate) %>% 
    left_join(
      tidy(sum_some) %>% 
        dplyr::select(term, estimate, p.value) %>%
        mutate(estimate = paste0(round(estimate, digits = 4), 
                                 ifelse(p.value < 0.001, " (***)", 
                                        ifelse(p.value <0.01, " (**)",
                                               ifelse(p.value < 0.05, " (*)", ""))))) %>% 
        dplyr::select(-p.value) %>% 
        dplyr::rename(coefficient_some = estimate),
      by = "term"
    ) %>% 
    left_join(
      tidy(sum_simple) %>% 
        dplyr::select(term, estimate, p.value) %>%
        mutate(estimate = paste0(round(estimate, digits = 4), 
                                 ifelse(p.value < 0.001, " (***)", 
                                        ifelse(p.value <0.01, " (**)",
                                               ifelse(p.value < 0.05, " (*)", ""))))) %>% 
        dplyr::select(-p.value) %>% 
        dplyr::rename(coefficient_simple = estimate),
      by = "term"
    ) %>% 
    arrange(is.na(coefficient_simple), is.na(coefficient_some), nchar(term), term) %>% 
    rbind(
      tibble(term = "Rsquared", 
             coefficient_all = round(sum_all$adj.r.squared, digits = 4),
             coefficient_some = round(sum_some$adj.r.squared, digits = 4),
             coefficient_simple = round(sum_simple$adj.r.squared, digits = 4))
    )
  
  table_formatter(x = comprised_table,
                  filename = paste0("pooledcrosssection_all_", scenarioID, ".tex"),
                  hlines_after = c(0)
  )
  
  return(paste0("Done with: ", scenarioID))
}
