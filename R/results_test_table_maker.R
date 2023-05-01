#### Data analysis

# results_test_table_maker <- function(){}

source("/Users/mariedyveke/Documents/GitHub/thesis/R/Lib.R")

################################################################################
############################# import the FF factors ############################
################################################################################

ff_data_files <- list.files(ffdatapath)

ff3_data <- suppressWarnings({
  read_csv(paste0(ffdatapath,"/F-F_Research_Data_Factors_weekly.csv"), 
           skip = 3, 
           show_col_types = FALSE,
  ) %>% 
    dplyr::rename(date = `...1`) %>% 
    mutate(date = ymd(date)) %>% 
    dplyr::filter(lubridate::year(date) >= 1996 & lubridate::year(date) < 2023)
})

ff5_data <- read_csv(paste0(ffdatapath,"/F-F_Research_Data_5_Factors_2x3_daily.CSV"), 
                        skip = 3, 
                        show_col_types = FALSE) %>% 
  dplyr::rename(date = `...1`) %>% 
  mutate(date = ymd(date)) %>% 
  dplyr::filter(lubridate::year(date) >= 1996 & lubridate::year(date) < 2023) %>% 
  mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date))) %>% 
  group_by(period_) %>% 
  mutate(across(c(where(is.numeric)), ~ .x/100 + 1)) %>% 
  mutate(across(c(where(is.numeric)), cumprod)) %>% 
  mutate(across(c(where(is.numeric)), ~ .x*100 - 100)) %>% 
  #mutate(across(c(where(is.numeric)), sum)) %>% 
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
################### Patton&Timmermann test for Monotonicity ####################
################################################################################

# 1: load returns

scenarioID <- 4
returns_file <- paste0("returns_",scenarioID,".csv")

returns_data <- read_csv(paste0(getwd(),"/portfolios/", returns_file),
                         show_col_types = FALSE,
                         col_select = -c(`...1`))

cols  <- length(colnames(returns_data))-2
iT    <- nrow(returns_data)
iB    <- 5000

pred <- TRUE

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
  
  dim(tmp) <- c(max(dimensions$V2),
                max(dimensions$V1))
  # First sorting is columns
  # Second sorting is rows
  
  diff_returns <- tmp %>% 
    # Rowwise differences
    diff() %>%
    as_tibble() %>% 
    min() %>% 
    rbind(
      # Columnwise differences
      tmp %>%
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

### THE BOOTSTRAPPING

test_decreasing <- monoSummary(data          = rel_returns, 
                               bootstrapRep  = 1000, 
                               wolakRep      = 100,
                               increasing    = FALSE, 
                               difference    = FALSE, 
                               plot          = FALSE, 
                               block_length  = 6, 
                               zero_treshold = 1e-10)

test_increasing <- monoSummary(data          = rel_returns, 
                               bootstrapRep  = 1000, 
                               wolakRep      = 100,
                               increasing    = TRUE, 
                               difference    = FALSE, 
                               plot          = FALSE, 
                               block_length  = 6, 
                               zero_treshold = 1e-10)

################################################################################
############################### import the signal ##############################
################################################################################

signals_file <- paste0("signalportfolios_",scenarioID,".csv")

signal_data_raw <- read_csv(paste0(getwd(),"/portfolios/", signals_file),
                         show_col_types = FALSE,
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
  #group_by(portfolios1,portfolios2) %>% 
  #mutate(group = paste0(cur_group_id(), "_")) %>%
  dplyr::select(group, mean) %>% 
  #ungroup() %>% 
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

### PLOTTING

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
  labs(color = "Portfolios:",
       y = "Simple Cumulative Returns") +
  theme(legend.position="bottom")
ggsave(filename = paste0("cumulativereturns_entireperiod_simple_",scenarioID,".png"),
  path = projectplotpath,
  width = 250, height = 110, units = "mm")

# Average returns for each portfolio -> 3d surface for bivariate

if(doublesort){
  # first sorting is per column, and second is per row
  plots <- returns_data %>% 
    dplyr::select(-date, -week) %>% 
    colMeans() %>% 
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
         width = 110, height = 110, units = "mm")
} else{
  returns_data %>% 
    dplyr::select(-date, -week) %>% 
    colMeans() %>% 
    reshape::melt() %>% 
    mutate(value = (value-1)*100,
           x = rownames(.)) %>% 
    ggplot(aes(x = factor(x, levels = c(1:cols)), 
               y = value)) +
    geom_bar(stat="identity") +
    labs(x = "Portfolios based on the First Sorting Signal",
         y = "Mean Simple Return, prc") +
    theme(#panel.background = element_blank(),
          axis.text.x = element_text(face="bold", #color="#993333", 
                                     size=14 #, angle=45
          )#,
          #axis.ticks = element_blank()
          )
  
  ggsave(filename = paste0("meanreturns_entireperiod_simple_",scenarioID,".png"),
         path = projectplotpath,
         width = 110, height = 110, units = "mm")
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

returns_data_excess <- returns_data_excess %>% mutate(LS = .[[cols+2]]-.[[3]])

if(grepl(":", colnames(returns_data_excess)[3], fixed = TRUE)){
  portfolios_number <- cols^0.5
  returns_data_excess <- returns_data_excess %>% 
    mutate(LSD = .[[3+portfolios_number*(portfolios_number-1)]] - .[[portfolios_number+2]])
}

# Might be fun to split it into subsamples 1996-2005 - 2006-2012 2013-2021

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
  mutate(across(c(everything(),-stat), as.numeric))


################################################################################
############################### Regression Time ################################
################################################################################

returns_excess_ff5 <- returns_data %>% 
  mutate(period_ = paste0(lubridate::year(date), "_",week)) %>% 
  left_join(ff5_data %>% 
              mutate(period_ = paste0(lubridate::year(date),"_",lubridate::week(date)))
            , by = "period_") %>% 
  fill(RF)

for(i in 1:cols){
  returns_excess_ff5[,2+i] <- (returns_excess_ff5[,2+i] - 1)*100 - returns_excess_ff5$RF
}

## FF3

output_tibs <- tibble(.rows = 9)

for(i in 1:cols){
  reg_data <- returns_data_excess %>% 
    dplyr::select(2+i, `Mkt-RF`, HML, SMB)
  
  colnames(reg_data) <- c("Y", "MKT", "HML", "SMB")
  
  lm.ff3 <- lm(Y ~ MKT + HML + SMB,
               data = reg_data)
  
  coefs <- summary(lm.ff3)$coefficients[,1] #coefs
  tstats <- summary(lm.ff3)$coefficients[,2] #tstat
  rsq <- summary(lm.ff3)$adj.r.squared
  
  output_tibs <- output_tibs %>% 
    cbind(
    i = c(coefs[1], tstats[1], coefs[2], tstats[2], coefs[3], tstats[3], coefs[4], tstats[4], rsq)
    )
}
for(i in cols:(ncol(returns_data_excess) - cols + 1)){
  reg_data <- returns_data_excess %>% 
    dplyr::select(9+i, `Mkt-RF`, HML, SMB)
  
  colnames(reg_data) <- c("Y", "MKT", "HML", "SMB")
  
  lm.ff3 <- lm(Y ~ MKT + HML + SMB,
               data = reg_data)
  
  coefs <- summary(lm.ff3)$coefficients[,1] #coefs
  stderror <- summary(lm.ff3)$coefficients[,2] #tstat
  tstats <- summary(lm.ff3)$coefficients[,3]
  rsq <- summary(lm.ff3)$adj.r.squared
  
  output_tibs <- output_tibs %>% 
    cbind(
      i = c(coefs[1], tstats[1], coefs[2], tstats[2], coefs[3], tstats[3], coefs[4], tstats[4], rsq)
    )
}

colnames(output_tibs) <- c(colnames(descr_returns_data)[2:(cols+1)],"LS",ifelse("LSD" %in% colnames(descr_returns_data),"LSD",""))
rownames(output_tibs) <- c("Alpha, coef", "Alpha, t-stat", 
                           "MKT, coef", "MKT, t-stat", 
                           "HML, coef", "HML, t-stat", 
                           "SMB, coef", "SMB, t-stat", 
                           "Adj.R-squared")

colnames(returns_data_excess) <- gsub(":","_",colnames(returns_data_excess))

## FF5

colnames(returns_excess_ff5) <- gsub("_",":",colnames(returns_excess_ff5))

returns_excess_ff5 <- returns_excess_ff5 %>% mutate(LS = .[[cols+2]]-.[[3]])

if(grepl(":", colnames(returns_excess_ff5)[3], fixed = TRUE)){
  portfolios_number <- cols^0.5
  returns_excess_ff5 <- returns_excess_ff5 %>% 
    mutate(LSD = .[[3+portfolios_number*(portfolios_number-1)]] - .[[portfolios_number+2]])
}

output_tibs_ff5 <- tibble(.rows = 13)

for(i in 1:cols){
  reg_data <- returns_excess_ff5 %>% 
    dplyr::select(2+i, `Mkt-RF`, HML, SMB, RMW, CMA)
  
  colnames(reg_data) <- c("Y", "MKT", "HML", "SMB", "RMW", "CMA")
  
  lm.ff5 <- lm(Y ~ MKT + HML + SMB + RMW + CMA,
               data = reg_data)
  
  coefs <- summary(lm.ff5)$coefficients[,1] #coefs
  tstats <- summary(lm.ff5)$coefficients[,2] #tstat
  rsq <- summary(lm.ff5)$adj.r.squared
  
  output_tibs_ff5 <- output_tibs_ff5 %>% 
    cbind(
      i = c(coefs[1], tstats[1], 
            coefs[2], tstats[2], 
            coefs[3], tstats[3], 
            coefs[4], tstats[4],
            coefs[5], tstats[5],
            coefs[6], tstats[6],
            rsq)
    )
}
for(i in cols:(ncol(returns_excess_ff5) - cols - 1)){
  reg_data <- returns_excess_ff5 %>% 
    dplyr::select(11+i, `Mkt-RF`, HML, SMB, RMW, CMA)
  
  colnames(reg_data) <- c("Y", "MKT", "HML", "SMB", "RMW", "CMA")
  
  lm.ff5 <- lm(Y ~ MKT + HML + SMB + RMW + CMA,
               data = reg_data)
  
  coefs <- summary(lm.ff5)$coefficients[,1] #coefs
  stderror <- summary(lm.ff5)$coefficients[,2] #stderror
  tstats <- summary(lm.ff5)$coefficients[,3]
  rsq <- summary(lm.ff5)$adj.r.squared
  
  output_tibs_ff5 <- output_tibs_ff5 %>% 
    cbind(
      i = c(coefs[1], tstats[1], 
            coefs[2], tstats[2], 
            coefs[3], tstats[3], 
            coefs[4], tstats[4], 
            coefs[5], tstats[5], 
            coefs[6], tstats[6], 
            rsq)
    )
}

colnames(output_tibs_ff5) <- c(colnames(descr_returns_data)[2:(cols+1)],"LS",ifelse("LSD" %in% colnames(descr_returns_data),"LSD"))
rownames(output_tibs_ff5) <- c("Alpha, coef", "Alpha, t-stat", 
                           "MKT, coef", "MKT, t-stat", 
                           "HML, coef", "HML, t-stat", 
                           "SMB, coef", "SMB, t-stat", 
                           "RMW, coef", "RMW, t-stat",
                           "CMA, coef", "CMA, t-stat",
                           "Adj.R-squared")

colnames(returns_data_excess) <- gsub(":","_",colnames(returns_data_excess))

################################################################################
################################# More Plotting ################################
################################################################################

png(filename = paste0(projectplotpath,"/Heatmap_correlation_", scenarioID,".png"))
heatmap(cor(returns_data_excess %>% 
              dplyr::select(-date.x, -week, -`period_`, -date.y, -`Mkt-RF`, -SMB, -HML, -RF)), 
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

fm_data_simple <- returns_data_excess %>% 
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
  group_by(Portfolio)

# First stage regression

fm_first_stage_simple <- fm_data_simple %>% 
  group_map(~ broom::tidy(lm(Return ~ median, data = .x))) %>% 
  bind_rows() %>% 
  mutate(group = sort(rep(1:cols, times=2)))

# Second Stage

fm_second_stage_simple <- fm_data_simple %>% 
  mutate(group = cur_group_id()) %>% 
  left_join(
    fm_first_stage_simple %>% 
      dplyr::filter(term == "median") %>% 
      dplyr::select(group, beta = estimate)
    , by = "group") %>% 
  ungroup() %>% 
  group_by(period) %>% 
  group_map(~ broom::tidy(lm(Return ~ beta, data = .x))) %>% 
  bind_rows() %>% 
  group_by(term) %>% 
  summarize(gamma_hat = mean(estimate),
            gamma_hat_var = var(estimate)) %>% 
  mutate(tstat = gamma_hat/(gamma_hat_var^0.5))

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
    
  coefs1 <- roll_lm(x = group_fm_first$median, 
                    y = group_fm_first$Return, 
                    width = window_length,
                    intercept = TRUE)$coefficients[,2] %>% 
    tibble()
  
  group_fm_second <- group_fm_first %>% 
    cbind(coefs1) %>% 
    rbind(group_fm_second)
  
}

colnames(group_fm_second) <- c("period", "Portfolio", "Return", "median", "group", "beta")

# Second Stage

fm_second_stage_simple_time <- group_fm_second %>% 
  dplyr::filter(!is.na(beta)) %>% 
  group_by(period) %>% 
  group_map(~ broom::tidy(lm(Return ~ beta, data = .x))) %>% 
  bind_rows() %>% 
  group_by(term) %>% 
  summarize(gamma_hat = mean(estimate),
            gamma_hat_var = var(estimate)) %>% 
  mutate(tstat = gamma_hat/(gamma_hat_var^0.5))

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
              dplyr::select(period_, `Mkt-RF`, SMB, HML, RMW, CMA, RF),
            by = c("period" = "period_")) %>% 
  group_by(Portfolio)

# First stage regression

fm_first_stage_ff5 <- fm_data_ff5 %>% 
  group_map(~ broom::tidy(lm(Return ~ median + `Mkt-RF` + SMB + HML + RMW + CMA, data = .x))) %>% 
  bind_rows() %>% 
  mutate(group = sort(rep(1:cols, times=7)))

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
  group_map(~ broom::tidy(lm(Return ~ median + MKT + SMB + HML + RMW + CMA, data = .x))) %>% 
  bind_rows() %>% 
  group_by(term) %>% 
  summarize(gamma_hat = mean(estimate),
            gamma_hat_var = var(estimate)) %>% 
  mutate(tstat = gamma_hat/(gamma_hat_var^0.5))

################################################################################
################## Fama Macbeth - Time Variant + Other Factors #################
################################################################################





