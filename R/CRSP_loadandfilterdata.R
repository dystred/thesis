source("/Users/mariedyveke/Documents/GitHub/thesis/R/Lib.R")

years <- 1996:2024

query <- "SELECT t.permno -- unique id number
                ,t.date -- date (!)
                ,t.shrout * COALESCE(ABS(t.openprc),ABS(t.prc),0) AS marketvalue
                --,t.bidlo -- lowest price / bid
                --,t.askhi -- highest price / ask
                ,ABS(t.prc) AS prc -- closing price
                ,t.vol -- share volume
                ,t.ret -- holding period return
                ,t.bid -- closing bid
                ,t.ask -- closing ask
                ,t.openprc -- open price
                ,t.numtrd -- number of trades
                ,t.cfacpr -- factor to adjust prices after events : only relevant when not 
          FROM crsp_a_stock.dsf t
          WHERE 1=1
                AND t.date > '1996-01-01' -- The first date of observations on option metrics
                AND t.date BETWEEN @from AND @to
                --AND t.SHRCD IN (10, 11)
                AND t.hexcd IN (1, 2, 3) " 
#%>%
#  str_replace_all("@from","'2002-01-01'") %>% 
#  str_replace_all("@to","'2003-01-01'")

for(step in 1:(length(years)-1)){
  # step=1
  # Setting the particular year
  query_tmp <- query %>%
    str_replace_all("@from",paste0("'",years[step],"-01-01'")) %>% 
    str_replace_all("@to",paste0("'",years[step+1],"-01-01'"))
  
  # Downloading the data
  res <- dbSendQuery(wrds, query_tmp)
  data <- dbFetch(res, n=-1)
  dbClearResult(res)
  loop_data <- data %>% 
    as_tibble() %>% 
    dplyr::filter(!is.na(prc) & !is.na(ret)) %>% 
    mutate(week = week(date),
           returns = 1+ret) %>% 
    group_by(permno,week) %>% 
    arrange(date) %>% 
    mutate(ret = cumprod(returns)) %>% 
    summarize(date = min(date),
              marketvalue = head(marketvalue, n=1),
              open = head(openprc, n=1),
              close = tail(prc, n=1),
              numtrd = sum(numtrd, na.rm = TRUE),
              bidaskspread = max(ask-bid, na.rm = TRUE),
              vol = sum(vol, na.rm = TRUE),
              ret = tail(ret,n=1),
              .groups = "keep"
    ) %>% 
    mutate(returns1 = close/open)
  
  # Saving the data
  loop_data %>% write_csv(paste0(datapath1,"/CRSP_allstocks_weekly_raw_",years[step],".csv"))
  
  loop_data %>% 
    ggplot(aes(x=ret, y=returns1)) +
    geom_point()
  ggsave(filename = paste0("plot_returns_",years[step],".png"),
         path = datapath1,
         width = 250, height = 110, units = "mm")
  
  print(paste0("Done with file: CRSP_allstocks_weeekly_raw_",years[step],".csv"))
}

#### Filtering out the relevant files

relevant_permnos <- read_csv(paste0(datapath,"/OPTIONM_secidstopermnos.csv"),
                             show_col_types = FALSE)

perms <- list()
i=1
years <- 1996:2022

for(year in years){
  # year = 1997
  
  print(year)
  
  data <- read_csv(paste0(datapath1,"/CRSP_allstocks_weekly_raw_",year,".csv"), show_col_types = FALSE) %>% 
    dplyr::filter(permno %in% relevant_permnos$PERMNO)
  
  data %>% nrow() %>% print()
  
  data %>% write_csv(paste0(datapath1,"/CRSP_filteredstocks_weekly_raw_",year,".csv"))
  
  perms[[i]] = data %>% 
    summarize(unique(permno))
  
  i=i+1
}

perms <- do.call(rbind.data.frame, perms) %>% summarize(v1 = unique(`unique(permno)`))

relevant_secs <- relevant_permnos %>% 
  dplyr::filter(PERMNO %in% perms$v1)

years <- 1996:2021

for(year in years){
  # year = 1996
  i=1
  data = list()
  
  yearly_files <- list.files(datapath) %>% 
    as_tibble() %>% 
    dplyr::filter(grepl(paste0(year), value, fixed = TRUE) &
                    (grepl("OPTIONM_alloptions_weekly_raw_week", value, fixed = TRUE) |
                       grepl("OPTIONM_alloptions_weekly_filtered_week", value, fixed = TRUE)))
  
  yearly_files_raw <- yearly_files %>% 
    dplyr::filter(grepl("raw",value, fixed = TRUE))
  
  yearly_files_filtered <- yearly_files %>% 
    dplyr::filter(grepl("filtered",value, fixed = TRUE))
  
  for(file in yearly_files_raw$value){
    # file = yearly_files_raw$value[1]
    # year = 1997
    # week = 4
    
    print(file)
    
    data[[i]] <- read_csv(paste0(datapath,"/",file), show_col_types = FALSE) %>% 
      dplyr::filter(secid %in% relevant_secs$SECID)
    i=i+1
  }
  do.call(rbind.data.frame, data) %>% 
    left_join(relevant_permnos, by=c("secid"="SECID"), multiple="all") %>% 
    dplyr::filter(ymd(sdate)<=date & date <= ymd(edate)) %>% 
    dplyr::select(-sdate,-edate) %>% 
    write_csv(paste0(datapath1,"/OPTIONM_filteredoptions_weekly_raw_",year,".csv"))
  
  i=1
  data = list()
  
  for(file in yearly_files_filtered$value){
    
    data[[i]] <- read_csv(paste0(datapath,"/",file), show_col_types = FALSE) %>% 
      dplyr::filter(secid %in% relevant_secs$SECID)
    i = i + 1
  }
  
  do.call(rbind.data.frame, data) %>% 
    left_join(relevant_permnos, by=c("secid"="SECID"), multiple="all") %>% 
    dplyr::filter(ymd(sdate)<=date & date <= ymd(edate)) %>% 
    dplyr::select(-sdate,-edate) %>% 
    write_csv(paste0(datapath1,"/OPTIONM_filteredoptions_weekly_filtered_",year,".csv"))
}

#### Make metadata plots:
files_data1 <- list.files(path = datapath1) %>% 
  tibble(value = .) %>% 
  mutate(
    provider = str_split_fixed(value, pattern = "_", n = 5)[,1],
    filtered = str_split_fixed(value, pattern = "_", n = 5)[,2],
    period   = str_split_fixed(value, pattern = "_", n = 5)[,3],
    version  = str_split_fixed(value, pattern = "_", n = 5)[,4],
    year     = as.numeric(str_remove_all(str_split_fixed(value, pattern = "_", n = 5)[,5], pattern = ".csv"))) %>% 
  dplyr::filter(provider != "plot")

CRSP_files <- files_data1 %>% 
  dplyr::filter(provider == "CRSP")

Option_files <- files_data1 %>% 
  dplyr::filter(provider == "OPTIONM")

crsp_metadata <- tibble(period = NA, tot_vol = NA, tot_market = NA, firms = NA, bid_ask = NA, Version = NA)

for(crsp_file in CRSP_files$value){
  if(grepl(pattern = "allstocks", x = crsp_file, fixed = TRUE)){
    dmy_all = TRUE
  } else{
    dmy_all = FALSE
  }
  #print(paste0(dmy_all, " : ", crsp_file))
  
  tmp_crsp_metadata_part1 <- read_csv(file = paste0(datapath1, "/", crsp_file), show_col_types = FALSE) %>% 
    dplyr::filter(permno %in% relevant_permnos$PERMNO) %>% 
    mutate(period = paste0(lubridate::year(date), "_", week)) %>% 
    group_by(period) %>% 
    summarize(tot_vol = sum(vol, na.rm = TRUE),
              bid_ask = median(bidaskspread, na.rm = TRUE))
  
  tmp_crsp_metadata <- read_csv(file = paste0(datapath1, "/", crsp_file), show_col_types = FALSE) %>% 
    dplyr::filter(permno %in% relevant_permnos$PERMNO) %>% 
    mutate(period = paste0(lubridate::year(date), "_", week)) %>% 
    group_by(period, permno) %>% 
    summarize(tot_market = median(marketvalue, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(period) %>% 
    summarize(firms = n(),
              tot_market = sum(tot_market, na.rm = TRUE)) %>% 
    left_join(tmp_crsp_metadata_part1, by = "period")
  
  crsp_metadata <- crsp_metadata %>% 
    rbind(tmp_crsp_metadata %>% 
            cbind(tibble(Version = rep(dmy_all, times = nrow(tmp_crsp_metadata)))))
}

crsp_metadata <- crsp_metadata %>% 
  dplyr::filter(!is.na(period)) %>% 
  mutate(year = as.numeric(str_split_fixed(period, pattern = "_", n = 2)[,1]),
         week = as.numeric(str_split_fixed(period, pattern = "_", n = 2)[,2]),
         date = lubridate::ymd(lubridate::parse_date_time(paste(year, week, 1, sep="/"),'Y/W/w')))

# plot of total volume traded + total market value
crsp_metadata %>%
  mutate(`Volume traded` = tot_vol/1000000,
         `Market Cap` = tot_market/1000000) %>% 
  dplyr::select(date, `Volume traded`, `Market Cap`, Version) %>%
  gather(key = "Measure", value = "Value", -date, -Version) %>% 
  ggplot(aes(x = date, y = Value, color = Measure)) +
  geom_line() +
  labs(x = 'Date', y = 'Million USD, log scaled', color = 'Variables:') +
  scale_y_log10() +
  scale_color_brewer() +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("1996-01-01", "2023-01-01")))
ggsave(filename = paste0("metadata_crsp_volumemarket_entireperiod.png"),
       path = projectplotpath,
       width = 300, height = 110, units = "mm")

# table of amount of firms
crsp_metadata %>% 
  dplyr::select(date, firms, Version) %>% 
  ggplot(aes(x = date, y = firms)) +
  geom_line() +
  labs(x = 'Date', y = 'Number of Unique Underlying Symbols') +
  scale_color_brewer() +
  theme_light() +
  scale_x_date(limits = as.Date(c("1996-01-01", "2023-01-01")))
ggsave(filename = paste0("metadata_crsp_firms_entireperiod.png"),
       path = projectplotpath,
       width = 300, height = 110, units = "mm")

# plot of median bid-ask spread
crsp_metadata %>%
  dplyr::select(date, bid_ask, Version) %>% 
  ggplot(aes(x = date, y = bid_ask)) +
  geom_line() +
  #geom_line(aes(y = tot_market, color = "Blue")) +
  labs(x = 'Date', y = 'Median Bid-Ask Spread, USD') +
  scale_color_brewer() +
  theme_light() +
  scale_x_date(limits = as.Date(c("1996-01-01", "2023-01-01")))
ggsave(filename = paste0("metadata_crsp_bidask_entireperiod.png"),
       path = projectplotpath,
       width = 300, height = 110, units = "mm")

# OPTIONM
optionm_metadata <- tibble(period = NA,
                           tot_open_interest = NA,
                           median_open_interest = NA,
                           tot_vol = NA,
                           median_vol = NA,
                           n_secid = NA,
                           Version = NA)

for(optionm_file in Option_files$value){
  if(grepl(pattern = "raw", x = optionm_file, fixed = TRUE)){
    dmy_all = TRUE
  } else{
    dmy_all = FALSE
  }
  
  tmp_optionm_metadata <- read_csv(file = paste0(datapath1, "/", optionm_file), show_col_types = FALSE) %>% 
    mutate(period = paste0(lubridate::year(date), "_", lubridate::week(date))) %>% 
    group_by(period) %>% 
    summarize(tot_open_interest = sum(sum_open_interest, na.rm = TRUE),
              median_open_interest = median(sum_open_interest, na.rm = TRUE),
              tot_vol = sum(sum_volume, na.rm = TRUE),
              median_vol = median(sum_volume, na.rm = TRUE),
              n_secid = length(unique(unlist(secid)))) %>% 
    mutate(Version = dmy_all)
    
    optionm_metadata <- optionm_metadata %>% 
      rbind(tmp_optionm_metadata)
}

# Plotting 
optionm_metadata <- optionm_metadata %>% 
  dplyr::filter(!is.na(period)) %>% 
  mutate(year = as.numeric(str_split_fixed(period, pattern = "_", n = 2)[,1]),
         week = as.numeric(str_split_fixed(period, pattern = "_", n = 2)[,2]),
         date = lubridate::ymd(lubridate::parse_date_time(paste(year, week, 1, sep="/"),'Y/W/w'))) %>% 
  arrange(date)

# Plotting open interest vs. volume
optionm_metadata %>% 
  dplyr::select(date, tot_open_interest, tot_vol, Version) %>% 
  gather(key = "Measure", value = "Value", -date, -Version) %>% 
  mutate(Measure = ifelse(Measure == "tot_vol", "Volume", "Open Interest"),
         Measure = paste0(Measure, " - ", ifelse(Version, "Raw", "Filtered")),
         Value = Value/1000000) %>% 
  ggplot(aes(x = date, y = Value, color = Measure)) +
  geom_line() +
  labs(x = 'Date', y = 'Weekly total, Million USD') +
  #scale_y_log10() +
  scale_color_brewer() +
  theme_light() +
  scale_x_date(limits = as.Date(c("1996-01-01", "2023-01-01"))) +
  theme(legend.position = "bottom")
ggsave(filename = paste0("metadata_optionm_openinterestVol_entireperiod.png"),
       path = projectplotpath,
       width = 300, height = 110, units = "mm")

# Plotting amount of underlying symbols
optionm_metadata %>% 
  dplyr::select(date, n_secid, Version) %>% 
  mutate(Version = ifelse(Version, "All Options", "Filtered Options")) %>% 
  ggplot(aes(x = date, y = n_secid, color = Version)) +
  geom_line() +
  labs(x = 'Date', y = 'Number of Unique Underlying Symbols') +
  #scale_y_log10() +
  scale_color_brewer() +
  theme_light() +
  scale_x_date(limits = as.Date(c("1996-01-01", "2023-01-01"))) +
  theme(legend.position = "bottom")
ggsave(filename = paste0("metadata_optionm_underlyingSymbols_entireperiod.png"),
       path = projectplotpath,
       width = 300, height = 110, units = "mm")

##### MISC PLOTS OF METADATA :DD

meta_files <- list.files(portfoliopath) %>% 
  tibble() %>% 
  dplyr::filter(grepl("meta", ., fixed = TRUE)) %>% 
  mutate(value = paste0(portfoliopath, "/", .), .keep = "unused")

metadata_comb <- list()

for(vers in meta_files$value){
  metadata_comb[[vers]] <- read_csv(vers, show_col_types = FALSE) %>% 
    mutate(ScenarioID = parse_double(substring(vers, 64, 65))) %>% 
    mutate(across(everything(), as.character))
}

metadata_all <- bind_rows(metadata_comb)

metadata_refined <- metadata_all %>% 
  mutate(signal2 = ifelse(splits_number == 1, NA, signal2),
         splits_2 = ifelse(splits_number == 1, NA, splits_2),
         doublesort = ifelse(splits_number == 1, NA, doublesort),
         ScenarioID = as.numeric(ScenarioID),
         signal = ifelse(grepl("absolute", signal, fixed = TRUE), "Level", "Change"),
         signal2 = ifelse(is.na(signal2), NA, ifelse(grepl("absolute", signal2, fixed = TRUE), "Level", "Change")),
         signal_weight_abs = ifelse(weighted_simple == "simple", NA, signal_weight_abs),
         period_signal = ifelse(signal == "Level" | signal2 == "Level", period_signal, NA)
         ) %>% 
  dplyr::select(-return_period, -...1, -method, -horizon_signal1, -horizon_signal2, -custom_split_1, -custom_split_2, -period_signal) %>% 
  dplyr::rename(Conditions = filtered_raw, 
                `Signal weight` = weighted_simple, 
                `Signal1` = signal, 
                `Signal2` = signal2, 
                #`Horizon for Level` = period_signal,
                by = signal_weight_abs,
                Returns = portfolio_weight) %>% 
  relocate(ScenarioID) %>% 
  arrange(ScenarioID) 

table_formatter(x = metadata_refined,
                filename = "metadata_ScenarioID.tex",
                num_decimals = 0,
                hlines_after = c(0))



