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