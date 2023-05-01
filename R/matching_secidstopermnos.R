source("/Users/mariedyveke/Documents/GitHub/thesis/R/Lib.R")

setwd <- "/Users/mariedyveke/Documents/GitHub/thesis"
#datapath <- "/Users/mariedyveke/Documents/GitHub/thesis/data"
sqlpath <- "/Users/mariedyveke/Documents/GitHub/thesis/SQL"
datafilterpath <- "/Users/mariedyveke/Documents/GitHub/thesis/filtered_data"

## Choose only the relevant data (the PERMNOS represented by option data also)

relevant_permnos <- read_csv(paste0(datapath,"/OPTIONM_secidstopermnos.csv"))

perms <- list()
i=1
years <- 1996:2022

for(year in years){
  # year = 1997
  
  print(year)
  
  data <- read_csv(paste0(datapath1,"/CRSP_allstocks_weekly_raw_",year,".csv"), show_col_types = FALSE) %>% 
    dplyr::filter(permno %in% relevant_permnos$PERMNO)
  
  # data %>% nrow() %>% print()
  
  data %>% write_csv(paste0(datafilterpath,"/CRSP_filteredstocks_weekly_raw_",year,".csv"))
  
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
    write_csv(paste0(datafilterpath,"/OPTIONM_filteredoptions_weekly_raw_",year,".csv"))
  
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
    write_csv(paste0(datafilterpath,"/OPTIONM_filteredoptions_weekly_filtered_",year,".csv"))
}

## And lastly filtering of the fundamental data:

fundamental_files <- list.files(datapath) %>% 
  as_tibble() %>% 
  dplyr::filter(grepl("CRSP_fundamentals_raw",value,fixed=TRUE))

relevant_gvkeys <- read_csv(paste0(datapath,"/CRSP_PERMNOS.csv"),show_col_types = FALSE) %>% 
  mutate(LINENDDT = ifelse(LINKENDDT=="E",today(),LINKENDDT))
year = 1995

for(file in fundamental_files$value){
  
  print(file)
  
  data <- read_csv(paste0(datapath,"/",file), show_col_types = FALSE) %>% 
    left_join(relevant_gvkeys, by=c("gvkey"="GVKEY"),multiple="first") 
  
  data %>% write_csv(paste0(datafilterpath,"/CRSP_fundamental_quarterly_filtered_",year,".csv"))
  
  year = year + 1

}
