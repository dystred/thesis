# Relevant runs for portfolio maker function

source("/Users/mariedyveke/Documents/GitHub/thesis/R/portfolio_maker.R")
source("/Users/mariedyveke/Documents/GitHub/thesis/R/results_test_table_maker.R")

# Scenario 1 - base case for predictability
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "simple"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                      # 1 or 2
)

for(i in 1:18){
  results_test_table_maker(scenarioID = i)
}

# Scenario 2 - double sorting 5-5, dependent
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "change in implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "absolute implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                      # 1 or 2
)

results_test_table_maker(scenarioID = 18)

# Scenario 3 - double sorting 3-3, dependent
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = "custom"                                    , # any integer or "custom"
                splits_2          = "custom"                                    , # any integer or "custom"
                custom_split_1    = c(3/10 , 7/10)                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = c(3/10 , 7/10)                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                      # 1 or 2
)

results_test_table_maker(scenarioID = 3)

# Scenario 4
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "independent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                    , # 1 or 2
                scenarioID        = 4
)

results_test_table_maker(scenarioID = 4)

# Scenario 5
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "independent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                    , # any integer or "custom"
                splits_2          = 10                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                    , # 1 or 2
                scenarioID        = 5
)

results_test_table_maker(scenarioID = 5)

# Scenario 6
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "simple"                             , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "independent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                    , # any integer or "custom"
                splits_2          = 10                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                    , # 1 or 2
                scenarioID        = 6
)

results_test_table_maker(scenarioID = 6)

# Scenario 7
portfolio_maker(filtered_raw      = "raw"                                , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "independent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                    , # any integer or "custom"
                splits_2          = 10                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                    , # 1 or 2
                scenarioID        = 7
)

results_test_table_maker(scenarioID = 7)

# Scenario 8
portfolio_maker(filtered_raw      = "raw"                                , # raw or filtered
                weighted_simple   = "simple"                             , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "independent"                        , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                    , # any integer or "custom"
                splits_2          = 10                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                    , # 1 or 2
                scenarioID        = 8
)

results_test_table_maker(scenarioID = 8)

# Scenario 9
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                   , # any integer or "custom"
                splits_2          = 10                                   , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                    , # 1 or 2
                scenarioID        = 9
)

results_test_table_maker(scenarioID = 9)

# Scenario 10 - adding the other signal as sorting signal - univaritate portfolio
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "simple"                              , # "simple" or "value"
                signal            = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                    , # 1 or 2
                scenarioID        = 17
)

results_test_table_maker(scenarioID = 17)

# Scenario 10 - adding the other signal as sorting signal - univaritate portfolio
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                   , # any integer or "custom"
                splits_2          = 10                                   , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                    , # 1 or 2
                scenarioID        = 11
)

results_test_table_maker(scenarioID = 11)

# Scenario 12 - univariate with 3 custom split
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = "custom"                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = c(3/10 , 7/10)                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1,                                      # 1 or 2
                scenarioID        = 12
)

results_test_table_maker(scenarioID = 12)

# Scenario 13 - univariate with 10 split
portfolio_maker(filtered_raw      = "filtered"                           , # raw or filtered
                weighted_simple   = "weighted"                           , # simple or weighted
                period_signal     = "last trading day"                   , # "last trading day" or "entire week"
                signal_weight_abs = "open interest"                      , # "open interest" or "volume" or "simple"
                return_period     = "1week"                              , # "1week" or 4weeks OBS STILL NOT CALCULATED
                doublesort        = "dependent"                          , # "dependent" or "independent"
                method            = "predictability"                     , # "predictability" or "cross sectionality"
                portfolio_weight  = "value"                              , # "simple" or "value"
                signal            = "absolute implied volatility spread" , # "absolute implied volatility spread" or "change in implied volatility spread"
                signal2           = "change in implied volatility spread", # "absolute implied volatility spread" or "change in implied volatility spread"
                horizon_signal1   = "1week"                              , # "1week" or "1day"
                horizon_signal2   = "1week"                              , # "1week" or "1day"
                splits_1          = 10                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1,                                      # 1 or 2
                scenarioID        = 13
)

results_test_table_maker(scenarioID = 13)

