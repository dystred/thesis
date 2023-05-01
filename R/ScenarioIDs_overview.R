# Relevant runs for portfolio maker function

# Scenario 1 - base case for predictability
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
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 1                                      # 1 or 2
)

# Scenario 2 - double sorting 5-5, dependent
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
                splits_1          = 5                                    , # any integer or "custom"
                splits_2          = 5                                    , # any integer or "custom"
                custom_split_1    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = FALSE                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                      # 1 or 2
)

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
                splits_1          = 3                                    , # any integer or "custom"
                splits_2          = 3                                    , # any integer or "custom"
                custom_split_1    = c(3/10 , 7/10)                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                custom_split_2    = c(3/10 , 7/10)                                , # logical FALSE or c(0.3,0.7) for a 0.3 - 0.4 - 0.3 split
                splits_number     = 2                                      # 1 or 2
)

# Scenario 4
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
                splits_number     = 1                                      # 1 or 2
)
