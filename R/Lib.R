# rm(list = ls())

set.seed(666)

library(tidyverse)
library(jsonlite)
library(remotes)
library(devtools)
library(DT)
library(tuple)
library(ggraph)
library(tidygraph)
library(readxl)
library(readr)
library(openxlsx)
library(skimr)
library(lubridate)
library(gapminder)
library(dslabs)
library(ggplot2)
library(patchwork)
library(dsbox)
library(fitdistrplus)
library(conflicted)
library(boot)
library(monotonicity)
library(moments)
library(RColorBrewer)
library(reshape)
library(rollRegres)

library(RPostgres) # wrds

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='aumari96a8')

setwd           <- "/Users/mariedyveke/Documents/GitHub/thesis"
datapath        <- "/Users/mariedyveke/Documents/GitHub/thesis/data"
datapath1        <- "/Users/mariedyveke/Documents/GitHub/thesis/data1"
sqlpath         <- "/Users/mariedyveke/Documents/GitHub/thesis/SQL"
datafilterpath  <- "/Users/mariedyveke/Documents/GitHub/thesis/filtered_data"
plotpath        <- "/Users/mariedyveke/Documents/GitHub/thesis/plots"
projectplotpath <- "/Users/mariedyveke/Documents/GitHub/thesis/Project/Plots"
portfoliopath   <- "/Users/mariedyveke/Documents/GitHub/thesis/portfolios"
ffdatapath      <- "/Users/mariedyveke/Documents/GitHub/thesis/ff_data"

# Functions

bold_formatter <- function(x, value, num_decimals=3, benchmark_value=None){
  if(x == benchmark_value | is.na(benchmark_value)){
    string <- str(round(x*1, num_decimals))
  } else {
    string <- str(round(x*1, num_decimals))
  }
  
  if(x == value){
    return("$\\mathbf{" + string + "}$")
  } else{
    return("$" + string + "$")
  }
}

# get_latex_table <- function(df, highlight_min_max, n_decimals = 2, caption='Table Name',
#                     description='Note: Table displays..', label='reference', benchmark=NA){
#   
#   benchmark_value = NA
#   if(highlight_min_max){
#     fmts <- list()
#   
#     for(col in 1:ncol(df){
#       if(!is.na(benchmark)){
#         
#         benchmark_value = df %>% dplyr::filter(model==benchmark) %>% dplyr::select(colnames(df)[i])
#         rest = df %>% dplyr::filter(model!=benchmark) %>% dplyr::select(colnames(df)[i])
#         
#         if(nrow(rest) > 0) & all(rest>0)){
#           target_value <- benchmark_value
#         } else {
#           target_value = rest %>% min(na.rm = TRUE)  # if 'MSE' in col else df[col].max()
#         }
#         
#       }
#       else:
#         target_value = df[col].min()
#       fmts[col] = partial(bold_formatter, value=target_value, num_decimals=n_decimals,
#                           benchmark_value=benchmark_value)
#       
#     }
#   }
#   else:
#     fmts = None
#   if fmts is not None:
#     fmts = dict(**fmts)
#   output = df.to_latex(escape=False, formatters=fmts)
#   output = output.replace('bottomrule', 'hline \hline')
#   output = output.replace('toprule', 'hline \hline')
#   output = '\\begin{table}[h] \n \centering \n \caption{\\textsc{'+caption+'}}' + \
#   '\n \\begin{threeparttable} \n \setlength{\\tabcolsep}{4pt} % Default value: 6pt \n' + \
#   '\\renewcommand{\\arraystretch}{1.5} % Default value: 1 \n' + output
#   output += '\\raggedright{ 	{\small \\textit{'+ description +'} }} \n' + \
#   '\end{threeparttable}  \n\label{tab:'+label+'} \n \end{table} \n'
#   return output
# }
