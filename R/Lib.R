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
library(quadprog)
library(fastDummies)
library(xtable)
library(broom)
library('corrr')
library(ggcorrplot)
library("scales")
library(roll)

library(RPostgres) # wrds

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='aumari96a8')

setwd            <- "/Users/mariedyveke/Documents/GitHub/thesis"
datapath         <- "/Users/mariedyveke/Documents/GitHub/thesis/data"
datapath1        <- "/Users/mariedyveke/Documents/GitHub/thesis/data1"
sqlpath          <- "/Users/mariedyveke/Documents/GitHub/thesis/SQL"
datafilterpath   <- "/Users/mariedyveke/Documents/GitHub/thesis/filtered_data"
plotpath         <- "/Users/mariedyveke/Documents/GitHub/thesis/plots"
projectplotpath  <- "/Users/mariedyveke/Documents/GitHub/thesis/Project/Plots"
projecttablepath <- "/Users/mariedyveke/Documents/GitHub/thesis/Project/Tables"
portfoliopath    <- "/Users/mariedyveke/Documents/GitHub/thesis/portfolios"
ffdatapath       <- "/Users/mariedyveke/Documents/GitHub/thesis/ff_data"
oadatapath       <- "/Users/mariedyveke/Documents/GitHub/thesis/oa_data"

# Functions

hush <- function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  unlink("NUL")
  return(tmp)
}

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

table_formatter <- function(x, 
                            filename, 
                            num_decimals = 2, 
                            row_names = FALSE, 
                            hlines_after = NULL){
  
  if(row_names){
    x <- x %>% 
      mutate("_" = rownames(x)) %>% 
      relocate("_")
  }
  
  try(
    print(xtable(x, 
                 caption = NULL, # the title
                 label = NULL, # label
                 align = NULL, #aligning, {l|l|r|r|r|r} fx
                 digits = num_decimals,
                 type = "latex", 
                 tabular.environment="longtable"), 
          file = paste0(projecttablepath, "/",filename),
          include.rownames=FALSE,
          hline.after = hlines_after,
          only.contents=TRUE)
    )
}

## Source : https://rpubs.com/JanpuHou/258620
eff.frontier <- function (returns, short="no", max.allocation=NULL,
                          risk.premium.up=.5, risk.increment=.005){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited)max.allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop
  
  covariance <- cov(returns)
  print(covariance)
  n <- ncol(covariance)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- colMeans(returns) * i # This moves the solution along the EF
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
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
