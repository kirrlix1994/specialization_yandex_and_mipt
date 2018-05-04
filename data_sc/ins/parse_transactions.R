### script for working with transactions data

library(magrittr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)

source("helpers.R")


parseTransactions <- function(df,
                              start_date = ymd("1970-01-01"),
                              end_date = today(),
                              top_n_cols = list(),
                              agg_cols = list(NULL, c(), c(), c()),
                              id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'),
                              wide = TRUE, 
                              codemap_file = "TRANS_codemap") {
  # Parsing transactions data
  
  ## Args:
  ## @trans_file : path to the file with transactions
  ## @start_date : consider only transactions that happened at or after @start_date
  ## @end_date : consider only transactions that happened at or before @end_date
  ## @top_n_cols : a list of named tuples of the form `c(COL_NAME=COL_NAME, 
  ##                                                     PERCENT=PERCENT)`
  ##               where `COL_NAME` is the name of the column to be considered,
  ##               `PERCENT` is the percentage of values needed to be explained by
  ##               top-k unique values. Corresponding top-k unique values will be
  ##               kept, others will be combined into a single column.
  ##               E.g., if a column contains a hundred of values, while 
  ##               there are only 5 unique values with one class being presented
  ##               90 times, then if passing this column name with `PERCENT=0.9`
  ##               the output column would contain only two classes: `0` and `1`,
  ##               where `1` would be the class with highest presence, and
  ##               `0` would be all other classes.
  ## @agg_cols : a list of the following form: `list(COL_TO_AGGREGATE,
  ##                                                 c(PERIODS_TO_AGGREGATE),
  ##                                                 c(COLS_TO_GROUP),
  ##                                                 c(AGGREGATE_FUNS))`, where
  ##               `COL_TO_AGGREGATE` is a numeric column, to which 
  ##               the aggregation functions will be applied;
  ##               `PERIODS_TO_AGGREGATE` is a vector of days from `end_date`
  ##               in which the aggregation should be performed;
  ##               `COLS_TO_GROUP` is a vector of columns for which the aggregation
  ##               should be grouped;
  ##               `AGGREGATE_FUNS` is a `funs` list of functions to apply.
  ##               The same periods and functions will be applied to all columns
  ##               in `COLS_TO_GROUP`, thus the total number of newly added columns
  ##               will be equal to |PERIODS_TO_AGGREGATE| x 
  ##                                |NUMBER_OF_UNIQUE_VALUES IN COLS_TO_GROUP| x
  ##                                              |AGGREGATE_FUNS|.
  ## @id_column : a string, client identifier column.
  ## @wide : a boolean, indicating whether to output the data.frame in a wide format.
  
  ## Returns:
  ## @df : modified data frame

  
  ## parsing columns ----

 # # df %<>%
 #    #rename(DATE = DATA) %>%
 #    mutate(DATE = dmy(DATE),
 #           TXN_RUB_AMT = as.numeric(gsub(',', '.', TXN_RUB_AMT)),
 #           TXN_DT = dmy(substr(TXN_DT, start=1, stop=10))) %>% # some txn_dt values have also min/hour info
 #    filter(TXN_DT <= end_date, TXN_DT >= start_date,
 #           TXN_RUB_AMT != '') # filter out transactions with empty amount
  
  ## creating top_n columns ----
  print(glue::glue('{str_dup("***", 5)}CREATING TOP-N FEATURES{str_dup("***", 5)}'))
  df_out <- createTopNCols(df, top_n_cols = top_n_cols)
  df <- df_out[[1]]
  trans_code_map <- df_out[[2]]
  
  saveRDS(object = trans_code_map, 
                   file = codemap_file, 
                   ascii = FALSE, version = NULL,
                   compress = TRUE, refhook = NULL)
  
  ## creating aggregation summaries ----
  print(glue::glue('{str_dup("***", 5)}CREATING AGGREGATED SUMMARIES{str_dup("***", 5)}'))
  df %<>% createAggSummaries(end_date = end_date,
                             agg_cols = agg_cols,
                             id_columns = id_columns,
                             wide = wide)
}

### script for working with transactions data

library(magrittr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)

source("helpers.R")


parseTransactions2 <- function(df,
                              start_date = ymd("1970-01-01"),
                              end_date = today(),
                              top_n_cols = list(),
                              agg_cols = list(NULL, c(), c(), c()),
                              id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'),
                              wide = TRUE, 
                              codemap_file = "TRANS_codemap", 
                              data_type ) {
  # Parsing transactions data
  
  ## Args:
  ## @trans_file : path to the file with transactions
  ## @start_date : consider only transactions that happened at or after @start_date
  ## @end_date : consider only transactions that happened at or before @end_date
  ## @top_n_cols : a list of named tuples of the form `c(COL_NAME=COL_NAME, 
  ##                                                     PERCENT=PERCENT)`
  ##               where `COL_NAME` is the name of the column to be considered,
  ##               `PERCENT` is the percentage of values needed to be explained by
  ##               top-k unique values. Corresponding top-k unique values will be
  ##               kept, others will be combined into a single column.
  ##               E.g., if a column contains a hundred of values, while 
  ##               there are only 5 unique values with one class being presented
  ##               90 times, then if passing this column name with `PERCENT=0.9`
  ##               the output column would contain only two classes: `0` and `1`,
  ##               where `1` would be the class with highest presence, and
  ##               `0` would be all other classes.
  ## @agg_cols : a list of the following form: `list(COL_TO_AGGREGATE,
  ##                                                 c(PERIODS_TO_AGGREGATE),
  ##                                                 c(COLS_TO_GROUP),
  ##                                                 c(AGGREGATE_FUNS))`, where
  ##               `COL_TO_AGGREGATE` is a numeric column, to which 
  ##               the aggregation functions will be applied;
  ##               `PERIODS_TO_AGGREGATE` is a vector of days from `end_date`
  ##               in which the aggregation should be performed;
  ##               `COLS_TO_GROUP` is a vector of columns for which the aggregation
  ##               should be grouped;
  ##               `AGGREGATE_FUNS` is a `funs` list of functions to apply.
  ##               The same periods and functions will be applied to all columns
  ##               in `COLS_TO_GROUP`, thus the total number of newly added columns
  ##               will be equal to |PERIODS_TO_AGGREGATE| x 
  ##                                |NUMBER_OF_UNIQUE_VALUES IN COLS_TO_GROUP| x
  ##                                              |AGGREGATE_FUNS|.
  ## @id_column : a string, client identifier column.
  ## @wide : a boolean, indicating whether to output the data.frame in a wide format.
  
  ## Returns:
  ## @df : modified data frame
  
  
  ## parsing columns ----
  
  # # df %<>%
  #    #rename(DATE = DATA) %>%
  #    mutate(DATE = dmy(DATE),
  #           TXN_RUB_AMT = as.numeric(gsub(',', '.', TXN_RUB_AMT)),
  #           TXN_DT = dmy(substr(TXN_DT, start=1, stop=10))) %>% # some txn_dt values have also min/hour info
  #    filter(TXN_DT <= end_date, TXN_DT >= start_date,
  #           TXN_RUB_AMT != '') # filter out transactions with empty amount
  
  ## creating top_n columns ----
 if ( data_type == "old")
 {
  print(glue::glue('{str_dup("***", 5)}CREATING TOP-N FEATURES{str_dup("***", 5)}'))
  df_out <- createTopNCols(df, top_n_cols = top_n_cols)
  df_new <- df_out[[1]]
  trans_code_map <- df_out[[2]]


  saveRDS(object = trans_code_map, 
          file = codemap_file, 
          ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
 }

 if(  data_type == "new" )
 {
   #--- USE TOP N COLS:
   print(glue::glue('{str_dup("***", 5)}USING TOP-N FEATURES{str_dup("***", 5)}'))
   cat("\n\n")
   
   #load codemap
   code_map <- readRDS(file =  codemap_file)
   
   cols_to_transform <- colnames(df)[ which(colnames(df) %in% names(code_map)) ]
   
   df_new <- NULL
   df_new <- df
   for( i in (1 :length(names(code_map))))
   {
     col_name <- names(code_map)[i]
     map_table <- code_map[i][[1]]
     df_new[,col_name] <- use_codemap( col_name, map_table, df = df_new)  
     cat(  "USE top N ecncoding for field:", col_name, "\n")
   }
   
   df_new$DATE <- max(  df_new$TXN_DT) + 1
 }
  
  cat("GO to aggr summ.. \n\n")
  ## creating aggregation summaries ----
  print(glue::glue('{str_dup("***", 5)}CREATING AGGREGATED SUMMARIES{str_dup("***", 5)}'))
  df_new %<>% createAggSummaries(end_date = end_date,
                             agg_cols = agg_cols,
                             id_columns = id_columns,
                             wide = wide)
  cat("finish aggr summ")
  rm(df)
  return(df_new)
}



use_codemap <- function( col_name, map_table, df)
{
  new_col <- numeric()
  if( col_name %in% colnames(df) )
  {
    #new_col <- numeric()
    new_col <- rep(NA,nrow(df))
    if( class(df[,col_name]) == 'factor')
    {
      df[,col_name] %<>% as.character() %<>% str_trim()
    }
    for( val in names(map_table) )
    {
      ind <-  which( df[,col_name] ==  val)
      new_col[ind] <-  as.numeric(map_table[val])
    }
  }
  return( new_col)
} 
