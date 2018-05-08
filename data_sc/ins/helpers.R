library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables

createAggSummaries <- function(df,
                               end_date = end_date,
                               agg_cols = list(NULL, c(), c(), c()),
                               id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'),
                               wide = FALSE) {
  # Aggregate through columns. 
  # Assuming that DATE_START column is presented to cut off changes from the right.
  
  ## Args:
  ## @df : data frame
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
  ##               will be equal to |PERIODS_TO_AGGREGATE| x |COLS_TO_GROUP| x
  ##                                              |AGGREGATE_FUNS|.
  ## @id_column : a string, client identifier column.
  ## @wide : a boolean, indicating whether to output the data.frame in a wide format.
  
  ## Returns:
  ## @df : modified data frame
  #cat( 1, "\n")
  
  agg_col <- agg_cols[[1]]
  agg_periods <- agg_cols[[2]]
  agg_group_cols <- agg_cols[[3]]
  agg_funs <- agg_cols[[4]]
  
  #cat( 2, "\n")
  
  ## print verbose info
  print(glue::glue("AGGREGATING COLUMN : {agg_col}\n",
                   "THROUGH PERIODS OF ({paste0(agg_periods, collapse = ', ')}) ",
                   "GROUPING BY COLUMNS ({paste0(agg_group_cols, collapse = ', ')})\n",
                   "USING FUNCTIONS ({paste0(agg_funs, collapse = ', ')})\n",
                   "WITH OUTPUT IN A {ifelse(wide, 'WIDE', 'LONG')} FORMAT"))
  
  sum_df <- NULL
  for (p in sort(agg_periods, decreasing=TRUE)) {
    cat( "DAYS window = ", p, "\n"); 
    
    
    tmp_df <- df %>%
      filter(TXN_DT >= DATE - days(p))
    ## add mean per day
    #agg_funs <- c(agg_funs, funs("MEAN_PER_DAY" = sum(. / p, na.rm = TRUE)))
    for (c_ in agg_group_cols) {
      c_ <- unlist(c_)
      c_name <-  c_ %>% paste0(collapse = '_')
      ## summarise grouping by agg column
      tmp_df2 <- tmp_df %>%
        group_by(!!!rlang::syms(c(id_columns, c_))) %>%
        summarise_at(agg_col, .funs = agg_funs) %>%
        mutate(COLUMN = c_name,
               PERIOD = p) %>%
        unite(COLUMN, COLUMN, !!!rlang::syms(c(c_))) 
      sum_df <- bind_rows(list(sum_df, tmp_df2))
    }
    ## summarise grouping by id column
    tmp_df %<>%
      group_by(!!!rlang::syms(id_columns)) %>%
      summarise_at(agg_col, .funs = agg_funs) %>%
      mutate(COLUMN = 'TOTAL',
             PERIOD = p)
    sum_df <- bind_rows(list(sum_df, tmp_df))
  }
  
  if (wide) 
    return (sum_df %>%
              gather(., variable, value, 
                     gather_cols = setdiff(colnames(sum_df), c(id_columns, 'COLUMN', 'PERIOD'))) %>%
              unite(FEAT, COLUMN, variable, PERIOD) %>%
              spread(FEAT, value))
  else
    return (sum_df)
}


createTopNCols <- function(df, top_n_cols = list(), code_maps = list()) {
  # Creating top-N columns feature
  
  ## Args:
  ## @df : data frame
  ## @top_n_cols : a list of named tuples of the form `c(name = COL_NAME, 
  ##                                                     percentage = PERCENT)`
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
  ## @code_maps : a named list of the same length as top_n_cols. 
  ##             Contains codes to map for each column in top_n_cols.
  ##             If empty will be inferred from df.
  
  ## Returns:
  ## @df : modified data frame
  ## @code_maps : a named list with code map for each column
  
  for (c_ in top_n_cols) {
    c_name <- c_['name']
    prcnt <- c_['percentage']
    ## fill in NAs with empty
    df %<>% 
      mutate(!!c_name := plyr::mapvalues(!!rlang::sym(c_name), '', NA))
    if (!(c_name %in% names(code_maps))) { # if no code map is provided for this column
      ## proportion table
      prop_tmp <- table(df[, c_name]) %>% 
        prop.table %>%
        sort(decreasing = TRUE)
      ## codes for new features
      codes <- 1:length(prop_tmp) 
      ## first index when cumulative ratio is higher than percentage
      #threshold_idx <- which(cumsum(prop_tmp) > prcnt)[1]
      threshold_idx <- which(prop_tmp < (prcnt))[1] - 1
      ## recode everything after threshold_idx with new class - `0`
      if (!is.na(threshold_idx))
        codes[(threshold_idx + 1) : length(prop_tmp)] <- 0
      ## create a map of new codes -> old names
      code_map <- setNames(c(codes), names(prop_tmp))
      code_maps[[c_name]] <- code_map
      ## print verbose info
      if ( is.na(threshold_idx) ) threshold_idx <- length(codes);
      print(glue::glue("COLUMN : {c_name}\n",
                       "KEEPING {threshold_idx} out of {length(codes)} unique features\n",
                       "CONTAINING {round(100 * cumsum(prop_tmp)[threshold_idx], 2)}%", 
                       " of all values"))
    } else {
      ## Load old map
      code_map <- code_maps[[c_name]]
      ## Replace new values with NA
      uq_vals <- unique(df[, c_name])
      not_found <- uq_vals[!(uq_vals %in% names(code_map))]
      for (v_ in not_found) 
        code_map <- c(code_map, setNames(c(NA), v_))
        
    }
    ## create new feature
    ## http://dplyr.tidyverse.org/articles/programming.html
    ## https://github.com/tidyverse/rlang/issues/116
    df %<>% 
      mutate(!!c_name := plyr::mapvalues(!!rlang::sym(c_name), names(code_map), code_map))
  }
  return(list(df, code_maps))
}

### helper functions
dayDiff <- function(d1, d2) {
  # @ difference in days
  # between two days
  return (as.numeric(difftime(d1, d2, units='secs')) / 60 / 60 / 24);
}

