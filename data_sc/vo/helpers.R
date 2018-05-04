library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)
library(readxl)
library(rlang)

transIndex <- function(v, h) {
  # applying transIndex
  # which iterates over
  # values of v (dates)
  # and return index
  # if current value is within h
  # from the anchor.
  # in the beginning the anchor is v[1]
  anchor <- v[1]
  idx <- 1
  l_v <- length(v)
  indices <- rep(idx, l_v)
  for (i in 1:l_v) {
    el <- v[i]
    if (dayDiff(el, anchor) > h) {
      idx <- idx + 1
      indices[i:l_v] <- idx
      anchor <- el
    }
  }
  return(indices)
}

createAggSummaries <- function(df,
                               agg_cols = list(NULL, c(), c(), c()),
                               id_columns = c('CONTRACT_REF', 'CLIENT_ID', 'START_DATE'),
                               date_column = 'DT',
                               wide = FALSE,
                               verbose = TRUE) {
  # Aggregate through columns. 
  # The `date_column` is presented to cut off changes from the right.
  
  ## Args:
  ##' @df : data frame
  ##' @agg_cols : a list of the following form: `list(COL_TO_AGGREGATE,
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
  ##' @id_columns : a vector of strings, client identifier column.
  ##' @date_column : a string column. With which column to compare 'START_DATE'
  ##' @wide : a boolean, indicating whether to output the data.frame in a wide format.
  
  ## Returns:
  ##' @df : modified data frame
  
  ## preventing any future problems
  df %<>% ungroup
  
  agg_col <- agg_cols[[1]]
  agg_periods <- agg_cols[[2]]
  agg_group_cols <- agg_cols[[3]]
  agg_funs <- agg_cols[[4]]
  
  ## print verbose info
  printInfo(glue("AGGREGATING COLUMNS : ({paste0(agg_col, collapse = ', ')})"), verbose)
  printInfo(glue("THROUGH PERIODS OF ({paste0(agg_periods, collapse = ', ')})"), verbose)
  printInfo(glue("GROUPING BY COLUMNS ({paste0(agg_group_cols, collapse = ', ')})"), verbose)
  printInfo(glue("USING FUNCTIONS ({paste0(agg_funs, collapse = ', ')})"), verbose)
  printInfo(glue("WITH OUTPUT IN A {ifelse(wide, 'WIDE', 'LONG')} FORMAT"), verbose)
  
  sum_df <- NULL
  for (p in sort(agg_periods, decreasing = TRUE)) {
    tmp_df <- df %>%
      filter_(paste(date_column, ">", 'START_DATE - days(p)'))
    for (c_ in agg_group_cols) {
      ## summarise grouping by agg column
      tmp_df2 <- tmp_df %>%
        group_by(!!!syms(c(id_columns, c_))) %>%
        summarise_at(agg_col, .funs = agg_funs) %>%
        mutate(COLUMN = c_,
               PERIOD = p) %>%
        unite(COLUMN, COLUMN, !!sym(c_)) 
      sum_df <- bind_rows(list(sum_df, tmp_df2))
    }
    ## summarise grouping by id column
    tmp_df %<>%
      group_by(!!!syms(id_columns)) %>%
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


createTopNCols <- function(df, top_n_cols = list(), code_maps_rds = '', verbose = TRUE) {
  # Creating top-N columns feature
  
  ## Args:
  ##' @df : data frame
  ##' @top_n_cols : a list of named tuples of the form `c(name = COL_NAME, 
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
  ##' @code_maps_rds : a path to the rds file with code maps
  ##' @verbose : whether to print out message (default = TRUE)
  
  ## Returns:
  ##' @df : modified data frame
  ##' @code_maps : a named list with code map for each column
  
  if (code_maps_rds != '') 
    code_maps <- readRDS(code_maps_rds)
  else
    code_maps <- list()
  
  ## ungroup in case there exists any group
  df %<>% ungroup()
  
  for (c_ in top_n_cols) {
    c_name <- c_['name']
    prcnt <- as.numeric(c_['percentage'])
    ## fill in empty with NAs
    if ('' %in% unique(df[, c_name]))
      df %<>% 
        mutate(!!c_name := plyr::mapvalues(!!sym(c_name), '', NA))
    if (!(c_name %in% names(code_maps))) { # if no code map is provided for this column
      ## proportion table
      prop_tmp <- table(df[, c_name]) %>% 
        prop.table %>%
        sort(decreasing = TRUE)
      ## codes for new features
      codes <- 1:length(prop_tmp) 
      ## first index when cumulative ratio is higher than percentage
      threshold_idx <- which(cumsum(prop_tmp) > prcnt)[1]
      ## recode everything after threshold_idx with new class - `0`
      codes[(threshold_idx + 1) : length(prop_tmp)] <- 0
      ## create a map of new codes -> old names
      code_map <- setNames(c(codes), names(prop_tmp))
      code_maps[[c_name]] <- code_map
      ## print verbose info
      printInfo(glue("COLUMN : {c_name}"), verbose)
      printInfo(glue("KEEPING {threshold_idx} OUT OF {length(codes)} UNIQUE VALUES"), verbose)
      printInfo(glue("CONTAINING {round(100 * cumsum(prop_tmp)[threshold_idx], 2)}% OF ALL VALUES"), verbose)
    } else {
      ## Load old map
      code_map <- code_maps[[c_name]]
      ## Replace new values with NA
      uq_vals <- df %>% select(!!sym(c_name)) %>% unique %>% pull %>% as.character
      not_found <- uq_vals[!(uq_vals %in% names(code_map))]
      for (v_ in not_found)  
        code_map <- c(code_map, setNames(c(NA), v_))
        
    }
    ## create new feature
    ## http://dplyr.tidyverse.org/articles/programming.html
    ## https://github.com/tidyverse/rlang/issues/116
    df %<>% 
      mutate(!!c_name := plyr::mapvalues(!!rlang::sym(c_name), 
                                         names(code_map), 
                                         as.vector(code_map)))
  }
  return(list(df, code_maps))
}

dropCols <- function(df, 
                     NA_percent = 0.6, 
                     one_col_max_percent = 0.95,
                     verbose = TRUE) {
  ## Find columns:
  ## 1) constants
  ## 2) where NA_percent > then given
  ## 3) where one value proportion is higher than > one_col_max_percent
  
  #' @df : data frame
  #' @NA_percent : drop columns with proportion of NAs (or empty) more than NA_percent
  #' @one_col_max_percent : drop columns where one value takes more than one_col_max_percent
  #' @verbose : whether to print out message (default = TRUE)
  
  ## prevent from any possible malfunctioning
  df %<>% ungroup()
  
  ## NA_cols
  na_cols <- df %>% 
    summarise_all(funs(mean(is.na(.)))) %>% 
    select(which(. > NA_percent)) %>% 
    colnames()
  printInfo(glue("FOUND NA COLS (NA_MAX = {NA_percent}) : ({paste0(na_cols, collapse = ',')})"),
            verbose)
  
  ## constant cols
  const_cols <- df %>% 
    summarise_all(funs(length(unique(.)))) %>% 
    select(which(. == 1)) %>% 
    colnames()
  printInfo(glue("FOUND CONST COLS : ({paste0(const_cols, collapse = ',')})"),
            verbose)
  
  ## one class cols
  one_class_cols <- df %>%
    ungroup() %>% 
    gather(var, value) %>%
    count(var, value) %>%
    mutate(prop = n / nrow(df)) %>% 
    group_by(var) %>% 
    filter(any(prop > one_col_max_percent)) %>%
    {unique(.$var)}
  printInfo(glue("FOUND ONE CLASS COLS (MAX_PERCENT = {one_col_max_percent}): ({paste0(one_class_cols, collapse = ',')})"),
            verbose)
  return (unique(c(na_cols, const_cols, one_class_cols)))
}

### helper functions
dayDiff <- function(d1, d2) {
  # difference in days
  # between two dates
  return (as.numeric(difftime(d1, d2, units='secs')) / 60 / 60 / 24);
}

printInfo <- function(text_string, verbose = TRUE) {
  #' Helper logging function
  #' @text_string : what to print
  #' @verbose : boolean flag
  
  # function call name
  f_name <- paste0(deparse(sys.calls()[[sys.nframe()-1]]), collapse = '')
  f_name <- sub(" *\\(.*", "", f_name)
  if (verbose)
    print(glue('[{now()}, {f_name}] : {text_string}'))
}

readCD <- function(file_path, bank, verbose = TRUE) {
  #' @file_path : path to file with CD data
  #' @bank: bank variable; used to filter rows.
  #' @verbose : whether to print out message (default = TRUE)
  
  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  df <- read_xlsx(file_path)
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(df)}, {ncol(df)})"), verbose)
  ## map ARM_3 -> ATM3, rename BANK COLUMN
  if ('SCENARIO_NAME_UNI' %in% colnames(df))
    df %<>% 
      mutate(SCENARIO_NAME_UNI = plyr::mapvalues(SCENARIO_NAME_UNI,
                                                 'ARM_3',
                                                 'ATM_3'))
  df %<>% 
    rename(label = CD_F) %>%
    mutate(CLIENT_ID = as.character(CLIENT_ID))
  ## rename BANK column as it might be corrupted
  bank_col_idx <- which(str_detect(colnames(df), 'BANK'))
  colnames(df)[bank_col_idx] <- "BANK"
  printInfo(glue("KEEPING ROWS WITH BANK = {bank}"), verbose)
  df %<>% filter(BANK == bank)
  ## drop unnecessary columns
  drop_cols <- c('CREATE_DTTM', 'SCENARIO_NAME', 'END_DATE', 'CD_SUM')
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  df %<>%
    select(-one_of(drop_cols))
  printInfo(glue("FINAL DATA SHAPE : ({nrow(df)}, {ncol(df)})"), verbose)
  return(df)
}

readBAL <- function(df, file_path, verbose = TRUE) {
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the balances data should be joined
  #' @file_path : path to file with BAL data
  #' @verbose : whether to print out message (default = TRUE)
  
  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  ## effectively, ID columns
  join_cols <- c('CLIENT_ID', 'CONTRACT_REF')
  bal_df <- fread(file_path,
                  sep = '>',
                  stringsAsFactors = FALSE) %>%
    rename(CONTRACT_REF = AR_NO) %>%
    mutate(ST_BAL_DT = ymd_hms(ST_BAL_DT),
           DEP_AMT = as.numeric(gsub(',', '', DEP_AMT)),
           CRD_AMT = as.numeric(gsub(',', '', CRD_AMT)),
           DLQ_AMT = as.numeric(gsub(',', '', DLQ_AMT)),
           CLIENT_ID = as.character(gsub(',', '', CLIENT_ID))) %>%
    group_by(!!!syms(join_cols)) %>%
    arrange(ST_BAL_DT) %>%
    filter(((DEP_AMT - lag(DEP_AMT, 1, default = DEP_AMT[1] + 1)) != 0) |
             ((CRD_AMT - lag(CRD_AMT, 1, default = CRD_AMT[1] + 1)) != 0) |
             ((DLQ_AMT - lag(DLQ_AMT, 1, default = DLQ_AMT[1] + 1)) != 0)) %>%
    ungroup() %>%
    mutate(BAL = DEP_AMT - CRD_AMT - DLQ_AMT,
           DEBT = DLQ_AMT) %>%
    select(-DEP_AMT, -CRD_AMT, -DLQ_AMT)
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(bal_df)}, {ncol(bal_df)})"), verbose)
  
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  bal_df %<>% 
    inner_join(df,
               by = join_cols) %>%
    filter(ST_BAL_DT < START_DATE)
  printInfo(glue("DATA SHAPE AFTER JOINING : ({nrow(bal_df)}, {ncol(bal_df)})"), verbose)
  rm(df)

  printInfo(glue("PARSING BAL AND DEBT SEPARATELY"), verbose)
  just_blnc <- bal_df %>% 
    select(-DEBT) %>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(ST_BAL_DT) %>%
    filter(((BAL - lag(BAL, 1, default = BAL[1] + 1)) != 0)) %>%
    ungroup()
  
  just_debt <- bal_df %>% 
    select(-BAL) %>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(ST_BAL_DT) %>%
    filter(((DEBT - lag(DEBT, 1, default = DEBT[1] + 1)) != 0)) %>%
    ungroup()
  
  printInfo(glue("BAL DATA SHAPE AFTER FILTERING : ({nrow(just_blnc)}, {ncol(just_blnc)})"), verbose)
  printInfo(glue("DEBT DATA SHAPE AFTER FILTERING : ({nrow(just_debt)}, {ncol(just_debt)})"), verbose)
  
  printInfo(glue("CREATING AGGREGATED SUMMARIES FOR BAL AND DEBT"), verbose)
  just_blnc %<>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(ST_BAL_DT) %>%
    mutate(BAL_CHNG = BAL - lag(BAL, n = 1,
                                default = NA),
           BAL_CHNG_SGN = sign(BAL_CHNG),
           BAL_NEG_CHNG = ifelse(BAL_CHNG_SGN == -1,
                                 BAL_CHNG,
                                 NA),
           BAL_POS_CHNG = ifelse(BAL_CHNG_SGN == 1,
                                 BAL_CHNG,
                                 NA)) %>%
    select(-BAL_CHNG, -BAL_CHNG_SGN, -BAL) %>%
    ungroup()
  just_blnc <- createAggSummaries(just_blnc,
                                  id_columns = c(join_cols, 'START_DATE'),
                                  agg_cols = list(c('BAL_NEG_CHNG', 'BAL_POS_CHNG'),
                                                   c(7, 31, 90, 180), 
                                                   c(),
                                                   funs(MAX = max(., na.rm = TRUE),
                                                        MIN = min(., na.rm = TRUE),
                                                        N = sum(!is.na(.)),#length(., na.rm = TRUE),
                                                        SUM = sum(., na.rm = TRUE))),
                                  date_column = 'ST_BAL_DT',
                                  wide = TRUE,
                                  verbose = verbose)
  # replace NAs, inf with 0
  just_blnc %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .)))
  
  just_debt %<>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(ST_BAL_DT) %>%
    mutate(DEBT_CHNG = DEBT - lag(DEBT, n = 1,
                                  default = NA),
           DEBT_CHNG_SGN = sign(DEBT_CHNG),
           DEBT_NEG_CHNG = ifelse(DEBT_CHNG_SGN == -1,
                                  DEBT_CHNG,
                                  NA),
           DEBT_POS_CHNG = ifelse(DEBT_CHNG_SGN == 1,
                                  DEBT_CHNG,
                                  NA)) %>%
    select(-DEBT_CHNG, -DEBT_CHNG_SGN, -DEBT) %>%
    ungroup()
  
  just_debt <- createAggSummaries(just_debt,
                                  id_columns = c(join_cols, 'START_DATE'),
                                  agg_cols = list(c('DEBT_NEG_CHNG', 'DEBT_POS_CHNG'),
                                                   c(7, 31, 90, 180), 
                                                   c(),
                                                   funs(MAX = max(., na.rm = TRUE),
                                                        MIN = min(., na.rm = TRUE),
                                                        N = sum(!is.na(.)),#length(., na.rm = TRUE),
                                                        SUM = sum(., na.rm = TRUE))),
                                  date_column = 'ST_BAL_DT',
                                  wide = TRUE,
                                  verbose = verbose)
  # replace NAs, inf with 0
  just_debt %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .)))
  
  printInfo(glue("TAKING MOST RECENT BAL AND DEBT"), verbose)
  rcnt_blnc <- bal_df %>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    summarise(BAL = BAL[which.max(ST_BAL_DT)],
              DEBT = DEBT[which.max(ST_BAL_DT)])
  
  rm(bal_df)
  printInfo(glue("JOINING ALL FEATURES"), verbose)
  just_blnc %<>%
    full_join(just_debt, by = c(join_cols, 'START_DATE')) %>%
    full_join(rcnt_blnc, by = c(join_cols, 'START_DATE'))
  printInfo(glue("FINAL DATA SHAPE : ({nrow(just_blnc)}, {ncol(just_blnc)})"), verbose)
  return(just_blnc)
}

readBIN <- function(df, file_path, verbose = TRUE) {
  ## number of times clients signed to each loyalty group
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with BIN data
  #' @verbose : whether to print out message (default = TRUE)
  
  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  ## effectively, ID columns
  join_cols <- c('CLIENT_ID', 'CONTRACT_REF')
  
  bin_df <- read.csv(file_path,
                     encoding = 'cp1251',
                     fileEncoding = 'cp1251', 
                     stringsAsFactors = FALSE) %>%
    rename(START_DATE_LOCAL = START_DATE) %>%
    mutate(START_DATE_LOCAL = ymd_hms(START_DATE_LOCAL),
           CLIENT_ID = as.character(gsub(',', '', CLIENT_ID)))
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(bin_df)}, {ncol(bin_df)})"), verbose)

  ## drop unnecessary columns
  drop_cols <- c('BANK', 'END_DATE')
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  bin_df %<>%
    select(-one_of(drop_cols)) %>%
    inner_join(df,
               by = join_cols) %>%
    filter(START_DATE_LOCAL < START_DATE)
  printInfo(glue("DATA SHAPE AFTER JOINING AND DROPPING COLUMNS : ({nrow(bin_df)}, {ncol(bin_df)})"), verbose)
  rm(df)
  
  printInfo(glue("CREATING BINBONUS FEATURES"), verbose)
  bin_df %<>%
    select(-START_DATE_LOCAL) %>%
    group_by(!!!syms(join_cols), START_DATE, CODE) %>%
    summarise(VALUE = n()) %>%
    spread(CODE, VALUE, fill = 0)
  printInfo(glue("FINAL DATA SHAPE : ({nrow(bin_df)}, {ncol(bin_df)})"), verbose)
  return(bin_df)
}

readLIM <- function(df, file_path, verbose = TRUE) {
  ## number of times clients signed to each loyalty group
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with LIM data
  #' @verbose : whether to print out message (default = TRUE)

  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  ## effectively, ID columns
  join_cols <- c('CONTRACT_REF')
  
  lim_df <- read.csv(file_path,
                     sep = '>',
                     encoding = 'cp1251',
                     fileEncoding = 'cp1251', 
                     stringsAsFactors = FALSE)
  colnames(lim_df) %<>% toupper() # somewhere it is 'lim', elsewhere it is 'LIM'
  lim_df %<>%
    mutate(LIM = as.numeric(gsub(",", "", LIM)),
           DT = as.Date(DT, '%Y-%m-%d')) #ymd_hms(DT))
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(lim_df)}, {ncol(lim_df)})"), verbose)

  drop_cols <- c('BANK')
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  lim_df %<>%
    select(-one_of(drop_cols)) %>%
    inner_join(df %>% select(-CLIENT_ID) %>% distinct(),
               by = join_cols) %>%
    filter(DT < START_DATE)
  printInfo(glue("DATA SHAPE AFTER JOINING AND DROPPING COLUMNS : ({nrow(lim_df)}, {ncol(lim_df)})"), verbose)
  rm(df)
  
  printInfo(glue("CREATING LIMIT FEATURES"), verbose)
  lim_df %<>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(DT) %>%
    mutate(CHNG = LIM - lag(LIM, n = 1, default = LIM[1])) %>%
    summarise(LIM_AVG_N_CHANGES = mean(CHNG != 0),
              LIM_MAX_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                           max(CHNG[CHNG > 0]),
                                           0),
              LIM_MIN_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                           min(CHNG[CHNG > 0]),
                                           0),
              LIM_MAX_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
                                           min(CHNG[CHNG < 0]),
                                           0),
              LIM_MIN_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
                                           max(CHNG[CHNG < 0]),
                                           0),
              LIM_AVG_CHANGE = mean(CHNG),
              LIM_AVG_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                           mean(CHNG[CHNG > 0]),
                                           0),
              LIM_AVG_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
                                           mean(CHNG[CHNG < 0]),
                                           0),
              LIM_AVG_N_POS_CHANGE = mean(CHNG > 0),
              LIM_AVG_N_NEG_CHANGE = mean(CHNG < 0),
              LIM_RECENT = LIM[which.max(DT)])
  # replace NAs, inf with 0
  lim_df %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .)))
  printInfo(glue("FINAL DATA SHAPE : ({nrow(lim_df)}, {ncol(lim_df)})"), verbose)
  return (lim_df)
} 

readSD <- function(df, 
                   file_path, 
                   NA_percent = 0.6, 
                   one_col_max_percent = 0.95, 
                   verbose = TRUE) {
  ## SOC. DEM. DATA
  
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with SD data
  #' @NA_percent : drop columns with proportion of NAs (or empty) more than NA_percent
  #' @one_col_max_percent : drop columns where one value takes more than one_col_max_percent
  #' @verbose : whether to print out message (default = TRUE)

  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  ## effectively, ID columns
  join_cols <- c('CONTRACT_REF')
  
  sd_df <- read_xlsx(file_path,
                     na = c('', '<null>')) %>%
    rename(CLIENT_ID = rep_clid,
           CONTRACT_REF = contract_number) %>%
    mutate(REP_POPULATION = as.numeric(gsub(',', '', REP_POPULATION)),
           CLIENT_ID = as.character(CLIENT_ID)) %>%
    select(-CLIENT_ID)
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(sd_df)}, {ncol(sd_df)})"), verbose)
  
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  sd_df %<>%
    inner_join(df %>% select(-START_DATE, -CLIENT_ID) %>% distinct(),
               by = join_cols) %>%
    distinct()
  printInfo(glue("DATA SHAPE AFTER JOINING : ({nrow(sd_df)}, {ncol(sd_df)})"), verbose)
  rm(df)
  
  ## drop unnecessary columns:
  ## 1) constants
  ## 2) NA_percent > then given
  ## 3) one column takes more than > one_col_max_percent
  drop_cols <- dropCols(sd_df, 
                        NA_percent = NA_percent,
                        one_col_max_percent = one_col_max_percent,
                        verbose = verbose)
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  printInfo(glue("FINAL DATA SHAPE : ({nrow(sd_df)}, {ncol(sd_df)})"), verbose)
  return(sd_df)
}

readStatus <- function(df, file_path, top_n_cols, 
                       code_maps_card_rds, code_maps_contr_rds,
                       verbose = TRUE) {
  ## parsing status data, creating features
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with status data
  #' @top_n_cols : a list of named tuples of the form `c(name = COL_NAME,
  #                                                     percentage = PERCENT)`
  #               where `COL_NAME` is the name of the column to be considered,
  #               `PERCENT` is the percentage of values needed to be explained by
  #               top-k unique values. Corresponding top-k unique values will be
  #               kept, others will be combined into a single column.
  #               E.g., if a column contains a hundred of values, while
  #               there are only 5 unique values with one class being presented
  #               90 times, then if passing this column name with `PERCENT=0.9`
  #               the output column would contain only two classes: `0` and `1`,
  #               where `1` would be the class with highest presence, and
  #               `0` would be all other classes.
  #' @code_maps_card_rds : a path to the rds file with code maps for card
  #' @code_maps_contr_rds : a path to the rds file with code maps for contr
  #' @verbose : whether to print out message (default = TRUE)

  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  
  ## effectively, ID columns
  join_cols <- c('CLIENT_ID', 'CONTRACT_REF')
  
  st_df <- read.csv(file_path,
                    sep = '>',
                    nrows = -1,
                    stringsAsFactors = FALSE,
                    encoding = 'cp1251',
                    fileEncoding = 'cp1251',
                    dec = ',') %>%
    select(CONTRACT_REF, CLIENT_ID, AMND_DATE, CONTRACT_NUMBER, CONTR_STATUS) %>%
    mutate(CLIENT_ID = as.character(gsub(',', '', CLIENT_ID)),
           AMND_DATE = ymd_hms(AMND_DATE)) %>%
    distinct()
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(st_df)}, {ncol(st_df)})"), verbose)
  
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  st_df %<>%
    inner_join(df,
               by = join_cols) %>%
    filter(AMND_DATE < START_DATE)
  printInfo(glue("DATA SHAPE AFTER JOINING : ({nrow(st_df)}, {ncol(st_df)})"), verbose)
  rm(df)

  # CARD_CONTRACT = if card = 0, contract = 1
  st_df %<>%
    mutate(CARD_CONTRACT = as.numeric(grepl("P", CONTRACT_NUMBER)),
           CONTR_STATUS = as.numeric(gsub(',', '', CONTR_STATUS))) %>%
    distinct() %>%
    group_by(CONTRACT_REF, START_DATE, CONTRACT_NUMBER) %>%
    arrange(AMND_DATE) %>%
    filter(CONTR_STATUS != lag(CONTR_STATUS, 1, default = -999.)) %>%
    ungroup()
  
  printInfo(glue("CREATING STATUS FEATURES INDEPENDENTLY FOR CARD AND CONTRACT"), verbose)
  # make independently: two status for cards and for contracts 
  status_card <- st_df %>% filter(CARD_CONTRACT == 0)
  status_contr <- st_df %>% filter(CARD_CONTRACT == 1)  
  
  card_out <- createTopNCols(df = status_card, top_n_cols = top_n_cols, 
                             code_maps_rds = code_maps_card_rds, verbose = verbose)
  status_card <- card_out[[1]]
  status_card_codemap <- card_out[[2]]
  rm(card_out)
  
  contr_out <- createTopNCols(df = status_contr, top_n_cols = top_n_cols,
                              code_maps_rds = code_maps_contr_rds, verbose = verbose)
  status_contr <- contr_out[[1]]
  status_contr_codemap <- contr_out[[2]]
  rm(contr_out)
  
  # returns last status and last date of the status change( before Date of communication)
  #  comment: status has new levels ( from top_N - 1: count in frequency decreasing oreder ) 
  #  comment: last date of status change by all! cards and its status
  last_card_status <-  status_card  %>% 
    select(-CONTRACT_NUMBER) %>%
    group_by(!!!syms(join_cols), START_DATE) %>% 
    slice(which.max(AMND_DATE)) %>%
    ungroup() %>%
    rename(LAST_CARD_STATUS = CONTR_STATUS,
           LAST_CARD_STATUS_DT = AMND_DATE)
  
  # same for contracts
  last_contr_status <-  status_contr  %>% 
    select(-CONTRACT_NUMBER) %>%
    group_by(!!!syms(join_cols), START_DATE) %>% 
    slice(which.max(AMND_DATE)) %>%
    ungroup() %>%
    rename(LAST_CONTR_STATUS = CONTR_STATUS,
           LAST_CONTR_STATUS_DT = AMND_DATE)
  
  # count times of most frequent top n status by card
  status_card %<>%  
    rename(CARD_STATUS = CONTR_STATUS) %>%
    group_by(!!!syms(join_cols), START_DATE, CARD_STATUS)  %>%
    summarise(count = n()) %>%
    gather(., variable, value, CARD_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count) %>%
    ungroup()
  
  # count times of most frequent top n status by contr
  status_contr %<>%  
    group_by(!!!syms(join_cols), START_DATE, CONTR_STATUS)  %>%
    summarise(count = n()) %>%
    gather(., variable, value, CONTR_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count) %>%
    ungroup()
  
  # table with count cards/contr;  mean diff days of status change
  st_aggr <- st_df %>% 
    group_by(!!!syms(join_cols), START_DATE, CONTRACT_NUMBER, CARD_CONTRACT) %>%
    arrange(AMND_DATE) %>%
    mutate(DIFF_DATE = dayDiff(AMND_DATE, lag(AMND_DATE, 1, default = NA))) %>%
    summarise(count = n() , 
              #mean_days_change = ifelse((count - 1) < 1, 0,
              #                          as.numeric( max(AMND_DATE) - min(AMND_DATE)) / ( n()-1)),
              mean_days_diff_change = ifelse(count < 2, 0, mean(DIFF_DATE, na.rm = TRUE))) %>%
    group_by(!!!syms(join_cols), START_DATE, CARD_CONTRACT) %>% 
    summarise(MEAN_STATUS_COUNT = mean(count), 
              CARD_CONTR_COUNT = n(), 
              #MEAN_DAYS_CHANGE = mean(mean_days_change),
              MEAN_DAYS_DIFF_CHANGE = mean(mean_days_diff_change)) %>%
    gather(., variable, value, gather_cols = c('MEAN_STATUS_COUNT', 
                                               'CARD_CONTR_COUNT',
                                               'MEAN_DAYS_DIFF_CHANGE')) %>%
    unite(var, variable, CARD_CONTRACT) %>%
    spread(var, value) %>%
    ungroup()
  
  # MEAN_STATUS_COUNT = mean status card/contract changes( mean by card/contr number)  
  # CARD_CONTR_COUNT =  number of contr/cards
  # MEAN_DAYS_CHANGE = mean number of days between status change
  #  comment: mean is taken 2 times  first by card and then by client
  #  comment: if status has never changed , MEAN_DAYS_CHANG = 0
  
  # join columns together by client_id
  st_final <- full_join(st_aggr, last_card_status %>% select(-CARD_CONTRACT),
                        by = c(join_cols, 'START_DATE')) %>%
    full_join(last_contr_status %>% 
                select(-CARD_CONTRACT), by = c(join_cols, 'START_DATE'))  %>%  
    full_join(status_card, by = c(join_cols, 'START_DATE'))  %>% 
    full_join(status_contr, by = c(join_cols, 'START_DATE'))
  printInfo(glue("FINAL DATA SHAPE : ({nrow(st_final)}, {ncol(st_final)})"), verbose)
  return(list(st_final, status_card_codemap, status_contr_codemap))
}

readAddress <- function(df, file_path, top_n_cols, code_maps_rds, verbose = TRUE) {
  ## parsing address data, creating features
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with status data
  #' @top_n_cols : a list of named tuples of the form `c(name = COL_NAME,
  #                                                     percentage = PERCENT)`
  #               where `COL_NAME` is the name of the column to be considered,
  #               `PERCENT` is the percentage of values needed to be explained by
  #               top-k unique values. Corresponding top-k unique values will be
  #               kept, others will be combined into a single column.
  #               E.g., if a column contains a hundred of values, while
  #               there are only 5 unique values with one class being presented
  #               90 times, then if passing this column name with `PERCENT=0.9`
  #               the output column would contain only two classes: `0` and `1`,
  #               where `1` would be the class with highest presence, and
  #               `0` would be all other classes.
  #' @code_maps_rds : a path to the rds file with code maps 
  #' @verbose : whether to print out message (default = TRUE)

  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  
  ## effectively, ID columns
  join_cols <- c('CONTRACT_REF')
  
  addr_df <- read.csv(file_path,
                      nrows = -1,
                      stringsAsFactors = FALSE,
                      encoding = 'cp1251',
                      fileEncoding = 'cp1251',
                      dec = ',') %>%
    filter((contract_number != '') | (!is.na(clientid))) %>%
    rename(CONTRACT_REF = contract_number) %>% #, CLIENT_ID = clientid) %>% not client_id
  #  mutate(CLIENT_ID = as.character(CLIENT_ID)) %>%
    distinct()
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(addr_df)}, {ncol(addr_df)})"), verbose)
  
  drop_cols <- c("rn", "BaseId", "AddrID", "GeoPrecision", "GeoKind", "VerifyCode",
                 "clientid", "AddrCode", "AddrWho", "AddrRemark", "AddrDTM",
                 "EditorLogin", "AddrCitySocr", "AddrStreetSocr", "AddrTownSocr",
                 "AddrSubTownSocr", "AddrAreaSocr", "AddrStateSocr",
                 "Latitude", "Longitude", "Source", "UADId", "UADKind",
                 "AddrStr..")
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  addr_df %<>%
    select(-one_of(drop_cols)) %>%
    inner_join(df %>% select(-CLIENT_ID, -START_DATE) %>% distinct(),
               by = join_cols) %>%
    distinct()
  printInfo(glue("DATA SHAPE AFTER JOINING AND DROPPING COLUMNS : ({nrow(addr_df)}, {ncol(addr_df)})"), verbose)
  rm(df)
  
  printInfo("PARSING CITY AND COUNTRY COLUMNS", verbose)
  
  pattern <- "(?<![^\\s\\z\\.])[\\s]*((Г[ОO]?[РP]?[OО]?Д?)|([CС]?[EЕ]?Л?[OО]?))[\\s\\z\\.]?(?![^\\s\\z\\.])[\\s\\z\\.]*"
  
  addr_df %<>%
    mutate(AddrCity2 = str_replace_all(toupper(AddrCity), pattern, '') %>%
             str_trim(),
           AddrTown2 = str_replace_all(toupper(AddrTown), pattern, '') %>%
             str_trim(),
           AddrState2 = str_replace_all(toupper(AddrState), pattern, '') %>%
             str_trim(),
           AddrStreet2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                              "\\1",
                              toupper(AddrStreet)) %>% str_trim(),
           AddrArea2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                            "\\1",
                            toupper(AddrArea)) %>% str_trim(),
           AddrSubTown2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                               "\\1",
                               toupper(AddrSubTown)) %>% 
             str_trim()) %>%
    mutate(AddrCity2 = ifelse(AddrCity2 == '',
                              ifelse(AddrTown2 == '',
                                     ifelse(AddrState2 == '',
                                            ifelse(AddrStreet2 == '',
                                                   ifelse(AddrArea2 == '',
                                                          AddrSubTown2,
                                                          AddrArea2),
                                                   AddrStreet2),
                                            AddrState2),
                                     AddrTown2),
                              AddrCity2),
           AddrCity2 = str_replace_all(AddrCity2, "[[:punct:] ]", ""))
  ## assuming that the bigger CallipsoDT, the more recent the record
  addr_df %<>% 
    select(!!!syms(join_cols), contacttype, AddrCity2, AddrCountry, CalipsoDT) %>%
    group_by(!!!syms(join_cols), contacttype) %>%
    slice(which.max(CalipsoDT)) %>%
    distinct() %>%
    ungroup()
  
  ## will only consider types 11, 12, 13
  addr_df %<>% filter(contacttype <= 13)
  ## creating features
  addr_out <- createTopNCols(addr_df, top_n_cols = top_n_cols, 
                             code_maps_rds = code_maps_rds, verbose = verbose)
  addr_df <- addr_out[[1]]
  addr_codemap <- addr_out[[2]]
  rm(addr_out)
  
  addr_df %<>%
    select(-CalipsoDT) %>%
    gather(property, value, -CONTRACT_REF, -contacttype) %>%
    unite(COLUMN, contacttype, property) %>%
    spread(COLUMN, value)
  
  printInfo(glue("FINAL DATA SHAPE : ({nrow(addr_df)}, {ncol(addr_df)})"), verbose)
  return(list(addr_df, addr_codemap))
}

readPhone <- function(df, file_path, verbose = TRUE) {
  ## phones data
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with LIM data
  #' @verbose : whether to print out message (default = TRUE)

  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  ## effectively, ID columns
  join_cols <- c('CLIENT_ID', 'CONTRACT_REF')
  
  ph_df <- read.csv(file_path,
                    nrows = -1,
                    stringsAsFactors = FALSE,
                    encoding = 'cp1251',
                    fileEncoding = 'cp1251') %>%
    rename(CLIENT_ID = ID) %>%
    mutate(CLIENT_ID = as.character(CLIENT_ID)) %>%
    distinct()

  printInfo(glue("INITIAL DATA SHAPE : ({nrow(ph_df)}, {ncol(ph_df)})"), verbose)
  printInfo(glue("JOINING ON COLUMNS : ({paste0(join_cols, collapse = ',')})"), verbose)
  ph_df %<>%
    inner_join(df %>% select(-START_DATE) %>% distinct(),
               by = join_cols) %>%
    distinct()
  printInfo(glue("DATA SHAPE AFTER JOINING : ({nrow(ph_df)}, {ncol(ph_df)})"), verbose)
  rm(df)
  
  ### will create two features: 
  ### 1) the number of home phones;
  ### 2) the number of mobile phones
  printInfo("CREATING FEATURES", verbose)
  ph_df %<>%
    group_by(!!!syms(join_cols), TYPE_PH) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    spread(TYPE_PH, N)
  
  printInfo(glue("FINAL DATA SHAPE : ({nrow(ph_df)}, {ncol(ph_df)})"), verbose)
  return (ph_df)
} 

readTrns <- function(df,
                     file_path,
                     file_path_mcc,
                     agg_window,
                     n_recent,
                     top_n_cols,
                     agg_cols,
                     code_maps_rds,
                     verbose) {
  ## parsing transactions data
  #' @df : intiial data with CLIENT_ID, CONTRACT_REF, START_DATE,
  #        on which the data should be joined
  #' @file_path : path to file with trns data data
  #' @file_path_mcc : path to file with mcc codes
  #' @agg_window : aggregate transactions of the same type within this window (in days)
  #' @n_recent : choose at most this number of recent transactions
  #' @top_n_cols : a list of named tuples of the form `c(name = COL_NAME,
  #                                                     percentage = PERCENT)`
  #               where `COL_NAME` is the name of the column to be considered,
  #               `PERCENT` is the percentage of values needed to be explained by
  #               top-k unique values. Corresponding top-k unique values will be
  #               kept, others will be combined into a single column.
  #               E.g., if a column contains a hundred of values, while
  #               there are only 5 unique values with one class being presented
  #               90 times, then if passing this column name with `PERCENT=0.9`
  #               the output column would contain only two classes: `0` and `1`,
  #               where `1` would be the class with highest presence, and
  #               `0` would be all other classes.
  #' @agg_cols : a list of the following form: `list(COL_TO_AGGREGATE,
  #                                                 c(PERIODS_TO_AGGREGATE),
  #                                                 c(COLS_TO_GROUP),
  #                                                 c(AGGREGATE_FUNS))`, where
  #               `COL_TO_AGGREGATE` is a numeric column, to which
  #               the aggregation functions will be applied;
  #               `PERIODS_TO_AGGREGATE` is a vector of days from `end_date`
  #               in which the aggregation should be performed;
  #               `COLS_TO_GROUP` is a vector of columns for which the aggregation
  #               should be grouped;
  #               `AGGREGATE_FUNS` is a `funs` list of functions to apply.
  #               The same periods and functions will be applied to all columns
  #               in `COLS_TO_GROUP`, thus the total number of newly added columns
  #               will be equal to |PERIODS_TO_AGGREGATE| x |COLS_TO_GROUP| x
  #                                              |AGGREGATE_FUNS|.
  #' @code_maps_rds : a path to the rds file with code maps
  #' @verbose : whether to print out message (default = TRUE)
  
  join_cols <- c('CONTRACT_REF')
  
  printInfo(glue("READING IN DATA FROM {file_path}"), verbose)
  trns_df <- fread(file_path,
                   sep = '>',
                   stringsAsFactors = FALSE,
                   nrows = -1)
  
  printInfo(glue("INITIAL DATA SHAPE : ({nrow(trns_df)}, {ncol(trns_df)})"), verbose)
  
  drop_cols <- c("CRD_NO", "PST_DT", "TXN_CT", "TXN_CURR", "TXN_DTL")
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  printInfo(glue("JOINING ON : ({paste0(join_cols, collapse = ',')})"), verbose)
  
  trns_df %<>% 
    select(-one_of(drop_cols)) %>%
    inner_join(df %>% select(-CLIENT_ID) %>% distinct(), by = join_cols) %>%
    distinct() %>%
    filter(TXN_RUB_AMT != '') %>%
    mutate(TXN_RUB_AMT = as.numeric(gsub(',', '', TXN_RUB_AMT)),
           TXN_DT = ymd_hms(TXN_DT)) %>%
    filter(TXN_DT < START_DATE)
  printInfo(glue("DATA SHAPE AFTER JOINING AND DROPPING : ({nrow(trns_df)}, {ncol(trns_df)})"), verbose)
  rm(df)
  
  ## mcc data
  mcc_df <- read.csv(file_path_mcc, sep = ';')
  mcc_df %<>% rename(TXN_MCC = MCC.код,
                     CATEGORY = Категория)
  mcc_df$CODE <- as.numeric(mcc_df$CATEGORY)
  
  trns_df %<>% left_join(mcc_df, by='TXN_MCC') %>%
    select(-TXN_MCC, -CATEGORY)
  
  rm(mcc_df)
  
  ## create top-N cols
  trns_out <- createTopNCols(df = trns_df, top_n_cols = top_n_cols, 
                             code_maps_rds = code_maps_rds, verbose = verbose)
  trns_df <- trns_out[[1]]
  trns_card_codemap <- trns_out[[2]]
  rm(trns_out)
  
  ## aggregate transactions that happened at almost the same time
  blend_cols <- c('TXN_TP', 'TXN_CHN', 'TXN_CTR', 'CODE')
  printInfo(glue("BLENDING TRANSACTIONS WITHIN {agg_window} DAYS WITH THE SAME VALUES",
                 " IN ({paste0(blend_cols, collapse = ',')})"), verbose)
  trns_df %<>%
    group_by(!!!syms(join_cols), START_DATE, TXN_TP, TXN_CHN, TXN_CTR, CODE) %>%
    mutate(TXN_GROUP = transIndex(TXN_DT, agg_window)) %>%
    group_by(!!!syms(join_cols), START_DATE, TXN_TP, TXN_CHN, TXN_CTR, CODE, TXN_GROUP) %>%
    summarise(TXN_RUB_AMT = sum(TXN_RUB_AMT), TXN_DT = min(TXN_DT)) %>%
    ungroup()
  printInfo(glue("DATA SHAPE AFTER BLENDING : ({nrow(trns_df)}, {ncol(trns_df)})"), verbose)
  ## take last transactions
  last_trns <- trns_df %>%
    group_by(!!!syms(join_cols), START_DATE) %>%
    arrange(!!!syms(join_cols), TXN_DT) %>%
    mutate(n_trns = row_number(), n_days = dayDiff(START_DATE, TXN_DT)) %>%
    top_n(n_recent, n_trns) %>%
    ungroup()
  printInfo(glue("LAST TRANSACTIONS SHAPE : ({nrow(last_trns)}, {ncol(last_trns)})"), verbose)
  ## summarise all transactions
  summ_trns <- createAggSummaries(trns_df,
                                  agg_cols = agg_cols,
                                  id_columns = c(join_cols, 'START_DATE'),
                                  date_column = 'TXN_DT',
                                  wide = TRUE,
                                  verbose = verbose) %>%
    ungroup()
  printInfo(glue("SUMMARY TRANSACTIONS SHAPE : ({nrow(summ_trns)}, {ncol(summ_trns)})"), verbose)
  return(list(summ_trns, last_trns, trns_card_codemap))
}

saveTable <- function(df, df_name, out_dir, verbose) {
  # Write table into the output directory
  #' @df : table to save
  #' @df_name : save file name
  #' @out_dir : output directory
  #' @verbose : whether to print information
  out_name <- glue("{out_dir}/{df_name}_{today()}.csv")
  printInfo(glue("SAVING TO {out_name}"), verbose)
  write.table(df, out_name, sep = '>', row.names = FALSE, quote = FALSE)
}

saveCodeMap <- function(codemap, codemap_name, out_dir, verbose) {
  # Write codemap into the output directory
  #' @codemap : codemap to save
  #' @codemap_name : save file name
  #' @out_dir : output directory
  #' @verbose : whether to print information
  out_name <- glue("{out_dir}/{codemap_name}_{today()}.rds")
  printInfo(glue("SAVING TO {out_name}"), verbose)
  saveRDS(codemap, out_name)
}

left_join_NA <- function(df1, df2, by, value) {
  # Left join with filling in NAs in columns from df2
  #' @df1 : orig data frame
  #' @df2 : joining data frame
  #' @by : a vector of columns, or a string
  #' @value : with which value to fill in NAs
  mutate_cols <- setdiff(colnames(df2), colnames(df1))
  left_join(x = df1, y = df2, by = by) %>% 
    mutate_at(mutate_cols, funs(replace(., which(is.na(.)), value)))
}