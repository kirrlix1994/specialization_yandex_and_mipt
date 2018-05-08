prepareData <- function(cd_file, 
                        bank,
                        balance_file,
                        binbonus_file,
                        limit_file,
                        sd_file,
                        NA_percent,
                        one_col_max_percent,
                        status_file,
                        st_top_n_cols,
                        code_maps_card_rds, code_maps_contr_rds,
                        addr_file,
                        addr_top_n_cols,
                        addr_code_maps_rds,
                        phone_file,
                        transactions_file, mcc_file, 
                        trns_agg_window, trns_n_recent,
                        trns_top_n_cols, trns_agg_cols,
                        trns_code_maps_rds,
                        # sd2016_file,
                        # sd2017_file,
                        # sd_cols = c("FL_GOLD",
                        #             "FL_ZP",
                        #             "FL_P24",
                        #             "QTY_CHLD",
                        #             "CL_AUTO_EX",
                        #             "CL_EDU",
                        #             "REP_RLCL_MAXCRIT",
                        #             "CL_INDUSTR",
                        #             "CONTRACT_REF",
                        #             "REP_CLID"),
                        # scenario_file,
                        output_dir,
                        verbose = TRUE) {
  #' @cd_file : file with CD data
  #' @bank: a string, either 'BBKK' or 'BinBank'
  #' @balance_file : file with balance data (with changes)
  #' @binbonus_file : file with loyalty data
  #' @limit_file : file with limits data
  #' @sd_file : file with soc. dem
  #' @NA_percent : drop columns with proportion of NAs (or empty) more than NA_percent
  #' @one_col_max_percent : drop columns where one value takes more than one_col_max_percent
  #' @status_file : file with statuses data
  #' @st_top_n_cols : top_n_cols for status data.
  #                   A list of named tuples of the form `c(name = COL_NAME,
  #                                                     percentage = PERCENT)`
  #                 where `COL_NAME` is the name of the column to be considered,
  #                 `PERCENT` is the percentage of values needed to be explained by
  #                 top-k unique values. Corresponding top-k unique values will be
  #                 kept, others will be combined into a single column.
  #                 E.g., if a column contains a hundred of values, while
  #                 there are only 5 unique values with one class being presented
  #                 90 times, then if passing this column name with `PERCENT=0.9`
  #                 the output column would contain only two classes: `0` and `1`,
  #                 where `1` would be the class with highest presence, and
  #                 `0` would be all other classes.
  #' @code_maps_card_rds : a path to the rds file with code maps for card
  #' @code_maps_contr_rds : a path to the rds file with code maps for contr
  #' @addr_file : file with address data
  #' @addr_top_n_cols : top_n_cols for address data.
  #' @addr_code_maps_rds : a path to the rds file with code maps for address
  #' @phone_file : file with phones data
  #' @transactions_file : file with transactions
  #' @mcc_file : file with mcc codes
  #' @trns_agg_window : aggregate transactions of the same type within this window (in days)
  #' @trns_n_recent : choose at most this number of recent transactions
  #' @trns_top_n_cols : top_n_cols for transactions data.
  #' @trns_agg_cols : a list of the following form: `list(COL_TO_AGGREGATE,
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
  #' @trns_code_maps_rds : a path to the rds file with code maps for transactions
  #' @output_dir : where to dump all data
  #' @verbose : whether to print out message (default = TRUE)
  
  ## creating output directory ----
  dir.create(output_dir)
  
  ## reading in the cd file ----
  if (!(bank %in% c('BBKK', 'BinBank')))
    stop("bank MUST BE EITHER BBKK or BinBank")
  cd_df <- readCD(cd_file, bank, verbose)
  saveTable(cd_df, 'CD', output_dir, verbose)
  
  ## identifiers data ----
  cd_id_df <- cd_df %>% 
    select('CLIENT_ID', 'CONTRACT_REF', 'START_DATE') %>%
    distinct()
  saveTable(cd_id_df, 'CD_ID', output_dir, verbose)
  ## reading in the balance data ----
  bal_df <- readBAL(cd_id_df,
                    balance_file,
                    verbose)
  saveTable(bal_df, 'BAL', output_dir, verbose)
  # ## reading in the bin data ----
  bin_df <- readBIN(cd_id_df,
                    binbonus_file,
                    verbose)
  saveTable(bin_df, 'BIN', output_dir, verbose)
  ## reading in the limit data ----
  lim_df <- readLIM(cd_id_df,
                    limit_file,
                    verbose)
  saveTable(lim_df, 'LIM', output_dir, verbose)
  ## reading in the sd data ----
  if (bank != 'BinBank') {
    sd_df <- readSD(cd_id_df,
                    sd_file,
                    NA_percent,
                    one_col_max_percent,
                    verbose)
    saveTable(sd_df, 'SD', output_dir, verbose)
  }
  ## reading in the status data ----
  st_out <- readStatus(cd_id_df,
                       status_file,
                       st_top_n_cols,
                       code_maps_card_rds,
                       code_maps_contr_rds,
                       verbose)
  st_df <- st_out[[1]]
  st_card_codemap <- st_out[[2]]
  st_contr_codemap <- st_out[[3]]
  saveTable(st_df, 'ST', output_dir, verbose)
  saveCodeMap(st_card_codemap, 'ST_CARD', output_dir, verbose)
  saveCodeMap(st_contr_codemap, 'ST_CONTR', output_dir, verbose)
  rm(st_out)
  ## reading in the address data ----
  if (bank != 'BinBank') {
    addr_out <- readAddress(cd_id_df,
                            addr_file,
                            addr_top_n_cols,
                            addr_code_maps_rds,
                            verbose)
    addr_df <- addr_out[[1]]
    addr_codemap <- addr_out[[2]]
    saveTable(addr_df, 'ADDR', output_dir, verbose)
    saveCodeMap(addr_codemap, 'ADDR', output_dir, verbose)
    rm(addr_out)
  }
  ## reading in the phones data ----
  ph_df <- readPhone(cd_id_df,
                     phone_file,
                     verbose)
  saveTable(ph_df, 'PH', output_dir, verbose)
  ## reading in the transactions data ----
  trns_out <- readTrns(cd_id_df,
                       transactions_file,
                       mcc_file,
                       trns_agg_window,
                       trns_n_recent,
                       trns_top_n_cols,
                       trns_agg_cols,
                       trns_code_maps_rds,
                       verbose)
  trns_summ <- trns_out[[1]]
  trns_last <- trns_out[[2]]
  trns_codemap <- trns_out[[3]]
  saveTable(trns_summ, 'TRNS_SUMM', output_dir, verbose)
  saveTable(trns_last, 'TRNS_LAST', output_dir, verbose)
  saveCodeMap(trns_codemap, 'TRNS', output_dir, verbose)
  rm(trns_out)
  ## joining all tables with summaries into one ----
  ## it is important to realise that some of NAs appearing after joining
  ## are not actually NAs, but other fixed values (most probably, zeroes):
  ## e.g., if some client did not have any BAL changes in last 7 days,
  ## it should be 0, not NA in the corresponding column;
  
  join_cols <- c('CONTRACT_REF', 'START_DATE')
  join_cols2 <- c('CONTRACT_REF', 'CLIENT_ID', 'START_DATE') # static
  join_cols3 <- c('CONTRACT_REF', 'CLIENT_ID') # static
  join_cols4 <- c('CONTRACT_REF') # static2
  cd_df %<>%
    left_join_NA(bal_df, by = join_cols2, value = 0) %>%
    left_join_NA(bin_df, by = join_cols2, value = 0) %>%
    left_join_NA(lim_df, by = join_cols, value = 0) %>%
    left_join(st_df, by = join_cols2, value = 0) %>%
    left_join(ph_df, by = join_cols3) %>%
    left_join_NA(trns_summ, by = join_cols, value = 0)
  if (bank != 'BinBank') {
    cd_df %<>%
      left_join(sd_df, by = join_cols4) %>%
      left_join(addr_df, by = join_cols4)
  }
  drop_cols <- dropCols(cd_df %>% select(-label, -BANK), ## do not drop label, BANK accidentally
                        NA_percent = NA_percent,
                        one_col_max_percent = one_col_max_percent,
                        verbose = verbose)
  printInfo(glue("DROPPING COLUMNS : ({paste0(drop_cols, collapse = ',')})"), verbose)
  cd_df %<>% select(-one_of(drop_cols))
  printInfo(glue("FINAL DATA SHAPE : ({nrow(cd_df)}, {ncol(cd_df)})"), verbose)
  saveTable(cd_df, 'CD_ALL', output_dir, verbose)
}