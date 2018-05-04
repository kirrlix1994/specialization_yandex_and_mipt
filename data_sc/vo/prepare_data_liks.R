prepareData <- function(campaigns_file, 
                        balance_file, bal_window,
                        binbonus_file, 
                        limit_file, 
                        sd2016_file, 
                        sd2017_file, 
                        sd_cols = c("FL_GOLD",
                                    "FL_ZP",
                                    "FL_P24",
                                    "QTY_CHLD",
                                    "CL_AUTO_EX",
                                    "CL_EDU",
                                    "REP_RLCL_MAXCRIT",
                                    "CL_INDUSTR",
                                    "CONTRACT_REF",
                                    "REP_CLID"),
                        scenario_file,
                        transactions_file, mcc_file, trsn_agg_window, trsn_n_recent,
                        verbose = TRUE) {
  #@ campaigns_file : file with campaigns data
  #@ balance_file : file with balance data
  #@ bal_window : size of balance window (in months) preceding the scenario start date
  #@ binbonus_file : file with loyalty data
  #@ limit_file : file with limits data
  #@ sd2016_file : file with soc. dem for the year of 2016
  #@ sd2017_file : file with soc. dem for the year of 2017
  #@ sd_cols : column names to choose from soc.dem (default = c("FL_GOLD",
  # "FL_ZP",
  # "FL_P24",
  # "QTY_CHLD",
  # "CL_AUTO_EX",
  # "CL_EDU",
  # "REP_RLCL_MAXCRIT",
  # "CL_INDUSTR",
  # "CONTRACT_REF",
  # "REP_CLID"))
  #@ scenario_file : file with scenarios descriptions
  #@ transactions_file : file with transactions
  #@ mcc_file : file with mcc codes
  #@ trsn_agg_window : aggregate transactions of the same type within this window (in days)
  #@ trsn_n_recent : choose at most this number of recent transactions
  #@ verbose : whether to print out message (default = TRUE)
  
  ## prepare data:
  
  ### SOC. DEM
  ### BALANCE
  ### LOYALTY
  ### LIMIT/BANK
  ### SCENARIOS
  ### TRANSACTIONS
  
  library(readr)
  library(dplyr)
  library(tidyr)
  library(eeptools)
  library(lubridate)
  library(magrittr)
  library(stringr)
  library(data.table)
  
  ## reading in the campaigns file ----
  if (verbose)
    print("Preparing campaigns data....")
  setwd("C:/Users/Liksakov/Desktop/VOVA/model_data_Vova")
  campaigns_file  <- "clients_full_model_BD.csv"
  campaigns <- read.csv(campaigns_file, sep = ';', dec = ',', 
                        stringsAsFactors = FALSE)
  
  campaigns %<>% select(CONTRACT_REF, Birth_date) %>%
    # check bd has digits at least 1
    mutate(VALID_BD = str_detect(Birth_date, '[0-9]')) %>%
    # if wrong bd - delete
    filter(VALID_BD) %>%
    # age = 5!
    mutate(AGE = floor(age_calc(dmy(Birth_date) %>% as.Date, units='years'))) %>%
    select(-VALID_BD, -Birth_date) %>%
    # only contract_ref and age
    right_join(campaigns %>% select(-Birth_date), by='CONTRACT_REF')
  #all initial rows, right join 
  # all not valid bd are still in campaings!!
  # ?? 
  nrow( campaigns ) #  91695
  sum( campaigns$VALID_BD)
 # nrow( campaigns)
  
 #s <-  str_detect( campaigns$Birth_date, '[0-9]')
# str_detect( "..sd", '[0-9]')
  
  ## works for campaigns file with 14 columns
  assertthat::are_equal(ncol(campaigns), 14)
 
   
  ## clearing up
  cols <- gsub("(.*)_[1-2]",
               '\\1',
               colnames(campaigns)) %>% unique
  # why do not work ? 
  cols <- gsub("(.*)_[1-2]",
               " ",
               colnames(campaigns)) %>% unique
  # choose all with _1 , _2
 # str_extract_all(    colnames(campaigns), "(.*)_[1-2]")
 # s <- ""
 # gsub("(.*)_[1-2]",'\\1',s)
  
  
  part1 <- campaigns %>% select(1, 2, 3:7, 13, 14)
  part2 <- campaigns %>% select(1, 2, 8:12, 13, 14)
  colnames(part1) <- cols
  colnames(part2) <- cols
  
  campaigns <- rbind(part1, part2) %>%
    filter(., CONTROL_GROUP_FLG != '') %>% #  183390 TO 106835
    mutate(label=1 * (PAYMENT != ''),
           DATE_START=dmy(DATE_START),
           DATE_END=dmy(DATE_END))

  #tmp <- rbind(part1, part2)
  #unique(tmp$CONTROL_GROUP_FLG )
  nrow(campaigns) # 106835
  
  
  ## check that the dates have been converted correctly
  all(campaigns$DATE_END > campaigns$DATE_START) == TRUE
  
  ## rm unnecessary data
  rm(part1, part2, cols)
  
  ## reading in balances ----
  if (verbose)
    print("Preparing balances data....")
  balance_file <- "balance_full_model.csv"
  blncs <- read.csv(balance_file, sep=';', dec=',', stringsAsFactors=FALSE)
  colnames(blncs) <- c("CONTRACT_REF", "DATE", "AMOUNT")
  
  ## fix amount
  blncs %<>% mutate(AMOUNT=as.numeric(gsub(' ', '', AMOUNT)),
                    DATE=dmy(DATE)) %>% 
    # contract_ref in compaings contracts set
    filter(CONTRACT_REF %in% unique(campaigns$CONTRACT_REF)) %>%
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(DATE_START >= DATE) %>%
    mutate(BAL = round(interval(DATE, DATE_START) / months(1))) %>%
    filter(BAL < bal_window) %>% 
    select(CONTRACT_REF, DATE_START, AMOUNT, BAL) %>%
    gather(., variable, value, BAL) %>% 
    unite(var, variable, value) %>% 
    spread(var, AMOUNT)
  ## fix amount
  bal_window = 12
  tmp <- blncs %>% mutate(AMOUNT=as.numeric(gsub(' ', '', AMOUNT)),
                    DATE=dmy(DATE)) %>% 
    # contract_ref in compaings contracts set
    filter(CONTRACT_REF %in% unique(campaigns$CONTRACT_REF)) %>%
    # may be more than one raw? 
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(DATE_START >= DATE) %>%
    mutate(BAL = round(interval(DATE, DATE_START) / months(1))) %>%
    # BAL = ?? month number 
    filter(BAL < bal_window) %>% 
    select(CONTRACT_REF, DATE_START, AMOUNT, BAL) %>%
    # amount = - 752105!! ; + 2932838
    # simply add column variable = BAl and value = BAL 
    gather(., variable, value, BAL) %>% 
   # join var and variable and value together and name it as var 
    unite(var, variable, value) %>% 
    # make columns with amout by each var 
    spread(var, AMOUNT)
  
  length( unique( blncs$CONTRACT_REF))
  nrow( blncs)
  # there are duplicates
  h <- blncs %>% group_by( CONTRACT_REF) %>% summarise(n = n())
  # max n = 2 
  blncs[ which(blncs$CONTRACT_REF == "271-P-00010220"),]
  # different date_start
  # different balances WHY??? 
  # one contract, different balances !
  
  
  ## join on campaigns
  campaigns %<>% left_join(blncs, by=c('CONTRACT_REF', 'DATE_START'))
  # error : `by` can't contain join column `DATE_START` which is missing from LHS
  tmp <- campaigns %>% left_join(blncs, by=c('CONTRACT_REF', 'DATE_START'))
  nrow( tmp); nrow(campaigns)
  # 106835 tmp  (new campaings ) because of left join instaed of 91695
  # is it ok ?? 
  ## remove unnecessary
  rm(blncs)
  
  ## binbonus loyalty data ----
  if (verbose)
    print("Preparing binbonus data....")
  binbonus_file <- 
  binbonus <- read.csv(b,
                       sep=';', dec=',', stringsAsFactors = FALSE)
  
  ## convert date
  binbonus %<>% mutate(START_LOCAL_DATE=dmy(START_LOCAL_DATE)) %>%
    rename(CONTRACT_REF = AR_NO) %>%
    select(-CL_ID, -BINBONUS_CATEGORY)
  
  binbonus %<>% 
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(DATE_START >= START_LOCAL_DATE) %>%
    select(CONTRACT_REF, DATE_START, CODE) %>%
    mutate(VALUE = 1) %>%
    spread(CODE, VALUE, fill = 0)
  
  ## join on campaigns
  campaigns %<>% left_join(binbonus, by=c('CONTRACT_REF', 'DATE_START'))
  
  ## remove unnecessary
  rm(binbonus)
  
  ## limit data ----
  if (verbose)
    print("Preparing limits data....")
  limits <- read.csv(limit_file,
                     sep=';', dec=',', stringsAsFactors=FALSE)
  limits %<>% mutate(LIM=as.numeric(gsub(",", "", LIM)),
                     DT=ymd_hms(DT)) %>%
    filter(CONTRACT_REF %in% unique(campaigns$CONTRACT_REF)) %>% 
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(DATE_START >= DT) %>%
    group_by(CONTRACT_REF, DATE_START) %>%
    summarise(LIM = LIM[which.max(DT)], BANK = BANK[which.max(DT)])
  
  ## join on campaigns
  campaigns %<>% left_join(limits, by=c('CONTRACT_REF', 'DATE_START'))
  
  ## remove unnecessary
  rm(limits)
  
  ## soc dem data ----
  if (verbose)
    print("Preparing soc.dem. data....")
  sd_iq_2016 <- read.csv(sd2016_file,
                         sep=';', dec=',', stringsAsFactors=FALSE)
  sd_iq_2016 %<>% rename(REP_CLID=rep_clid)
  
  not_found <- campaigns %>% 
    select(REP_CLID, CONTRACT_REF) %>%
    mutate(is_in_sd_iq_2016 = REP_CLID %in% unique(sd_iq_2016$REP_CLID))
  
  sd_iq_2017 <- read.csv(sd2017_file,
                         sep=';', dec=',', stringsAsFactors=FALSE)
  sd_iq_2017 %<>% rename(., 
                         CONTRACT_REF=Contract_ref, 
                         CLIENT_ID=client_id, 
                         REP_CLID=rep_clid)

  sd_iq_2016 %<>% select(one_of(sd_cols))
  sd_iq_2017 %<>% select(one_of(sd_cols))
  
  not_found %<>%
    mutate(is_in_sd_iq_2017_rep = REP_CLID %in% unique(sd_iq_2017$REP_CLID),
           is_in_sd_iq_2017_cref = CONTRACT_REF %in% unique(sd_iq_2017$CONTRACT_REF)) %>%
    distinct()
  
  not_found_cref <- not_found %>%
    filter(is_in_sd_iq_2016 == FALSE,
           is_in_sd_iq_2017_cref == FALSE,
           is_in_sd_iq_2017_rep == FALSE) %>%
    .$CONTRACT_REF
  
  
  ## take 2017 over 2016
  sd_iq_2016_ids <- not_found %>% filter(is_in_sd_iq_2016, !is_in_sd_iq_2017_cref) %>% .$REP_CLID
  # not_found %>% filter(REP_CLID == '1004586078')
  # 2 contract_ref <-> 1 rep_clid
  sd_iq_2017_ids <- not_found %>% filter(CONTRACT_REF != '271-P-24630243', is_in_sd_iq_2017_cref) %>% .$CONTRACT_REF
  
  campaigns_2016 <- campaigns %>% filter(REP_CLID %in% sd_iq_2016_ids)
  campaigns_2017 <- campaigns %>% filter(CONTRACT_REF %in% sd_iq_2017_ids)
  campaigns_no_sd <- campaigns %>% filter(CONTRACT_REF %in% not_found_cref)
  
  ## assertion
  dim(campaigns_2016)[1] + dim(campaigns_2017)[1] + dim(campaigns_no_sd)[1] == dim(campaigns)[1]
  
  campaigns_2016 %<>% left_join(., sd_iq_2016, by='REP_CLID')
  campaigns_2017 %<>% left_join(., sd_iq_2017, by=c('REP_CLID', 'CONTRACT_REF'))
  
  campaigns_2016$HAS_SD <- 1
  campaigns_2017$HAS_SD <- 1
  campaigns_no_sd$HAS_SD <- 0
  
  campaigns_old <- campaigns
  campaigns <- plyr::rbind.fill(campaigns_2016, campaigns_2017, campaigns_no_sd)
  assertthat::are_equal(ncol(campaigns_old), ncol(campaigns))

  ## remove unnecessary  
  rm(campaigns_old, campaigns_no_sd, campaigns_2017, campaigns_2016,
     sd_iq_2017_ids, sd_iq_2016_ids, not_found_cref, not_found,
     sd_iq_2017, sd_iq_2016)

  ## scenarios ----
  if (verbose)
    print("Preparing scenarios data....")
  scenarios <- read.csv(scenario_file, sep=';',
                        stringsAsFactors=FALSE)
  campaigns %<>% left_join(scenarios, by='SCENARIO_NAME')
  
  ## remove unnecessary
  rm(scenarios)
  
  ## transactions ----
  if (verbose)
    print("Preparing transactions data....")
  transactions <- fread(transactions_file,
                        sep='>')
  
  ## For the RNN model
  ## we will take only the following columns:
  ## AMOUNT
  ## TP
  ## DAY OF TRANSACTION (or equivalently, the number of days before the campaign start)
  ## MCC
  transactions %<>% select(CONTRACT_REF, TXN_RUB_AMT,
                           TXN_TP, TXN_DT,
                           TXN_MCC) %>%
    arrange(CONTRACT_REF, TXN_DT)
  
  transactions %<>% filter(TXN_RUB_AMT != '')
  transactions$TXN_RUB_AMT %<>% {gsub(',', '', .)} %>% as.numeric
  transactions %<>% mutate(TXN_DT=ymd_hms(TXN_DT))
  
  campaigns %<>% arrange(CONTRACT_REF, DATE_START)
  
  ## filter transactions after the campaign started
  transactions %<>%
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(TXN_DT <= DATE_START) %>% 
    select(-DATE_START)
  
  ## mcc data
  mcc_df <- read.csv(mcc_file, sep=';')
  mcc_df %<>% rename(TXN_MCC=MCC.код,
                     CATEGORY=Категория)
  mcc_df$CODE <- as.numeric(mcc_df$CATEGORY)
  
  transactions %<>% left_join(mcc_df, by='TXN_MCC') %>%
    select(-TXN_MCC, -CATEGORY)
  
  rm(mcc_df)
  
  ### if transactions of the same type are close to each other
  ### within some window
  ### we will sum up the prices
  ### and replace the transactions with the most recent one out of the bunch
  
  ### helper functions
  dayDiff <- function(d1, d2) {
    # @ difference in days
    # between two days
    return (as.numeric(difftime(d1, d2, units='secs')) / 60 / 60 / 24);
  }
  transIndex <- function(v, h) {
    # @ apply transIndex
    # which iterates over
    # values of v (dates) 
    # and return index
    # if current value is within h
    # from the anchor
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
  
  transactions %<>%
    group_by(CONTRACT_REF, TXN_TP, CODE) %>%
    mutate(TXN_GROUP = transIndex(TXN_DT, trsn_agg_window)) %>%
    group_by(CONTRACT_REF, TXN_TP, CODE, TXN_GROUP) %>%
    summarise(TXN_RUB_AMT = sum(TXN_RUB_AMT), TXN_DT = min(TXN_DT)) %>%
    left_join(campaigns %>% select(CONTRACT_REF, DATE_START), by='CONTRACT_REF') %>%
    filter(TXN_DT <= DATE_START) %>%
    group_by(CONTRACT_REF, DATE_START) %>%
    arrange(CONTRACT_REF, TXN_DT) %>%
    mutate(n_trns = row_number(), n_days = dayDiff(DATE_START, TXN_DT)) %>%
    top_n(trsn_n_recent, n_trns)
  
  if (verbose)
    print("Saving files...")
  write.table(transactions %>% select(-TXN_DT, -TXN_GROUP), 
              paste0('transactions_', today(), '_agg_', trsn_agg_window, '_recent_', trsn_n_recent, '.csv'),
              sep=';', row.names=FALSE, quote=FALSE)
  write.table(campaigns, 
              paste0('campaigns_', today(), '_bal_', bal_window, '.csv'),
              sep=';', row.names=FALSE, quote=FALSE)
  
  rm(transactions, campaigns)
}

prepareData(campaigns_file = '../../data/model/clients_full_model_BD.csv', 
            balance_file = '../../data/model/balance_full_model.csv', bal_window = 12,
            binbonus_file = '../../data/model/binbonus_full_model.csv',
            limit_file = '../../data/model/2017-04-17_limits.csv',
            sd2016_file = '../../data/model/SOC_DEM_IQ2016.csv',
            sd2017_file = '../../data/model/2017-04-06_SOC_DEMO_IQ.csv',
            sd_cols = c("FL_GOLD",
                        "FL_ZP",
                        "FL_P24",
                        "QTY_CHLD",
                        "CL_AUTO_EX",
                        "CL_EDU",
                        "REP_RLCL_MAXCRIT",
                        "CL_INDUSTR",
                        "CONTRACT_REF",
                        "REP_CLID"),
            scenario_file = '../../data/model/scenarios_parsed.csv',
            transactions_file = '../../data/model/transactions_full_model.csv', 
            mcc_file = '../../data/model/mcc.csv', 
            trsn_agg_window = 45, trsn_n_recent = 20,
            verbose = TRUE)