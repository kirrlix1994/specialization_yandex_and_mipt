library(ggplot2)
library(plotly)
library(magrittr)
library(dplyr)
#library(reshape)
library(eeptools)
library(tidyr)
library(corrplot)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)
#library(reshape2) 
library(dummies)



#setwd("C:/Users/Liksakov/Desktop/Liks/insurance/scripts")

## creating agg summaries; top_N_cols
source("helpers.R") 
#source("parse_transactions.R")

###### trans----

#trans_file <- "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017_06_23_transaction.csv"
#trans <-fread(file=trans_file, sep = "," )
#mcc_table <- read.csv(file = "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/mcc_v2.csv" , sep = ";", dec = ",",header = T)
#colnames(mcc_table) <- c("TXN_MCC", "CATEGORY")

 parse_TRANSACTION_MAIN_new <- function(trans_file,
                                        mcc_table_file,
                                        top_n_cols = list(),
                                        agg_cols = list(),
                                        min_trans_amount = 10, 
                                        codemap_file = "TRANS_codemap")
 {
   
  trans <- fread(file = trans_file, sep = "," ) 
  mcc_table <- read.csv(file = mcc_table_file , 
                        sep = ";", dec = ",",header = T)
  
  colnames(mcc_table) <- c("TXN_MCC", "CATEGORY")
  #cat(1,"\n")
  trans %<>% mutate( CLIENT_ID = as.numeric(CLIENT_ID ), 
                   CONTRACT_NUMBER = DOG ,
                   TXN_RUB_AMT =  as.numeric( gsub(",",".",TXN_RUB_AMT)),
                   TXN_DIR = as.numeric(TXN_DIR),
                   TXN_DT = as.Date(TXN_DT, format = "%d.%m.%Y"), 
                   TXN_TP = as.factor(TXN_TP),
                   TXN_CTR  = as.factor( TXN_CTR),
                   TXN_CURR = as.factor( TXN_CURR ), # !!!
                   RUB_TR = as.numeric( TXN_CURR == 810),#!!!
                   TXN_MCC = as.numeric( TXN_MCC),
                   FLG_CATEGORY = as.numeric(FLG_CATEGORY)) %>% 
  select( -TXN_OW4_TP, -DOG)
  #dplyr::rename(  TXN_CURR  = TXT_CURR )
  
  #cat("test, \n")
  #select( -TXN_OW4_TP, -DOG, -DOC__OID) 
  #cat(2,"\n")
    #  have to join on EVAL 
    n1 = nrow( trans ) 
    trans %<>%  filter( TXN_RUB_AMT > min_trans_amount) 
    n2 = nrow( trans ) 
    #cat(3,"\n")
    trans %<>% left_join( mcc_table, by = "TXN_MCC" )
    #cat(4,"\n")
      trans %<>% parseTransactions2(
                                   end_date = today(),
                                   top_n_cols = top_n_cols, 
                                   agg_cols =  agg_cols, 
                                   wide = TRUE, 
                                   codemap_file = "TRANS_codemap", 
                                   data_type = "new" )
    return(  trans )
}
    
    # top_n_cols = list(
    #   c('name' = 'TXN_TP', 'percentage' = 0.02), 
    #   c('name' = 'TXN_CHN', 'percentage' = 0.02),
    #   c('name' = 'TXN_CTR', 'percentage' = 0.02),
    #   #c('name' = 'TXN_CT', 'percentage' = 0.05),
    #   c('name' = 'TXN_MCC', 'percentage' = 0.03), 
    #   c('name' = 'CATEGORY', 'percentage' = 0.03))
    # 
    # 
    # # example : pass aggr cols  
    # # tmp1[[3]] <- list(list("TXN_TP", "TXN_CHN"), "TXN_MCC")    
    # aggr_cols <- list('TXN_RUB_AMT',
    #                   c(7, 31, 90, 180), 
    #                   list('TXN_DIR','TXN_TP', 'TXN_CHN','TXN_CTR',
    #                        'TXN_MCC','RUB_TR', 'CATEGORY', 'FLG_CATEGORY'),
    #                   funs(MEAN = mean, MAX = max, N = length, SUM = sum),
    #                   id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'))
             
    # trans_feat <- parseTransactions(   df = trans, 
    #                                    end_date = today(),
    #                                    top_n_cols = top_n_cols, 
    #                                    agg_cols =  aggr_cols, 
    #                                    wide = T)
    
###### parse soc_dem function ----  
 
 
 # 
 use_codemap <- function( col_name, map_table, df)
 {
   if( col_name %in% colnames(df) )
   {
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
 
 
 
# function to parse soc_dem from human factor data

#source("helpers_liks.R") 
# top N function
              
parse_socdem_hf_new <- function(socdem_hf_file,
                            start_date = ymd("1970-01-01"),
                            end_date = today(),
                            #top_n_cols = list(), 
                            max_class_percent = 0.9, 
                            max_na_percent = 0.67, 
                            omit_cols = TRUE,
                            code_map_file =  "SD_HF_codemap")
                              {
  ## Args:
  ## socdem_hf_file: path to the file with socdem_hf_file from human factor
  ## start_date : consider only transactions that happened at or after start_date
  ## end_date : consider only transactions that happened at or before end_date
  ## top_n_cols unique values. Corresponding top-n unique values will be
  ##               kept, others will be combined into a single column (0).
  ##               rename values to 1 : n by their frequency
  ##  max_class_percent: column features, which has more than max_class_percent of one class
  ##               will be removed automatically
  ##               usually na, empty space, unknown
  ##  max_na_percent: olumn features, which has more than max_na_percent 
  ##               will be removed automatically
  ## omit_cols: whether to omit bad columns, if TRUE , cols are omitted 
  ## code_map_file: file with mapper for top N cols(which was calcilated on train data)
  ##
  ## Returns:
  ## df : modified data frame
  
  # 1. read file 
  df <- read.csv(file = socdem_hf_file, header = TRUE, sep = ",", dec = ".",
                 na.strings = c("NA","NaN", ""), stringsAsFactors = TRUE)
  cat(" Initial number of rows = " , nrow(df));  cat("\n")

  
  # 2. change fields
  if ( 'DATA' %in% colnames(df) )
  {
    df %<>% mutate( DATA = as.Date( DATA,"%d.%m.%Y" ), CHANNEL = as.factor(CHANNEL))
    df %<>% dplyr:: rename(  DATE = DATA) 
  }

  
  # drop unvalid client_id:
  if ( class(df$CLIENT_ID) == 'factor' )
     { df %<>% mutate( CLIENT_ID = as.numeric(levels(CLIENT_ID))[CLIENT_ID] ) }
  if (  max(as.numeric(is.na(df$CLIENT_ID)) ) > 0 )
   {   df <- df[ -which( is.na(df$CLIENT_ID)),] }
  df %<>% mutate( BIRTHDATE = as.Date( BIRTHDATE,"%d.%m.%Y" ))
  df %<>% mutate( AGE =  2017 - year(BIRTHDATE) )
  # remove commas from HID_PARTY
  if ( 'HID_PARTY' %in% colnames(df) )
   { df %<>% mutate( HID_PARTY =  as.numeric(( gsub(",","",HID_PARTY)))) }
  #remove letters from IQ_ID
  if ( 'IQ_ID' %in% colnames(df) )
   { df %<>% mutate( IQ_ID = as.numeric( str_replace_all(IQ_ID, "[A-Za-z  ]" , "" )) ) }
  # remove duplicated colums( if they exist)
  df<-  df[!duplicated(df ), ]
  cat(" Number of raws after removing  duplicates = " , nrow(df));  cat("\n")
  # remove colums with NA  percent ( if they exist)
  #na_perc_tmp <- sapply( 1:ncol(df), function(x,y) round( sum( is.na( y[,x])/ nrow(y)),3 ), y = df)
  #summary(df)
  
  cat(" Initial number of columns = " , ncol(df));  cat("\n")
  
  # 3. drop outlyers
  # change  too young and too all to na 
  df$AGE[which( df$AGE < 15)] <- NA
  df$AGE[which( df$AGE > 95)] <- NA
  
  
  #--- USE TOP N COLS:
  
  print(glue::glue('{str_dup("***", 5)}USING TOP-N FEATURES{str_dup("***", 5)}'))
  cat("\n\n")
  
  #load codemap
  code_map <- readRDS(file =  code_map_file)

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
  
 # df_new <- apply(df[, which(colnames(df) %in% names(code_map))], 
 #                2, function(x, y) use_codemap(x,y), y =  ]
  
 # df <- createTopNCols(df, top_n_cols = top_n_cols)
 
  
  #5. remove columns with too high percent( > max_class_percent) of one class 
  ##   max_class_freq returns max class percent 
  max_class_freq <- function( col, df ) {
    prop_tmp <- table(df[, col]) %>% 
    prop.table %>%
    sort(decreasing = TRUE)
    return( round(prop_tmp[1],3) )
  }
  cat("ncol  df:", ncol(df_new), "\n")
  # most frequent class and its perc by all columns 
  freq_class_perc_tmp <- sapply( 1:ncol(df_new), function(x,y) max_class_freq(x,y), y = df_new)
  df_tmp <- df_new[ ,which(as.numeric(freq_class_perc_tmp) <  max_class_percent) ]
  colums_to_remove_1 <- colnames( df_new[,which(as.numeric(freq_class_perc_tmp) >=  max_class_percent)] )
  cat(" Column number after removing features with too large class: ", ncol(df_tmp)); cat( "\n")
  cat(" Names of removed columns:\n")
  for( i in (1: length( colums_to_remove_1)))
  {
    cat(colums_to_remove_1[i] , "\n")
  } 
  cat("ncol  df_tmp:", ncol(df_tmp), "\n")
 
   
  
  # na perc by features
  na_perc_tmp <- sapply( 1:ncol( df_tmp), function(x,y) round( sum( is.na(y[,x]))/nrow(  df_tmp), 3), y =  df_tmp)
  colums_to_remove_2 <- colnames(   df_tmp[,which(na_perc_tmp >=  max_na_percent)])
  df_tmp <-  df_tmp[ ,which(na_perc_tmp <  max_na_percent)]
  cat(" Column number after removing features with too large NA percent: ", ncol( df_tmp), "\n")
  cat(" Names of removed columns:\n")
  for( j in (1: length( colums_to_remove_2)))
  {
    cat( colums_to_remove_2[j] , "\n")
  } 
  cat( "\n")
  cat("ncol  df_tmp:", ncol(df_tmp), "\n")
  
  
  #7. transform to factors: 
  # all top N cols
  #for (c_ in  cols_to_transform) {
  #  df_tmp[,c_] %<>% as.factor()
  #} 
  #df_tmp$GENDER  %<>% as.factor()
  #df_tmp$OWN_CAR_FLAG  %<>%  as.factor()
  
      
  #8. choose columns
  if(omit_cols)
  {
    cat("Remove also columns:\n")
    cat("WAY4_ID", "BIRTH_PLACE" ,"BIRTHDATE", "ORGANIZATION_TYPE", "\n\n") 
    # need check that such columns exist!
    omit_cols_tmp <- c( "WAY4_ID", "BIRTH_PLACE", "BIRTHDATE", "ORGANIZATION_TYPE")
    omit_cols_tmp <- omit_cols_tmp[ which(omit_cols_tmp %in% colnames(df))]
    df_tmp %<>% select(-one_of(omit_cols_tmp))
    cat("Final column number =", ncol(df_tmp), "\n" )
  }
  
  #9. check that diffrent soc dem info for one client_id 
  
  rm(df, df_new)
  return(df_tmp) 
}

# test 
# socdem_hf_file <-
#   "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/insurance_hf.csv"
# top_n_cols = list(c('name' = 'MARITAL_STATUS', 'percentage' = 0.05), 
#                   c('name' = 'SOURCE_CHANNEL', 'percentage' = 0.05),
#                   c('name' = 'BRANCH', 'percentage' = 0.05),
#                   c('name' = 'EDUCATION_LEVEL', 'percentage' = 0.05),
#                   c('name' = 'OCCUPATION', 'percentage' = 0.05), 
#                    c('name' = 'APPOINTMENT', 'percentage' = 0.05))
# 
# h <- parse_socdem_hf(socdem_hf_file, top_n_cols = top_n_cols,
#                      max_class_percent = 0.98, max_na_percent = 0.67)
# 
# # client_id hid_party and iq_id are not one to one !
# # select columns 
# # end test 



 
####limits---- 
 
 #if (verbose)
 #  print("Preparing limits data....")
 
parse_limits_new <- function( limits_file,
                          month_balance_file,
                          start_date = ymd("1970-01-01"),
                          end_date = today()) {
  ## limits_file: path to the file with limits_file( limits twice a month for every client)
  ## month_balance_file: file with balance by month end for every client
  ## Returns:
  ##limits_final : modified data frame 
  
  
  limits <- read.csv(file = limits_file , header = T, sep = ",", dec = ",")
  n <- nrow(limits)
  limits %<>% mutate( DT = as.Date( DATEODB, format = "%d.%m.%Y")) %>%
    select( -CL_IQ, -DATEODB ) %>%
    distinct()
  cat("Initial rows number=: \n", n , "\n\n")     
  
  # two limits for one client and same date, what to do (2 contract_numbers for one cl_id) ? 
  #limits %<>% group_by(CLIENT_ID, DT ) %>%
  # summarise( LIM = max(LIM))
  
  n1 = nrow(limits)
  #limits %<>% left_join( EVAL %>% select(CLIENT_ID, DATE), by = "CLIENT_ID") 
  #limits %<>%  filter( DT < DATE)
  #n2 = nrow(limits) # many DATES; 979058
 # cat(" Keep only rows in which changes occured before communication, rows removed: \n",n1- n2 , "\n")
  
  limits %<>% filter( LIM > 0)
  # filter out clients without limits
   
    limits_aggr <- limits %>% 
    group_by(CLIENT_ID) %>%
    arrange(DT) %>%
    mutate(CHNG = LIM - lag(LIM, n = 1, default = LIM[1])) %>%
    summarise( #LIM_AVG_N_CHANGES = mean(CHNG != 0),
      LIM_MAX_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                   max(CHNG[CHNG > 0]),
                                   0)/mean(LIM),
      LIM_MAX_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                   max(CHNG[CHNG > 0]),
                                   0)/mean(LIM),
      LIM_MIN_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                   min(CHNG[CHNG > 0]),
                                   0)/mean(LIM),
     # LIM_MAX_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
     #                               max(CHNG[CHNG < 0]),
     #                               0),
     #  LIM_MIN_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
     #                               min(CHNG[CHNG < 0]),
     #                               0),
      LIM_AVG_CHANGE = mean(CHNG)/mean(LIM), # 1 / count 
      LIM_AVG_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                   mean(CHNG[CHNG > 0]),
                                   0)/mean(LIM),
      LIM_AVG_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
                                   mean(CHNG[CHNG < 0])/ mean(LIM),
                                   0),
      LIM_N_POS_CHANGE = sum(CHNG > 0), # count pos changes 
      LIM_N_NEG_CHANGE = sum(CHNG < 0),  # countneg  changes 
      LAST_LIM = LIM[which.max(DT)], #last limit
      DAYS_LAST_CHANGE = DT[which.max(CHNG > 0)], # positive change!
      DATE = max(DT) +1)  %>%
    mutate(DAYS_LAST_CHANGE  =  as.numeric(  DATE - DAYS_LAST_CHANGE) ) %>%
    select( -DATE)
  
  # many NaN, only  for those who has limit == 0 
  limits_aggr %<>% 
    mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) # replace NANs with 0
  
  # calculate utilization, use month_balalnce file  
  month_bal <- read.csv(file = month_bal_file , header = TRUE, sep = ",", dec = ",")
  month_bal %<>% mutate(CONTRACT_NUMBER = AR_NO,
                        CLIENT_ID = as.integer(CL_ID),
                        #CLIENT_ID = as.integer( gsub(",", "",CL_ID)),
                        #DEP_AMT = as.numeric( gsub( ",","",DEP_AMT)), 
                        #CRD_AMT = as.numeric( gsub( ",","",CRD_AMT)), 
                        #DLQ_AMT = as.numeric( gsub( ",","",DLQ_AMT)), 
                        DT= as.Date( DT, format = "%d.%m.%Y")) %>%
                 select( -AR_NO, -CL_ID ) 
  
  util<- month_bal %>%
    inner_join( limits, by = c("CLIENT_ID","DT" ) ) %>%
    mutate( UT = if_else( LIM > 0, CRD_AMT/LIM, 0),
            DLQ = if_else( LIM > 0, DLQ_AMT/LIM, 0) )   %>%
    select( -DLQ_AMT, -CRD_AMT, -DEP_AMT, -LIM  ) %>% 
    group_by(CLIENT_ID, CONTRACT_NUMBER) %>%
    summarise( MAX_UT  =  max(UT),  MAX_DLQ =  max(DLQ),
               MIN_UT  =  min(UT) , MIN_DLQ =  min(DLQ),
               MEAN_UT =  mean(UT), MEAN_DLQ = mean(DLQ),
               LAST_UT =  UT[which.max(DT)], LAST_DLQ = DLQ[which.max(DT)])
  # UT = utilization = credit/limit
  # DLQ = deliquency / limit, close to 0
  # MIN_DLQ == 0
  
  util$MAX_UT  %<>% bound_feat(upper_bound = 1, replacement = 1)
  util$LAST_UT  %<>% bound_feat(upper_bound = 1, replacement = 1)
  
  limits_final <- limits_aggr %>% 
    left_join(util, by = c("CLIENT_ID") )
  
  # may change, optional!
  limits_final %<>% select( -MEAN_UT, -MEAN_DLQ, - MIN_DLQ)
  limits_final %<>% as.data.frame()
  
  
  rm( limits, limits_aggr, util)
  
  return( limits_final)
} 
 
  # test: 
  #limits_file    <- 
   "C:/Users/Liksakov/Desktop/Liks/insurance/insur/model_data/2017-06-27 strahov_lim.csv"
  #  
  #   month_bal_file <- 
     "C:/Users/Liksakov/Desktop/Liks/insurance/insur/model_data/2017_06_23_balance.csv"
  # 
  # G <- parse_limits(limits_file = limits_file, month_balance_file = month_bal_file  )
  # 
  # 
###parse_balance----  
  
    parse_balance_new <- function( balance_file,
                                   start_date = ymd("1970-01-01"),
                                   end_date = today()) {
    ## balance_file: path to the file with balance (balance on change date) 
    ## top_n_cols: list of named tuples of the form `c(COL_NAME=COL_NAME, 
    ##                                                     PERCENT=PERCENT)`
    ##    example:  top_n_cols = list(c('name' = 'CONTR_STATUS', 'percentage' = 0.02) )
    ## Returns:
    ## balance_final : modified data frame 
 
    
    #1. join client_id and date_start ( to set window for  balance)
    #2. 2 features: bal and debt instead of 1
    #3. remoove duplicated rows ( bal changed, but debt - not _> count changes is different)
    #4. deltas instead 
    #5. aggFunctions 
    #6. recent balanc
    #7. join all (see balance_change_Vova)  
      
    
   balance <- read.csv(file = balance_file, header = TRUE, sep = ",", dec = ",")
   balance %<>%  mutate( ST_BAL_DT  = as.Date(ST_BAL_DT, format = "%d.%m.%Y"), 
                         # same, need for aggSummaries function
                         TXN_DT = as.Date(ST_BAL_DT, format = "%d.%m.%Y"),
                        CONTRACT_NUMBER = AR_NO)  %>% 
                select( -AR_NO)
  n1 <- nrow(  balance)
  cat("Initial rows number:", n1,   "\n")  
 ## join aon EVAL, keep only rows in which changes occured before communication
 #balance %<>% left_join( EVAL %>% select(CONTRACT_NUMBER,DATE), by = "CONTRACT_NUMBER")
 #balance %<>%  filter( ST_BAL_DT < DATE)
 #n2 = nrow(balance) # many DATES;  1819698
 #cat("Remove observation after communication: \n")
 #cat("Rows removed:", n1 - n2, "\n\n")

  balance %<>% 
    mutate(BAL = DEP_AMT - CRD_AMT - DLQ_AMT, DEBT = DLQ_AMT) %>%
    select(-DEP_AMT, -CRD_AMT, -DLQ_AMT)
  
  ## divide into two data frames: one for balance, one for debt
  ## and filter unncecessary rows
  just_blnc <-   balance %>% 
    select(-DEBT) %>%
    group_by(CONTRACT_NUMBER) %>%
    arrange((ST_BAL_DT)) %>%
    filter((BAL- lag(BAL, 1, default = BAL[1] + 1))!= 0) %>%
    ungroup()
  m1 = nrow( just_blnc);  #1815930
  cat("Only BALANCE change table, rows number =", m1, "\n")
  
  just_debt <-  balance %>% 
    select(-BAL) %>%
    group_by(CONTRACT_NUMBER) %>%
    arrange((ST_BAL_DT)) %>%
    filter((DEBT- lag(DEBT, 1, default = DEBT[1] + 1))!= 0) %>%
    ungroup()
  m2 = nrow( just_debt);  #90053
  cat("Only DEBT change table, rows number =", m2, "\n\n")
  # debt( DLQ) changes rarelly, usually 0 !

   just_blnc %<>%
    group_by(CONTRACT_NUMBER) %>%
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
   
   just_debt %<>%
     group_by(CONTRACT_NUMBER) %>%
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
  
  cat("--------Creating aggregate summaries:--------\n\n")
  
  ## NEED DATE to AGGREGATE by time window:
  
  DATE <- max( balance$ST_BAL_DT) + 1
  just_blnc$DATE <- rep( DATE, nrow(just_blnc))
  
  just_blnc2 <- createAggSummaries(just_blnc,
                   id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER'),
                   agg_cols = list(c('BAL_NEG_CHNG', 'BAL_POS_CHNG'),
                                   c(7, 31, 90, 180), 
                                   c(),
                                   funs(MAX = max(., na.rm = TRUE),
                                        MIN = min(., na.rm = TRUE),
                                        N = sum(!is.na(.)),#length(., na.rm = TRUE),
                                        SUM = sum(., na.rm = TRUE))),
                   wide = TRUE)
  # replace NAs, inf with 0 
  just_blnc2 %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) 
  
  
  just_debt$DATE <- rep( DATE, nrow(just_debt))
  
  just_debt2 <- createAggSummaries(just_debt,
       id_columns = c('CLIENT_ID','CONTRACT_NUMBER'),
       agg_cols = list(c('DEBT_NEG_CHNG', 'DEBT_POS_CHNG'),
                       c(7, 31, 90, 180), 
                       c(),
                       funs(MAX = max(., na.rm = TRUE),
                            MIN = min(., na.rm = TRUE),
                            N = sum(!is.na(.)),#length(., na.rm = TRUE),
                            SUM = sum(., na.rm = TRUE))),
       wide = TRUE)
  # replace NAs, inf with 0
  just_debt2 %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) 
  cat("\n");   cat("Rows in aggregate balance table: ", nrow(just_blnc2), "\n")
  cat("\n");   cat("Rows in aggregate debt table: ",  nrow(just_debt2), "\n")

  
    
  # most recent balances, debts 
  # rcnt_blnc <- balance %>%
  #  group_by(CLIENT_ID,CONTRACT_NUMBER, DATE) %>%
  #   summarise(LAST_BAL = BAL[which.max(ST_BAL_DT)],
  #            LAST_DEBT = DEBT[which.max(ST_BAL_DT)])
  #cat("Recent balance, rows number: ",  nrow(rcnt_blnc), "\n\n")
  

  
   balance_final <- balance %>% 
      select( CLIENT_ID,CONTRACT_NUMBER) %>% 
      distinct() %>%
      left_join(just_blnc2, by = c('CLIENT_ID','CONTRACT_NUMBER')) %>%
      left_join(just_debt2, by = c('CLIENT_ID','CONTRACT_NUMBER'))
      #left_join(rcnt_blnc, by = c('CLIENT_ID','CONTRACT_NUMBER'))
    
    
  cat("Final balance table columns: ",  ncol(  balance_final), "\n\n")  
  rm(balance, just_blnc, just_blnc2, just_debt, just_debt2)

  return( balance_final )
}   
  # test:   
    # balance_file  <- 
    #   "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-27 ins_full_bal.csv"  
    # # need also modified  EVAL table
    # H <- parse_balance( balance_file ) 
    #  #cor(x[sapply(x, is.numeric)])
    # H1 <-H %>% select( -CONTRACT_NUMBER, -CHANNEL, - DATE_OPEN, -DATE)
    # colnames(H1) <- NULL
    # corr_bal <-cor( H1, use="complete.obs")
    # 
    # corrplot(   corr_bal , t1.cex = 0.3  )
    

### IQ soc_dem-----

 # empty space replace with NA
 # Y/N on 1/0
 # remove features with much NA
 # remove with one class
 # top_N

 max_class_freq <- function( col, df ) {
   prop_tmp <- table(df[, col]) %>% 
               prop.table %>%
               sort(decreasing = TRUE)
   return( round(prop_tmp[1],3) )
 }
 
parse_socdem_iq_new <- function(socdem_iq_file,
                            max_class_percent = 0.99, 
                            max_na_percent = 0.6, 
                            codemap_file = "SD_IQ_codemap")
  # ??   id_column = 'REP_CLID',
{
  ## Args:
  ## socdem_iq_file: path to the file with socdem_iq_file from IQ base
  ## top_n_cols unique values. Corresponding top-n unique values will be
  ##               kept, others will be combined into a single column (0).
  ##               rename values to 1 : n by their frequency
  ##  max_class_percent: column features, which has more than max_class_percent of one class
  ##               will be removed automatically;
  ##               usually na, empty space, unknown
  ##  max_na_percent: column features, which has more na than max_na_percent 
  ##               will be removed automatically
  ## Returns:
  ## sd_iq : modified data frame
  
  # 1. read file 
  sd_iq<- read.csv(file = soc_dem_iq_file, header = TRUE, sep = ",", dec = ".", na.strings = c("","NA", "<NA>"))
  cat(" Initial number of columns =" , ncol(sd_iq));  cat("\n\n")
  
  sd_iq %<>%  mutate( AGE = 2017 - year( as.Date(REP_BDATE, format = "%d.%m.%Y" )), 
                      REP_CLID = as.numeric( REP_CLID)) %>%
    select( -REP_BDATE ) 
  sd_iq %<>% dplyr:: rename(IQ_ID = REP_CLID )
  # remove columns with nearly one class:
  freq_class_perc_tmp <- sapply( 1:ncol(sd_iq), function(x,y) max_class_freq(x,y), y = sd_iq)
  colums_ro_remove_1 <- colnames(  sd_iq[,which(as.numeric(freq_class_perc_tmp) >=  max_class_percent)])
  sd_iq <- sd_iq[ ,which(as.numeric(freq_class_perc_tmp) <  max_class_percent)]
  cat(" Column number after removing features with too large class: ", ncol(sd_iq)); cat( "\n")
  cat(" Names of removed columns:\n")
  for( i in (1: length( colums_ro_remove_1)))
  {
    cat(colums_ro_remove_1[i] , "\n")
  } 
  cat( "\n")
  
  # remove columns with high NA perc:
  na_perc_tmp <- sapply( 1:ncol( sd_iq), function(x,y) round( sum( is.na(y[,x]))/nrow(y), 3), y =sd_iq)
  colums_ro_remove_2 <- colnames(  sd_iq[,which(na_perc_tmp >=  max_na_percent)])
  sd_iq <- sd_iq[ ,which(na_perc_tmp <  max_na_percent)]
  cat(" Column number after removing features with too large NA percent: ", ncol(sd_iq)); cat( "\n")
  cat(" Names of removes columns:\n")
  for( j in (1: length( colums_ro_remove_2)))
  {
    cat(colums_ro_remove_2[j] , "\n")
  } 
  cat( "\n")
  
  # print table with NA:
  NA_perc_table <- data.frame(t( na_perc_tmp[which(na_perc_tmp <  max_na_percent)]))
  colnames(NA_perc_table ) <- colnames(sd_iq )
  cat( "**********Many NA in data, NA percent by features:********** \n\n")
  print(NA_perc_table )
  cat( "\n")
  
  # top N 
  
  #sd_iq <- createTopNCols(sd_iq, top_n_cols = top_n_cols)
  
  #code_map <-  sd_iq[[2]]
  #sd_iq <- sd_iq[[1]]
  
  
  #code_map <- readRDS(file = "SD_IQ_codemap", refhook = NULL)
  
  
  #--- USE TOP N COLS:
  cat("\n")
  print(glue::glue('{str_dup("***", 5)}USING TOP-N FEATURES{str_dup("***", 5)}'))
  cat("\n\n")
  
  #load codemap
  code_map <- readRDS(file =  codemap_file)
  
  cols_to_transform <- colnames(sd_iq)[ which(colnames(sd_iq) %in% names(code_map)) ]
  
  df_new <- NULL
  df_new<- sd_iq
  for( i in (1 :length(names(code_map))))
  {
    col_name <- names(code_map)[i]
    map_table <- code_map[i][[1]]
    df_new[,col_name] <- use_codemap( col_name, map_table, df = df_new)  
    cat(  "USE top N ecncoding for field:", col_name, "\n")
  }
  # top N
  #summary( sd_iq)
  # CL_AMT_ZP /   CL_AMT_DOP/  REP_POPULATION  too large, may be outlyers
  rm(sd_iq)
  return(  df_new)
}
 
 # test :
#socdem_iq_file  <- 
 #  "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-23 socdem_IQ.csv"
# 
# H <- parse_socdem_iq( socdem_iq_file = socdem_iq_file, max_class_percent = 0.99, 
 #                      max_na_percent = 0.5)
 
 

 
####### omit_corr_function ------
 
 # FUNC which takes dataframe and omit 
 # too correlated features
 # and print top correlations (positive and negative)
 
 
 omit_corr_feat <- function( df, top_n_corr = 5,
                             omit_action = FALSE,
                             corr_threshold = 0.99,
                             print_info = T, 
                             print_plot = F,
                             plot_names = T,
                             save_plots = F)
 {
  library(corrplot)
   
   corr_matrix <- cor( df[sapply(df, is.numeric)],
                       use="pairwise", method="pearson")
   # FIND TOP N CORRaLATION PAIRS: 
   corr_matrix <- apply( corr_matrix, 1:2, function(x) round(x,3) )
   corr_pairs  <- as.data.frame(as.table( corr_matrix))
   corr_pairs  <- corr_pairs[order(corr_pairs$Freq, decreasing = T),]
   corr_pairs %<>% filter( Var1 !=Var2) 
   # remove all odd rows ( each corr pair is duplicated)
   corr_pairs<- corr_pairs[-seq(1, nrow(corr_pairs),2), ]
   colnames( corr_pairs)[3] <- "CORR"
   tmp_corr <-  corr_pairs[ which(abs(corr_pairs$CORR) > corr_threshold),]
   colums_to_remove = unique(tmp_corr$Var2 )
   cat(" Some correlations may be unreliable, rows  NA  cells are omitted \n")
   if(print_info)
   {
    cat( "TOP positive corr pairs:\n" )
     print( corr_pairs[1:top_n_corr,]);  glue( '{str_dup("***", 20)}')
    cat( "TOP negative corr pairs:\n" )
     print( corr_pairs[( (nrow(corr_pairs)-top_n_corr) :nrow(corr_pairs)) ,])
   }
     
  if( omit_action)
  {
     df_omitted <- df[ ,which( !( colnames(df) %in%  colums_to_remove) ) ]
     cat( "Omit column features with too high correlation,\n\n " )
     cat( "Columns removed:", length( colums_to_remove), "\n")
     cat( "Columns left:", ncol(df_omitted), "\n")
  }
   
   if( print_plot)
   {
     if( !plot_names) 
     {
       colnames(corr_matrix) <- NULL;
       rownames(corr_matrix) <- NULL;
     }
     correlations_plot <-corrplot(corr_matrix,tl.cex=0.6 )
   }   
   if( omit_action)
      return(df_omitted)
   else
     return(df)
 }
 
 # test
 #df <-  trans_feat[(1:10000),(1:200)]
 #H <- omit_corr_feat(df = df, top_n_corr = 50, print_plot = T, omit_action = T)
 
 
 ## remove columns with too high percent( > max_class_percent) of one class 
 ## returns modified data drame without some columns
 remove_ONE_CLASS_features <- function( df,max_class_percent) 
 {
   ##   max_class_freq, returns class name with max class percent 
   max_class_freq <- function( col, df )
   {
     prop_tmp <- table(df[, col]) %>% 
       prop.table %>%
       sort(decreasing = TRUE)
     return( round(prop_tmp[1],3) )
   }
   
   # most frequent class and its perc by all columns 
   freq_class_perc_tmp <- sapply( 1:ncol(df), function(x,y) max_class_freq(x,y), y = df)
   colums_ro_remove_1 <- colnames(  df[,which(as.numeric(freq_class_perc_tmp) >=  max_class_percent)])
   df <- df[ ,which(as.numeric(freq_class_perc_tmp) <  max_class_percent)]
   cat(" Column number after removing features with too large one class: ", ncol(df)); cat( "\n")
   cat(" Names of removed columns:\n")
   for( i in (1: length( colums_ro_remove_1)))
   {
     cat(colums_ro_remove_1[i] , "\n")
   } 
   cat( "\n")
   
   return(df)
 }
 # COMMENT: such columns may appear after top_n_cols
   
 
 ## remove columns with too high percent( > max_class_percent) of one class 
 ## returns modified data drame without some columns
 remove_NA_features <- function( df, max_na_percent, print_table = T)
 {
   na_perc_tmp <- sapply( 1:ncol(df), function(x,y) round( sum( is.na(y[,x]))/nrow( df), 3), y = df)
   df_col_names <- colnames(df)
   colums_ro_remove_2 <- colnames(  df[,which(na_perc_tmp >=  max_na_percent)])
   df <- df[ ,which(na_perc_tmp <  max_na_percent)]
   cat(" Column number after removing features with too large NA percent: ", ncol(df)); cat( "\n")
   cat(" Names of removes columns:\n")
   for( j in (1: length( colums_ro_remove_2)))
   {
     cat(colums_ro_remove_2[j] , "\n")
   } 
   cat( "\n")
   # print features which we leave 
   if(print_table)
   {
     na_perc_tmp <- as.data.frame( t(na_perc_tmp))
     colnames(na_perc_tmp ) <-  df_col_names; rownames(na_perc_tmp ) <-NULL
     cat(" Table with percent of NA values by column, \n ")
     print( na_perc_tmp)
   }
   return(df)
 }
 
 
  paste_names <- function( df,df_name, id_cols)
  {
    ids_index <- (colnames(df)  %in% id_cols)
    names_tmp <- colnames(df)[!( ids_index )]
    names_tmp_new <- sapply(1: length(names_tmp),
                        function(n) paste0(df_name,".",names_tmp[n]) )
    colnames(df)[!( ids_index )] <-   names_tmp_new
    return(df)
  }
 
  # test
  # df <- TRANS[(1:1000), ]
  # df <- paste_names( df = df , df_name = "TRANS", id_cols = c( "CLIENT_ID", "DATE" ,"CONTRACT_NUMBER"))
  # head(df)
  
  
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
  
  
  # FUNCTION to remove "bad observation"
  # "bad observation" if it has many(>threshold) NA or
  #                   if it has  many(>threshold) concrete value 
  # parms: df = data frame
  # omit_action_na = if to omit obs woth high NA
  # omit_action_value = if to omit obs woth high  value
  # value = value to check ( for example, 0 )
  #
  # returns list of 2: 1: modified df(if any omit TRUE)
  #                    2: table with percents of na and value by row in df (in observation)
  remove_obs <- function(df,col_idx = (1:ncol(df)),
                         omit_action_na = FALSE, 
                         omit_action_value = FALSE, 
                         threshold = 0.05,
                         Value = 0)
  {
    #df <-  df[,col_idx]
    l <- ncol( df[,col_idx])
    na_perc_obs <- numeric()
    na_perc_obs <- apply( df[,col_idx],1,function(x)  round( sum(is.na(x))/l ,2) )
    oneValue_perc_obs <- numeric()
    oneValue_perc_obs <-  apply( df[,col_idx],1,function(x)  round( sum( x  == Value)/l ,2) )
    if(omit_action_na)
    {
      remove_idx_na <-  which(na_perc_obs > threshold)
      if(length(remove_idx_na) > 0)
      df<-df[ -remove_idx_na,]
      cat(" Omit observations with high NA \n")
      cat("Remove threshold= ", threshold, "  Obs removed:", length(remove_idx_na) , "\n\n")
    }
    if(omit_action_value)
    {
      remove_idx_val <-  which( oneValue_perc_obs > threshold)
      if(length(remove_idx_val) > 0)
      df<-df[ -remove_idx_val,]
      cat(" Omit observations with one Value \n")
      cat("Remove threshold= ", threshold, "  Obs removed:", length(remove_idx_val), "\n\n" )
    }
    tab <- data.frame();
    tab <- cbind(na_perc_obs, oneValue_perc_obs )
    colnames(tab ) <- c("na_perc_obs","oneValue_perc_obs")
    return(list( df, tab ))
  }
  
  # test:
  #remove_obs( )
  
  
  # function to work with factors in data frame
  # data = initial data frame
  # facor_cols = names of factor variables  
  # add_factors  if to add all variables of type factor to  facor_cols vector
  # dummy if to transform  factor variables to dummy 
  # returns: modified data 
  # 
  # to add:   fill na for factors with impirical sampling, 
  #           factors value frequances, smoothed likelihood, 
  #           .., dummy codemap
  factor_process <- function(data, factor_cols, add_factors = TRUE, dummy = TRUE)
  {
    if( add_factors)
    {
      # table with column types
      data_types <- data.frame()   
      for( i in (1: ncol(data)))
      {
        data_types[i,1] <- class(data[,i])
        rownames(data_types)[i] <- colnames(data)[i]
        colnames(data_types) <- "TYPE"
      }
      
      cat("Transform to factor variables,  add to factor_cols vector:\n")
      for( f in factor_cols)
      {
        data[,f] <- as.factor( data[,f])
        cat(f, "\n")
      }
      
      factor_vars <- rownames(data_types)[ which(  data_types$TYPE =="factor" ) ] 
      cat( "Factor variablers in data_frame:\n")
      print( factor_vars ) ; cat("\n\n")
      factor_cols <- c( factor_cols, factor_vars)
    }
    
    cat( "Factors number total = ", length(  factor_cols),"\n\n")
    
    if(dummy)
    {
      cat("*****MAKE DUMMY*******\n\n")
      data <- dummy.data.frame(data = data, 
                     names = factor_cols, 
                     omit.constants = TRUE,
                     sep = "_",
                     verbose = TRUE)
    }
    #cat("")
    return( data )
  }
  
  bound_feat <- function(feat, low_bound = -1000000, upper_bound = 1000000, replacement = NA)
  {
    feat[ which( (feat < low_bound )|(feat > upper_bound)) ] <- replacement
    return( feat)
  }
  
  
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  