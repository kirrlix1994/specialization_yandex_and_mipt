library(ggplot2)
library(plotly)
library(magrittr)
library(dplyr)
library(reshape)
library(eeptools)
library(tidyr)
library(corrplot)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)
library(reshape2) 
library(dummies)



#setwd("C:/Users/Liksakov/Desktop/Liks/insurance/scripts")

## creating agg summaries; top_N_cols
source("helpers.R") 
source("parse_transactions.R")

###### trans----

#trans_file <- "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017_06_23_transaction.csv"
#trans <-fread(file=trans_file, sep = "," )
#mcc_table <- read.csv(file = "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/mcc_v2.csv" , sep = ";", dec = ",",header = T)
#colnames(mcc_table) <- c("TXN_MCC", "CATEGORY")

 parse_TRANSACTION_MAIN <- function(trans_file,
                                    mcc_table_file,
                                    top_n_cols = list(),
                                    agg_cols = list(),
                                    min_trans_amount = 10)
 {
  
  trans <- fread(file = trans_file, sep = "," ) 
  mcc_table <- read.csv(file = mcc_table_file , 
                        sep = ";", dec = ",",header = TRUE)
  
  colnames(mcc_table) <- c("TXN_MCC", "CATEGORY")
  #cat(1,"\n")
  trans %<>% mutate( CLIENT_ID = as.numeric(CLIENT_ID ), 
                   CONTRACT_NUMBER = DOG ,
                   TXN_RUB_AMT =  as.numeric( gsub(",",".",TXN_RUB_AMT)),
                   TXN_DIR = as.numeric(TXN_DIR),
                   TXN_DT = as.Date(TXN_DT, format = "%d.%m.%Y"), 
                   TXN_TP = as.factor(TXN_TP),
                   TXN_CTR  = as.factor( TXN_CTR),
                   TXN_CURR = as.factor( TXN_CURR ),
                   RUB_TR = as.numeric( TXN_CURR == 810),
                   TXN_MCC = as.numeric( TXN_MCC),
                   FLG_CATEGORY = as.numeric(FLG_CATEGORY )) %>% 
  select( -TXN_OW4_TP, -DOG) 
  #cat(2,"\n")
    #  have to join on EVAL 
    n1 = nrow( trans ) 
    trans %<>% right_join( EVAL %>% select(CLIENT_ID, CONTRACT_NUMBER,DATE) , 
                           by = c("CONTRACT_NUMBER","CLIENT_ID") ) %>% 
                filter( TXN_DT < DATE, 
                        TXN_RUB_AMT > min_trans_amount) 
    n2 = nrow( trans ) 
    #cat(3,"\n")
    trans %<>% left_join( mcc_table, by = "TXN_MCC" )
    #cat(4,"\n")
      trans %<>% parseTransactions(df = trans, 
                                   end_date = today(),
                                   top_n_cols = top_n_cols, 
                                   agg_cols =  agg_cols, 
                                   wide = TRUE, 
                                   codemap_file = "TRANS_codemap")
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
# function to parse soc_dem from human factor data

#source("helpers_liks.R") 
# top N function
              
parse_socdem_hf <- function(socdem_hf_file,
                            start_date = ymd("1970-01-01"),
                            end_date = today(),
                            top_n_cols = list(), 
                            max_class_percent = 0.98, 
                            max_na_percent = 0.67, 
                            omit_cols = TRUE, 
                            save_code_map = TRUE, 
                            code_map_file ="SD_HF_codemap")
                        # ??   id_column = 'CONTRACT_REF',
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
  ##  omit_cols: whether to omit bad columns, if TRUE , cols are omitted 
  ## Returns:
  ## df : modified data frame
  
  # 1. read file 
  df <- read.csv(file = socdem_hf_file, header = T, sep = ";", dec = ",",
                 na.strings = c("NA","NaN", ""), stringsAsFactors = TRUE)
  cat(" Initial number of rows = " , nrow(df));  cat("\n")

  
  # 2. change fields
  if ( 'DATA' %in% colnames(df) )
  {
    df %<>% mutate( DATA = as.Date( DATA,"%d.%m.%Y" ), CHANNEL = as.factor(CHANNEL))
    df %<>% dplyr:: rename(  DATE = DATA) 
  }

  
  # drop unvalid client_id:
  #df <- df[ -which( is.na( as.numeric(as.numeric(levels(df$CLIENT_ID))[df$CLIENT_ID] ) ) ), ]
  df %<>% mutate( CLIENT_ID = as.numeric(levels(CLIENT_ID))[CLIENT_ID] )
  df <- df[ - which( is.na(df$CLIENT_ID)),]
  df %<>% mutate( BIRTHDATE = as.Date( BIRTHDATE,"%d.%m.%Y" ))
  df %<>% mutate( AGE =  2017 - year(BIRTHDATE) )
  # remove commas from HID_PARTY
  df %<>% mutate( HID_PARTY =  as.numeric(( gsub(",","",HID_PARTY))))
  #remove letters from IQ_ID
  df %<>% mutate( IQ_ID = as.numeric( str_replace_all(IQ_ID, "[A-Za-z  ]" , "" )) )
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
  
  #4. creating top_n columns
  print(glue::glue('{str_dup("***", 5)}CREATING TOP-N FEATURES{str_dup("***", 5)}'))
 # df %<>% createTopNCols(top_n_cols = top_n_cols)
  df <- createTopNCols(df, top_n_cols = top_n_cols)
  # TopNCols returns list with df and code maps
  code_map <-  df[[2]]
  df <- df[[1]]
  #5. remove columns with too high percent( > max_class_percent) of one class 
  ##   max_class_freq returns max class percent 
  max_class_freq <- function( col, df ) {
    prop_tmp <- table(df[, col]) %>% 
    prop.table %>%
    sort(decreasing = TRUE)
    return( round(prop_tmp[1],3) )
  }
  # most frequent class and its perc by all columns 
  freq_class_perc_tmp <- sapply( 1:ncol(df), function(x,y) max_class_freq(x,y), y = df)
  df_tmp <- df[ ,which(as.numeric(freq_class_perc_tmp) <  max_class_percent) ]
  colums_ro_remove_1 <- colnames(  df[,which(as.numeric(freq_class_perc_tmp) >=  max_class_percent)])
  cat(" Column number after removing features with too large class: ", ncol(df_tmp)); cat( "\n")
  cat(" Names of removed columns:\n")
  for( i in (1: length( colums_ro_remove_1)))
  {
    cat(colums_ro_remove_1[i] , "\n")
  } 
  cat( "\n")
  
  # na perc by features
  na_perc_tmp <- sapply( 1:ncol( df_tmp), function(x,y) round( sum( is.na(y[,x]))/nrow(  df_tmp), 3), y =  df_tmp)
  colums_to_remove_2 <- colnames(   df_tmp[,which(na_perc_tmp >=  max_na_percent)])
  df_tmp <-  df_tmp[ ,which(na_perc_tmp <  max_na_percent)]
  cat(" Column number after removing features with too large NA percent: ", ncol( df_tmp)); cat( "\n")
  cat(" Names of removed columns:\n")
  for( j in (1: length( colums_to_remove_2)))
  {
    cat(colums_to_remove_2[j] , "\n")
  } 
  cat( "\n")
  # print features which we leave 
  
  #6. leave many IQ_ID for one client id 
  # to join with iq data and choose one than
  # table of cases with 2 iq_id on one client_id
  
  #7. if many communications take only the first one
  #tmp_df <-  select(df, c("CLIENT_ID","DATA"))
  #tmp_df <-  tmp_df[!duplicated(tmp_df),] do not need, want to get indeces
  #tmp_df %<>% group_by(CLIENT_ID) %>%  summarise(diff_comm_date_count = n()) 
  #tmp_df <-  tmp_df[ order( tmp_df$diff_comm_date_count, tmp_df, decreasing = T) , ]
  #rm( tmp_df)
  
  #7. transform to factors: 
  # all top N cols
  for (c_ in top_n_cols) {
    c_name <- c_['name']
    df[,  c_name] <- as.factor( c_name)
  } 
   df$GENDER <- as.factor( df$GENDER)
   df$OWN_CAR_FLAG <-  as.factor(  df$OWN_CAR_FLAG)
  
      
  #8. choose columns
  if(omit_cols)
  {
    cat("Remove also columns:\n")
    cat("WAY4_ID", "BIRTH_PLACE" ,"BIRTHDATE", "ORGANIZATION_TYPE", "\n\n") 
    # need check that such columns exist!
    df_tmp %<>% select( -WAY4_ID, -BIRTH_PLACE, -BIRTHDATE, -ORGANIZATION_TYPE)
    cat("Final column number =", ncol(df_tmp), "\n" )
  }
  
   if(save_code_map)
   {
     saveRDS(object =  code_map, 
             file = "SD_HF_codemap", 
             ascii = FALSE, version = NULL,
             compress = TRUE, refhook = NULL)

  #9. check that diffrent soc dem info for one client_id 
  
  rm(df)
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
# h %<>% select( -WAY4_ID, -BIRTH_PLACE, -BIRTHDATE, -ORGANIZATION  )
# # end test 



###### parse binbonus function ----  
# function to parse binbonus data

parse_binbonus <- function( binbonus_file,
                            start_date = ymd("1970-01-01"),
                            end_date = today()) {
  ## binbonus_file: path to the file with binbonus_file 
  ## Returns:
  ## df : modified data frame (see new fields description in the end)
  
# 1. read file 
binbonus <- read.csv(file = binbonus_file, header = T, sep = ";", dec = ",")
cat(" Initial number of rows = " , nrow(binbonus));  cat("\n")
cat(" Initial number of columns = " , ncol(binbonus));  cat("\n")   

#2. CLIENT_ID is the same as CLIENT_ID_NUMBER;
# transform dates
binbonus %<>% select( - CLIENT_ID_NUMBER) %>%
              mutate( DATA = as.Date( DATA,"%d.%m.%Y" ), 
                      START_DATE = as.Date(  START_DATE,"%d.%m.%Y" ), 
                      END_DATE = as.Date( END_DATE,"%d.%m.%Y" ), 
                      CLIENT_ID = as.numeric( CLIENT_ID)) %>%
              filter( START_DATE < DATA)
             

binbonus %<>% dplyr:: rename( DATE = DATA)

#3. many rows with the same code and end-date , but different start_date 
# take only first date: ( many cards -> many such rows)
binbonus %<>% group_by(CLIENT_ID,DATE,CHANNEL,CODE, END_DATE)  %>%
  summarise(START_DATE  = min(START_DATE)) 

#4. many times one category
binbonus %<>% group_by(CLIENT_ID, DATE,CHANNEL, CODE)  %>%
  summarise( COUNT_CATEGORY = n(),
             #MAX_DURATION = as.numeric(today() - min(START_DATE)), 
             MAX_DURATION = as.numeric(min(DATE) - min(START_DATE)),
             START_DATE = min(START_DATE), END_DATE = max(END_DATE), 
             #DIFF_DAYS = min( na.omit(as.numeric( END_DATE - START_DATE )) , 
             #                as.numeric( today() - START_DATE ) ) )
             DIFF_DAYS = ifelse(is.na(END_DATE), as.numeric( DATE - max(START_DATE)), 
                                mean( as.numeric( END_DATE - START_DATE)) )  )

# MAX_DURATION = days from first connection till DATE of communication
# DIFF_DAYS = DAYs category  was active( if still actice => DATE - last_connection date)

#5. make indicators of category times days it was active
binbonus %<>% mutate( AUTO =    DIFF_DAYS*( as.numeric( CODE ==  "IS_LOYALTY_AUTO")),  
                      BEAUTY =  DIFF_DAYS*( as.numeric( CODE == "IS_LOYALTY_BEAUTY")), 
                      ONLINE =  DIFF_DAYS*( as.numeric( CODE == "IS_LOYALTY_ONLINE")),
                      TRAVEL =  DIFF_DAYS*( as.numeric( CODE == "IS_LOYALTY_TRAVEL")),
                      WEEKEND = DIFF_DAYS*( as.numeric( CODE == "IS_LOYALTY_WEEKEND")))

#6. group many different cose to indicators (AUTO, BEAUTY,..)
binbonus %<>% group_by(CLIENT_ID,DATE,CHANNEL) %>% 
  summarize( AUTO = sum( AUTO),   BEAUTY = sum(BEAUTY), ONLINE = sum(ONLINE),
             TRAVEL = sum(TRAVEL), WEEKEND   = sum( WEEKEND), 
             COUNT_CATEGORY = max( COUNT_CATEGORY), 
             MAX_DURATION = max(MAX_DURATION ),  
             CANCELED_CATEGORY = sum(!is.na(END_DATE)), 
             DAYS_TO_REACT = -mean( as.numeric(( START_DATE - DATE))) ) 
# description:
# AUTO = number of days of auto_bonus was active
# TRAVEL,  BEAUTY, the same
# COUNT_CATEGORY = max times which one same category was taken
# MAX_DURATION = days from today and first connection
#                 equals sum of days each categoey was connected 
# CANCELED_CATEGORY = if any category was canceled (changed on the other)
# DAYS_TO_REACT = average days between communication date(dat ) and connection date

#7. check: one raw for client
# DATE and Channel are not unique for client! 
duplicates_number = nrow(binbonus) - length( unique(binbonus$CLIENT_ID ))
cat( "There are many rows for same client because of many connections (DATE, CHANEL), \n")
cat( "Duplicated rows number  =",  duplicates_number, "\n")

 return(binbonus)
}

# test
# binbonus_file  <- 
# "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-22 Binbonus.csv"
# 
# tmp <- parse_binbonus( binbonus_file)


 # check! 
 # days to react  < 0 ? 




###### parse commitions OLD ----
parse_commitions_old <- function(commitions_file,
                            agg_funs = list(),
                            start_date = ymd("1970-01-01"),
                            end_date = today()) {
  ## commitions_file: path to the file with commitions_file from way4 
  ## agg_funs: funs list to aggregate fee (min, max, ...) 
  ##  example: funs("min" = min, "max" = max, "mean" = mean, "count" = length)
  ## Returns:
  ## commitions : modified data frame 

commitions <- read.csv(file = commitions_file, header = T, sep = ";", dec = ",")
 commitions %<>%  mutate( COMM_DATE = as.Date(DATA , format = "%d.%m.%Y"),
                         DATE_CANC = as.Date(DATE_, format = "%d.%m.%Y"))%>%
                 filter( COMM_DATE > start_date, COMM_DATE < end_date) %>%
                 group_by(CLIENT_ID,CONTRACT_NUMBER, CHANNEL, COMM_DATE) %>% 
                 summarise(DURATION  = as.numeric(end_date - min(DATE_CANC))) %>%
                 summarise_at(.vars  = "FEE", .funs = agg_funs ) 
  
#  DURATION = days  between end_date and first cancellation date

 return(commitions)
}
# test 
# commitions_file  <- 
#   "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-19 ins_comis.csv"
# funs_to_aggr_fee <- funs("MIN_FEE" = min, "MAX_FEE" = max,
#                          "MEAN_FEE" = mean, "COUNT_FEE" = length)
# 
# tmp <- parse_commitions(commitions_file,  agg_funs =  funs_to_aggr_fee)



###### parse commitions----
parse_commitions <- function(commitions_file,
                             short_form = FALSE,
                                start_date = ymd("1970-01-01"),
                                end_date = today()) 
 {
  # commitions_file = file with commitions for insurance
  #  short_form = whether to return fee by every month, or just 
  #               aggregation (sum; count)
  # return: modified data frame, with 2 parts: 
  # BEFORE - payments( count, sum) which were made before communication, 
  #          may be used as regressors ( quite rare)
  # AFTER -  payments, which were made after, spread by all dates of cancellation 
  #          (every 01 of month since 01.01.2017)
  #          (cannot be used as regressors, need to formulate maximization problem)
  # COMMENT1: all NA are replaced with 0; NA may be because of 2 reasons: 
  #           1) communication happend after, so NA is ok
  #           2) comm happend, but some payments were NA, whichh means the were some 
  #           restrictions ; need to differ theese cases
  #
  
  
  commitions <- read.csv(file = commitions_file,
                         header = T, sep = ";", dec = ",", 
                         stringsAsFactors = FALSE)
  
  commitions %<>%  mutate( DATA = as.Date(DATA , format = "%d.%m.%Y"),
                           DATE_ = as.Date(DATE_, format = "%d.%m.%Y")) %>%
           dplyr:: rename(DATE = DATA, 
                          DATE_CANC =  DATE_ )
  
  commitions_before <- commitions %>% 
    filter( DATE_CANC <= DATE) %>%
    group_by(CLIENT_ID,CONTRACT_NUMBER, DATE) %>%
    summarise( CANC_N_BEFORE = n(), 
               CANC_SUM_BEFORE = sum( FEE))
  
  if( !short_form)
  {
    commitions_after <- commitions %>%
      filter( DATE_CANC > DATE) %>%
      gather(., variable, value,  DATE_CANC) %>% 
      unite(var, variable, value) %>% 
      spread(var, FEE) 
   # commitions_after <- mutate( commitions_after , 
   #  CANC_N_AFTER = sum( !(is.na(commitions_after[,5:ncol(commitions_after)] )) ) , 
   #                            CANC_SUM_AFTER = sum( na.omit(commitions_after[,5:ncol(commitions_after)])) )
    
    CANC_N_AFTER <-  sapply(1:nrow( commitions_after ), 
                            function(x)  sum( !(is.na(commitions_after[x,5:ncol(commitions_after)]))) )
    CANC_SUM_AFTER = sapply(1:nrow( commitions_after ), 
                            #function(x) sum( as.numeric(na.omit(commitions_after[x,5:ncol(commitions_after)]))) )
                            function(x) sum( commitions_after[x,5:ncol(commitions_after)], na.rm = TRUE))
    commitions_after %<>% mutate( CANC_N_AFTER = CANC_N_AFTER, 
                                  CANC_SUM_AFTER = CANC_SUM_AFTER)
  } 
  else 
  {
    commitions_after <- commitions %>%
      filter( DATE_CANC > DATE) %>%
      group_by(CLIENT_ID, CONTRACT_NUMBER, DATE, CHANNEL) %>%
      summarise( CANC_N_AFTER = n(), 
                 CANC_SUM_AFTER = sum( FEE))
  }

  commitions_final  <-  commitions  %>%
    select(CLIENT_ID, CONTRACT_NUMBER, DATE, CHANNEL ) %>% 
    distinct() %>%
    left_join( commitions_before, by = c("CLIENT_ID", "CONTRACT_NUMBER", "DATE")) %>%
    left_join( commitions_after,  by = c("CLIENT_ID", "CONTRACT_NUMBER", "DATE", "CHANNEL"))
   
  commitions_final[ is.na( commitions_final  )] <- 0
  
  rm(commitions,commitions_after, commitions_before )
  
  return( commitions_final)
 }
    
   #test
   # H <-  parse_commitions(commitions_file = commitions_file, 
   #                       short_form = T)
  

###parse_insurance----

 parse_insurance <- function( insurance_file,
                              start_date = ymd("1970-01-01"),
                              end_date = today()) {
   ## insurance_file: path to the file with insurance_file(clients who accepted insurance) 
   ## Returns:
   ## insurance_final : modified data frame 
   
   ## TO ADD:
   # was called before, will find in EVAL
   # if start_date == end date( duration = 0), can continue then
   # days of reaction plot 
   # days of duration ( if canceled == 1 and canceled == 0 - strange plot, need check)
   
   insurance <- read.csv(file = ins_file , header = T, sep = ";", dec = ",",
                         stringsAsFactors = FALSE)
   insurance %<>% mutate( DATE = as.Date( DATA, format = "%d.%m.%Y"),
                          START_DATE = as.Date( START_DATE, format = "%d.%m.%Y"), 
                          END_DATE = as.Date( END_DATE, format = "%d.%m.%Y")) %>%
                  filter( (START_DATE != END_DATE) | (is.na(END_DATE)) )  %>%
                  select( -DATA)
   #nrow( insurance)
  
   # remove observations with same client_id, contract-number and dfifferent CHANNEL, 
   dupl_id <- insurance %>% group_by( CLIENT_ID) %>% summarise(count = n()) %>%
     filter( count > 1) %>% 
     select(CLIENT_ID) %>% unique()
   
   dupl_id <-  as.numeric( unlist( dupl_id) )
   
   n1 <- nrow(insurance)
   for( i in (1: nrow(insurance)))
   { 
     if( (insurance[i,]$CLIENT_ID %in% (dupl_id)) && ( insurance[i,]$CHANNEL =="KG" ) )
     {
       #cat( i, "\n")
       insurance <-  insurance[-i,]
     }  
   }
   n2 <- nrow(insurance)
   cat(" Remove  duplicated rows, when one is from KG \n")
   cat( "Rows removed: ", n1 - n2, "\n\n")
   
   
   # set of people, who were called twice and finally accepted
   # pairs (id, date) of first call, should be removed, as they did not accept
   ids_repeated_calls <- insurance %>%
     group_by(CONTRACT_NUMBER, CLIENT_ID) %>%
     summarise( max_date = max(DATE), min_date  = min(DATE), n  = n ()) %>%
     filter( n > 1, max_date != min_date ) %>% 
     select( CONTRACT_NUMBER, CLIENT_ID, min_date)
   
   m1 = nrow(  insurance)
   insurance <- insurance[ -which( (insurance$DATE %in% ids_repeated_calls$min_date)&
                                     (insurance$CLIENT_ID %in% ids_repeated_calls$CLIENT_ID) ) ,] 
   m2 = nrow(  insurance)
   cat(" Remove pairs = ( client_id, date) who has not accepted (but accepted later) \n")
   cat( "Rows removed :", m1 - m2, "\n\n")
   
   cat("...Aggregating data...\n")
   insurance_aggr <- insurance %>%
     group_by( CONTRACT_NUMBER, CLIENT_ID, DATE, CHANNEL) %>%
     summarise(  
       CANCELED = (1 - as.numeric(is.na( min(END_DATE)))), # only first one!
       DURATION = as.numeric(sum( if_else(is.na(END_DATE),today(), END_DATE) - START_DATE)),
       TAKEN_BEFORE_COMM  = as.numeric( min(START_DATE) < min(DATE) ),
       DAYS_REACT =  ( as.numeric(min(START_DATE) - max(DATE))), 
       LAST_DURATION = as.numeric( if_else(is.na(max(END_DATE)),today(), max(END_DATE)) - max(START_DATE)),
       #LAST_END_DATE =  if_else(is.na(max(END_DATE)),today(), max(END_DATE)), 
       STILL_CONTINUE = as.numeric(if_else(is.na(max(END_DATE)),today(), max(END_DATE)) == today()),# LAST_END_DATE == today
       TARGET = as.numeric( (max(START_DATE) >= max(DATE))&     
                            (as.numeric(sum( if_else(is.na(END_DATE),today(), END_DATE) - START_DATE)) >0) ) ) %>% 
                            # start after date of comm and duration  > 0 
    filter(DAYS_REACT < 62 )  # too much time was after  START_DATE
    # think about bound 
  
     
   # CANCELED = if client has ever canceled 
   # DURATION = mean number of days beetwen acceptance and cancellation or NAn id hasnt canceled
   # TAKEN_BEFORE_COMM = if has ever taken before communication, by himself
   # DAYS_REACT = mean number of days between call and acceptance
   # LAST_DURATION = numer of days from today till last start_date
   # LAST_END_DATE = date of last end date, today if it still has not ended
   # STILL_CONTINUE = 1, if insurance is still on
   # !! TARGET: 1/0 = if insurance was taken after call, 
   #  !!          comment:  all clients in theese table besides large DAYS_REACT and TAKEN_BEFORE_COMM
   # filter days_react < L
   
  rm( insurance, dupl_id) 
  return( insurance_aggr )
 }  
 
# test
 #ins_file <- 
 #  "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-19 ins_table.csv"
 
# Q <- parse_insurance( insurance_file = ins_file)
  
 
####limits---- 
 
 #if (verbose)
 #  print("Preparing limits data....")
 
 parse_limits <- function( limits_file,
                           month_balance_file,
                           start_date = ymd("1970-01-01"),
                           end_date = today()) {
   ## limits_file: path to the file with limits_file( limits twice a month for every client)
   ## month_balance_file: file with balance by month end for every client
   ## Returns:
   ##limits_final : modified data frame 
 

 limits <- read.csv(file = limits_file , header = T, sep = ",", dec = ",")
 n <- nrow(limits)
 limits %<>% mutate( CLIENT_ID = client_id,
                     DT = as.Date( dateodb, format = "%Y-%m-%d"), 
                     LIM = as.numeric(gsub(",", "",limits$lim ))) %>% 
   select( -cl_iq, -dateodb, -lim, - client_id ) %>%
   distinct()
 cat("Initial rows number=: \n", n , "\n\n")     

 # two limits for one client and same date, what to do (2 contract_numbers for one cl_id) ? 
  limits %<>% group_by(CLIENT_ID, DT ) %>%
             summarise( LIM = max(LIM))
 
 n1 = nrow(limits)
 limits %<>% left_join( EVAL %>% select(CLIENT_ID, DATE), by = "CLIENT_ID") 
 limits %<>%  filter( DT < DATE)
 n2 = nrow(limits) # many DATES; 979058
 cat(" Keep only rows in which changes occured before communication, rows removed: \n",n1- n2 , "\n")
 

   limits_aggr <- limits %>% 
   group_by(CLIENT_ID, DATE) %>%
   arrange(DT) %>%
   mutate(CHNG = LIM - lag(LIM, n = 1, default = LIM[1])) %>%
   summarise( #LIM_AVG_N_CHANGES = mean(CHNG != 0),
             LIM_MAX_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                          max(CHNG[CHNG > 0]),
                                           0)/mean(LIM),
             LIM_MAX_POS_CHANGE_ABS = if_else(length(CHNG[CHNG > 0]) > 0,
                                          max(CHNG[CHNG > 0]),
                                          0),
             LIM_MIN_POS_CHANGE = if_else(length(CHNG[CHNG > 0]) > 0,
                                          min(CHNG[CHNG > 0]),
                                          0)/mean(LIM),
             LIM_MAX_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
                                          min(CHNG[CHNG < 0]),
                                          0)/mean(LIM),
            # LIM_MAX_NEG_CHANGE_ABS = if_else(length(CHNG[CHNG < 0]) > 0,
            #                              max(CHNG[CHNG < 0]),
            #                              0),
            # LIM_MIN_NEG_CHANGE = if_else(length(CHNG[CHNG < 0]) > 0,
             #                             min(CHNG[CHNG < 0]),
             #                             0),
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
             LIM_RANGE  = if_else( max(LIM) > min(LIM) , max(LIM)/ min(LIM), 1),
             DAYS_LAST_CHANGE = DT[which.max(CHNG > 0)]) %>%
   mutate(DAYS_LAST_CHANGE  =  as.numeric( DATE - DAYS_LAST_CHANGE) )%>%
   data.frame()
            
  # many NaN, only  for those who has limit == 0 
   limits_aggr %<>% 
     mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) # replace NANs with 0

 # calculate utilization, use month_balalnce file  
 month_bal <- read.csv(file = month_bal_file , header = T, sep = ";", dec = ",")
 month_bal %<>% mutate(CONTRACT_NUMBER = AR_NO, 
                       DT= as.Date( DT, format = "%d.%m.%Y")) %>%
                select( -AR_NO ) 

 # LAST VERTION:
 utill_old<- month_bal %>%
            inner_join( limits, by = c("CLIENT_ID","DT" ) ) %>%
            mutate( UT = if_else( LIM > 0, CRD_AMT/LIM, 0),
                    DLQ = if_else( LIM > 0, DLQ_AMT/LIM, 0),
                    DLQ_ABS =  DLQ)  %>%
            select( -DLQ_AMT, -CRD_AMT, -DEP_AMT, -LIM  ) %>% 
            group_by(CLIENT_ID, CONTRACT_NUMBER, DATE ) %>%
            summarise(# MAX_UT  =  max(UT),  MAX_DLQ =  max(DLQ),
                      #MIN_UT  =  min(UT) , MIN_DLQ =  min(DLQ),
                      #MEAN_UT =  mean(UT), MEAN_DLQ = mean(DLQ),
                      LAST_UT =  UT[which.max(DT)], 
                      MAX_DLQ = max(DLQ), 
                      MAX_DLQ_ABS = max(DLQ_ABS), 
                      DAYS_TILL_MONTH_END = as.numeric(max(DATE) - max(DT)) ) %>%
                      data.frame()

# COMM: LAST_UT has to 100% corr with  X1_UT, check!

 # NEW: UTILL TABLE
utill_tab <- month_bal %>%
  inner_join( limits, by = c("CLIENT_ID","DT" ) ) %>%
  mutate( UT = if_else( LIM > 0, CRD_AMT/LIM, 0))  %>%
  select( -DLQ_AMT, -CRD_AMT, -DEP_AMT, -LIM) %>% 
  #filter(  as.numeric(DATE - DT) < 200 ) %>%
  group_by(CLIENT_ID, CONTRACT_NUMBER, DATE ) %>%
  arrange( CLIENT_ID, CONTRACT_NUMBER, DATE, desc(DT)) %>% 
  slice(1:6) %>% # leave last 6 month
  mutate( MON_AGO = row_number(), UTIL = "UT") %>% 
  select( -DT) %>% 
  unite( col = UT_MOM_AGO, c("MON_AGO", "UTIL"), sep = "_") %>% 
  spread( key = UT_MOM_AGO, value = UT) %>%
  data.frame()
# UT = utilization = credit/limit
# DLQ = deliquency / limit, close to 0
# MIN_DLQ == 0

  limits_final <- limits_aggr %>% 
          left_join(utill_old, by = c("CLIENT_ID","DATE") )
          left_join(utill_tab, by = c("CLIENT_ID","DATE") )
  
  # may change, optional!
  limits_final %<>% select( -MEAN_UT, -MEAN_DLQ, - MIN_DLQ)
  
  rm( limits, limits_aggr, util)
  
  return(limits_final)
 } 
 
 # test: 
limits_file    <- 
   "C:/Users/Liksakov/Desktop/Liks/insurance/insur/model_data/2017-06-27 strahov_lim.csv"
  #  
   month_bal_file <- 
     "C:/Users/Liksakov/Desktop/Liks/insurance/insur/model_data/2017_06_23_balance.csv"
  # 
  # G <- parse_limits(limits_file = limits_file, month_balance_file = month_bal_file  )
  # 
  # 
###parse_balance----  
  
    parse_balance <- function( balance_file,
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
      
    
   balance <- read.csv(file = balance_file, header = T, sep = ",", dec = ",")
   balance %<>%  mutate( ST_BAL_DT  = as.Date(ST_BAL_DT, format = "%d.%m.%Y"), 
                         # same, need for aggSummaries function
                         TXN_DT = as.Date(ST_BAL_DT, format = "%d.%m.%Y"),
                        CONTRACT_NUMBER = AR_NO)  %>% 
                select( -AR_NO)
  n1 <- nrow(  balance)
  cat("Initial rows number:", n1,   "\n")  
 ## join aon EVAL, keep only rows in which changes occured before communication
 balance %<>% left_join( EVAL %>% select(CONTRACT_NUMBER,DATE), by = "CONTRACT_NUMBER")
 balance %<>%  filter( ST_BAL_DT < DATE)
 n2 = nrow(balance) # many DATES;  1819698
 cat("Remove observation after communication: \n")
 cat("Rows removed:", n1 - n2, "\n\n")

  balance %<>% 
    mutate(BAL = DEP_AMT - CRD_AMT - DLQ_AMT, DEBT = DLQ_AMT) %>%
    select(-DEP_AMT, -CRD_AMT, -DLQ_AMT)
  
  ## divide into two data frames: one for balance, one for debt
  ## and filter unncecessary rows
  just_blnc <-   balance %>% 
    select(-DEBT) %>%
    group_by(CONTRACT_NUMBER, DATE) %>%
    arrange((ST_BAL_DT)) %>%
    filter((BAL- lag(BAL, 1, default = BAL[1] + 1))!= 0) %>%
    ungroup()
  m1 = nrow( just_blnc);  #1815930
  cat("Only BALANCE change table, rows number =", m1, "\n")
  
  just_debt <-  balance %>% 
    select(-BAL) %>%
    group_by(CONTRACT_NUMBER, DATE) %>%
    arrange((ST_BAL_DT)) %>%
    filter((DEBT- lag(DEBT, 1, default = DEBT[1] + 1))!= 0) %>%
    ungroup()
  m2 = nrow( just_debt);  #90053
  cat("Only DEBT change table, rows number =", m2, "\n\n")
  # debt( DLQ) changes rarelly, usually 0 !

   just_blnc %<>%
    group_by(CONTRACT_NUMBER, DATE) %>%
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
     group_by(CONTRACT_NUMBER, DATE) %>%
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
  just_blnc2 <- createAggSummaries(just_blnc,
                   id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'),
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
  
  
  just_debt2 <- createAggSummaries(just_debt,
       id_columns = c('CLIENT_ID','CONTRACT_NUMBER', 'DATE'),
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
  
    ## most recent balances, debts 
  rcnt_blnc <- balance %>%
    group_by(CLIENT_ID,CONTRACT_NUMBER, DATE) %>%
    summarise(LAST_BAL = BAL[which.max(ST_BAL_DT)],
              LAST_DEBT = DEBT[which.max(ST_BAL_DT)])
  cat("Recent balance, rows number: ",  nrow(rcnt_blnc), "\n\n")
  
    balance_final <- EVAL %>%
      select( CLIENT_ID,CONTRACT_NUMBER, DATE) %>%
      left_join(just_blnc2, by = c('CLIENT_ID','CONTRACT_NUMBER', 'DATE')) %>%
      left_join(just_debt2, by = c('CLIENT_ID','CONTRACT_NUMBER', 'DATE')) %>%
      left_join(rcnt_blnc, by = c('CLIENT_ID','CONTRACT_NUMBER', 'DATE'))
  
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
    
       
###EVAL, initial base ----   
  
  parse_EVAL <-function( EVAL_file )  
  {
 EVAL<- read.csv(file = EVAL_file,
                 header = T, sep = ";", dec = ",", 
                 stringsAsFactors = FALSE)

 EVAL %<>% select( -TYPE_CRD, -NAME) %>%
           mutate( DATE = as.Date(DATA, format= "%d.%m.%Y"), 
                   DATE_OPEN =  as.Date(DATE_OPEN, format= "%d.%m.%Y"), 
                   CONTRACT_NUMBER = DOG) %>%
           filter( DATE_OPEN  < DATE) %>% # only cards? that were open before date of communication
           select( -DATA, -DOG)
 
 # remove 274-P-48377445 contract, 2 client ids!
 EVAL <-  EVAL[ -which(( EVAL$CONTRACT_NUMBER == "274-P-48377445")&(EVAL$CHANNEL == "CC")),]
 #EVAL <-  EVAL[ -which( EVAL$CONTRACT_NUMBER == "274-P-48377445"),]
 #length( unique( EVAL$CONTRACT_NUMBER))    #  54951
 #length( unique( EVAL$CLIENT_ID))          #  54948
 #nrow(EVAL)                               #  55180
 
 # remove rows where were to rows for one client, one is KG:
 # remove observations with same client_id, contract-number and dfifferent CHANNEL, 
 dupl_id <-  EVAL %>% 
   group_by( CLIENT_ID,CONTRACT_NUMBER ) %>% 
   summarise(count = n()) %>%
   filter( count > 1) %>% 
   select(CLIENT_ID) %>%
   unique() %>% 
   unlist()%>% 
   as.numeric() 
 
 n1 <- nrow( EVAL)
 for( i in (1: nrow(EVAL)))
 { 
   if( (EVAL[i,]$CLIENT_ID %in% (dupl_id)) && ( EVAL[i,]$CHANNEL =="KG" ) )
   {
     #cat( i, "\n")
     EVAL <-  EVAL[-i,]
   }  
 }
 n2 <- nrow( EVAL)
 cat(" Remove  duplicated rows, when one is from KG \n")
 cat( "Rows removed: ", n1 - n2, "\n\n")
 
 
 # remove cases where 2 contracts for one client id: leave only one 
 doubled_contr_numbers <- EVAL %>% 
              select(CLIENT_ID, CONTRACT_NUMBER) %>% 
              distinct() %>% 
              group_by(CLIENT_ID) %>% 
              summarise( n = n())%>% 
              filter(n > 1) %>% 
              select(CLIENT_ID)%>%
              unlist() %>%
              as.numeric()
 
  double_tmp_df <-  EVAL[ which(EVAL$CLIENT_ID %in% doubled_contr_numbers) ,] 
  double_tmp_df %<>% arrange(CLIENT_ID,  DATE)
  cat("Many contract numbers for 1 client id table:\n ")
  print( double_tmp_df )
  
 t <- double_tmp_df %>% select(CLIENT_ID, CONTRACT_NUMBER, DATE) %>%
                        arrange(CLIENT_ID, CONTRACT_NUMBER, DATE )%>%
                        #group_by(CONTRACT_NUMBER) %>% 
                        filter(CONTRACT_NUMBER != CONTRACT_NUMBER[which.min(DATE)])
                        
  m1 = nrow(EVAL)  #55117
  
  EVAL %<>% arrange(CLIENT_ID, CONTRACT_NUMBER, DATE)%>% 
            group_by(CLIENT_ID,DATE )%>% 
            dplyr:: slice(1) %>% 
            ungroup()
           
  m2 = nrow(EVAL) #55112
  cat(" Take only one (random) contract for one client id, rows removed: " ,m2 - m1, "\n\n")
  
  
  # make feature was comm before: 
  # BE CAREFUL: this is not feature, as all who were  called  before has target == 1
  # this column is just for info, remove it before modelling!
  SECOND_COMM <- EVAL %>% 
    group_by(CLIENT_ID, CONTRACT_NUMBER) %>% 
    summarise( CALL_COUNT = n() , DATE = max(DATE)) %>% 
    filter(CALL_COUNT > 1)  
  # DATE here = first communication date
  
  # SECOND_COMM  table with second comm ( as DATE = max(DATE)), so after 
  # join not NA only clients with 2 comm and with latest one, get  IS_BEEN_CALLED column
  # COMM_NUMBER = all comm  ( 2 for one client id), get  COMM_NUMBER
  EVAL %<>% left_join( SECOND_COMM, by = c("CLIENT_ID", "CONTRACT_NUMBER", "DATE") ) %>%
            left_join (EVAL %>%
                         group_by( CONTRACT_NUMBER) %>%
                         summarise( COMM_NUMBER = n()) %>% 
                         filter( COMM_NUMBER > 1), by =  "CONTRACT_NUMBER") %>% 
            mutate(IS_BEEN_CALLED = as.numeric( (!is.na(CALL_COUNT))), 
                   DAYS_FROM_OPEN = as.numeric(DATE - DATE_OPEN))  %>%
            select( -CALL_COUNT, -DATE_OPEN) %>%
            filter( DAYS_FROM_OPEN > 0)  
  EVAL$COMM_NUMBER[ is.na(EVAL$COMM_NUMBER)] <- 1
  
  # where person was load in base for call centre
  EVAL$GROUP = factor(EVAL$DATE, labels= c("1", "2", "3", "4", "5", "6", "7") )
  
    rm( SECOND_COMM )
  
    return(EVAL)
  }
  # test
 #   EVAL_file  <- 
 #    "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-26 EVAM.csv"
  
 #   G <- parse_EVAL(EVAL_file = EVAL_file )
 # was more than one communication 
 # t <-  EVAL %>% group_by( CONTRACT_NUMBER) %>% summarise( n = n()) %>% filter( n > 1) #230
 # s <-  EVAL %>% group_by( CLIENT_ID) %>% summarise( n = n()) %>% filter( n > 1) # 226
  

#source("helpers_liks.R")  

### parse_status function OLD----
parse_status_old <- function( status_file,
                             top_n_cols = list(),
                             start_date = ymd("1970-01-01"),
                             end_date = today()) {
  ## status_file: path to the file with status  
  ##  top_n_cols: list of named tuples of the form `c(COL_NAME=COL_NAME, 
  ##                                                     PERCENT=PERCENT)`
  ##    example:  top_n_cols = list(c('name' = 'CONTR_STATUS', 'percentage' = 0.02) )
  ## Returns:
  ## status_final : modified data frame 
  
  status <- read.csv(file = status_file , header = T, sep = ";", dec = ",")
  
  n1 = nrow(status)  # 502702
  cat( "Initial row number : ", n1 , "\n")
  # contract_number here is together  contr number and cart rbs number
  status %<>% mutate(  DATE = as.Date( DATA, format = "%d.%m.%Y"), 
                       AMND_DATE = as.Date(  AMND_DATE, format = "%d.%m.%Y"), 
                       CARD_CONTRACT = as.numeric( grepl("P",CONTRACT_NUMBER ))) %>%
    select( - DATA) %>% 
    distinct()  %>%
    filter( AMND_DATE <  DATE )
  # CARD_CONTRACT = if card = 0, contract = 1
  # leave only status change: 
  # remove DATE of communication to avoid dubblicates
  communication_table <-  status  %>% select(CLIENT_ID, CHANNEL,DATE) %>% distinct()
  # triples: (client, date of communication, channel)
  
  status  %<>%   arrange(CLIENT_ID, CONTRACT_NUMBER, AMND_DATE )
  status %<>%
    select( -DATE, -CHANNEL) %>%
    group_by(CLIENT_ID, CONTRACT_NUMBER, CARD_CONTRACT ) %>%
    arrange(  AMND_DATE) %>%
    mutate(CONTR_STATUS = as.character(CONTR_STATUS)) %>%
    filter(CONTR_STATUS != lag(CONTR_STATUS, 1, default = 'D'))
  n2 = nrow( status)   #    
  status$CONTR_STATUS %<>% as.factor
  cat( "Leave only status changes \n")
  cat( "Rows removed : ", n1 - n2 , "\n\n")
  
  # make independently: two status for cards and for contracts 
  status_card <- status %>% filter(CARD_CONTRACT == 0)
  status_contr <- status %>% filter(CARD_CONTRACT == 1)  
cat( "Use top N transformation for card/contract status " , "\n")
  status_card <- createTopNCols(df = status_card , top_n_cols = top_n_cols )
  status_contr <- createTopNCols(df = status_contr , top_n_cols = top_n_cols )
  # nearly only one status!
  #cat("0\n")
 # returns last status and last date of the status change( before Date of communication)
 #  comment: status has new levels ( from top_N - 1: count in frequency decreasing oreder ) 
 #  comment: last date of status change by all! cards and its status
  last_card_status <-  status_card  %>% 
    mutate( CARD_STATUS = CONTR_STATUS) %>% ### !!!!!!!
    group_by(CLIENT_ID) %>% 
    arrange(desc(AMND_DATE)) %>%
    mutate(N_ = 1:n()) %>%
    filter(N_ == 1) %>%
    select(-N_) %>% ungroup()  
  colnames( last_card_status)[3:4] <- c( "LAST_CARD_STATUS", "LAST_CARD_STATUS_CHANGE")
  #### slice(which.max(AMND_DATE))
  # same for contracts
  last_contr_status <-  status_contr  %>% 
    group_by(CLIENT_ID) %>% 
    arrange(desc(AMND_DATE)) %>%
    mutate(N_ = 1:n()) %>%
    filter(N_ == 1) %>%
    select(-N_) %>% ungroup()  
  colnames( last_contr_status)[3:4] <- c( "LAST_CONTR_STATUS", "LAST_CONTR_STATUS_CHANGE")
  #cat("1\n")
  
  # count times of most frequent top n status by card
  status_card %<>%  mutate( CARD_STATUS = CONTR_STATUS) %>%
    group_by(CLIENT_ID, CARD_STATUS)  %>%
    summarise( count = n()) %>%
    gather(., variable, value, CARD_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count)   
  
  #cat("2\n")
  
  # count times of most frequent top n status by contr
  status_contr %<>% group_by(CLIENT_ID,CONTR_STATUS)  %>%
    summarise( count = n()) %>%
    gather(., variable, value, CONTR_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count)     
  
  #cat("3\n")
  
  # table with count cards/contr;  mean diff days of status change
  status_aggr <- status %>% group_by(CLIENT_ID,CONTRACT_NUMBER,CARD_CONTRACT) %>% 
    summarise( count = n() , 
               mean_days_change = ifelse( (n()-1) < 1, 0,
                as.numeric( max(AMND_DATE) - min(AMND_DATE)) / ( n()-1))) %>%
    group_by(CLIENT_ID , CARD_CONTRACT)  %>% 
    summarise( MEAN_STATUS_COUNT = mean( count ), 
               CARD_CONTR_COUNT = n(), 
               MEAN_DAYS_CHANGE = mean(mean_days_change)) 
  
 # cat("4\n")
  status_aggr <-  data.table:: dcast(setDT(status_aggr),
                    CLIENT_ID ~ CARD_CONTRACT,
                    value.var = c("MEAN_STATUS_COUNT", "CARD_CONTR_COUNT","MEAN_DAYS_CHANGE")) 

 # MEAN_STATUS_COUNT = mean status card/contract changes( mean by card/contr number)  
 # CARD_CONTR_COUNT =  number of contr/cards
 # MEAN_DAYS_CHANGE = mean number of days between status change
 #  comment: mean is taken 2 times  first by card and then by client
 #  comment: if status has never changed , MEAN_DAYS_CHANG = 0
  
   cat("5\n")
  # join columns together by client_id
  status_final <- communication_table %>%
    left_join(  status_aggr,   by='CLIENT_ID' )  %>%
    left_join( last_card_status %>% 
                 select( -CONTRACT_NUMBER, -CARD_CONTRACT), by='CLIENT_ID' )  %>% 
    left_join( last_contr_status %>% select( -CONTRACT_NUMBER, -CARD_CONTRACT),
               by='CLIENT_ID' )  %>%  
    left_join(  status_card, by='CLIENT_ID' )  %>% 
    left_join(  status_contr, by='CLIENT_ID' )

  #cat("6\n")
  
  rm(  communication_table, 
  status_aggr,
  last_card_status,
  last_contr_status,
  status_card,
  status_contr,status )
  
  return(status_final)
}
 

# test 
status_file  <- 
  "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-26 status.csv"

 #top_n_cols = list(c('name' = 'CONTR_STATUS', 'percentage' = 0.05) )

# H <- parse_status( status_file = status_file, top_n_cols = top_n_cols ) 
 
### parse_status function----
parse_status <- function( status_file,
                              top_n_cols = list(),
                              start_date = ymd("1970-01-01"),
                              end_date = today()) {
  ## status_file: path to the file with status  
  ##  top_n_cols: list of named tuples of the form `c(COL_NAME=COL_NAME, 
  ##                                                     PERCENT=PERCENT)`
  ## example:  top_n_cols = list(c('name' = 'CONTR_STATUS', 'percentage' = 0.02) )
  ## Returns:
  ## status_final : modified data frame 
  
  status <- read.csv(file = status_file , header = T, sep = ";", dec = ",")
  
  n1 = nrow(status)  # 502702
  cat( "Initial row number : ", n1 , "\n")
  # contract_number here is together  contr number and cart rbs number
  status %<>% mutate(  DATA = as.Date( DATA, format = "%d.%m.%Y"), 
                       AMND_DATE = as.Date(  AMND_DATE, format = "%d.%m.%Y"), 
                       CARD_CONTRACT = as.numeric( grepl("P",CONTRACT_NUMBER ))) %>%
    distinct()  %>%
    filter( AMND_DATE <  DATA )
  status <-status %>% dplyr:: rename(DATE = DATA)
  # CARD_CONTRACT = if card = 0, contract = 1
  
  # leave only status change: 
  status  %<>%   arrange(CLIENT_ID, CONTRACT_NUMBER,DATE,AMND_DATE)
  status %<>%
    group_by(CLIENT_ID, CONTRACT_NUMBER, DATE, CARD_CONTRACT ) %>%
    arrange(  AMND_DATE) %>%
    mutate(CONTR_STATUS = as.character(CONTR_STATUS)) %>%
    filter(CONTR_STATUS != lag(CONTR_STATUS, 1, default = 'D')) %>%
    ungroup()
  n2 = nrow( status)   #    
  status$CONTR_STATUS %<>% as.factor
  cat( "Leave only status changes \n")
  cat( "Rows removed : ", n1 - n2 , "\n\n")
  
  # make independently: two status for cards and for contracts 
  status_card <- status %>% filter(CARD_CONTRACT == 0)
  status_contr <- status %>% filter(CARD_CONTRACT == 1)  
  cat( "Use top N transformation for card/contract status " , "\n")
  status_card <- createTopNCols(df = status_card , top_n_cols = top_n_cols )
  status_card  <- status_card[[1]]
  status_contr <- createTopNCols(df = status_contr , top_n_cols = top_n_cols )
  status_contr  <- status_contr[[1]]
  # nearly only one status!
  #cat("0\n")
  # returns last status and last date of the status change( before Date of communication)
  #  comment: status has new levels ( from top_N - 1: count in frequency decreasing oreder ) 
  #  comment: last date of status change by all! cards and its status
  last_card_status <-  status_card  %>% 
    #mutate( CARD_STATUS = CONTR_STATUS) %>% ### !!!!!!!
    dplyr:: rename(LAST_CARD_STATUS = CONTR_STATUS, 
                   LAST_CARD_STATUS_CHANGE = AMND_DATE)  %>% 
    group_by(CLIENT_ID, DATE) %>% 
    arrange(desc(LAST_CARD_STATUS_CHANGE)) %>%
    mutate(N_ = 1:n()) %>%
    filter(N_ == 1) %>%
    select(-N_) %>% 
    ungroup()  
  #colnames( last_card_status)[5:6] <- c( "LAST_CARD_STATUS", "LAST_CARD_STATUS_CHANGE")
  # ERROR: colnames differ ( because of top_n_percent)
  last_card_status %<>% mutate( LAST_CARD_STATUS_CHANGE = as.numeric( DATE - LAST_CARD_STATUS_CHANGE ), 
                                LAST_CARD_STATUS = as.factor(LAST_CARD_STATUS))
  #### slice(which.max(AMND_DATE))
  # same for contracts
  last_contr_status <-  status_contr  %>% 
    dplyr:: rename( LAST_CONTR_STATUS = CONTR_STATUS,
                    LAST_CONTR_STATUS_CHANGE = AMND_DATE)  %>%
    group_by(CLIENT_ID, DATE) %>% 
    arrange(desc(LAST_CONTR_STATUS_CHANGE)) %>%
    mutate(N_ = 1:n()) %>%
    filter(N_ == 1) %>%
    select(-N_) %>% ungroup()  
  # ERROR: colnames differ ( because of top_n_percent)
  #colnames( last_contr_status)[5:6] <- c( "LAST_CONTR_STATUS", "LAST_CONTR_STATUS_CHANGE")
  last_contr_status %<>% mutate( LAST_CONTR_STATUS_CHANGE = as.numeric( DATE - LAST_CONTR_STATUS_CHANGE ), 
                                 LAST_CONTR_STATUS = as.factor(LAST_CONTR_STATUS))
  #cat("1\n")
  
  # count times of most frequent top n status by card
  status_card %<>%  mutate( CARD_STATUS = CONTR_STATUS) %>%
    group_by(CLIENT_ID, DATE, CARD_STATUS)  %>%
    summarise( count = n()) %>%
    gather(., variable, value, CARD_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count)   
  
  #cat("2\n")
  
  # count times of most frequent top n status by contr
  status_contr %<>% group_by(CLIENT_ID,DATE,CONTR_STATUS)  %>%
    summarise( count = n()) %>%
    gather(., variable, value, CONTR_STATUS) %>%  
    unite(var, variable, value) %>% 
    spread(var, count)     
  
  #cat("3\n")
  
  # table with count cards/contr;  mean diff days of status change
  status_aggr <- status %>% group_by(CLIENT_ID,CONTRACT_NUMBER,DATE,CARD_CONTRACT) %>% 
    summarise( count = n() , 
               mean_days_change = ifelse( (n()-1) < 1, 0,
                                          as.numeric( max(AMND_DATE) - min(AMND_DATE)) / ( n()-1))) %>%
    group_by(CLIENT_ID , DATE, CARD_CONTRACT)  %>% 
    summarise( MEAN_STATUS_COUNT = mean( count ), 
               CARD_CONTR_COUNT = n(), 
               MEAN_DAYS_CHANGE = mean(mean_days_change)) 
  
  # cat("4\n")
  status_aggr <-  data.table:: dcast(setDT(status_aggr),
                                     fun.aggregate = sum,
                                     CLIENT_ID +DATE ~ CARD_CONTRACT,
                                     value.var = c("MEAN_STATUS_COUNT", "CARD_CONTR_COUNT","MEAN_DAYS_CHANGE")) 
  
  # MEAN_STATUS_COUNT = mean status card/contract changes( mean by card/contr number)  
  # CARD_CONTR_COUNT =  number of contr/cards
  # MEAN_DAYS_CHANGE = mean number of days between status change
  #  comment: mean is taken 2 times  first by card and then by client
  #  comment: if status has never changed , MEAN_DAYS_CHANG = 0
  
  cat("5\n")
  # join columns together by client_id, date
  status_final <- status %>% select(CLIENT_ID, DATE, CHANNEL) %>% 
    distinct() %>%
    left_join(  status_aggr,   by=c('CLIENT_ID', 'DATE') )  %>%
    left_join( last_card_status %>% 
                 select( -CONTRACT_NUMBER, -CARD_CONTRACT), by=c('CLIENT_ID', 'DATE', 'CHANNEL' ) )  %>% 
    left_join( last_contr_status %>% select( -CONTRACT_NUMBER, -CARD_CONTRACT),
               by=c('CLIENT_ID', 'DATE', 'CHANNEL') )  %>%  
    left_join(  status_card, by= c('CLIENT_ID', 'DATE' ) )  %>% 
    left_join(  status_contr, by= c('CLIENT_ID', 'DATE' ))
  
  #cat("6\n")
  
  rm(  status_aggr,
       last_card_status,
       last_contr_status,
       status_card,
       status_contr,
       status )
  
  return(status_final)
}
   
# test 
# status_file  <- 
#   "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-26 status.csv"
# 
#  top_n_cols = list(c('name' = 'CONTR_STATUS', 'percentage' = 0.05) )
# 
#  H <- parse_status( status_file = status_file, top_n_cols = top_n_cols ) 
 

### geo_data ----

  # addr_iq <- read.csv( file  = addr_iq_file , sep = ";", header = T)
  # addr_iq %<>% dplyr:: rename(IQ_ID = rep_clid ) %>%
  #   filter( ContactType < 13) %>%
  #   select(IQ_ID, AddrState,AddrCity )
      
# addr_iq_file <- "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-26 iq_geo.csv"
  


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
 
 parse_socdem_iq <- function(socdem_iq_file,
                             top_n_cols = list(), 
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
   sd_iq<- read.csv(file = soc_dem_iq_file, header = T, sep = ";", dec = ",", na.strings = c("","NA"))
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
   sd_iq <- createTopNCols(sd_iq, top_n_cols = top_n_cols)
   
   code_map <-  sd_iq[[2]]
   sd_iq <- sd_iq[[1]]

   
   saveRDS(object = code_map, 
                    file = "SD_IQ_codemap", 
                    ascii = FALSE, version = NULL,
                    compress = TRUE, refhook = NULL)
   
   # top N
   #summary( sd_iq)
   # CL_AMT_ZP /   CL_AMT_DOP/  REP_POPULATION  too large, may be outlyers
   return( sd_iq)
 }
 
 # test :
 #soc_sem_iq_file  <- 
 #  "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-23 socdem_IQ.csv"
 
 #H <- parse_socdem_iq( socdem_iq_file = socdem_iq_file, max_class_percent = 0.99, 
 #                      max_na_percent = 0.5)
 
 
 ### phones_DATA----
 
 # COUNT PHONENUMBER BY PHONE  TYPES 
 parse_phones <- function( phone_file)
 {
  phone <- read.csv( file = phone_file, header = T, sep = ",", dec = ".")
  phone %<>% dplyr:: rename( CLIENT_ID = ID)
  phone%<>% select( - PH)  %>%
      group_by( CLIENT_ID,TYPE_PH ) %>%
      summarise( count = n()) %>%
      gather(., variable, value, TYPE_PH) %>%  
      unite(var, variable, value) %>% 
      spread(var, count)   
  
  return(phone )
 }
  
  #TEST
  #phone_file <- "C:/Users/Liksakov/Desktop/Liks/insurance/model_data/2017-06-26 phones.csv"   
  # H <- parse_phones(phone_file )
 
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
  