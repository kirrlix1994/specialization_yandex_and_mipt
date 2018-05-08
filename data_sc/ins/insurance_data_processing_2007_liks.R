
# join all tables together 
## path to model data
data_folder <- '../model_data'

# functions for each dataset preprocessing
source( "insurance_proceccing_data_functions_2007_liks.R")

##FILES csv: -----
# ALL FILES NAMES IN DIRECTORY:
EVAL_file         <- glue("{data_folder}/2017-06-26 EVAM.csv")
trans_file        <- glue("{data_folder}/2017_06_23_transaction.csv")
# also need for transactions
mcc_table_file    <- glue("{data_folder}/mcc_v2.csv")
socdem_hf_file    <-  glue("{data_folder}/2017-06-20 soc_dem_hf.csv")
binbonus_file     <-  glue("{data_folder}/2017-06-22 Binbonus.csv")
commitions_file   <-  glue("{data_folder}/2017-06-19 ins_comis.csv")
ins_file          <-  glue("{data_folder}/2017-06-19 ins_table.csv")
limits_file       <-  glue("{data_folder}/2017-06-27 strahov_lim.csv")
# file is also need for limits, calculate utilization 
month_bal_file    <-  glue("{data_folder}/2017_06_23_balance.csv")
balance_file      <-  glue("{data_folder}/2017-06-27 ins_full_bal.csv")  
status_file       <-  glue("{data_folder}/2017-06-26 status.csv")
soc_dem_iq_file   <-  glue("{data_folder}/2017-06-23 socdem_IQ.csv")
phone_file        <-  glue("{data_folder}/2017-06-26 phones.csv")   
# address

# DATA LINK TO ORACLE DB----
# ALL THE SAME FILES IN ORACLE DATABASE:


###DEFINE_JOINS:-----
join_cols2 <- c('CLIENT_ID','CONTRACT_NUMBER', 'DATE')
join_cols3 <- c('CLIENT_ID', 'DATE') 
join_cols4 <- c('CLIENT_ID', 'DATE', 'CHANNEL') 
join_cols5 <- c('CLIENT_ID')
join_cols_all <- c('CLIENT_ID','CONTRACT_NUMBER', 'DATE', 'CHANNEL')

#LOADING_DATA----
#    LOAD_DATA  <- function()
#{
      
cat( "*********** DATA PROCESSING IS STARTED ***********\n\n\n" )
cat("*******Loading EVAM initial  dataset:******* \n")
  EVAL <- parse_EVAL( EVAL_file = EVAL_file)
  EVAL <- paste_names(  df =   EVAL , 
                        df_name = "EVAL", 
                        id_cols = c( "CLIENT_ID", "DATE" ,"CONTRACT_NUMBER", "CHANNEL"))

# trans
  cat("*******Loading TRANSACTION:******* \n")
trans_top_n_cols = list(
     c('name' = 'TXN_TP', 'percentage' = 0.05), 
     c('name' = 'TXN_CHN', 'percentage' = 0.05),
     #c('name' = 'TXN_CTR', 'percentage' = 0.02),
    #c('name' = 'TXN_MCC', 'percentage' = 0.03), 
     c('name' = 'CATEGORY', 'percentage' = 0.03))

trans_aggr_cols <- list('TXN_RUB_AMT',
                        #c(7, 31, 90, 180, 360), 
                        c(31, 180),
                       #list('TXN_DIR','TXN_TP', 'TXN_CHN','TXN_CTR',
                       #     'TXN_MCC','RUB_TR', 'CATEGORY', 'FLG_CATEGORY'),
                         list('TXN_TP', 'TXN_CHN','FLG_CATEGORY','CATEGORY'),
                         funs(MEAN = mean, MAX = max, N = length, SUM = sum),
                         id_columns = c('CLIENT_ID', 'CONTRACT_NUMBER', 'DATE'))


  TRANS <-parse_TRANSACTION_MAIN( trans_file = trans_file,
                                  mcc_table_file  = mcc_table_file,
                                  top_n_cols = trans_top_n_cols,
                                  agg_cols = trans_aggr_cols)

   # COMMENT: for such data (loke transactions) NA means actually 0, none
   # example: client hasnt by anything during n days in such category, with means hi spent sum = 0, not NA
  
   # do it with transactions to drop columns where almost all values == 0 (==NA)
   # and the fill the rest NA with 0 
  TRANS <- remove_NA_features( TRANS, 
                               max_na_percent = 0.8, 
                               print_table = TRUE) 
  TRANS[is.na(TRANS)] <- 0 
  TRANS <- omit_corr_feat(df = TRANS, 
                          top_n_corr = 100,
                          corr_threshold = 0.98,
                          omit_action = TRUE,
                          print_info = TRUE,
                          print_plot = FALSE, 
                          plot_names = FALSE)   
  
  TRANS <- paste_names( df =  TRANS , 
                        df_name = "TRANS", 
                        id_cols = c( "CLIENT_ID", "DATE" ,"CONTRACT_NUMBER"))
  
  cat("*******Loading  SOCDEM HUMAN FACTOR dataset:******* \n")
  # socdem_hf
  socdem_hf_top_n_cols = list(c('name' = 'MARITAL_STATUS', 'percentage' = 0.05), 
                              c('name' = 'EDUCATION_LEVEL', 'percentage' = 0.05),
                              c('name' = 'OCCUPATION', 'percentage' = 0.05), 
                              c('name' = 'BRANCH', 'percentage' = 0.05))
                     
  SD_HF <- parse_socdem_hf(socdem_hf_file, 
                           top_n_cols =  socdem_hf_top_n_cols,
                           max_class_percent = 0.98, 
                           max_na_percent = 0.9)
    
  ##  SD data is uncorrelated 
  SD_HF <- paste_names( df =   SD_HF , 
                        df_name = "SD_HF", 
                        id_cols = c( "CLIENT_ID", "DATE" ,"CHANNEL", 
                                     "IQ_ID", "HID_PARTY"))
  
  cat("*******Loading  BINBONUS LOYALTY data:******* \n")
  BONUS <- parse_binbonus(binbonus_file = binbonus_file)
  BONUS[is.na( BONUS)] <- 0 
  BONUS <- omit_corr_feat(BONUS, omit_action = FALSE, top_n_corr = 10, 
                          corr_threshold = 0.97, print_plot = TRUE)
  BONUS <- paste_names( BONUS, df_name = "BONUS", 
                        id_cols = c("CLIENT_ID", "DATE" ,"CHANNEL") )
  
  
  cat("*******Loading  INSURANCE COMMITIONS dataset:******* \n")
  COMMITIONS <- parse_commitions(commitions_file =commitions_file, short_form = FALSE)
  COMMITIONS[is.na(  COMMITIONS)] <- 0
  COMMITIONS <- paste_names(  COMMITIONS, df_name = "COMM", 
                        id_cols = c("CLIENT_ID","CONTRACT_NUMBER", "DATE" ,"CHANNEL") )
  # BE CAREFUL!
  #y_columns_commitions<-c(colnames(COMMITIONS)[7:ncol( COMMITIONS)])
  y_columns_commitions <- 
    colnames(COMMITIONS) [ which( sapply( colnames(COMMITIONS), function(x) grepl("2017", x) ) ) ]
  y_columns_commitions <- c(y_columns_commitions, "COMM.CANC_N_AFTER", "COMM.CANC_SUM_AFTER")
  
  cat(" COMMITIONS columns which are from future \n( after communication, do not use for model):\n",
      y_columns_commitions, "\n")
  
  #grep("[0-9]+")
  
  
  cat("*******Loading  INSURANCE  dataset******* \n")
  cat( "( start and end cancellation date):\n")
  INSURANCE <- parse_insurance(insurance_file = ins_file) 
  #INSURANCE[is.na(  INSURANCE)] <- 0       
  INSURANCE <- omit_corr_feat( INSURANCE, omit_action = FALSE, top_n_corr = 10, 
                               corr_threshold = 0.99, print_plot = T)
  # remove ideally correlated pairs 
  INSURANCE  <- paste_names(   INSURANCE, df_name = "INS", 
                              id_cols = c("CLIENT_ID","CONTRACT_NUMBER", "DATE", "CHANNEL") )
  # BE CAREFUL:
  #y_columns_insurance<-c(colnames(INSURANCE)[c(4,5,7,8,9)])
  y_columns_insurance <- colnames(INSURANCE) [ which( sapply( colnames(INSURANCE), function(x) grepl("INS.", x) ) ) ]
  y_columns_insurance <-  y_columns_insurance[  which( !(  y_columns_insurance%in% c("INS.TAKEN_BEFORE_COMM","INS.TARGET") ) )] 
  cat(" INSURANCEcolumns which are from future \n( after communication, do not use for model):\n",
      y_columns_insurance, "\n")
  
  cat("*******Loading  LIMITS  dataset******* \n")
  LIMITS <- parse_limits( limits_file =limits_file, 
                          month_balance_file = month_bal_file)
  LIMITS  <- omit_corr_feat(LIMITS , omit_action = FALSE, top_n_corr = 25, 
                               corr_threshold = 0.99, print_plot = TRUE)
  LIMITS <- paste_names(  LIMITS, df_name = "LIM", 
                          id_cols = c("CLIENT_ID","CONTRACT_NUMBER", "DATE") )
  
  # test comm
  cat("*******Loading   BALANCE  dataset******* \n")
  BALANCE <- parse_balance(balance_file = balance_file)
  BALANCE <- remove_NA_features(  BALANCE, max_na_percent = 0.99, print_table = TRUE) 
  BALANCE[is.na( BALANCE)] <- 0 
  BALANCE <- omit_corr_feat(BALANCE,omit_action = FALSE, top_n_corr = 20, 
                             corr_threshold = 0.99, print_plot = T, 
                             plot_names = F)
  BALANCE <-  paste_names(  BALANCE, df_name = "BAL", 
                            id_cols = c("CLIENT_ID","CONTRACT_NUMBER", "DATE", "CHANNEL") )
  
  
  cat("*******Loading   STATUS  dataset******* \n")
  status_top_n_cols <-list(c('name' = 'CONTR_STATUS', 'percentage' = 0.02) )

  STATUS <- parse_status( status_file = status_file, 
                          top_n_cols =  status_top_n_cols )
  STATUS <- remove_NA_features( STATUS, max_na_percent = 0.98, print_table = TRUE) 
  STATUS[is.na(STATUS)] <- 0
  STATUS <- omit_corr_feat( STATUS,omit_action = FALSE, top_n_corr = 20, 
                            corr_threshold = 0.99, print_plot = TRUE, 
                            plot_names = TRUE)
  STATUS  <-  paste_names( STATUS , df_name = "STATUS", 
                            id_cols = c("CLIENT_ID","DATE", "CHANNEL") )
  
  cat("*******Loading   SOCDEM IQ  dataset******* \n")
  
  socdem_iq_top_n_cols = list(c('name' = 'CL_INDUSTR', 'percentage' = 0.03),
                              c('name' = 'CL_SOC_ST', 'percentage' = 0.05),
                              c('name' = 'CL_ESTATE_TYPE', 'percentage' = 0.05))
  
  SD_IQ <- parse_socdem_iq(socdem_iq_file = soc_dem_iq_file, 
                           top_n_cols =  socdem_iq_top_n_cols,
                           max_class_percent =0.99, 
                           max_na_percent = 0.5)
  
  SD_IQ <- omit_corr_feat(SD_IQ,omit_action = F, top_n_corr = 15, 
                          corr_threshold = 0.99, print_plot = T, 
                          plot_names = T)
  
  SD_IQ <-  paste_names( SD_IQ , df_name = "SD_IQ", 
                          id_cols = c("IQ_ID") )
  
  # address
  cat("*******Loading ADDRESS IQ  dataset******* \n")
  
  
  # phones
  cat("*******Loading PHONES dataset******* \n")
  PHONES <- parse_phones(phone_file =  phone_file)
  PHONES[is.na(PHONES)] <- 0
  PHONES <-  paste_names(  PHONES , df_name = "PH", 
                         id_cols = c("CLIENT_ID") )
  
  cat("\n\n\n")
  cat(" **********ALL DATASETS ARE LOADED **********")
  cat("\n\n\n\n\n")
  
   # }  
  #    LOAD_DATA( )
 
###JOIN_TABLES-----
  
  cat(" **********JOIN ALL DATA TOGETHER **********\n\n")
  
  FEATURE_TABLE <- data.frame() 
  FEATURE_TABLE <- EVAL
  
  #FEATURE_TABLE %<>%  left_join( TRANS, by = join_cols2 )
  # 283 new rows !!
  FEATURE_TABLE %<>%  left_join( SD_HF, by = join_cols4 )
  FEATURE_TABLE %<>%  left_join_NA( BONUS, by = join_cols4, value = 0  )
  FEATURE_TABLE %<>%  left_join_NA( COMMITIONS, by = join_cols_all, value = 0 )
  FEATURE_TABLE %<>%  left_join_NA( INSURANCE, by = join_cols_all, value = 0)
  FEATURE_TABLE %<>%  left_join( LIMITS, by = join_cols2  )
  FEATURE_TABLE %<>%  left_join_NA( BALANCE, by = join_cols2, value = 0 )
  FEATURE_TABLE %<>%  left_join( STATUS, by = join_cols4 )
  # мы заменили на INNER пока (но потеряли 300 человек со всеми NA в соцдем_IQ, но если LEFT, то будут лишние 700)
  # чтобы найти эти 300; количество id-iq для каждого client_id == 1 и все NA в SD_IQ)
  FEATURE_TABLE %<>%   inner_join( SD_IQ, by = "IQ_ID" )
  FEATURE_TABLE %<>%  left_join_NA( PHONES, by = "CLIENT_ID" , value = 0)
  FEATURE_TABLE %<>%  left_join_NA( TRANS, by = join_cols2 , value = 0)
  
  #FEATURE_TABLE %>% dim 
  # 54740 310
  
  cat(" **********FEATURE TABLE IS READY **********")
  
###CHECKS------
  
    # REMOVE ROWS WHERE 1 CLIENT ID AND  many iIQ_ID AND MORE THAB ONE ROW IS JOINED
    # LEAVE ONLY ONE ROW WHICH IS MORE FILLED IN
  
    #0. TARGET VARIABLES: 
    # I (INS.DURATION > 0) ?? 
    # COMM.CANC_SUM_AFTER (devide by month count from connection)
    #FEATURE_TABLE %<>% mutate( TARGET = as.numeric(INS.DURATION > 0 ))
  
  
  #1. NA_table and MAX ONE CLASS
    NA_TABLE <- remove_NA_features(df =FEATURE_TABLE, max_na_percent = 0.9,print_table = T)
    #COMM NUMBER AND IS_BEEN_CALLED are rare 
    #FEATURE_TABLE <-remove_ONE_CLASS_features(df =   FEATURE_TABLE, max_class_percent = 0.99 )
    # COMMENT1: many NA in commitions/insuramce/ balance debt = ok
    # COMMENT2: same NA percent in TRANS/BALANCE - some percent of data is missing
    #           for example TRANS: ~3500 do not have trans at all
    # Only SOC DEM DATA has NA 
    # SD data are bigger, some rows are omitted
  rm(NA_TABLE)
  
  # 2.CORRELATIONS
    #FEATURE_TABLE <- omit_corr_feat(df = FEATURE_TABLE, top_n_corr = 50, corr_threshold = 0.98,
     #                             omit_action = TRUE, print_plot = FALSE, plot_names = FALSE)
       # Columns removed: 24 
    
    #3. DATA TYPES: 
        DATA_TYPE_TABLE <- data.frame()   
        #DATA_TYPE_TABLE <- apply( FEATURE_TABLE,2, function(x) class(x) )
        #DATA_TYPE_TABLE <- sapply( 1: ncol(FEATURE_TABLE), function(x) class(x) )
        for( i in (1: ncol(FEATURE_TABLE)))
        {
          DATA_TYPE_TABLE[i,1] <- class(FEATURE_TABLE[,i])
          rownames(DATA_TYPE_TABLE)[i] <- colnames(FEATURE_TABLE)[i]
        }
          
        # after top N may be numeric columns
        
     #4.  columns_types( ids, targets, x):
      ids_columns <-c( "CLIENT_ID","CONTRACT_NUMBER","DATE","IQ_ID", "HID_PARTY")
      #ids_columns %in% colnames( FEATURE_TABLE)
      
      y_columns<-c( y_columns_commitions,  y_columns_insurance)
      #y_columns %in% colnames( FEATURE_TABLE)
      
      # male one ID   = CLIENT_ID_DATE      
       FEATURE_TABLE %<>% mutate( ID = paste0(CLIENT_ID, "_", DATE) )
      
      
       # NEED filters ( with y cols) for: 
       # NEXT can try (money > 0): "COMM.CANC_N_AFTER" > 0
       FEATURE_TABLE1 <- FEATURE_TABLE %>% 
         filter(  (((INS.DURATION > 45)&(INS.TARGET == 1)) | 
                    (INS.TARGET == 0 ) ), # differ for differernt groups # 1025 out
                  ( ((INS.STILL_CONTINUE == 1)&(INS.TARGET == 1)  ) |
                  (INS.TARGET == 0 ) ),  # 318  out !
                   INS.DAYS_REACT <  32  # 813  out  
                  ) 
      dim( FEATURE_TABLE1); 
       dim( FEATURE_TABLE)
      
      g <- ggplot( data = FEATURE_TABLE1 %>% filter(INS.TARGET ==1),
                   aes( x = INS.DURATION, fill = CHANNEL)) +
           geom_histogram( bins  = 100) + facet_wrap( ~ factor(DATE)) +
           ggtitle( "DURATION by CHANNEL and DATE of communication( days)")
      g
      # good 
      
       FEATURE_TABLE1  <- FEATURE_TABLE1[ which( !(FEATURE_TABLE1$CHANNEL == "KG")) , ]
       FEATURE_TABLE1 <- FEATURE_TABLE1[ ,which( (!(colnames(FEATURE_TABLE1) %in% c(y_columns, ids_columns) ))) ] 
       
        dim( FEATURE_TABLE1) #- 2400
       # 50206   273
      
        write.csv(file = "res_files/FEATURE_TABLE_clean.csv",
                  x = FEATURE_TABLE1, row.names = FALSE)
        
        
      # devide addirional data( after) and daya_matrix for model:
      # also remove KG data: 
      n1 = nrow(FEATURE_TABLE)
      ADDIT_TABLE <- FEATURE_TABLE[ ,c( which((colnames(FEATURE_TABLE) %in% c( ids_columns,  y_columns, "ID"))))]  
      FEATURE_TABLE  <-  FEATURE_TABLE[ which( !(FEATURE_TABLE$CHANNEL == "KG")) , ]
      FEATURE_TABLE <- FEATURE_TABLE[ ,which( (!(colnames(FEATURE_TABLE) %in% c(y_columns, ids_columns) ))) ] 
      n2 = nrow(FEATURE_TABLE)
      cat( n1 - n2, "\n")
      cat( n2, "\n")
      
      # remove datasets:
      #rm()
      
###DAYS normalization-----
      
    write.csv(file = "res_files/ADDIT_TABLE.csv", x =  ADDIT_TABLE, row.names = FALSE)
    write.csv(file = "res_files/FEATURE_TABLE_v1.csv", x = FEATURE_TABLE, row.names = FALSE)
    
    
    
     
    
    
  
  
  
  
  