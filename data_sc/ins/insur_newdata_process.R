
#************* Prosess newdata **********************

## libraries----

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

###----
source("insurance_proceccing_newdata_functions_liks.R")
source("parse_transactions.R")


### -------------
## path to model data
data_folder <- '../data_04_2018'

# functions for each dataset preprocessing
#source( "insurance_proceccing_data_functions_2007_liks.R")
#  source( )

##FILES csv: -----
# ALL FILES NAMES IN DIRECTORY:
#EVAL_file         <- glue("{data_folder}/2017-06-26 EVAM.csv")
trans_file        <- glue("{data_folder}/trans.csv")
# also need for transactions
mcc_table_file    <- glue("{data_folder}/mcc_v2.csv")
socdem_hf_file    <-  glue("{data_folder}/sd_hf.csv")
limits_file       <-  glue("{data_folder}/lim.csv")
# file is also need for limits, calculate utilization 
month_bal_file    <-  glue("{data_folder}/bal_month.csv")
balance_file      <-  glue("{data_folder}/full_bal.csv")  
soc_dem_iq_file   <-  glue("{data_folder}/sd_iq.csv")


# DATA LINK TO ORACLE DB----
# ALL THE SAME FILES IN ORACLE DATABASE:


###DEFINE_JOINS:-----
join_cols2 <- c('CLIENT_ID','CONTRACT_NUMBER', 'DATE')
join_cols3 <- c('CLIENT_ID', 'DATE') 
join_cols4 <- c('CLIENT_ID', 'DATE', 'CHANNEL') 
join_cols5 <- c('CLIENT_ID')
join_cols_all <- c('CLIENT_ID','CONTRACT_NUMBER', 'DATE', 'CHANNEL')

### trans ------------------



## soc_dem HF ------------
cat("*******Loading  SOCDEM HUMAN FACTOR dataset:******* \n")
# socdem_hf
socdem_hf_top_n_cols = list(c('name' = 'MARITAL_STATUS', 'percentage' = 0.05), 
                            c('name' = 'EDUCATION_LEVEL', 'percentage' = 0.05),
                            c('name' = 'OCCUPATION', 'percentage' = 0.05), 
                            c('name' = 'BRANCH', 'percentage' = 0.05))

SD_HF_new <- parse_socdem_hf_new(socdem_hf_file, 
                             #top_n_cols =  socdem_hf_top_n_cols,
                             max_class_percent =  0.9, 
                             max_na_percent = 0.6, 
                             omit_cols = TRUE)

SD_HF_new <- paste_names( df =   SD_HF_new , 
                      df_name = "SD_HF", 
                      id_cols = c( "CLIENT_ID"))

#  SD_HF_new %>% View()

# saveRDS(object = SD_HF_new_codemap, 
#         file = "SD_HF_new_codemap", 
#         ascii = FALSE, version = NULL,
#         compress = TRUE, refhook = NULL)
# 
# tmp1 <- readRDS(file = "SD_HF_codemap", refhook = NULL)
# tmp2 <- readRDS(file = "SD_HF_new_codemap", refhook = NULL)


## soc dem IQ ------------
SD_IQ_new <- parse_socdem_iq_new(socdem_iq_file, 
                                 #top_n_cols =  socdem_hf_top_n_cols,
                                 max_class_percent =  0.9, 
                                 max_na_percent = 0.6, 
                                 codemap_file = "SD_IQ_codemap")
                                 
SD_IQ_new <-  paste_names( df =   SD_IQ_new , 
                           df_name = "SD_IQ", 
                           id_cols = c( "CLIENT_ID", "IQ_ID"))

# SD_IQ_new  %>% View()

## balance ---------------

BALANCE_new <- parse_balance_new(balance_file = balance_file) 
                                 #DATE = as.Date('01.11.2017', format = "%d.%m.%Y"))
BALANCE_new <- remove_NA_features(  BALANCE_new, max_na_percent =1.1, print_table = TRUE) 
BALANCE_new[is.na( BALANCE_new)] <- 0 
#BALANCE <- omit_corr_feat(BALANCE_new, omit_action = FALSE, top_n_corr = 20, 
#                          corr_threshold = 0.99, print_plot = T, 
#                          plot_names = TRUE)

BALANCE_new <-  paste_names(  BALANCE_new, df_name = "BAL", 
                             id_cols = c("CLIENT_ID","CONTRACT_NUMBER") )

# BALANCE_new %>% View()


## limits ---------------

LIMITS_new <- parse_limits_new( limits_file =limits_file, 
                                month_balance_file = month_bal_file)

LIMITS_new  <- omit_corr_feat(LIMITS_new , omit_action = FALSE, top_n_corr = 25, 
                          corr_threshold = 0.99, print_plot = TRUE)

# clients with null limits are removed (~1000)
LIMITS_new <- paste_names(  LIMITS_new, df_name = "LIM", 
                            id_cols = c("CLIENT_ID","CONTRACT_NUMBER") )
# LIMITS_new  %>% View()

## trasns ---------------
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


TRANS_new <-parse_TRANSACTION_MAIN_new( trans_file = trans_file,
                                  mcc_table_file  = mcc_table_file,
                                  top_n_cols = trans_top_n_cols,
                                  agg_cols = trans_aggr_cols,
                                  codemap_file = "TRANS_codemap")
  

TRANS_new[is.na(TRANS_new)] <- 0 

TRANS_new <- omit_corr_feat(df = TRANS_new, 
                        top_n_corr = 30,
                        corr_threshold = 0.98,
                        omit_action = FALSE,
                        print_info = TRUE,
                        print_plot = FALSE, 
                        plot_names = FALSE)   


TRANS_new %<>% ungroup() %>% select(-DATE)
TRANS_new <- paste_names( df =  TRANS_new , 
                      df_name = "TRANS", 
                      id_cols = c( "CLIENT_ID","CONTRACT_NUMBER"))

# TRANS_new   %>% View()

## join all -----
join_cl <- c("CLIENT_ID")
join_pair  <- c( "CLIENT_ID", "CONTRACT_NUMBER")
     
#  OMIT clients without trans ( 55 000 from 87 000 have at least one transaction)
#  omit clients without limits ( new clients, ~1000)
 
#LIMITS_new

   FEATUER_MATRIX <- TRANS_new %>%
          left_join(SD_HF_new, by = join_cl) %>%
          left_join(SD_IQ_new, by = join_cl) %>%
          left_join_NA(BALANCE_new, by = join_pair, value = 0) %>%
          inner_join(LIMITS_new, by = join_pair)
          
        


## features to select ----


## writing feature matrix----
 
file_to_write <- glue("{data_folder}/FEATURE_MATRIX_NEW.csv")
write.csv( file = file_to_write,x = FEATUER_MATRIX, row.names=FALSE)

  
  







