library(ggplot2)
library(plotly)
library(magrittr)
library(dplyr)
library(eeptools)
library(tidyr)
library(corrplot)
library(stringr)
library(lubridate) # for working with date objects
library(data.table)
library(glue)
library(dummies)
library(xgboost)


source("proceccing_data_functions.R")
source("model_functions.R")

### READ_DATA -----------

 data <- fread("credit_data_2.csv", sep = "," )
# data  %>% View()
 
#### TRANSFORM_DATA ------
 
 # set data types:
 data %<>% 
   mutate( DT = as.Date(DT, format = "%d.%m.%Y"), 
           CL_ID = as.integer(CL_ID),
           OPEN_DT = as.Date(OPEN_DT, format = "%d.%m.%Y"), # cannot calc n = today - open_dt : for each obs it is different
           LIM = as.numeric(LIM), 
           BANK = as.integer( ifelse(BANK == "ББКК",1,(ifelse(BANK == "БинБанк" ,0, NA)))),
           DEP_AMT = as.numeric(gsub(",", ".", DEP_AMT)), 
           CRD_AMT = as.numeric(gsub(",", ".", CRD_AMT)),
           DLQ_AMT = as.numeric( gsub(",", ".", DLQ_AMT)),
           CRD_CNT = as.integer(CRD_CNT),
           LAST_CRD_EXPR = as.Date( LAST_CRD_EXPR, format = "%d.%m.%Y"),
           LAST_CRD_ACT = as.Date( LAST_CRD_ACT, format = "%d.%m.%Y"),
           AR_ST = as.factor(ifelse( AR_ST ==  100000021, "OK",
                             (ifelse( AR_ST == 100000023 ,"LIGHT_BLOC", "BLOC")))),
           TXN_CNT  = as.numeric( TXN_CNT), 
           TXN_AMT  = as.numeric(gsub(",", ".", TXN_AMT)), 
           LN_TXN_CNT  = as.numeric(gsub(",", ".", LN_TXN_CNT)), 
           LN_TXN_AMT  = as.numeric( gsub(",", ".", LN_TXN_AMT)), 
           CR_CNT  = as.numeric(gsub(",", ".", CR_CNT)), 
           CR_AMT  = as.numeric(gsub(",", ".", CR_AMT)), 
           POS_CNT  = as.numeric( gsub(",", ".", POS_CNT)), 
           POS_AMT  = as.numeric(gsub(",", ".", POS_AMT)), 
           MT_CNT  = as.numeric( gsub(",", ".", MT_CNT)), 
           MT_AMT  = as.numeric(gsub(",", ".", MT_AMT)), 
           ATM_CNT  = as.numeric( gsub(",", ".", ATM_CNT)), 
           ATM_AMT  = as.numeric(gsub(",", ".", ATM_AMT)),  
           FEE  = as.numeric(gsub(",", ".", FEE)),  
           PROC  = as.numeric(gsub(",", ".", PROC)),  
           POS_INT  = as.numeric(gsub(",", ".", POS_INT)), 
           #AGE = 2018  - year( as.Date(REP_BDATE, format = "%d.%m.%Y")), 
           QTY_CHLD = bound_feat( as.integer(QTY_CHLD), upper_bound = 8),
           CL_WORK_YEARS  = as.integer(CL_WORK_YEARS),
           CL_WORK_YEARS = as.integer(CL_WORK_YEARS),
           CL_AMT_ZP = as.numeric( gsub(",", ".", CL_AMT_ZP)),
           CL_AMT_DOP = as.numeric( gsub(",", ".", CL_AMT_DOP)), 
           CL_WORK_YEARS = as.integer( gsub(",", ".", CL_WORK_YEARS)), 
           CL_INDUSTR = as.factor( ifelse(CL_INDUSTR == "", NA, CL_INDUSTR )),
           REP_POPULATION = as.numeric(REP_POPULATION),
           AGE_1 =  bound_feat (2018  - year( as.Date(BIRTHDATE, format = "%d.%m.%Y")), low_bound = 15),
           AGE_2 =  bound_feat (2018  - year( as.Date(REP_BDATE, format = "%d.%m.%Y")), low_bound = 15),
           GENDER = as.integer(GENDER),
           MARITAL_STATUS = as.factor( cut_rare_values( MARITAL_STATUS, 0.04)),
           EMPLOYEE_FLAG = as.integer(EMPLOYEE_FLAG),
           OWN_CAR_FLAG = as.integer( OWN_CAR_FLAG ), 
           #VIP_FLAG = as.integer( VIP_FLAG), 
           OCCUPATION = as.factor(cut_rare_values( OCCUPATION, 0.04)),
           #UT = ifelse( as.numeric(LIM) > 0 ,as.numeric(gsub(",", ".", CRD_AMT))/as.numeric(LIM), NA),
           UT = round( bound_feat( as.numeric(gsub(",", ".", CRD_AMT))/as.numeric(LIM), 
                                   upper_bound = 1, replacement = 1),5),
           DLQ_AMT_IND  = as.integer(as.numeric(gsub(",", ".", DLQ_AMT)) > 0),
           CL_INDUSTR = as.factor(cut_rare_values(  CL_INDUSTR, 0.02 )),
           CL_FAM_ST =  as.factor(cut_rare_values(  str_trim(CL_FAM_ST), 0.04)),
           CL_EDU   = as.factor( cut_rare_values(ifelse(  CL_EDU == "", NA,  CL_EDU),0.02)),
           CL_OFF_POS  = as.factor( cut_rare_values(ifelse(  CL_OFF_POS == "", NA, CL_OFF_POS ),0.02)),
           CL_OWN_TYPE =  as.factor( cut_rare_values(ifelse( CL_OWN_TYPE == "", NA, CL_OWN_TYPE  ),0.02)),
           CL_SOC_ST  = as.factor( cut_rare_values(ifelse(   CL_SOC_ST  == "", NA,  CL_SOC_ST  ),0.02)),
           CL_ESTATE_TYPE =  as.factor( cut_rare_values(ifelse( CL_ESTATE_TYPE  == "", NA,CL_ESTATE_TYPE  ),0.02)),
           NM =  as.factor( cut_rare_values( NM, 0.03 )),
           GRP = as.factor( cut_rare_values( GRP, 0.03 )),
           TP = as.factor(TP),
           PORTRAIT  = as.factor( PORTRAIT),
           REP_SEX = as.integer( ifelse(REP_SEX == 'M', 1, (ifelse(REP_SEX == 'F', 0, NA)))),
           EDUCATION_LEVEL = as.factor(cut_rare_values( data$EDUCATION_LEVEL,0.03))
           ) %>% 
   select( -AR_ID, -ACC_NO, -CL_WORK_MONTH, -CL_CCY_ZP, -CL_CCY_DOP,-REP_BDATE, -BIRTHDATE, 
           -CL_WORK_YEARS, -MAIN_INCOME_AMT, -VIP_FLAG, -PORTRAIT)
           
 # MAP Y/N <- 1/0
 soc_dem_flag_cols <- c("CL_AUTO_EX","FL_GOLD", "FL_ZP", "FL_PENS", "FL_STUD", "FL_SOC", "FL_P24", "FL_MOBB", "FL_SMSI")
 for ( col in  soc_dem_flag_cols)
 {
   data[, col] %<>% flag_map()
   cat(col, "\n")
 }
 
 # AGE 
 # MONTH_OF_YEAR
 data %<>% mutate( AGE = ifelse( !is.na(AGE_1),  AGE_1, 
                                        (ifelse(!is.na(AGE_2), AGE_2, NA))),
                   MONTH_OF_YEAR = as.factor(month(DT))) %>%
           select( -AGE_1, -AGE_2)
 
 # client_count_feature <- data %>% 
 #   select( AR_NO, CL_ID) %>%
 #   group_by( AR_NO) %>% 
 #   summarise( n = length(unique(.))) %>%
 #   ungroup()
 
 data$UT[ is.nan(data$UT) ] <- NA
 
 data$PROC[is.na(data$PROC)] <- 0
 data$FEE[is.na(data$FEE)] <- 0
 data$POS_INT[is.na(data$POS_INT)] <- 0
 
 # PORTRAITS ARE BAD: calculated as mean stats by time 
 #
 
 # HISTOGRAMS ---------------
 
 #data  %>% select( FEE, PROC, POS_INT) %>% 
 #data$FEE[ is.na(data$FEE)] <- 0
 #data$PROC[ is.na(data$PROC)] <- 0
# data$POS_INT[ is.na(data$POS_INT)] <- 0 

 # plot distributio for PROC income ( only when status = ..21)
 
 data_income  <- select(data, PROC, FEE, POS_INT, PORTRAIT, CRD_CNT, AR_ST, DT, BANK)
 
 proc_inc_hist <- ggplot(data = data_income ,# %>% filter(AR_ST == 100000021, CRD_CNT >=1), 
                         aes(x = log(POS_INT+1))) 
 proc_inc_hist =  proc_inc_hist + geom_histogram(fill = "green", alpha = 0.5)
 proc_inc_hist + facet_wrap(~DT) + xlab("LN of FEE income") + ggtitle("Histograms of LN of PROCENT income (by contract status)")
 
# hist( x = log(data$PROC + 1), breaks = 100, col = "blue")
 sum( data$PROC < 7)/ length(data$PROC)
         
  sum(  data$PROC)              
  sum(  data$FEE)                 
  sum(  data$POS_INT)   
          
          
##########

#  OPEN_DT /    LAST_CRD_EXPR /    LAST_CRD_ACT 
  
# BANK
# UT  (UT > 0)
#  DLQ_AMT  > 0
# UT  
# data$NM 39 values  
#  AR_ST == 21/ 23/ NA
# CL is not unique for contr
# CL sd data different for contracts  
#   AGE in []      
# QTY_CHLD  in  [] 
  
#   data FLAG
  
#### ADDITIONAL FUNS ------  
  
 flag_map <- function(feat)
 {
   feat %<>% plyr:: mapvalues(from = c("", "N", "Y"), to =  c(NA, 0,1)) %>% as.integer()
   return( feat)
 }
  
# soc_dem_flag_cols <- grepl("FL", colnames(data))

 cut_rare_values <- function(feat, bound, replacement = NA)
{
  freq_list <- round( table(feat)/length(feat),4)
  rare_values <- names(freq_list)[freq_list <  bound]
  feat[ which(feat %in%  rare_values)] <- replacement
  return(feat)
}
 
 bound_feat <- function(feat, low_bound = -1000000, upper_bound = 1000000, replacement = NA)
 {
   feat[ which( (feat < low_bound )|(feat > upper_bound)) ] <- replacement
   return( feat)
 }
 
 add_month <- function(dates_array, nday = 1)
 {
   # dates_array <- sapply(dates_array,function(x)
   for( i in (1 : length(dates_array)))
   {
     dates_array[i] <- (as.Date( dates_array[i]) %m-% months(nday))
   }
   return( dates_array)
 }
 
 ## function to return distinct values:
 #cl_cols <- grep( 'CL_', colnames(data))
 #tmp <-  apply(data[,cl_cols[-1]], 2, function(x) length(unique(x)))
 
 #  OPEN_DT /    LAST_CRD_EXPR /    LAST_CRD_ACT 
 
 # BANK
 # UT  (UT > 0)
 # DLQ_AMT  > 0
 # UT  
 # data$NM 39 values  
 # AR_ST == 21/ 23/ NA
 # CL is not unique for contr
 # CL sd data different for contracts  
 
 ### FILL NA ----- 
 
 
                 
 ### DATA TYPES ----
 
 ### 
 

 
# tmp_df <- head(data, 100000)
 
 #tmp_df_1<- remove_NA_features(tmp_df,max_na_percent = 0.5, print_table = TRUE)
# tmp_df_1<- remove_ONE_CLASS_features(tmp_df,max_class_percent = 0.99)
 
 #tmp_df_1 <- factor_process( tmp_df, factor_cols = NULL, add_factors = TRUE, dummy = TRUE)
 
 #### REMOVE LINEAR DEPENDENCIES:
 
 tmp_df_1  %<>% omit_corr_feat(top_n_corr = 30, omit_action = TRUE, corr_threshold = 1)
 
 # max NA 
 data %<>% remove_NA_features(max_na_percent = 0.5, print_table = TRUE)
 
 # one class features
 data %<>% select( -DLQ_AMT, -FL_STUD, -FL_SOC)
 
 # DUMMY
 data %<>% factor_process(  factor_cols = NULL, add_factors = TRUE, dummy = TRUE)
 
 # CORRELATION: too large> do it by subsetting:
 tmp_df <- data[ sample(1:nrow(data), size = 300000),]
 tmp_df %<>% omit_corr_feat(top_n_corr = 20, omit_action = FALSE, corr_threshold = 1)
 
 data %<>% select( -POS_AMT,  -CL_OWN_TYPE_NA, -CL_OFF_POS_NA, -REP_SEX)
 data$'GRP_Карта БКК' <- NULL
 
### FEATURE TRANSFORMATION:
 
 # TARGETS:
 data %<>% mutate(  PROC =  log(PROC + 1), PROC_IND = as.integer(log(PROC + 1) > 0 ),
                    FEE =   log(FEE + 1), FEE_IND = as.integer(log(FEE + 1) > 0 ),
                    POS_INT = log(POS_INT + 1), POS_INT_IND = as.integer( log(POS_INT + 1) > 0))

 
 # FEATURES:
 # h1 <- hist( data$LIM, breaks = 50, col  = "red")
 # h2 <- hist( log(data$LIM    + 1) , breaks = 50, col = "blue")
 col_classes <- sapply( data, class) 
 
#  ******  Features  hitt ********
pdf(file ="Features_hist.pdf", width = 20, height = 12) 
  for ( i in (1:ncol(data)))
  {      
    if( (col_classes[[i]] == "numeric")) #| ( col_classes[[i]] == "integer") )
    {
       h1 <- ggplot( data, aes(x = data[,i])) + geom_histogram(fill = "red", bins = 100) + 
             ggtitle( paste0("Hist of ", colnames(data)[i]))
       h2 <- ggplot( data, aes(x = log(data[,i] +  1))) + geom_histogram(fill = "blue", bins = 100)
       multiplot(h1,h2)
    }
  cat(i, "\n")  
  } 
dev.off()
 

# transfrom nearly all numeric features
for( i in (1:ncol(data)))
{
  if((col_classes[[i]] == "numeric")&( !(colnames(data)[i] %in% c("AGE", "PERC", "FEE", "POS_INT", "UT"))))
  {
    data[,i] <-  log(data[,i] + 1)
    cat( "Log transform of",colnames(data)[i], "\n")
  }
 # cat(i, "\n\n")
}

 # col_dif_values <- apply( data, 2, function(x) length(unique(x)))

# REMOVE ALL UNNECECCARY
# rm(A, data_income, tmp, tmp_df, tmp_df_1)


### CREATE X,y ----

data$DT %<>% format( format = "%Y-%m") 
data$DT <-  as.Date( paste0(data$DT,"-01"), format = "%Y-%m-%d" )
#data %<>% arrange( DT, AR_NO)

dates_set <- unique(data$DT) %>% sort()
target <- "PROC"
 
target_data <- data %>% 
  select(AR_NO, DT, target) 
#  mutate( DT =  add_month(DT))
target_data$DT <- plyr:: mapvalues( target_data$DT, from = dates_set, to = add_month(dates_set))
colnames(target_data)[3] <-  "TARGET" #paste0(target,"_TARGET")

# join target
 data %<>% left_join( target_data, by = c("AR_NO", "DT"))
 
 colnames(data) <- str_trim( colnames(data))
    
 # new data: data without target values = X for new month
 new_data <- data %>% filter( is.na(TARGET)) # filter( DT == "2017-09-01")
 
 # remove data from  
 data %<>% filter( !is.na(TARGET))
 
 
 #  **************  SAVING R data  ************** 
 
 # http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
 #  Saving on object in RData format
 #    save(data1, file = "data.RData")
 # Save multiple objects
 #    save(data1, data2, file = "data.RData")
 # To load the data again
 #    load("data.RData")

 
 ### SAVE DATA ----
 save( data, file = "credit_data_processed.RData")
 save( new_data, file = "new_month_credit_data_processed.RData")
 rm( new_data, tmp1, target_data)
 #rm(data)
 
### TRAIN - TEST SPLIT:
 
 # data load:
 load("credit_data_processed.RData")
 
 # sort 
 data %<>% arrange( DT, AR_NO)
  
 data %<>% mutate( #PROC_TARGET_IND = as.integer(PROC_TARGET > 0),
                       TARGET  = as.integer(TARGET > 0))
 
 
 # change all integers types on numeric:
 cols_to_drop <- c( "CL_ID", "LAST_CRD_ACT", "LAST_CRD_EXPR", "OPEN_DT", 
                    "PROC_TARGET", "PERC_IND")
  
 cols_to_select <- c("PROC", "UT", "LIM", "TXN_CNT", "TXN_AMT", "CRD_CNT", "FEE")
 
 data %<>% select( -one_of(cols_to_drop)) 
 
 dates_set <- unique(data$DT) %>% sort()
 # split by dates:
 test_dates <- as.Date("2017-08-01")
 train_dates <-  dates_set[ which(!(dates_set %in% test_dates)) ]

 test_df  <- data %>% filter( DT %in% test_dates)
 train_df <- data %>% filter( DT %in% train_dates) 
 
 train_df  %<>%  select( -AR_NO, -DT)
 test_df   %<>%  select( -AR_NO, -DT)
 
 #train_df <- apply(train_df, 2, function(x) as.numeric(x))
 col_classes <- sapply(train_df, class)
 for( i in (1:ncol(train_df)))
 {
   if( col_classes[i] ==  "integer")
   {
     train_df[,i] %<>% as.numeric()
     test_df[,i] %<>% as.numeric()
     #cat( names( col_classes[i]), "\n")
   }
 }
 
 #rm(data)
  
### Plots:
 #train_df %<>% mutate( PROC_TARGET_IND = as.integer(PROC_TARGET > 0),
  #                     TARGET  = as.integer(PROC_TARGET > 0))
 
pdf("features_distrib_by_binary_PROC_2.pdf", width = 16, height = 9)
  for ( i in (92: ncol(train_df)))
  {
   distribution_plot( dat = train_df, index = i)
   cat(i, "\n")
  } 
dev.off() 
 
 
### BINARY MODELS ------

  # set xgb binary model param list:

  xgb_binary_params_list <- list(
    "objective"           = "binary:logistic",
    "eval_metric"         = "logloss",
    "eta"                 = 0.1,
    "max_depth"           = 6,
    "min_child_weight"   = 8,
    "gamma"              = 0.7,
    #"subsample"         = 1,
    # "colsample_bytree"  = 1,
    #"scale_pos_weight   = 2
    "alpha"              = 0,
    "lambda"             = 0,
    "seed"               = seed2
  )

model_xgb <-  XGB_learn_ts(
   trainData = train_df %>% 
     select(one_of(cols_to_select_3)) %>%
     mutate( PROC_SQ = PROC*PROC,
             CR_AMT_SQ = CR_AMT*CR_AMT,
             UT_SQ = UT*UT,
             FEE_SQ = FEE*FEE), 
   testData = test_df  %>% 
     select(one_of(cols_to_select_3)) %>%
     mutate( PROC_SQ = PROC*PROC,
           CR_AMT_SQ = CR_AMT*CR_AMT,
           UT_SQ = UT*UT,
           FEE_SQ = FEE*FEE), 
   param_list =  xgb_binary_params_list, 
   nrounds = 50, 
   threshold = 0.5,
   auc_ind =FALSE,
   seed1 = 42, seed2 = 42)
  
  model_xgb[[1]] %>% View()
  model_xgb[[2]]

#select(one_of(cols_to_select)
cols_to_select <- c("DEP_AMT", "CRD_AMT","CRD_CNT", "NM_BLACK","NM_CLASSIC_Классическая_карта_VK",
                    "TXN_AMT","CR_CNT", "CR_AMT", 
                     "FEE", "REP_POPULATION", "EDUCATION_LEVEL_HIGHER_PROFESSIONAL",
                    "UT", "PROC","FEE_IND", 
                    "TARGET")

cols_to_select_2 <- c( "CRD_AMT","CRD_CNT", 
                    "TXN_AMT", "CR_AMT", 
                    "FEE", "UT", "PROC","FEE_IND", 
                    "TARGET")

cols_to_select_3 <-  c( "CRD_AMT", "CR_AMT", "TXN_AMT",
                        "FEE","UT", "PROC",
                        "TARGET")

cols_to_select_4 <- c("PROC","TARGET")


 # "TP_Кредитная ББКК", GRP_Карта БКК , "POS_AMT

 
## CORR matrix:

corr <- train_df %>% 
  select(one_of(cols_to_select_3))%>% 
  cor(use = "pairwise.complete.obs")
corr %<>% apply(1:2, function(x) round(x,3))
corrplot.mixed( corr)

# find wrong-classified observations:



### BASELINE ERROR:

#1. last month model: y_today = y_month_ago:
 
 TARGET_table <- data %>% select(TARGET, DT)
 
 # 0 income percent per month
 tmp <- TARGET_table %>% 
   group_by(DT) %>% 
   summarise( COUNT_0 = sum( TARGET > 0), 
           COUNT = n(),
           RATIO = sum( TARGET > 0)/ n())
 
 
 target_by_month <- data %>% 
   select(AR_NO,DT,TARGET) %>% 
   spread( key = DT, value = TARGET ) %>%
   data.frame() 
 
 # NA values appear! 
 target_by_month %>% View()
 
 # groundhog day model accuracy by month:
  month_acc <-target_by_month 
  
  accuracy( target_by_month[,16],  target_by_month[,15] )
  
  pairwise_accuracy <- function(x, x_lag)
  {
    tmp_tab  <- data.frame(cbind(x,x_lag))
    colnames(tmp_tab ) <- c("x", "x_lag")
    tmp_tab %<>% filter(!(is.na(x)|(is.na(x_lag))))
    return( round(sum( tmp_tab[,1] == tmp_tab[,2])/ nrow(tmp_tab),3))
  }
  
  #  test accuracy: 0.922
  pairwise_accuracy(target_by_month[,16], target_by_month[,15])
  
  # mean accuracy:
  acc_tmp <- numeric()
  for( i in (2:(ncol(target_by_month)-1) ) )
  {
    acc_tmp[i-1] <- pairwise_accuracy( target_by_month[,i+1], target_by_month[,i])
    cat(i, "\n")
  }
  plot(acc_tmp, type = "l")
  
  # 2 month horizont accuracy:
  acc_tmp_2 <- numeric()
  for( i in (3:(ncol(target_by_month)-1) ) )
  {
    acc_tmp_2[i-2] <- pairwise_accuracy( target_by_month[,i+1], target_by_month[,i-1])
    cat(i, "\n")
  }
  plot(acc_tmp_2, type = "l")
  
  
  # 3 month horizont accuracy:
  acc_tmp_3 <- numeric()
  for( i in (4:(ncol(target_by_month)-1) ) )
  {
    acc_tmp_3[i-3] <- pairwise_accuracy( target_by_month[,i+1], target_by_month[,i-2])
    cat(i, "\n")
  }
  plot(acc_tmp_3, type = "l", col = "red")
  
  mean(acc_tmp)
  mean(acc_tmp_2)
  mean(acc_tmp_3)

   
### Regression model:
  
  

 


 
 
  
  
 
 
 
 