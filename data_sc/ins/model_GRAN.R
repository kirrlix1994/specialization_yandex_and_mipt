
source("model_functions.R")



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
#library(reshape2)
library(dummies)
library(cvTools)
library(caret)
library(AUC)
library(pROC)
library(randomForest)       
library(xgboost)             


### PREPROCECCING-----

features_file <- "res_files/FEATURE_TABLE_clean.csv"
feat_table <- read.csv(file =features_file , header = TRUE )

dim( feat_table)    # 50206   296
summary(feat_table)
str(feat_table)

bad_id_list <- feat_table %>% 
     group_by( ID) %>% 
     summarise( n = n()) %>% 
     filter( n > 1) %>%
     select(ID) %>%
     unlist() 
     #as.numeric()

length(bad_id_list) # 162

# remove them:
dim( feat_table)
feat_table %<>% filter( !(feat_table$ID %in% bad_id_list) ) 
dim( feat_table) 
# - 529 
# - 326 only after clean


## how to find bad ids:

# 3 same ids: 
# 103530403_2017-03-16   
# 104343352_2017-03-30:

 # tmp <-  feat_table %>% filter( ID == "103530403_2017-03-16")
 # tmp[, grep("SD_HF", colnames(tmp))] %>% distinct()
 # tmp[, grep("SD_IQ", colnames(tmp))] %>% distinct()

 
 
#FILTERING CLEAN DATA---------- 
 # filters: 
 
 # duration > n 
 # has cancelled == 0 
 # taken_before_comm == 0 
 # group = ? 
 # DAYS_REACT < m 
 # choose channel (GRAN)
  
# clean data -----
 feat_table_1 <- feat_table %>%
   filter( #INS.DAYS_REACT < 30, 
           INS.TAKEN_BEFORE_COMM == 0, 
           EVAL.COMM_NUMBER == 1, 
           EVAL.IS_BEEN_CALLED == 1, # error: meen  IS_BEEN_CALLED == 0
           CANC_N_BEFORE == 0) # COMM.  all: -396 obs only!
           #CHANNEL == "CC")
           #EVAL.GROUP == 1) 
 nrow(feat_table) - nrow(feat_table_1)
 #table(feat_table_1$INS.TARGET )
 round( sum( feat_table_1$INS.TARGET == 1 )/ nrow( feat_table_1),2)
 # 6% of 1
 
 
#remove wrong features-----
 tmp <- feat_table_1 %>% colnames( )
 
 #feat_table_2 <- remove_ONE_CLASS_features( df = feat_table_1, max_class_percent = 0.999)
 feat_table_2 <- remove_ONE_CLASS_features( df = feat_table_1, max_class_percent = 0.99)
 dim( feat_table_2)
 
 tmp2 <- feat_table_2 %>% colnames( )
 
 dupl <- feat_table_2 %>% group_by(ID ) %>% summarise( n = n()) %>% filter( n > 1) 
 
  feat_table_3 <- feat_table_2 %>%
                  select( -ID , -CANC_SUM_AFTER, -CANC_N_AFTER )
        
 # remove bad obs----------
  feat_table_4 <- remove_obs(df = feat_table_3, 
                             col_idx = grep('SD_IQ', colnames(feat_table_3)),
                             omit_action_na = TRUE, omit_action_value =TRUE,
                             threshold =  1) # 0.5
  # -1885 ( -100 "1")
  na_perc_sd <- hist( feat_table_4[[2]][,1], 100)
  feat_table_4 <- feat_table_4[[1]]
  sum(feat_table_4$INS.TARGET)
  
  # do not consider people without transactions at all 
  feat_table_5 <- remove_obs(df = feat_table_4, 
                             col_idx = grep('TRANS', colnames(feat_table_4)),
                             omit_action_na = TRUE, omit_action_value =TRUE,
                             threshold =  1)  
  
  oneVAlue_perc_trans <- hist( feat_table_5[[2]][,2], 100)
  # at least all "1" has to be removed
  # - 1601 
   feat_table_5 <- feat_table_5[[1]]
   
   # remove y cols!
   # feat_table_5 %<>% select( -CANC_SUM_AFTER, -CANC_N_AFTER )
  
   feat_table_6 <- remove_obs(df = feat_table_5, 
                              col_idx = grep('BAL', colnames(feat_table_5)),
                              omit_action_na = TRUE, omit_action_value =TRUE,
                              threshold =  1)  
  oneVAlue_perc_trans <- hist( feat_table_6[[2]][,2], 100)
  # -169 
  feat_table_6 <- feat_table_6[[1]]
  

  #  class balance change
  # 
  #  t0 <- table( feat_table$INS.TARGET)
  #  t1 <- table( feat_table_1$INS.TARGET) 
  #  t2 <- table( feat_table_2$INS.TARGET) 
  #  t3 <- table( feat_table_3$INS.TARGET) 
  #  t4 <- table( feat_table_4$INS.TARGET) 
  #  t5 <- table( feat_table_5$INS.TARGET)
  #  t6 <- table( feat_table_6$INS.TARGET)
  #  
  # H <- rbind( t0,t1,t2,t3,t4, t5, t6)
  # H <- data.frame( H)
  # colnames(H) <- c("neg_class_n", "pos_class_n")
  # H %<>% mutate( rate =  round( pos_class_n/ (pos_class_n + neg_class_n), 3) )
  # H
  
   # fill NA values: 
   NA_list <-apply( feat_table_6, 2, 
                    function(x) round( sum( is.na(x))/nrow( feat_table_6) ,2) )
   NA_list %<>% sort(decreasing = TRUE )
   NA_list[1:10]
 
   #FIll na with sample: 
   #cols_to_fill_na <- names( NA_list[1:9])
   # only facors( charectors) to make sample
   #cols_to_fill_na <- 
   #   cols_to_fill_na[ !cols_to_fill_na  %in% 
   #                     c("SD_IQ.CL_AMT_DOP", "SD_IQ.CL_CCY_DOP", "D_IQ.CL_CCY_ZP")]
       
   #   trandform all cols to factors 
   #   count unique value to check
        
    feat_table_6$SD_IQ.CL_INDUSTR %<>%  as.factor()
   cols_to_fill_na_facotrs  <-  names(Filter(is.factor, feat_table_6))
   for( col in cols_to_fill_na_facotrs )
   {
     feat_table_6[, col] <-
       FILL_NA( column =  feat_table_6[, col], fill_type = "sample", seed = 11)
     cat( col, "\n")
   }
  
   
   # factors: work with them
   # facors <- c("GENDER", "BRANCH", "OCCUPATION","OWN_CAR_FLAG", 
              
   # Fill other NA with median
   for( i in (1:ncol(feat_table_6)))
   {
     feat_table_6[,i] <- FILL_NA(column =  feat_table_6[,i], fill_type = "median" )
     cat(i,  "\n")
   }
     
   sum( is.na(feat_table_6)) == 0
   
   # dummy: 
   # have to remove linear dependences!
   feat_table_7<- dummy.data.frame(  feat_table_6, sep = "_",verbose = TRUE )
  
   # SD_IQ.CL_INDUSTR : 10 dummy varibles created !!!
     dim( feat_table_6); dim( feat_table_7)
     
   # omit correlations: 
     feat_table_8 <- omit_corr_feat(df = feat_table_7, top_n_corr = 50,
                                    omit_action = TRUE, print_info = TRUE,
                                    print_plot = FALSE, plot_names = FALSE, 
                                    corr_threshold = 0.99)
     
     
     dim(feat_table_7); dim(feat_table_8)
      
     feat_table_8$INS.TARGET %<>% as.factor() 
     
     
     ## plots
     distribution_plot( dat = feat_table_8, index = 3 )
     
     
###modelling---------
     # filter one channel 
     feat_table_8 %<>% filter(CHANNEL_CC == 0)
     
    # omit bonus !
     feat_table_8 <- feat_table_8[ ,-grep("BONUS",colnames( feat_table_8)) ]
     
     
     smp_size <- floor(0.8 * nrow(feat_table_8))
     set.seed(423342)
     train_ind <- sample(seq_len(nrow(feat_table_8)), size = smp_size)
     train <- data.frame( feat_table_8[train_ind, ])
     test <-  data.frame( feat_table_8[-train_ind, ])
     dim(train) ; dim(test)
     
     #  all_cols _list
     all_cols_list <- colnames(train)
   
      
# xgb baseline: 
     
# 5 features:  LIM.LAST_UT, TRANS.TXN_TP_2_MAX_31, 
#              SD_IQ.CL_EDU_HIG, LIM.MIN_UT, TRANS.TXN_TP_3_MEAN_180
# params: eta = 0.4, nrounds = 9, threshold = 0.08- 0.09, "max_depth = 2
# not overfit, stats: f = 0.18, auc = 0.68 , kappa = 0.09

# top import
#train_2 <- train_1 %>% select( INS.TARGET, LIM.LAST_UT, TRANS.TXN_TP_2_MAX_31, 
#                   SD_IQ.CL_EDU_HIG, LIM.MIN_UT, AGE, TRANS.TXN_TP_3_MEAN_180)

 
     ## test XGB_learn
     ## need train_1
     
      params_to_test <- list(
        "objective"           = "binary:logistic",
        "eval_metric"         = "logloss",
        "eta"                 = 0.4,
        "max_depth"           = 2,
        "min_child_weight"   = 8,
        "gamma"              = 0.7,
        #"subsample"         = 1,
        #"colsample_bytree"  = 0.95,
        #"scale_pos_weight   = 2
        "alpha"              = 0,
        "lambda"             = 0,
        "seed"               = seed2
      )
     
     tmp <- XGB_learn( train =  train_1, param_list = params_to_test,
                        nrounds = 9, threshold = 0.09, 
                        seed1 =2432, seed2 =32)
      #tmp[[2]]
      #tmp[[3]]
      tmp[[1]] %>% View() 
     
     
## logistic reg and svm: 

 # test LOGIT_learn
  stat_tmp <- LOGIT_learn( train =  train, sampling = 0,seed1 =1432, seed2 = 423,
                           target = "INS.TARGET", threshold = 0.09, normalize = FALSE)
 
 stat_tmp[[1]] %>% View()
# stat_tmp[[2]]
 
 
 cols_to_select <-c("INS.TARGET","AGE", "LIM.LAST_UT", 
                    "SD_IQ.CL_EDU_HIG", "SD_IQ.FL_PENS_N", 
                    "EVAL.DAYS_FROM_OPEN", "SD_IQ.CL_EDU_TWO", 
                    "BAL.LAST_BAL")
 train_1 <-  train %>% select( one_of( cols_to_select)) 
 stat_tmp1 <- LOGIT_learn( train = train_1, sampling = 0,seed1 =9230 , seed2 = 343431,
                           target = "INS.TARGET", threshold = 0.09, normalize = FALSE)
 stat_tmp1[[1]] %>% View()
 stat_tmp1[[2]] 
 
 
    # "AGE", "LIM.LAST_UT"  f1 = 0.16, recall = 0.5, auc = 0.64
    # cols_for_model: better

    cols_to_select <-c("INS.TARGET", "AGE", "LIM.LAST_UT","SD_IQ.CL_EDU_HIG", 
                       "TRANS.CATEGORY_1_MEAN_180") #"AGE"
    cols_to_select <- c("INS.TARGET",  "BAL.TOTAL_BAL_NEG_CHNG_N_7"  )
    #cols_for_model
    # forwards_cols
    train_1 <-  train %>% select( one_of( forwards_cols_all ))
    stat_tmp1 <- LOGIT_learn( train = train_1, sampling = 0,seed1 =64 , seed2 = 65,
                              target = "INS.TARGET", threshold = 0.09, normalize = FALSE)
    stat_tmp1[[1]] %>% View()
    stat_tmp1[[2]] 

              
    # choose by max score(AUC):
    cols_for_model_start <-  forwards_cols_all
    
    column_to_leave <- c("SD_IQ.CL_EDU_TWO",
                         "SD_IQ.CL_AMT_ZP",
                         "STATUS.MEAN_DAYS_CHANGE_1",
                         "LIM.LIM_MAX_NEG_CHANGE", 
                         "SD_IQ.CL_OWN_TYPE_INTERNATIONAL", 
                         "SD_IQ.CL_ESTATE_TYPE", 
                         "TRANS.FLG_CATEGORY_1_N_180", 
                         "BAL.TOTAL_BAL_POS_CHNG_MIN_31", 
                         "SD_IQ.CL_AMT_DOP",  # ???
                          "LIM.MIN_UT", 
                        "BAL.TOTAL_DEBT_NEG_CHNG_MIN_31", 
                        "OCCUPATION", 
                        "TRANS.CATEGORY_4_SUM_180")
                      

    cols_for_model <-  cols_for_model_start[ !( cols_for_model_start %in% column_to_leave )]
    train_tmp <- train %>% select( one_of(   cols_for_model ))
    stat_tmp1 <- LOGIT_learn( train =train_tmp, seed1 = 4332, seed2 = 52325,
                              target = "INS.TARGET", threshold = 0.09, 
                              normalize = FALSE)
    stat_tmp1[[1]] %>% View()
    stat_tmp1[[2]]
    
    
    
    # stepwise procedure: 
   #as.formula(glue("as.factor(INS.TARGET)~{paste(main_cols, collapse='+')}"))
   # nothing_formula <-   as.formula(glue("as.factor(INS.TARGET)~1"))
    nothing_model <- glm( data = trainData, 
                          formula = as.formula(paste(target, "~ 1")),
                          family=binomial(link = "logit") )
    fullmod <- glm( data = trainData, 
                    formula = as.formula(paste(target, "~ .")),
                    family=binomial(link = "logit") )
    forwards = step( nothing_model,
                    scope=list(lower=formula( nothing_model),upper=formula(fullmod)),
                     direction="forward")    
    summary( forwards) #AIC: 6109.4
    
    # to try!
    #backwards = step(fullmod,
    #                 scope=list(lower=formula( nothing_model),upper=formula(fullmod)),
    #                 direction="backward")
    
    
   #check when formula is too long!     
   #glm.fit: возникли подогнанные вероятности 0 или 1
    
    forwards_cols <- rownames(summary(forwards)$coefficients)[-1]
    forwards_cols_all <- rownames(summary(forwards)$coefficients)[-1]
    forwards_cols_all <-  c( "INS.TARGET",
     "LIM.LAST_UT"                ,     "SD_IQ.CL_EDU_HIG"  ,             
     "AGE"                        ,    "TRANS.TXN_TP_3_N_31"   ,         
      "SD_IQ.FL_PENS_DOC_N"       ,      "LIM.LIM_N_POS_CHANGE"  ,         
     "TRANS.CATEGORY_NA_MAX_31"   ,   "SD_IQ.CL_OFF_POS_WORKER"  ,      
     "SD_IQ.FL_GOLD_N"            ,     "TRANS.CATEGORY_1_MEAN_31",       
     "LIM.LIM_MAX_NEG_CHANGE"     ,    "EMPLOYEE_FLAG"       ,           
     "TRANS.CATEGORY_4_SUM_180"   ,     "BAL.TOTAL_BAL_NEG_CHNG_N_90"   , 
     "OCCUPATION"                 ,   "GENDER",                         
     "SD_IQ.FL_P24_N"             ,     "LIM.MAX_DLQ" ,                   
     "SD_IQ.CL_OWN_TYPE_INTERNATIONAL" ,"TRANS.TXN_CHN_NA_SUM_180",       
     "STATUS.CARD_CONTR_COUNT_1"    ,   "EVAL.DAYS_FROM_OPEN",            
     "STATUS.MEAN_DAYS_CHANGE_0"    ,   "LIM.MIN_UT",                     
     "TRANS.TXN_TP_2_MEAN_31"       ,  "BAL.TOTAL_BAL_POS_CHNG_MIN_31",  
     "TRANS.CATEGORY_0_MEAN_180"    , "TRANS.TXN_TP_2_SUM_31",          
     "TRANS.CATEGORY_2_N_180"       ,   "TRANS.TXN_CHN_1_MEAN_31",        
     "TRANS.TXN_TP_2_MAX_31"        ,   "SD_IQ.QTY_CHLD",               
     "TRANS.TXN_TP_3_MEAN_180"      ,  "SD_IQ.CL_ESTATE_TYPE" ,         
     "BAL.TOTAL_DEBT_NEG_CHNG_N_180",   "STATUS.MEAN_STATUS_COUNT_1" ,    
     "STATUS.MEAN_DAYS_CHANGE_1"    ,   "BAL.TOTAL_DEBT_NEG_CHNG_MIN_31", 
     "TRANS.CATEGORY_0_N_180"       ,   "TRANS.TXN_CHN_2_N_31" ,          
     "TRANS.TXN_TP_1_MEAN_31"       , "EVAL.KOL_CRD_14",                
     "SD_IQ.CL_AMT_DOP"             ,   "SD_IQ.CL_EDU_TWO",               
     "TRANS.TXN_TP_2_MEAN_180"      ,   "TRANS.CATEGORY_2_MEAN_180" ,     
     "TRANS.FLG_CATEGORY_1_N_180"    ,  "SD_IQ.CL_AMT_ZP")
    
    
    # CV/validation  model choice with  score maximization: 
    # MAX AUC
    
  ### RandomForest------  
    train$INS.TARGET %<>% as.factor()
  
    train_1 <-  train %>% select( one_of( cols_to_select)) 
    train_1$INS.TARGET %<>% as.factor()
    stat_tmp1 <- FOREST_learn(train = train_1, sampling = 0,seed1 = 112622, seed2 =421, 
                              target = "INS.TARGET",
                              threshold = 0.68, 
                              ntree =60, 
                              mtry = 1, 
                              weight =1,
                              maxnodes = 6,
                              nodesize = 600)
    stat_tmp1[[1]] %>% View()
    stat_tmp1[[2]] 
    
    # RANDOM FOREST BASELINE: 
    # METRICS: F1 = 0.19, RECALL = 0.115, ACC = 0.71; AUC = 0.67, PR = 0.5
    # PARAMS: threshold = 0.7;  ntree = 100-200; weight =1; nodesize = 500-600
    # QUITE unstable

   
    
   # Anomaly detection -----
   # https://www.coursera.org/learn/machine-learning/lecture/Rkc5x/anomaly-detection-vs-supervised-learning
    
    
    