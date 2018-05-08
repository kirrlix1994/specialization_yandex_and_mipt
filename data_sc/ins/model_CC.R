
source("model_functions.R")



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
#library(data.table) # for working with big tables
library(glue)
#library(reshape2)
library(dummies)
library(cvTools)
library(caret)
library(AUC)
library(pROC)
library(randomForest)       
library(xgboost)             

# features matrix download
features_file <- "res_files/FEATURE_TABLE_clean.csv"
feat_table_old <- read.csv(file =features_file , header = TRUE )
# feat_table %>%  View()



# REDO!!!!!!!!!!!
# old cols rename:!!!!!!!!!!!!!!!
feat_table_old %<>% dplyr:: rename(
  SD_HF.GENDER = GENDER,
  SD_HF.MARITAL_STATUS =  MARITAL_STATUS,               
  SD_HF.EMPLOYEE_FLAG = EMPLOYEE_FLAG,
  SD_HF.BRANCH =  BRANCH,                
  SD_HF.EDUCATION_LEVEL = EDUCATION_LEVEL,
  SD_HF.OCCUPATION  =   OCCUPATION,           
  SD_HF.APPOINTMENT_LEVEL = APPOINTMENT_LEVEL,        
  SD_HF.OWN_CAR_FLAG  = OWN_CAR_FLAG,                 
  SD_HF.AGE = AGE) 


   # redo:!!!!!!!!!!!!!!!!
feat_table_old$SD_HF.GENDER %<>% as.factor()
feat_table_old$SD_HF.MARITAL_STATUS %<>%  as.factor()
feat_table_old$SD_HF.EMPLOYEE_FLAG %<>% as.factor()
feat_table_old$SD_HF.BRANCH %<>% as.factor()
feat_table_old$SD_HF.EDUCATION_LEVEL %<>% as.factor()
feat_table_old$SD_HF.OCCUPATION %<>%  as.factor()
feat_table_old$SD_HF.APPOINTMENT_LEVEL  %<>% as.factor()
feat_table_old$SD_HF.OWN_CAR_FLAG  %<>% as.factor()
feat_table_old$SD_IQ.CL_SOC_ST %<>% as.factor()
feat_table_old$SD_IQ.CL_ESTATE_TYPE %<>% as.factor()


feat_table_old <- insur_data_preprocess(feat_table_old)
feat_table_old %>% View()


     ## plots
     distribution_plot( dat = feat_table_old, index = 27 )
     
     
     
###modelling---------
     # filter one channel 
     feat_table_old %<>% filter(CHANNEL_CC == 1)
     
     smp_size <- floor(0.8 * nrow(feat_table_old))
     set.seed(42)
     train_ind <- sample(seq_len(nrow(feat_table_old)), size = smp_size)
     train <- data.frame( feat_table_old[train_ind, ])
     test <-  data.frame( feat_table_old[-train_ind, ])
     dim(train) ; dim(test)
     #  all_cols _list
     all_cols_list <- colnames(train)
   
    ## omit cols which are not in test: 
     
     
      
# xgb baseline: 
     
# 
#              SD_IQ.CL_EDU_HIG, LIM.MIN_UT, TRANS.TXN_TP_3_MEAN_180
# params: eta = 0.4, nrounds = 9, threshold = 0.08- 0.09, "max_depth = 2
# not overfit, stats: f = 0.18, auc = 0.68 , kappa = 0.09

# top import
# train_2 <- train_1 %>% select( INS.TARGET, LIM.LAST_UT, TRANS.TXN_TP_2_MAX_31, 
#                   SD_IQ.CL_EDU_HIG, LIM.MIN_UT, AGE, TRANS.TXN_TP_3_MEAN_180)

 
     ## test XGB_learn
     ## need train_1
     
      params_to_test <- list(
        "objective"           = "binary:logistic",
        "eval_metric"         = "logloss",
        "eta"                 = 0.3,
        "max_depth"           = 2,
        "min_child_weight"   = 8,
        "gamma"              = 0.7,
        #"subsample"         = 1,
       # "colsample_bytree"  = 1,
        #"scale_pos_weight   = 2
        "alpha"              = 0,
        "lambda"             = 0,
        "seed"               = 10
      )
     
     tmp <- XGB_learn( train =  train, param_list = params_to_test,
                        nrounds = 20, threshold = 0.26, 
                        seed1 = 523, seed2 = 5124)
      #tmp[[2]]
      #tmp[[3]]
      tmp[[1]] %>% View() 

     
   #  c() model_tmp_cols_list
      train_tmp <- train %>% select( one_of(model_tmp_cols_list), INS.TARGET ) 
      
      tmp1 <- XGB_learn( train =  train_tmp, param_list = params_to_test,
                         nrounds = 20, threshold = 0.19, 
                         seed1 = 23, seed2 = 351)
      #tmp[[2]]
      #tmp[[3]]
      tmp1[[1]] %>% View() 
      tmp1[[6]]  
              
      
      
  ### build test precision and auc dist:
      auc_sample_ts <- numeric()
      pr_sample_ts <- numeric()
      auc_sample_tr <-  numeric()
      pr_sample_tr <-  numeric()
      rate_ts <-   numeric()
      K <- 500
      for( i in (1:K))
      {
        seed_1 =  sample(1: 10^8, 1)
        seed_2 =  sample(1: 10^8, 1)
        
        tmp1 <- XGB_learn( train =  train_tmp, param_list = params_to_test,
                           nrounds = 20, threshold = 0.253, 
                           seed1 =  seed_1, seed2 = seed_2)
        
        auc_sample_ts[i] <- tmp1[[1]]$test[4]
        pr_sample_ts[i] <-  tmp1[[1]]$test[9]
        auc_sample_tr[i] <- tmp1[[1]]$train[4]
        pr_sample_tr[i] <-  tmp1[[1]]$train[9]
        cat(i,"\n")
      }
      
      
      ### SAME LOGIT
      auc_sample_ts_log <- numeric()
      pr_sample_ts_log <- numeric()
      auc_sample_tr_log <-  numeric()
      pr_sample_tr_log <-  numeric()
      rate_ts <-   numeric()
      K2 <- 500
      for( i in (1:K2))
      {
        seed_1 =  sample(1: 10^8, 1)
        seed_2 =  sample(1: 10^8, 1)
        
        stat_tmp <- LOGIT_learn( train =  train_tmp, sampling = 0,seed1 =seed_1, seed2 =seed_2,
                                 target = "INS.TARGET", threshold = 0.273, normalize = FALSE)
        
        auc_sample_ts_log[i] <-    stat_tmp[[1]]$test[4]
        pr_sample_ts_log[i] <-     stat_tmp[[1]]$test[9]
        auc_sample_tr_log[i] <-    stat_tmp[[1]]$train[4]
        pr_sample_tr_log[i] <-     stat_tmp[[1]]$train[9]
        cat(i,"\n")
      }
      
      
      
      
## logistic reg and svm: 

 # test LOGIT_learn
  stat_tmp <- LOGIT_learn( train =  train_tmp, sampling = 0,seed1 =23, seed2 =351,
                           target = "INS.TARGET", threshold = 0.273, normalize = FALSE)
 
 stat_tmp[[1]] %>% View()
# stat_tmp[[2]]
 
 
 cols_to_select <-c("INS.TARGET","AGE", "LIM.LAST_UT", 
                    "SD_IQ.CL_EDU_HIG", "SD_IQ.FL_PENS_N", 
                    "EVAL.DAYS_FROM_OPEN", "SD_IQ.CL_EDU_TWO", 
                    "BAL.LAST_BAL")
 train_1 <-  train %>% select( one_of( cols_to_select)) 
 stat_tmp1 <- LOGIT_learn( train = train_1, sampling = 0,seed1 =12 , seed2 = 43,
                           target = "INS.TARGET", threshold = 0.18, normalize = FALSE)
 stat_tmp1[[1]] %>% View()
 stat_tmp1[[2]] 
 
 
    # "AGE", "LIM.LAST_UT"  f1 = 0.16, recall = 0.5, auc = 0.64
    # cols_for_model: better

    cols_to_select <-c("INS.TARGET", "AGE", "LIM.LAST_UT","SD_IQ.CL_EDU_HIG", 
                       "TRANS.CATEGORY_1_MEAN_180") #"AGE"
    cols_to_select <- c("INS.TARGET",  "BAL.TOTAL_BAL_NEG_CHNG_N_7"  )
    #cols_for_model
    # forwards_cols
    train_1 <-  train_tmp %>% select( one_of( forwards_cols_all, "INS.TARGET" ))
    stat_tmp1 <- LOGIT_learn( train = train_1, sampling = 0,seed1 = 474, seed2 = 533,
                              target = "INS.TARGET", threshold = 0.19, normalize = FALSE)
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
    
    cols_lasso = c("INS.TARGET", names(coef_lasso_not_null)[-1])       
    cols_lasso_1 <- cols_lasso[-c(3,4)]
    
    cols_for_model <-  cols_for_model_start[ !( cols_for_model_start %in% column_to_leave )]
    train_tmp <- train %>% select( one_of(cols_for_model, "INS.TARGET" ))
    train_tmp$INS.TARGET %<>% as.factor() 
    
    stat_tmp1 <- LOGIT_learn( train = train_tmp, seed1 = 14324, seed2 = 4453,
                              target = "INS.TARGET", threshold = 0.277, 
                              normalize = FALSE)
    stat_tmp1[[1]] %>% View()
    stat_tmp1[[3]]
    
    
    
    # stepwise procedure: 
   #as.formula(glue("as.factor(INS.TARGET)~{paste(main_cols, collapse='+')}"))
   # nothing_formula <-   as.formula(glue("as.factor(INS.TARGET)~1"))
    
    set.seed(22)
    testIndexes <- sample(1:nrow(train_tmp))[ ( 1 : trunc(0.2*nrow(train))) ] 
    testData <- train_tmp[testIndexes, ]
    trainData <- train_tmp[-testIndexes, ]
    nothing_model <- glm( data = trainData, 
                          formula = as.formula(paste(target, "~ 1")),
                          family=binomial(link = "logit") )
    fullmod <- glm( data = trainData, 
                    formula = as.formula(paste(target, "~ .")),
                    family=binomial(link = "logit") )
    
    forwards = step( nothing_model,
                    scope=list(lower=formula( nothing_model),upper=formula(fullmod)),
                     direction="forward")    
    
    summary( forwards) #AIC: 15862
    
    cols_for_model <- readRDS(file = "LOGIT_forward_features_list")
      
    # to try!
    #backwards = step(fullmod,
    #                 scope=list(lower=formula( nothing_model),upper=formula(fullmod)),
    #                 direction="backward")
    
    
   #check when formula is too long!     
   #glm.fit: возникли подогнанные вероятности 0 или 1
    
    forwards_cols <- rownames(summary(forwards)$coefficients)[-1]
    forwards_cols_all <- rownames(summary(forwards)$coefficients)[-1]
   # saveRDS(object =  forwards_cols_all, file = "LOGIT_forward_features_list")
    
    
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
  
    train_1 <-  train_tmp %>% select( one_of( forwards_cols_all)) 
    train_1$INS.TARGET %<>% as.factor()
    stat_tmp1 <- FOREST_learn(train = train, sampling = 0,seed1 = 12, seed2 = 43, 
                              target = "INS.TARGET",
                              threshold = 0.67, 
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
    
    
    
## plots----- 
 a <-  hist(x = pred_prob_ts, breaks=30, 
       main="Histogram for model predicted probabilities",
       xlab="Probabilities",
       border="black", 
       col="blue")
    
  abline(a,v = 0.2, col = "red" , lty = 1)
  abline(a,v = 0.273, col = "green" , lty = 1) 
    
    
    
  a <-  hist(x = pred_prob_ts, breaks=30, 
             main="Histogram for model predicted probabilities",
             xlab="Probabilities",
             border="black", 
             col="blue")
  
  abline(a,v = 0.2, col = "red" , lty = 1)
  abline(a,v = 0.273, col = "green" , lty = 1) 
    
  
  #pr_sample_tr
  #pr_sample_ts
  
  
  b <-  hist(x =  pr_sample_ts, breaks=20, 
             main="Histogram of XGBoost model precision distribution on test data",
             xlab="Precision",
             border="blue", 
             col="green")
  
  abline(b,v = 0.266, col = "red" , lty = 2, ,lwd=3)
  abline(b,v =  0.2980, col = "brown" , lty = 10, ,lwd= 3)
  abline(b,v =  0.2989, col = "purple" , lty = 1, ,lwd= 3)
  
  summary(pr_sample_ts)
  
  quantile(pr_sample_ts, c(0.05, 0.5, 0.95) )
  
    
  
  
  
  c <-  hist(x =  auc_sample_ts, breaks=20, 
             main="Histogram of XGBoost model AUC distribution on test data",
             xlab="AUC",
             border="blue", 
             col="green")
  
  summary( auc_sample_ts)
  quantile( auc_sample_ts, c(0.05, 0.5, 0.95) )
  
  abline(b,v = 0.59995, col = "red" , lty = 2, ,lwd=3)
  abline(b,v =  0.615, col = "brown" , lty = 10, ,lwd= 3)
  abline(b,v = 0.6153, col = "purple" , lty = 1, ,lwd= 3)
  
  
 
 e <- line(x = 0.59995, col = "red" , lty = 2, ,lwd=3)
  