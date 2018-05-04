
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

source("model_functions.R")

# set working direction 
#setwd("C:/Users/Liksakov/Desktop/Liks/insurance/scripts")
# functions for each dataset preprocessing
source( "insurance_proceccing_data_functions_2007_liks.R")

# set working direction 
#setwd("C:/Users/Liksakov/Desktop/Liks/insurance/model_data")
data_file <- "res_files/FEATURE_TABLE_clean.csv"
data <- read.csv(file = data_file , header = TRUE )

cat("BAD ids: remove\n")

#bad_id_list =  one id has 2 or mmore rows
# id = ( client_id,date)
bad_id_list <- data %>% 
  group_by( ID) %>% 
  summarise( n = n()) %>% 
  filter( n > 1) %>%
  select(ID) %>%
  unlist() 

  cat("Duplicated ids count:", length(bad_id_list), "\n") # 162

# remove them
data %<>% filter( !(data$ID %in% bad_id_list) ) 

data <- data[ ,(names( data) != "ID")] 
    
#ids_columns <-c( "CLIENT_ID","CONTRACT_NUMBER","DATE","IQ_ID", "HID_PARTY") 
 # y_columns <-  c( "EVAL.COMM_NUMBER", "COMM.DATE_CANC_2017-02-01", "COMM.DATE_CANC_2017-03-01",
 #                 "COMM.DATE_CANC_2017-04-01", "COMM.DATE_CANC_2017-05-01",
 #                 "COMM.DATE_CANC_2017-06-01", "COMM.CANC_N_AFTER",
 #                 "COMM.CANC_SUM_AFTER","CANCELED", "DURATION", "DAYS_REACT",
 #                 "LAST_DURATION","STILL_CONTINUE" )         


#2. dummy one hot encoding  
#df <- dummy.data.frame(df, names=c("MyField1"), sep="_")

# DATA TYPES: 
 # data$ID <-  as.character(data$ID)
  #data$EVAL.KOL_CRD_14 <- as.numeric( data$EVAL.KOL_CRD_14)
  
  data_types <- data.frame()   
  for( i in (1: ncol(data)))
  {
    data_types[i,1] <- class(data[,i])
    rownames(data_types)[i] <- colnames(data)[i]
    colnames(data_types) <- "TYPE"
  }
  
   # CHOOSE ALL FACTORS FOR DUMMY ENCODING: 
     facor_vars1 <- c("SD_HF.GENDER", "SD_HF.MARITAL_STATUS", "SD_HF.EMPLOYEE_FLAG", "SD_HF.BRANCH", 
                    "SD_HF.EDUCATION_LEVEL", "SD_HF.OCCUPATION", "SD_HF.APPOINTMENT_LEVEL", "SD_HF.OWN_CAR_FLAG",
                    "STATUS.LAST_CONTR_STATUS", "STATUS.LAST_CARD_STATUS")
  
     facor_vars1 <- c("GENDER", "MARITAL_STATUS", "EMPLOYEE_FLAG", "BRANCH", 
                      "EDUCATION_LEVEL", "OCCUPATION", "APPOINTMENT_LEVEL",
                      "OWN_CAR_FLAG", "STATUS.LAST_CONTR_STATUS", "STATUS.LAST_CARD_STATUS")
     
     
       for( f in facor_vars1)
       {
        data[,f] <- as.factor( data[,f])
        cat( f, "\n")
       }
     
     
     
     facor_vars2 <- rownames(data_types)[ which(  data_types$TYPE =="factor" ) ] 
     factor_vars_all <- c(facor_vars1, facor_vars2)
     length( factor_vars_all )
     length( factor_vars_all ) ==32
      #sapply( facor_vars, function(x,y)  as.factor( y[,x]), y = data )


     # ok!
     #all.equal( data_mod, data_mod1)
     
     #data$EVAL.KOL_CRD_14[is.na(  data$EVAL.KOL_CRD_14)]  <-
    #              median( data$EVAL.KOL_CRD_14, na.rm = TRUE) 
     
   # make DUMMY:
     #which.dummy
     #add level - how to drop?
     # CODE_MAP!!
     data_mod <- dummy.data.frame(data = data, 
                                  names = factor_vars_all, 
                                  omit.constants = TRUE,
                                  sep = "_",
                                  verbose = TRUE)

     ncol(data_mod) - ncol(data) #122 # 112
     
    
     ## smothed_likelihood_features
     
     ## count by category 
     '''
     category_count_df <- function( df, cols_names)
     {
       # df = dataframe; cols = c() - vector of cols
       # takes some factor cols in df and returns modified df;
       # with additional cols1 which aelements are count of theese category level;
       for( s in cols)
       {
         col_tmp <- df[,s];
         freqTable <- data.frame(table(col_tmp))
         col_tmp_1
       }
     }
      
     category_count <- function( col)
     {
       freqTable <- data.frame(table(col))
       col_freq <- numeric()
       for( i in (1: length(col)))
       {
         col_freq[i] <- freqTable$Freq[ which(  freqTable$col == col[i] ) ]
       }
       df <- cbind( col,col_freq ); #names_cols <- c( , paste0( col, "_","Freq"))
      #colnames(df) <- names_cols
       return(df)
     }
     '''
  
     
  # function to generate factor imperical distrib:
  #  N = number of values to generate
  # prop_tab  - named vector of class probabilities
  # returns: vector length N with charectors 
  #         ( which are random with prop_tab impiric distribution)
  imperic_distr <- function( N, prop_tab, seed = 123 )
  {
    out_vec <- vector()
    point_vec <- numeric()
    for( i in (1:length( prop_tab)))
    {
      point_vec[i]  <- sum( prop_tab[(1:i)])
    }
    names( point_vec) <- names( prop_tab)
    set.seed(seed)
    random <- runif(min = 0, max = 1, n= N)
    for( k in (1: N))
    {
      out_vec[k]  <- 
        names(point_vec)[ min( which( (random[k] < point_vec) == TRUE )) ]
      #cat( out_vec[k], "\n")
    }
    return( out_vec )
  }
      
       # check number of NA!
     
 #1. FILL NA (for numeric)
FILL_NA <- function( column, fill_type = c("median", "mean", "sample"), seed = 42)
{
 if( sum(is.na(column)) == 0 )
  {
    cat("No na values, nothiing to fill in \n")
    return(column)
  }
 if(fill_type  == "median")
 {
   NA_perc = sum(  is.na(column)/ length(column) )
   column[ is.na(column)] <-  median( column, na.rm = TRUE)
   #cat(" Fill NA in ... with median\n")
   #cat("Rows  percent fill = " ,NA_perc, "\n\n")
 }
 if(fill_type  == "mean")
 {
   NA_perc = sum(  is.na(column)/ length(column) )
   column[ is.na(column)] <-  mean( column, na.rm = TRUE)
   #cat(" Fill NA in ...with mean\n")
   #cat("Rows  percent fill = " ,NA_perc, "\n\n")
 }
 if(fill_type  == "sample")
 {
   if( length(unique(column)) > 50 )
   { 
     cat("To many distinct values, \n") 
     return(column)
   }
   else
   {
     column %<>% as.character()
     prop_tab <- column %>% 
       na.omit() %>%
       table()%>%
       prop.table() %>% 
       sort( decreasing = TRUE)
     # remove too small classes:
     #prop_tab <- prop_tab[which( prop_tab > 0.01)] 
     na_count_fill <- sum( is.na(column))
     column[ is.na(column)] <- 
       imperic_distr(N = na_count_fill, prop_tab = prop_tab, seed = seed)
     column %<>% as.factor()
   }
 }
 return(column)
} 
 
     
###clean observations----   
     
   # omit bad obs: 
   # 1. remove people  without trans data(all trans are 0) :
     n1 = nrow( data)
     dat_tmp <- remove_obs(df = data,
                      col_idx =  grep("TRANS", colnames(data)), 
                      omit_action_na = TRUE, 
                      omit_action_value = TRUE, 
                      Value = 0 ,
                      threshold = 0.9)
     data <- dat_tmp[[1]]
    n2 = nrow( data)
    cat(n1 - n2, "\n")
    
    # hist of one value for trans: (before omit action)
    hist( dat_tmp[[2]][,2], breaks = 100 ,
          main = "Value (0) percent distribution",  xlab = "percent") 
    
    #2. same for balance
    n1 = nrow( data)
    dat_tmp2 <- remove_obs(df = data,
                          col_idx =  grep("BAL", colnames(data)), 
                          omit_action_na = TRUE, 
                          omit_action_value = TRUE, 
                          Value = 0 ,
                          threshold = 0.85)
    data <- dat_tmp2[[1]]
    n2 = nrow( data)
    cat(n1 - n2, "\n")
    
    # hist of one value for balance: (before omit action)
    hist( dat_tmp2[[2]][,2], breaks = 100 ,
          main = "Value (0) percent distribution", xlab = "percent") 
     
    #3. same for SD_IQ
    n1 = nrow( data)
    dat_tmp3 <- remove_obs(df = data,
                           col_idx =  grep("SD_IQ", colnames(data)), 
                           omit_action_na = TRUE, 
                           omit_action_value = FALSE, 
                           Value = 0 ,
                           threshold = 0.3)
    data <- dat_tmp3[[1]]
    n2 = nrow( data)
    cat(n1 - n2, "\n")
    
    # hist of NA perc for SD_IQ : (before omit action)
    hist( dat_tmp3[[2]][,1], breaks = 100,
          main = "Value (0) percent distribution", xlab = "percent") 
    
    #dim(data)
    
  data_mod <- factor_process( data = data,
                              factor_cols = facor_vars1, 
                              add_factors = TRUE, 
                              dummy = TRUE)
    
 #data_mod <- apply( data_mod, 2,  function(x)  FILL_NA(x, fill_type = "median")  )
  for( i in (1:ncol(data_mod)))
  {
    data_mod[,i]   <- FILL_NA(  data_mod[,i] , fill_type = "median")
    cat(i, "\n")
  }
 # check : no NA
 sum(is.na(data_mod)) == 0
  #apply( data_mod, 2,  function(x)  sum( is.na(x) ) )
  #data_mod <- as.data.frame( data_mod)
  
 
 #make all data numeric: 
   
 # # check we do not have factors:
 # data_types <- data.frame()   
 # for( i in (1: ncol(data_mod)))
 # {
 #   data_types[i,1] <- class(data_mod[,i])
 #   data_types[i,2] <- colnames(data_mod)[i]
 #   #rownames(data_types)[i] <- colnames(data_mod)[i]
 #   colnames(data_types) <- c("TYPE", "NAME")
 #   #cat(i, "\n")
 # }
 
 #5. trandform to numeric type( types are int after dummy)

    #data_mod <- apply(data_mod, 1:2,function(x) as.numeric(x) )
 
 
#3. plots: outlyers/ transformation 
    
  #variable histograms: 
 #pdf("features_hist_vo")
  for( i in (1:ncol(data_mod)))
  {
    plot <- 
      ggplot( data = data, aes( x= data_mod[,i])) + 
      geom_histogram( 
                     col="green",fill="blue",alpha = .4) +
      labs(title= paste0("Histogram of " , colnames(data_mod)[i]," feature ")) +
      labs(x=paste0(colnames(data_mod)[i]), y="Count") 
    print( plot)
    Sys.sleep(3)
  }
 #  dev.off()

# delete rare cases: 
   #data_mod  <- remove_ONE_CLASS_features(df = data_mod, 
   #                                       max_class_percent = 0.9)
   #  510 366
   #  427 252
 
   
#6. normalization
     # data_mod <- cbind(data_mod %>% dplyr:: select(INS.TARGET) , 
     #                    data_mod %>% dplyr:: select(-INS.TARGET) %>%
     #                        scale( center = TRUE, scale = TRUE) ) 
     # 
     # dim( data_mod)
   
# function to remove continious x with discrete x : I( x > 0 ) 
#   (choose x with  count(0) > threshold)
#   do not take both, corr = 1!   
   

 
###clean_data------
 # ADD FILTERS: 
 data_mod1 <-  data_mod %>% 
   filter( INS.TAKEN_BEFORE_COMM == 0, 
           EVAL.COMM_NUMBER == 1, 
           CANC_N_BEFORE == 0)
           
           
 data_mod1 %<>% select( -CANC_SUM_AFTER, -CANC_N_AFTER)
 

   
    dim( data_mod);  dim( data_mod1); 
    
   # delete rare cases: 
     data_mod1 <- remove_ONE_CLASS_features( data_mod1,
                                              max_class_percent = 0.99)
                                       
   # 252 from  379
    
    
    data_mod2 <- omit_corr_feat(data_mod1, 
                                top_n_corr = 10,
                                omit_action = TRUE, 
                                corr_threshold = 0.97) 
    
    
    data_mod2 %<>% filter(CHANNEL_CC == 0)
    data_mod2 %<>% select(-CHANNEL_CC)
    
    data_mod2 %>% View()
    
    data_mod2 %<>% remove_ONE_CLASS_features(max_class_percent = 0.98)
    
    for( i in (1:ncol(data_mod2)))
    {
      data_mod2[,i] <- FILL_NA(column =  data_mod2[,i], fill_type = "median" )
      cat(i,  "\n")
    }
    
#7. train/test
    ## 75% of the sample size
    smp_size <- floor(0.8 * nrow(data_mod2))
    set.seed(42)
    train_ind <- sample(seq_len(nrow(data_mod2)), size = smp_size)
    train <- data.frame( data_mod2[train_ind, ])
    test <-  data.frame( data_mod2[-train_ind, ])
    dim(train) ; dim(test)
    
#   K - fold CV;  
   #cvTools package:
    #cv_folds <- cvFolds( n = nrow(train), K = 5, type = "random")
    #Randomly shuffle the data
  
    #Create 5 equally size folds
    folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)
  
    
#### GLM CV ---------    
      
# does not learn    
    
    #Perform 5 fold cross validation
    for(i in 1:5){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- train[testIndexes, ]
      trainData <- train[-testIndexes, ]
     dim( trainData);  dim(testData)
      
     
     train_tmp <-select(trainData, INS.TARGET, AGE,
                        SD_IQ.FL_GOLD_N, SD_IQ.CL_EDU_HIG,  LIM.LAST_UT)
     
     
        # model_function:
       model <- glm( as.factor(INS.TARGET) ~.,family=binomial(link='logit'),
                    data= train_tmp ) 
       #toselect.x <- summary(model)$coeff[-1,4] < 0.05 # credit to kith
       # select sig. variables
       #relevant.x <- names(toselect.x)[toselect.x == TRUE]     
       #sig.formula <- as.formula(paste("INS.TARGET ~",paste(relevant.x, collapse= "+"))) 
       sig.formula <- as.formula("INS.TARGET" ~ "AGE" + "LIM.LAST_UT" + 
                                 "SD_IQ.FL_GOLD_N" + "SD_IQ.CL_EDU_HIG")
       
       #sig.formula <- as.formula("INS.TARGET" ~ "AGE" + "SD_IQ.FL_GOLD_N" +"INS.TARGET" ~ "AGE" + "SD_IQ.FL_GOLD_N" + "SD_IQ.CL_EDU_HIG") 
       #"SD_IQ.FL_GOLD_N" + "SD_IQ.CL_EDU_HIG" + "SD_IQ.FL_GOLD_N" )
       

       
       
       sign.model <- model
      sign.model <- glm( formula = sig.formula ,
                         family=binomial(link='logit'),
                         data= trainData )
     
      cat("*********  FOLD:", i, "  ********* \n")
      relevant.x <- a[-1]
      test_data_1  <- testData[ ,which( (colnames(train)) %in% relevant.x)  ]
      train_data_1  <-trainData[ ,which( (colnames(train)) %in% relevant.x)  ]
      mod_pred_ts <- predict(object = sign.model, newdata = test_data_1, type = "response") 
      mod_pred_tr <- predict(object = sign.model, newdata = train_data_1, type = "response") 
      h1 <- hist( mod_pred_ts, breaks  = 100, main = "predicted prob,train 4 folds")
      h2 <- hist( mod_pred_ts, breaks  = 100, main = "predicted prob,test fold")
      #print(h1);
      #print(h2);
    
      
      threshold <- c(0.07, 0.08, 0.1, 0.12)
      for(  j in (1:4))
      {
        my_pred_tr <- as.numeric( mod_pred_tr >  threshold[j] )
        my_pred_ts <- as.numeric( mod_pred_ts >  threshold[j] )
        
        my_acc_tr <- sum(  my_pred_tr == trainData$INS.TARGET )/ length(  trainData$INS.TARGET)
        my_acc_ts <- sum(  my_pred_ts == testData$INS.TARGET )/ length( testData$INS.TARGET)
        
        acc_0_tr <- sum( rep(0,length( trainData$INS.TARGET)) == trainData$INS.TARGET)/ length(trainData$INS.TARGET)
        acc_0_ts <- sum( rep(0,length( testData$INS.TARGET)) == testData$INS.TARGET)/ length(testData$INS.TARGET)
        #my_acc; acc_0
        conf_matr_tr <- confusionMatrix(data =  my_pred_tr, reference =  trainData$INS.TARGET,positive = "1")
        conf_matr_ts <- confusionMatrix(data =  my_pred_ts, reference =  testData$INS.TARGET, positive = "1")
        
        auc_tr = auc(trainData$INS.TARGET, mod_pred_tr);
        auc_ts = auc(testData$INS.TARGET, mod_pred_ts);
        
        #USE  conf_matr_tr$byClass!!
        
         N_tr <- length(my_pred_tr);        N_ts <- length(my_pred_ts)
        # TP_tr <- conf_matr_tr$table[2,2];  TP_ts <- conf_matr_ts$table[2,2];
        # FN_tr <- conf_matr_tr$table[1,2];  FN_ts <- conf_matr_ts$table[1,2];
        # FP_tr <- conf_matr_tr$table[2,1];  FP_ts <- conf_matr_ts$table[2,1];
        # TN_tr <- conf_matr_tr$table[1,1];  TN_ts <- conf_matr_ts$table[1,1];
        # 
        # acc_tr <-  (TP_tr +TN_tr)/N_tr;                     acc_ts  <-(TP_ts +TN_ts)/N_ts;        
        # sens_tr <-  TP_tr /( TP_tr + FN_tr );               sens_ts <- TP_ts /( TP_ts + FN_ts ); 
        # spec_tr <-   TN_tr/( TN_tr + FP_tr);                spec_ts <-  TN_ts/( TN_ts + FP_ts);
        # f1_tr <-  2*sens_tr*spec_tr/( sens_tr + spec_tr);   f1_ts   <-  2*sens_ts*spec_ts/( sens_ts + spec_ts);
        # kappa_tr <- conf_matr_tr$overall[2];                kappa_ts<- conf_matr_ts$overall[2]
        #   
        # stat_tab  <- data.frame()
        # stat_tab[1,(1:2)] <- c(N_tr, N_ts)
        # stat_tab[2,(1:2)] <- c(acc_tr, acc_ts)
        # stat_tab[3,(1:2)] <- c(acc_tr, acc_ts)
        # stat_tab[4,(1:2)] <- c(sens_tr,sens_ts)
        # stat_tab[5,(1:2)] <- c(spec_tr , spec_ts )
        # stat_tab[6,(1:2)] <- c( f1_tr, f1_ts)
        # stat_tab[7,(1:2)] <- c(  kappa_tr,  kappa_ts)
        # stat_tab[8,(1:2)] <- c( auc_tr,auc_ts)
        # colnames( stat_tab ) <- c("train", "test")
        # rownames( stat_tab ) <- c("N", "A")
        
        stat_tab  <- matrix(nrow = 15, ncol = 2)
        stat_tab[1,(1:2)] <- c( as.integer(N_tr), as.integer(N_ts) )
        stat_tab[2,(1:2)] <- c(conf_matr_tr$overall[1],conf_matr_ts$overall[1]  )
        stat_tab[3,(1:2)] <- c(conf_matr_tr$overall[2],conf_matr_ts$overall[2]  )
        stat_tab[4,(1:2)] <- c( auc_tr,auc_ts)
        stat_tab[(5:15),1] <- as.numeric( conf_matr_tr$byClass ) 
        stat_tab[5:15,2]  <-  conf_matr_ts$byClass
        
        stat_tab <- apply(stat_tab,1:2, function(x) round(x,2))
        stat_tab <- as.data.frame( stat_tab)
        colnames( stat_tab ) <- c("train", "test")
        rownames( stat_tab ) <- c("N", "accuracy", "kappa", "AUC", names(conf_matr_tr$byClass) )
          
        cat("******STAT_TABLE for ",i,"CV step with threshold = ",threshold[j],"****** \n" )
        print(stat_tab)  
        cat("\n\n")
      }
      cat(" -------------------------------------------------------------\n\n\n\n\n")
    }
    
### RANDOM_FOREST CV-------  
    
    #Perform 5 fold cross validation
    for(i in 1:5){
      #Segement your data by fold using the which() function 
      #i = 1
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- train[testIndexes, ]   # 8533
      trainData <- train[-testIndexes, ] # 34130 
      
      # model_function:
      main_cols <- c('SD_HF.AGE', 'BAL.LAST_BAL', 'CHANNEL_CC', 'SD_IQ.CL_INDUSTR_ENGINEERING', 
                    'EVAL.KOL_CRD_14',  'EVAL.DAYS_FROM_OPEN', ' LIM.LAST_LIM', 
                    'LIM.LAST_UT','BAL.TOTAL_BAL_POS_CHNG_SUM_180', 'SD_IQ.CL_AUTO_EX_Y', 
                    'SD_IQ.CL_SOC_ST_FULLWORK', 'SD_IQ.CL_INDUSTR_TRANSPORTATION', 
                     'SD_IQ.FL_SOC_Y',
                    'STATUS.CARD_STATUS_1', 'STATUS.CARD_STATUS_2', 'LIM.MAX_DLQ')
      
      simple_formula <- as.formula(glue("as.factor(INS.TARGET)~{paste(main_cols, collapse='+')}"))
      formula <- as.formula(glue("as.factor(INS.TARGET) ~.") )
      
      model_rf <- randomForest( simple_formula, 
                                data = trainData,
                                ntree = 5,
                                keep.forest = T)
      
      cat("*********  FOLD:", i, "  ********* \n")
      # not all data, select some of x cols
      relevant.x  <- colnames(trainData)[ -which(colnames(trainData) =="INS.TARGET")]
      test_data_1  <- testData[ ,which( (colnames(train)) %in% relevant.x)  ]
      train_data_1  <-trainData[ ,which( (colnames(train)) %in% relevant.x)  ]
      mod_pred_ts <- predict(object = model_rf, newdata = test_data_1,type = "prob")[,2] 
      mod_pred_tr <- predict(object = model_rf, newdata = train_data_1, type = "prob")[,2] 
      h1 <- hist( mod_pred_tr, breaks  = 100, main = "predicted prob,train 4 folds")
      h2 <- hist( mod_pred_ts, breaks  = 100, main = "predicted prob,test fold")
      #print(h1);
      #print(h2);
      
      threshold <- c(0.3, 0.4, 0.5, 0.6)
      for(  j in (1:length(threshold)))
      {
        my_pred_tr <- as.numeric( mod_pred_tr >  threshold[j] )
        my_pred_ts <- as.numeric( mod_pred_ts >  threshold[j] )
        
        #my_acc_tr <- sum(  my_pred_tr == trainData$INS.TARGET )/ length(  trainData$INS.TARGET)
        #my_acc_ts <- sum(  my_pred_ts == testData$INS.TARGET )/ length( testData$INS.TARGET)
        
        #acc_0_tr <- sum( rep(0,length( trainData$INS.TARGET)) == trainData$INS.TARGET)/ length(trainData$INS.TARGET)
        #acc_0_ts <- sum( rep(0,length( testData$INS.TARGET)) == testData$INS.TARGET)/ length(testData$INS.TARGET)
        #my_acc; acc_0
        conf_matr_tr <- confusionMatrix(data =  my_pred_tr, reference =  trainData$INS.TARGET,positive = "1")
        conf_matr_ts <- confusionMatrix(data =  my_pred_ts, reference =  testData$INS.TARGET, positive = "1")
        
        auc_tr = auc(trainData$INS.TARGET, mod_pred_tr);
        auc_ts = auc(testData$INS.TARGET, mod_pred_ts);
        
        #USE  conf_matr_tr$byClass!!
        
        N_tr <- length(my_pred_tr);        N_ts <- length(my_pred_ts)
        # TP_tr <- conf_matr_tr$table[2,2];  TP_ts <- conf_matr_ts$table[2,2];
        # FN_tr <- conf_matr_tr$table[1,2];  FN_ts <- conf_matr_ts$table[1,2];
        # FP_tr <- conf_matr_tr$table[2,1];  FP_ts <- conf_matr_ts$table[2,1];
        # TN_tr <- conf_matr_tr$table[1,1];  TN_ts <- conf_matr_ts$table[1,1];
        # 
        # acc_tr <-  (TP_tr +TN_tr)/N_tr;                     acc_ts  <-(TP_ts +TN_ts)/N_ts;        
        # sens_tr <-  TP_tr /( TP_tr + FN_tr );               sens_ts <- TP_ts /( TP_ts + FN_ts ); 
        # spec_tr <-   TN_tr/( TN_tr + FP_tr);                spec_ts <-  TN_ts/( TN_ts + FP_ts);
        # f1_tr <-  2*sens_tr*spec_tr/( sens_tr + spec_tr);   f1_ts   <-  2*sens_ts*spec_ts/( sens_ts + spec_ts);
        # kappa_tr <- conf_matr_tr$overall[2];                kappa_ts<- conf_matr_ts$overall[2]
        #   
        # stat_tab  <- data.frame()
        # stat_tab[1,(1:2)] <- c(N_tr, N_ts)
        # stat_tab[2,(1:2)] <- c(acc_tr, acc_ts)
        # stat_tab[3,(1:2)] <- c(acc_tr, acc_ts)
        # stat_tab[4,(1:2)] <- c(sens_tr,sens_ts)
        # stat_tab[5,(1:2)] <- c(spec_tr , spec_ts )
        # stat_tab[6,(1:2)] <- c( f1_tr, f1_ts)
        # stat_tab[7,(1:2)] <- c(  kappa_tr,  kappa_ts)
        # stat_tab[8,(1:2)] <- c( auc_tr,auc_ts)
        # colnames( stat_tab ) <- c("train", "test")
        # rownames( stat_tab ) <- c("N", "A")
        
        stat_tab  <- matrix(nrow = 15, ncol = 2)
        stat_tab[1,(1:2)] <- c( as.integer(N_tr), as.integer(N_ts) )
        stat_tab[2,(1:2)] <- c(conf_matr_tr$overall[1],conf_matr_ts$overall[1]  )
        stat_tab[3,(1:2)] <- c(conf_matr_tr$overall[2],conf_matr_ts$overall[2]  )
        stat_tab[4,(1:2)] <- c( auc_tr,auc_ts)
        stat_tab[(5:15),1] <- as.numeric( conf_matr_tr$byClass ) 
        stat_tab[5:15,2]  <-  conf_matr_ts$byClass
        
        stat_tab <- apply(stat_tab,1:2, function(x) round(x,2))
        stat_tab <- as.data.frame( stat_tab)
        colnames( stat_tab ) <- c("train", "test")
        rownames( stat_tab ) <- c("N", "accuracy", "kappa", "AUC",
                                  names(conf_matr_tr$byClass) )
        
        cat("******STAT_TABLE for ",i,"CV step with threshold = ",threshold[j],"****** \n" )
        print(stat_tab)  
        cat("\n\n")
      }
      cat(" -------------------------------------------------------------\n\n\n\n\n")
    }    
    
    
    
    feat_imp <- importance( model_rf, type = 2)
    varImpPlot( model_rf, sort = TRUE)
    
    # weights: 
    
    #https://stats.stackexchange.com/questions/164693/weights-in-glm-logistic-regression-imbalanced-data
    #Logistic Regression in Rare Events Data (King 2001)

    summary( model)
    summary(sign.model)
    test_data_1  <- testData[ ,which( (colnames(train)) %in% relevant.x)  ]
    mod_pred <- predict(object = sign.model, 
                        newdata = test_data_1, 
                        type = "response") 

     mean( mod_pred ); median( mod_pred); max( mod_pred); min( mod_pred)
     hist( mod_pred, breaks  = 100)
    
    #confusionMatrix(data =  mod_pred, reference =  testData$INS.TARGET)
    #acc  <- accuracy(predictions = , labels = ) 
    
    auc( acc)
    plot(acc)
    
    
    threshold <- 0.35
    my_pred <- as.numeric( mod_pred >  threshold  )
    
    my_acc <- sum(  my_pred == testData$INS.TARGET )/ length( testData$INS.TARGET)
    acc_0 <- sum( rep(0,length( testData$INS.TARGET)) == testData$INS.TARGET)/ length(testData$INS.TARGET)
    my_acc; acc_0
    
    
    conf_matr <- confusionMatrix(data =  my_pred, reference =  testData$INS.TARGET)
    conf_matr
    
    conf_matr$overall[1]
    
    
    
    auc(testData$INS.TARGET, mod_pred)
    
    # library(SDMTools)
    # confusion.matrix(obs, pred, threshold = 0.5)
    
    #confusionMatrix(data =  mod_pred, reference =  testData$INS.TARGET)
    #acc  <- accuracy(predictions = , labels = ) 
    
    
    # stepwise glm
    # nothing <- glm(low ~ 1,family=binomial)
    