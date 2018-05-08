library(ggplot2)
library(plotly)
library(magrittr)
library(dplyr)
#library(reshape)
#library(eeptools)
#library(tidyr)
#library(corrplot)
#library(stringr)
#library(lubridate) # for working with date objects
library(data.table) # for working with big tables
#library(glue)
#library(reshape2)
library(dummies)
#library(cvTools)
library(caret)
library(AUC)
library(pROC)
library(randomForest)       
library(xgboost)  
library(caret)


## NEED!
## 1) function to different model learn (one func)
## 2) function to  model learn which returns probabilities( without threshold)



## ADDITIONAL FUNCTIONS

# make sub sample of  "0"(major) class
# k = #neg_class/ #pos_class 
# train: df with INS.TARGET variable 
# return: random train df with appropriate class balance(k) 
#
# COMM: test has to be with prior class balances
subSample <- function(train, k, seed1 =1212, seed2 =1211)
{
  # subsample of "0" class: 
  #k = 1 # class balance
  n_pos <- sum( train$INS.TARGET == 1)
  n_neg <- k*n_pos
  
  train_pos <- train %>% filter( INS.TARGET == 1)
  train_neg <- train %>% filter( INS.TARGET == 0)
  
  set.seed( seed1 )
  rand_tmp <- sample.int(n = nrow(train_neg), 
                         size =  n_neg, replace = F)
  train_neg <- train_neg[ rand_tmp, ] 
  
  train <- rbind( train_pos, train_neg)
  set.seed( seed2)
  train <-  train[ sample(1:nrow(train)), ]
  #table( train$INS.TARGET)
  return(train)
}



#  FUNCTION to normalize data: 
# tr = train data set 
# ts = test data set
# col_idx = index of columns which should be trandformed ( exclude target, binary variables)
# 
# COMMENT: calculate statistics on train and implement them on test 
# return: modified train and test data sets
normalization <- function( tr, ts, col_idx, type = c("mean_sd", "min_max"))
{
  if( type == "mean_sd")
  {
    cat(" Mean - sd normalization on train data\n")
    mean_tr <- apply( tr[, col_idx], 2,function(x) mean(x, na.rm = TRUE))
    sd_tr   <- apply( tr[, col_idx], 2,function(x) sd(  x, na.rm = TRUE))
    tr[,col_idx] <- sapply(  1: length(col_idx),  function(x, y) 
      ( y[,x] -  mean_tr[x])/ sd_tr[x], y = tr[,col_idx])
    ts[,col_idx] <- sapply(  1: length(col_idx),  function(x, y) 
      ( y[,x] -  mean_tr[x]/ sd_tr[x]), y = ts[,col_idx])
  }
  if( type == "min_max")
  {
    cat(" MAX - MIN  normalization on train data\n")
  }
  return( list(tr, ts))
}
# test:
# a1 <- c( 1,54,7,4,78,4)
# a2 <- c( 3,534,23,24,0.78,0)
# a3 <- c( 1,54,7,4,78, NA)
# a4 <- c( 123,32, 23,78,4, -100)
# A <-  rbind(a1, a2,a3, a4 )
# 
# B <- rbind( c(1,2,3,4,5,6), c(-1,-3,34,-56,12, NA))
# tmp <- normalization( tr =A, ts = B,col_idx = c(2,3,4,5), type = "mean_sd")
# tmp[[1]]

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

# distributions plot-----
  distribution_plot <- function( dat, index)
  {
      # age : i= 136
      i <- index    
      dat$INS.TARGET %<>% as.factor()
      plot1 <- ggplot( data = dat, aes(x = (dat[,i]+1),
                                   fill = INS.TARGET)) + 
      #geom_histogram(bins = 100 ) + 
      #geom_histogram(bins = 30,  alpha = 0.5) + 
      geom_density(  alpha = 0.5) + 
      #facet_wrap( ~factor(EVAL.GROUP) , ncol = 2) + 
      ggtitle( paste0( "Density of ", colnames(dat)[i] )) + 
      xlab( "log(x)" )
      #print( plot1)  
      #plot1
      
      plot2 <- ggplot( data = dat, aes(x = (dat[,i]+1) , fill = INS.TARGET)) + 
      #geom_histogram(bins = 100 ) + 
      geom_histogram(bins = 50,  alpha = 0.5) + 
      # facet_wrap( ~factor(EVAL.GROUP) , ncol = 2) + 
      ggtitle( paste0( "Hist of ", colnames(dat)[i] ))+ 
      xlab( "log(x)" )
      #plot2 
      #print( plot2)
      
      plot_return  <-  multiplot(plot1, plot2 )
      #Sys.sleep(8)
      cat(colnames(dat)[i], "\n")
      return( plot_return)
  }

# add boxplots, descreate graphics


# LEARM MODELS FUNCTIONS

FOREST_learn <- function(train, target,
                         threshold = 0.8, 
                         ntree = 50,
                         weight = 1,
                         nodesize = 1, 
                         maxnodes = 100,
                         mtry = floor( sqrt(ncol(train) -1 )),
                         seed1 = 9230, seed2 = 3233, sampling = 0, ...)
{ 
  set.seed(seed1)
 # testIndexes <- sample(1:nrow(train))[ ( 1 : trunc(0.2*nrow(train))) ] 
  #testData <- train[testIndexes, ]
  #trainData <- train[-testIndexes, ]
  
  train.index <- createDataPartition( train$INS.TARGET, p = .8, list = FALSE)
  trainData <-  train[ train.index,]
  testData  <- test[-train.index,]
  
  if(sampling != 0 )
  {
    trainData %<>% subSample(k = sampling)
  }
  cat( class(  trainData[, target]), "\n")
  cat( "TRAN DATA OBS NUMBER = " , nrow(trainData) ,
       "******* TEST DATA OBS NUMBER = ",  nrow(testData), "\n")
  cat("FeaTUERS  number: ", ncol(trainData) -1, "\n")
  #table( testData$INS.TARGET);  table( trainData$INS.TARGET)
  tr_rate <- round( sum(trainData$INS.TARGET == 1  )/ nrow(trainData ),3)
  ts_rate <- round( sum(testData$INS.TARGET == 1  )/ nrow(testData ),3)
  cat( "TRAN DATA RATE = " , tr_rate , "******* TEST DATA RATE = ",  ts_rate, "\n")
  
  # https://stats.stackexchange.com/questions/157714/r-package-for-weighted-random-forest-classwt-option
  
  model <- randomForest( x = trainData[-which(colnames(trainData) == target)],
                         y = trainData[, target],
                         formula = as.formula(paste(target, "~ .")), 
                         importance=TRUE,
                         strata = trainData[, target],
                         ntree = ntree,
                         mtry= mtry, 
                         replace = FALSE,
                         #sampsize = c(), 
                         classwt = c( 1, weight),
                         #cutoff = c( 0.94, 0.06), 
                         nodesize = nodesize, 
                         maxnodes = maxnodes , # if omitted full trees are grown
                         do.trace = 1, 
                         keep.forest = TRUE)
  
  
  
  #summary( model )
  pred_prob_tr <- predict(object = model, newdata = trainData, type = "prob")[,2]
  pred_prob_ts <- predict(object = model, newdata = testData, type = "prob")[,2]
  prob_hist_tr <- hist(  pred_prob_tr, breaks = 100)
  train_preds <- as.numeric( pred_prob_tr > threshold)
  test_preds  <- as.numeric( pred_prob_ts > threshold)
  
  tr_one_class_check  <- (length(unique( train_preds)) > 1) 
  ts_one_class_check  <- (length(unique(test_preds)) > 1)
  if(  tr_one_class_check )
    cm0 <- confusionMatrix(reference = trainData[,target], data = train_preds, positive = '1')
  if( ts_one_class_check  ) 
    cm1 <- confusionMatrix(reference =  testData[ ,target],data = test_preds, positive = '1')
  auc_tr <- auc(trainData[,target], pred_prob_tr)
  auc_ts <- auc(testData[ ,target], pred_prob_ts)
  
  #cat("2\n")
  stat_tab  <- matrix(nrow = 15, ncol = 5) # ncol = 4
  stat_tab[1,(1:2)] <- c( nrow(trainData), nrow(testData) )
  if(  tr_one_class_check & ts_one_class_check )
  {
    stat_tab[2,(1:2)] <- c(cm0$overall[1],cm1$overall[1]  )
    stat_tab[3,(1:2)] <- c(cm0$overall[2],cm1$overall[2]  )
    stat_tab[4,(1:2)] <- c( auc_tr,auc_ts)
    stat_tab[(5:15),1] <- as.numeric( cm0$byClass ) 
    stat_tab[(5:15),2] <-  as.numeric(cm1$byClass )
  }
  stat_tab <- apply(stat_tab,1:2, function(x) round(x,3))
  stat_tab <- as.data.frame( stat_tab)
  
  colnames( stat_tab )[1:2] <- c("train", "test")
  if(  tr_one_class_check & ts_one_class_check )
    rownames( stat_tab ) <- c("N", "accuracy", "kappa", "AUC", names(cm0$byClass) )
  colnames( stat_tab )[3] <- "threshold"
  stat_tab[(1:15), 3 ] <-  threshold
  colnames( stat_tab )[4] <- "ntree"
  stat_tab[(1:15), 4 ] <- ntree
  #colnames( stat_tab )[5] <- "mtry"
  #stat_tab[(1:15), 5 ] <- mtry
  
  tr_pr_rate  <- round( sum(train_preds==1)/ length( train_preds),3)
  ts_pr_rate  <- round( sum(test_preds==1)/ length( test_preds),3)
  cat( "  TRAIN PREDICTED RATE = " , tr_pr_rate  , "\n ",
       "TEST PREDICTED RATE = ",   ts_pr_rate , "\n\n\n")
  rm(cm0, cm1)
  return( list( stat_tab, model, varImpPlot( model, type = 2))  )
}

# test
# need train_1 data set
# stat_tmp1 <- FOREST_learn(train = train_1, sampling = 0,seed1 = 112622, seed2 =421, 
#                           target = "INS.TARGET",
#                           threshold = 0.68, 
#                           ntree = 60, 
#                           mtry = 1, 
#                           weight =1,
#                           maxnodes = 6,
#                           nodesize = 600)
# stat_tmp1[[1]] %>% View()
# stat_tmp1[[2]] 



LOGIT_learn <- function( train, target, normalize = FALSE,  threshold = 0.5, 
                         seed1 = 9230, seed2 = 3233, sampling = 0, ...)
{ 
  set.seed(seed1)
  #testIndexes <- sample(1:nrow(train))[ ( 1 : trunc(0.2*nrow(train))) ] 
  #testData <- train[testIndexes, ]
  #trainData <- train[-testIndexes, ]
  
  train.index <- createDataPartition( train$INS.TARGET, p = .8, list = FALSE)
  trainData <-  train[ train.index,]
  testData  <- train[-train.index,]
  
  
  if(sampling != 0 )
  {
    trainData %<>% subSample(k = sampling)
  }
  if(normalize)
  {
    # data_mod <- cbind(data_mod %>% dplyr:: select(INS.TARGET) , 
    #                                       data_mod %>% dplyr:: select(-INS.TARGET) %>%
    #   
    cat("normalize data...\n")
    norm_tmp <-   normalization(tr =trainData,
                                ts =testData, 
                                col_idx = which(colnames(trainData)!= 'target'), 
                                type = "mean_sd")
    trainData <- norm_tmp[[1]]
    testData <- norm_tmp[[2]]
  }
  cat( " TRAN DATA OBS NUMBER = " , nrow(trainData) ,"\n",
       " TEST DATA OBS NUMBER = ",  nrow(testData), "\n")
  cat("FeaTUERS  number: ", ncol(trainData) -1, "\n")
  #table( testData$INS.TARGET);  table( trainData$INS.TARGET)
  tr_rate <- round( sum(trainData$INS.TARGET == 1  )/ nrow(trainData ),3)
  ts_rate <- round( sum(testData$INS.TARGET == 1  )/ nrow(testData ),3)
  cat( "TRAN DATA RATE = " , tr_rate , "******* TEST DATA RATE = ",  ts_rate, "\n")
  
  
  model <- glm( data = trainData, formula = as.formula(paste(target, "~ .")), 
                family = binomial(link = "logit"))
  
  #summary( model )
  #cat("1\n")
  
  #cat("sas_1,\n")
  pred_prob_tr <- predict(object = model, newdata = trainData, type = "response")
  pred_prob_ts <- predict(object = model, newdata = testData, type = "response")
  prob_hist_tr <- hist(  pred_prob_tr, breaks = 100)
  train_preds <- as.numeric( pred_prob_tr > threshold)
  test_preds  <- as.numeric( pred_prob_ts > threshold)
  
  #cat("sas_2,\n")
  tr_one_class_check  <- (length(unique( train_preds)) > 1) 
  ts_one_class_check  <- (length(unique(test_preds)) > 1)
  if(  tr_one_class_check )
    cat("")
    cm0 <- confusionMatrix(reference =  trainData[,target],data = train_preds, positive = '1')
  if( ts_one_class_check  ) 
    cm1 <- confusionMatrix(reference = testData[ ,target],  data = test_preds, positive = '1')
    
  auc_tr <- auc( as.numeric(levels(trainData[,target]))[trainData[,target]] , pred_prob_tr)
  auc_ts <- auc(as.numeric(levels(testData[,target]))[testData[,target]], pred_prob_ts)
  
  #cat("sas_3,\n")
  stat_tab  <- matrix(nrow = 15, ncol = 3) # ncol = 4
  stat_tab[1,(1:2)] <- c( nrow(trainData), nrow(testData) )
  if(  tr_one_class_check & ts_one_class_check )
  {
    stat_tab[2,(1:2)] <- c(cm0$overall[1],cm1$overall[1]  )
    stat_tab[3,(1:2)] <- c(cm0$overall[2],cm1$overall[2]  )
    stat_tab[4,(1:2)] <- c( auc_tr,auc_ts)
    stat_tab[(5:15),1] <- as.numeric( cm0$byClass ) 
    stat_tab[(5:15),2] <-  as.numeric(cm1$byClass )
  }
  stat_tab <- apply(stat_tab,1:2, function(x) round(x,3))
  stat_tab <- as.data.frame( stat_tab)
  
  colnames( stat_tab )[1:2] <- c("train", "test")
  if(  tr_one_class_check & ts_one_class_check )
    rownames( stat_tab ) <- c("N", "accuracy", "kappa", "AUC", names(cm0$byClass) )
  colnames( stat_tab )[3] <- "threshold"
  stat_tab[(1:15), 3 ] <-  threshold
  #colnames( stat_tab )[4] <- "scale"
  #stat_tab[(1:15), 4 ] <- normalize ==TRUE
  
  tr_pr_rate  <- round( sum(train_preds==1)/ length( train_preds),3)
  ts_pr_rate  <- round( sum(test_preds==1)/ length( test_preds),3)
  cat( "TRAIN PREDICTED RATE = " , tr_pr_rate  , "\n ")
  cat( "TEST PREDICTED RATE = ",   ts_pr_rate , "\n\n\n")
  rm(cm0, cm1)
  return( list( stat_tab, summary(model), ts_pr_rate)  )
}

# test LOGIT_learn
# need train_1   
# stat_tmp <- LOGIT_learn( train =  train, sampling = 0,seed1 =1432, seed2 = 423,
#                          target = "INS.TARGET", threshold = 0.09, normalize = FALSE)
# 
# stat_tmp[[1]] %>% View()
# stat_tmp[[2]]


# to add: oversampling and undersampling of trin data
# test class  balances have to remain the same  
XGB_learn <- function( train, param_list = list(),nrounds, threshold = 0.5, 
                       seed1 = 23, seed2 = 132)
{ 
  set.seed(seed1)
  
  #testIndexes <- sample(1:nrow(train))[ ( 1 : trunc(0.2*nrow(train))) ] 
  if( is.factor(train$INS.TARGET))
    train$INS.TARGET <- as.numeric(levels(train$INS.TARGET))[train$INS.TARGET]
  
  train.index <- createDataPartition( train$INS.TARGET, p = .8, list = FALSE)
  trainData <-  train[ train.index,]
  testData  <- train[-train.index,]
  #testData <- train[testIndexes, ]
  #trainData <- train[-testIndexes, ]
  #dim(testData ); dim( trainData)
  
  #table( testData$INS.TARGET);  table( trainData$INS.TARGET)
  #round( sum(testData$INS.TARGET == 1  )/ nrow(testData ),3)
  #round( sum(trainData$INS.TARGET == 1  )/ nrow(trainData ),3)
  xgb_train <- xgb.DMatrix( trainData %>% 
                              select(-INS.TARGET) %>% as.matrix,
                            label = trainData$INS.TARGET)
  xgb_test <- xgb.DMatrix( testData %>% select(-INS.TARGET) %>% as.matrix,
                           label = testData$INS.TARGET)
  
  xgb_model <- xgboost(params = param_list, nrounds = nrounds, 
                       data = xgb_train )
  feature_names <- colnames(trainData  %>% select(-INS.TARGET ))
  importance1 <- xgb.importance(feature_names,
                                model = xgb_model )
  importance_plot <- xgb.ggplot.importance(importance1,top_n = 70)
  tree_plot <- xgb.plot.tree(xgb_model, n_first_tree = 2, feature_names = feature_names)
  
  #hist(predict(xgb_model, newdata = xgb_train), breaks = 200)
  #threshold <- 0.5
  train_preds <- predict(xgb_model, newdata = xgb_train) %>% {1 * (. > threshold)}
  tr_one_class_check  <- (length(unique( train_preds)) > 1) 
  #mean(  train_preds)
  #table(train_preds)
  #mean(train_preds == trainData$INS.TARGET)
  #table(trainData$INS.TARGET, train_preds)
  if(  tr_one_class_check )
  {
    cm0 <- confusionMatrix(reference= trainData$INS.TARGET, data = train_preds, positive = '1')
    cm0$byClass['F1']
  }
  # hist(predict(xgb_model, newdata = xgb_test), breaks = 200)
  test_preds <- predict(xgb_model, newdata = xgb_test) %>%{1 * (. > threshold)}
  mean(test_preds == testData$INS.TARGET)
  ts_one_class_check  <- (length(unique(test_preds)) > 1)
  #table(testData$INS.TARGET, test_preds)
  
  #table(test_preds)
  if(  ts_one_class_check )
  {
    cm1 <- confusionMatrix(reference = testData$INS.TARGET, data = test_preds, positive = '1')
    cm1$byClass['F1']
  }
  auc_tr <- auc(trainData$INS.TARGET, predict(xgb_model, newdata = xgb_train))
  auc_ts <- auc(testData$INS.TARGET, predict(xgb_model, newdata = xgb_test))
  
  
  tr_pr_rate  <- round( sum(train_preds==1)/ length( train_preds),3)
  ts_pr_rate  <- round( sum(test_preds==1)/ length( test_preds),3)
  
  stat_tab  <- matrix(nrow = 15, ncol = 9)
  stat_tab[1,(1:2)] <- c( nrow(trainData), nrow(testData) )
  if(  tr_one_class_check &  ts_one_class_check )
  {
    stat_tab[2,(1:2)] <- c(cm0$overall[1],cm1$overall[1]  )
    stat_tab[3,(1:2)] <- c(cm0$overall[2],cm1$overall[2]  )
  }
  stat_tab[4,(1:2)] <- c( auc_tr,auc_ts)
  stat_tab[(5:15),1] <- as.numeric( cm0$byClass ) 
  stat_tab[(5:15),2] <-  as.numeric(cm1$byClass )
  
  stat_tab <- apply(stat_tab,1:2, function(x) round(x,3))
  stat_tab <- as.data.frame( stat_tab)
  
  colnames( stat_tab )[1:2] <- c("train", "test")
  if( tr_one_class_check)
    rownames( stat_tab ) <- c("N", "accuracy", "kappa", "AUC", names(cm0$byClass) )
  colnames( stat_tab )[3] <- "threshold"
  stat_tab[(1:15), 3 ] <- threshold
  colnames( stat_tab )[4] <- "nrounds"
  stat_tab[(1:15), 4 ] <- nrounds
  colnames( stat_tab )[5:9] <- names(unlist(param_list)[-c(1,2)])
  param_vec <- as.numeric( unlist(param_list)[-c(1,2)] )
  for( i in (1:15))
  {
    stat_tab[i,(5:9) ] <-   param_vec
  }
  #stat_tab %<>% mutate(  fit_qual  = round( test/train,3) )
  return(  list(stat_tab, importance_plot, tree_plot, xgb_model,  cm1 ,  ts_pr_rate)  )
}

## test XGB_learn
## need train_1

# params_to_test <- list(
#   "objective"           = "binary:logistic",
#   "eval_metric"         = "logloss",
#   "eta"                 = 0.4,
#   "max_depth"           = 2,
#   "min_child_weight"   = 8,
#   "gamma"              = 0.7,
#   #"subsample"         = 1,
#   #"colsample_bytree"  = 0.95,
#   #"scale_pos_weight   = 2
#   "alpha"              = 0,
#   "lambda"             = 0,
#   "seed"               = seed2
# )

# tmp <- XGB_learn( train =  train_1, param_list = params_to_test,
#                   nrounds = 9, threshold = 0.09, 
#                   seed1 =2322, seed2 =32)
# #tmp[[2]]
# #tmp[[3]]
# stats<-  tmp[[1]]
# stats %>% View()




##choose formula with xgb on validaion--------

# WANT :
# 1. formula has better score ( choose f1, AUC)
# 2. formula do not overfit ( choose list with metrics and its train/test bound)
# 3. some params tuning


# # try steps by feature importance, increase  feature number 
# # need ordered list of features  
# 
# train_tmp <- XGB_learn( train =  train_3, param_list = params_to_test,
#                         nrounds = 30, threshold = 0.09, 
#                         seed1 = 35, seed2 = 423)
# features_ordered_list <- train_tmp[[2]]$data %>% 
#   select( Feature, Gain ) %>% 
#   arrange( desc(Gain)) %>% 
#   select( Feature)
# features_ordered_list <-  features_ordered_list$Feature
# cat( "STEP O: all featurs, N = ", length( features_ordered_list ))
# 
# while( feature_count >=3 )
# {
#   train_tmp <- train
# }


### GRID SEARCH PARAMS XGB-----

# define grids:
# 648
# eta_grid <- c(0.1, 0.05, 0.01) 
# nrounds_grid <- c( 1 , 2, 3, 5, 10, 20, 100, 200)
# alpha_grid <- c( 0, 10, 30)
# lambda_grid <- c( 0, 10, 30)
# threshold_grid <- c( 0.45, 0.5, 0.55)






## logistic regression with penalty------
# library(glmnet)
# 
# X = as.matrix( train %>% select(-INS.TARGET, -CHANNEL_CC))
# X_sc <- scale(X, scale = TRUE, center = TRUE)
# y = as.matrix( train %>% select(INS.TARGET))
# glmmod_l1 <- glmnet(x = X, y=as.factor(y), alpha=0.3, family="binomial")
# glmmod_l1
# plot(glmmod_l1, xvar="lambda")
# coef_lasso <- coef(glmmod_l1)[, 10]
# coef_lasso_not_null <- coef_lasso[ which( abs(coef_lasso) > 0.00001 )]
# coef_lasso_not_null
# 
# # cv
# cv.glmmod <- cv.glmnet(x=X, y = as.factor(y), alpha=0.5,  family="binomial", 
#                        nfolds = 10, type.measure = "auc")
# #  type.measure = "auc"
# plot(cv.glmmod)
# cv.glmmod$lambda.min


### DATA PROCESSING ----------------
# old data:

insur_data_preprocess <- function( feat_table, verbose=FALSE,
                                   max_class_flag=TRUE, max_class_percent=0.99, 
                                   fill_factors=TRUE,
                                   fill_continious=TRUE, fill_type="median",
                                   omit_corr=TRUE)
{
  cat("BAD ids: remove\n")
  
  #bad_id_list =  one id has 2 or mmore rows
  # id = ( client_id,date)
  bad_id_list <- feat_table %>% 
    group_by( ID) %>% 
    summarise( n = n()) %>% 
    filter( n > 1) %>%
    select(ID) %>%
    unlist() 
  #as.numeric()
  if(verbose)
    cat("Duplicated ids count:", length(bad_id_list), "\n") # 162
  
  # remove them:
  feat_table %<>% filter( !(feat_table$ID %in% bad_id_list) ) 
  
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
  feat_table %<>% filter( #INS.DAYS_REACT < 30, 
    INS.TAKEN_BEFORE_COMM == 0, 
    EVAL.COMM_NUMBER == 1, 
    EVAL.IS_BEEN_CALLED == 1, # error: meen  IS_BEEN_CALLED == 0
    CANC_N_BEFORE == 0) # COMM.  all: -396 obs only!
  #CHANNEL == "CC")
  #EVAL.GROUP == 1) 
  
  #remove wrong features-----
  tmp <- feat_table %>% colnames( )
  
  if( max_class_flag)
    feat_table %<>% remove_ONE_CLASS_features( max_class_percent = max_class_percent)
  cat("\n\n")
  
  # warning: may be problem with some fields absence 
  feat_table %<>% select( -ID , -CANC_SUM_AFTER, -CANC_N_AFTER )
  
  
  # fill NA values: 
  NA_list <-apply( feat_table, 2, 
                   function(x) round( sum( is.na(x))/nrow( feat_table_6) ,2) )
  NA_list %<>% sort(decreasing = TRUE )
  NA_list[1:10]
  
  #FIll na with sample: 
  
  #   trandform all cols to factors 
  #   count unique value to check
  
  # work with it!  
  feat_table$SD_IQ.CL_INDUSTR %<>%  as.factor()
  
  cols_to_fill_na_facotrs  <-  names(Filter(is.factor, feat_table))
  
  
  if(fill_factors)
  {
    cat("Fill in NA values in factors, use sampling\n\n")
    for( col in cols_to_fill_na_facotrs )
    {
      feat_table[, col] <-
        FILL_NA( column =  feat_table[, col], fill_type = "sample", seed = 11)
      cat( col, "\n")
    }
  }
  
  if(fill_continious)
  {
    # Fill other NA with median
    for( i in (1:ncol(feat_table)))
    {
      feat_table[,i] <- FILL_NA(column =  feat_table[,i], fill_type = fill_type )
      cat(i,  "\n")
    }
  } 
  
  cat("NA count in feature matrix:\n")
  sum( is.na(feat_table))
  
  # dummy: 
  # have to remove linear dependences!
  # train and test have to be transformed together idenntically!
  cat("Dummy hot encoding\ n")
  cat("Warning: 
      train and test have to be transformed together identically\ n")
  feat_table %<>% dummy.data.frame( sep = "_", verbose = TRUE )
  
  if(omit_corr)
    feat_table %<>% omit_corr_feat( top_n_corr = 20,
                                    omit_action = TRUE, print_info = TRUE,
                                    print_plot = FALSE, plot_names = FALSE, 
                                    corr_threshold = 0.99)
  
  feat_table$INS.TARGET %<>% as.factor() 
  
  
  # omit bonus!
  feat_table <- feat_table[ ,-grep("BONUS",colnames( feat_table)) ]
  
  return( feat_table) 
}



 # NEW DATA
insur_data_preprocess_new <- function( feat_table, verbose=FALSE,
                                       max_class_flag=FALSE, max_class_percent=0.99, 
                                       fill_factors=TRUE,
                                       fill_continious=TRUE, fill_type="median",
                                       omit_corr=FALSE)
{
  
  
  #FILTERING CLEAN DATA---------- 
  # filters: 
  
  # duration > n 
  # has cancelled == 0 
  # taken_before_comm == 0 
  # group = ? 
  # DAYS_REACT < m 
  # choose channel (GRAN)
  
  # clean data -----
  # feat_table %<>% filter( #INS.DAYS_REACT < 30, 
  #   INS.TAKEN_BEFORE_COMM == 0, 
  #   EVAL.COMM_NUMBER == 1, 
  #   EVAL.IS_BEEN_CALLED == 1, # error: meen  IS_BEEN_CALLED == 0
  #   CANC_N_BEFORE == 0) # COMM.  all: -396 obs only!
  #CHANNEL == "CC")
  #EVAL.GROUP == 1) 
  
  #remove wrong features-----
  #tmp <- feat_table %>% colnames( )
  
  if( max_class_flag)
    feat_table %<>% remove_ONE_CLASS_features( max_class_percent = max_class_percent)
  cat("\n\n")
  
  # fill NA values: 
  NA_list <-apply( feat_table, 2, 
                   function(x) round( sum( is.na(x))/nrow( feat_table) ,2) )
  NA_list %<>% sort(decreasing = TRUE )
  NA_list[1:10]
  
  #FIll na with sample: 
  
  #   trandform all cols to factors 
  #   count unique value to check
  
  # work with it!  
  #feat_table$SD_IQ.CL_INDUSTR %<>%  as.factor()
  
  feat_table$CONTRACT_NUMBER %<>%  as.character()
  
  cols_to_fill_na_facotrs  <-  names(Filter(is.factor, feat_table))
  cols_to_fill_na_facotrs <- c(  cols_to_fill_na_facotrs, 
                                 "SD_HF.GENDER",  "SD_HF.MARITAL_STATUS" ,          
                                 "SD_HF.BRANCH",   "SD_HF.EDUCATION_LEVEL",       
                                 "SD_HF.OCCUPATION",  "SD_HF.APPOINTMENT_LEVEL",        
                                 "SD_HF.OWN_CAR_FLAG", 
                                 "SD_IQ.CL_SOC_ST", 
                                 "SD_IQ.CL_INDUSTR", 
                                 "SD_IQ.CL_ESTATE_TYPE")
  
  
  if(fill_factors)
  {
    cat("Fill in NA values in factors, use sampling\n\n")
    for( col in cols_to_fill_na_facotrs )
    {
      feat_table[, col] <-
        FILL_NA( column =  feat_table[, col], fill_type = "sample", seed = 11)
      cat( col, "\n")
    }
  }
  
  if(fill_continious)
  {
    # Fill other NA with median
    for( i in (1:ncol(feat_table)))
    {
      feat_table[,i] <- FILL_NA(column =  feat_table[,i], fill_type = fill_type )
      cat(i,  "\n")
    }
  } 
  
  cat("NA count in feature matrix:\n")
  sum( is.na(feat_table))
  
  # dummy: 
  # have to remove linear dependences!
  # train and test have to be transformed together idenntically!
  cat("Dummy hot encoding\ n")
  cat("Warning: 
      train and test have to be transformed together identically\ n")
  
  feat_table_ids <- feat_table %>%
    select( CONTRACT_NUMBER, CLIENT_ID)
  
  feat_table %<>% select(-CONTRACT_NUMBER, -CLIENT_ID, -IQ_ID) %>% 
    dummy.data.frame( sep = "_", verbose = TRUE )
  
 # feat_table <- cbind(feat_table_ids,feat_table)
  
  
  if(omit_corr)
    feat_table %<>% omit_corr_feat( top_n_corr = 20,
                                    omit_action = FALSE, print_info = TRUE,
                                    print_plot = FALSE, plot_names = FALSE, 
                                    corr_threshold = 0.99)
  
  
  # omit bonus!
  #feat_table <- feat_table[ ,-grep("BONUS",colnames( feat_table)) ]
  
  # return( list(feat_table, feat_table_ids) )
  return( list(feat_table,feat_table_ids ) )
}






