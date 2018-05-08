# simple model



test_size <- 0.15
set.seed(423434)
test_idx <- sample(1:dim(train)[1], size=as.integer(test_size * dim(train)[1]))

test_df <- train[test_idx,]
train_df <- train[-test_idx,]

assertthat::are_equal(dim(train)[1], dim(test_df)[1] + dim(train_df)[1])

#bal_columns <- train_df %>% colnames %>% {.[str_detect(.,'BAL')]}
#main_cols <- c('SD_HF.AGE', 'BAL.LAST_BAL', 'CHANNEL_CC', 'SD_IQ.CL_INDUSTR_ENGINEERING')
main_cols <- c('SD_HF.AGE', 'BAL.LAST_BAL', 'CHANNEL_CC', 
               'EVAL.KOL_CRD_14',  'EVAL.DAYS_FROM_OPEN', ' LIM.LAST_LIM', 
               'LIM.LAST_UT','BAL.TOTAL_BAL_POS_CHNG_SUM_180', 'LIM.MAX_UT', 
               'SD_IQ.CL_SOC_ST_FULLWORK',
               'STATUS.CARD_STATUS_1', 'STATUS.CARD_STATUS_2', 
               'TRANS.TOTAL_MEAN_31','  TRANS.TOTAL_N_31')
                

simple_formula <- as.formula(glue("as.factor(INS.TARGET)~{paste(main_cols, collapse='+')}"))
simple_formula 

simple_model <- glm(simple_formula,
                    family = binomial(link = 'logit'),
                    data = train_df) # %>%
#mutate_at(bal_columns, scale))

#simple_model
#formula <- as.formula("as.factor(INS.TARGET) ~ .")
#train_preds <- predict(simple_formula, newdata = train_df,# %>%
                       #mutate_at(bal_columns, scale),
                       #type='response')
#mean(train_preds == train_df$INS.TARGET)
#table(1 * (train_preds > 0.5), train_df$INS.TARGET)

#mean(train_preds > 0.5)

library(randomForest)

rf <- randomForest(simple_formula , data = train_df, ntree = 10, mtry = 4,
                   nodesize=5, classwt=c(0.815, 0.82),
                   maxnodes=2)
#rf  #36264
#train_preds <- predict(rf, newdata = train_df, type='prob')[,2] %>%
# {as.numeric(.) - 1}
train_preds <- predict(rf, newdata = train_df, type='response')%>%
{as.numeric(.) - 1}
mean(train_preds == train_df$INS.TARGET)
table(train_df$INS.TARGET, train_preds)

test_preds <- predict(rf, newdata = test_df, type='response') %>%
{as.numeric(.) - 1}
mean(test_preds == test_df$INS.TARGET)
table(test_df$INS.TARGET, test_preds)

### RRF-----
library(RRF)
set.seed(1)


# Feature selection via RRF
lambda <- 0.9 # Both the number of features and the quality of the features are quite sensitive to lambda for RRF. A smaller lambda leads to fewer features.
rrf <- RRF(train_df %>% select(-INS.TARGET),#one_of(main_cols)),
           as.factor(train_df$INS.TARGET),
           ntree = 8,
           flagReg = 1,
           classwt = c(0.9, 0.91),
           coefReg = lambda) # coefReg is a constant for all variables.   #either "X,as.factor(class)" or data frame like "Y~., data=data" is fine, but the later one is significantly slower. 
train_preds <- predict(rrf, newdata = train_df) %>% 
  {as.numeric(.) - 1}
mean(train_preds == train_df$INS.TARGET)
table(train_df$INS.TARGET, train_preds)

test_preds <- predict(rrf, newdata = test_df, type='response') %>%
  {as.numeric(.) - 1}
mean(test_preds == test_df$INS.TARGET)
table(test_df$INS.TARGET, test_preds)
cm1 <- confusionMatrix(test_df$INS.TARGET, test_preds, 
                       positive = '1')
cm1$byClass
cm1$byClass['F1']

######## xgboost -----
library(xgboost)

set.seed(133443)

dim( train_df)
pos_class_n <- nrow( train_df  %>%  filter(INS.TARGET == 1))    
neg_class_n <- nrow( train_df  %>%  filter(INS.TARGET == 0))  


pos_class_n_tr <- 5000
neg_class_n_tr <- 1*pos_class_n_tr

pos_class_n_ts <- pos_class_n - pos_class_n_tr
#neg_class_n_ts <- neg_class_n - neg_class_n_ts
neg_class_n_ts = 5*pos_class_n_ts

# balance train: train "0":
pos_class_n_tr
# train "1": 
neg_class_n_tr

train_df1 <- train_df %>% 
  filter(INS.TARGET == 1) %>%
  dplyr::slice(1:pos_class_n_tr)

test_df1 <- test_df %>% 
  filter(INS.TARGET == 1) %>%
  dplyr::slice(1:pos_class_n_ts)

train_df0 <- train_df %>% 
  filter(INS.TARGET == 0) %>%
  dplyr::slice(1:neg_class_n_tr)

test_df0 <- test_df %>% 
  filter(INS.TARGET == 0) %>%
  dplyr::slice(1:neg_class_n_ts)


#assertthat::are_equal(dim(train)[1], dim(test_df)[1] + dim(train_df)[1])

### balanced ----
# Feature selection via RRF
train_df_bal <- rbind(train_df1, train_df0) %>% sample_n( pos_class_n_tr + neg_class_n_tr)
test_df_bal <- rbind(test_df1, test_df0) %>% sample_n(pos_class_n_ts + neg_class_n_ts)
dim(train_df_bal)
dim(test_df_bal)

# check: prevalance train  = 
round( nrow( train_df_bal %>% filter(INS.TARGET == 1))/ nrow( train_df_bal) ,2)
#check: prevalance test =
round( nrow( test_df_bal %>% filter(INS.TARGET == 1))/ nrow( test_df_bal) ,2)


xgb_train <- xgb.DMatrix(train_df %>% select(-INS.TARGET,
                                                 #-CHANNEL_CC,
                                                 -CHANNEL_GRAN) %>% as.matrix,
                         label = train_df$INS.TARGET)
xgb_test <- xgb.DMatrix(test_df %>% select(-INS.TARGET,
                                               #-CHANNEL_CC,
                                               -CHANNEL_GRAN) %>% as.matrix,
                        label = test_df$INS.TARGET)

params <- list(
  "objective"           = "binary:logistic",
   "eval_metric"         = "logloss",
   "eta"                 = 0.1,
   "max_depth"           = 5
  # "min_child_weight"    = 8,
  # "gamma"               = 0.70,
  # "subsample"           = 0.8,
  # "colsample_bytree"    = 0.95,
  # "alpha"               = 200,
  # "lambda"              = 50
)

xgb_model <- xgboost(params = params,
                     data = xgb_train,
                     nrounds = 10
                     #lambda = 14, #60
                     #alpha = 60,
                     #scale_pos_weight = 1.5,
                     #colsample_bytree = 1.
                     )
feature_names <- colnames(train_df %>% select(-INS.TARGET,
                                                  #-CHANNEL_CC,
                                                  -CHANNEL_GRAN))
importance1 <- xgb.importance(feature_names,
                              model = xgb_model )
xgb.ggplot.importance(importance1)
#xgb.ggplot.deepness(xgb_model)
xgb.plot.multi.trees(xgb_model, feature_names = feature_names)
xgb.plot.tree(xgb_model, n_first_tree = 2, feature_names = feature_names)
hist(predict(xgb_model, newdata = xgb_train), breaks = 100)
threshold <- 0.5
train_preds <- predict(xgb_model, newdata = xgb_train) %>%
  {1 * (. > threshold)}
mean(train_preds == train_df$INS.TARGET)
table(train_df$INS.TARGET, train_preds)
cm0 <- confusionMatrix(train_df$INS.TARGET, train_preds, 
                       positive = '1')
cm0$byClass['F1']

test_preds <- predict(xgb_model, newdata = xgb_test) %>%
  {1 * (. > threshold)}
mean(test_preds == test_df_bal$INS.TARGET)
table(test_df_bal$INS.TARGET, test_preds)


cm1 <- confusionMatrix(test_df_bal$INS.TARGET, test_preds, 
                       positive = '1')
cm1$byClass
cm1$byClass['F1']
auc(test_df_bal$INS.TARGET, predict(xgb_model, newdata = xgb_test))




# Feature selection via RRF
train_df <- train_df %>% sample_n(nrow(train_df))

xgb_train <- xgb.DMatrix(train_df %>% select(-INS.TARGET) %>% as.matrix,
                         label = train_df$INS.TARGET)
xgb_test <- xgb.DMatrix(test_df %>% select(-INS.TARGET) %>% as.matrix,
                         label = test_df$INS.TARGET)

params <- list(
  "objective"           = "binary:logistic"
  # "eval_metric"         = "logloss",
  # "eta"                 = 0.075,
  # "max_depth"           = 5,
  # "min_child_weight"    = 8,
  # "gamma"               = 0.70,
  # "subsample"           = 0.6,
  # "colsample_bytree"    = 0.95,
  # "alpha"               = 200,
  # "lambda"              = 500
)

xgb_model <- xgboost(params = params,
                     data = xgb_train,
                     nrounds = 190,
                     scale_pos_weight = 6.5)
hist(predict(xgb_model, newdata = xgb_train), breaks = 100)
threshold <- 0.5
train_preds <- predict(xgb_model, newdata = xgb_train) %>%
  {1 * (. > threshold)}
mean(train_preds == train_df$INS.TARGET)
table(train_df$INS.TARGET, train_preds)

test_preds <- predict(xgb_model, newdata = xgb_test) %>%
  {1 * (. > threshold)}
mean(test_preds == test_df$INS.TARGET)
table(test_df$INS.TARGET, test_preds)
cm1 <- confusionMatrix(test_df$INS.TARGET, test_preds, 
                       positive = '1')
cm1$byClass
cm1$byClass['F1']
#########

rrf <- RRF(train_df %>% select(-INS.TARGET),#one_of(main_cols)),
           as.factor(train_df$INS.TARGET),
           ntree = 8,
           flagReg = 1,
           classwt = c(0.9, 0.91),
           coefReg = lambda) # coefReg is a constant for all variables.   #either "X,as.factor(class)" or data frame like "Y~., data=data" is fine, but the later one is significantly slower. 
train_preds <- predict(rrf, newdata = train_df) %>% 
{as.numeric(.) - 1}
mean(train_preds == train_df$INS.TARGET)
table(train_df$INS.TARGET, train_preds)

test_preds <- predict(rrf, newdata = test_df, type='response') %>%
{as.numeric(.) - 1}
mean(test_preds == test_df$INS.TARGET)
table(test_df$INS.TARGET, test_preds)
cm1 <- confusionMatrix(test_df$INS.TARGET, test_preds, 
                       positive = '1')
cm1$byClass
cm1$byClass['F1']

######

train_preds <- as.numeric( predict(rf, newdata = train_df, type='prob')[,2])
hist(train_preds , breaks = 100 )
train_preds <- as.numeric( train_preds > 0.5)
table(train_preds, train_df$INS.TARGET)

confusionMatrix( train_preds, train_df$INS.TARGET)$table



test_preds <- predict(rf, newdata = test_df, type='prob')[,2] %>%
{as.numeric(.) }
hist(test_preds, breaks = 100)
test_preds <- as.numeric( test_preds > 0.5)
mean(test_preds == test_df$INS.TARGET)
table(test_preds, test_df$INS.TARGET)




# model_function:
#model <- glm( as.factor(INS.TARGET) ~.,family=binomial(link='logit'),
#             data= trainData ) 
#toselect.x <- summary(model)$coeff[-1,4] < 0.05 # credit to kith
# select sig. variables
#relevant.x <- names(toselect.x)[toselect.x == TRUE]     
#sig.formula <- as.formula(paste("INS.TARGET ~",paste(relevant.x, collapse= "+"))) 
sign.model <- glm( formula = sig.formula,
                   family=binomial(link='logit'),
                   data= trainData )




###### plot train / test distributions-----------


dat <- rbind( train_df %>% mutate( type  = "train"), test_df %>% mutate( type  = "test") )
dat$type %<>% as.factor()
dat$INS.TARGET %<>% as.factor()

# check 
nrow( dat %>% filter( type =="train") ) == nrow( train_df)
nrow( dat %>% filter( type =="test") ) == nrow( test_df)

# age : i= 136
i <- 203
plot <- ggplot( data = dat, aes(x = log( dat[,i] + 1) , fill = INS.TARGET)) + 
  #geom_histogram(bins = 100 ) + 
  geom_density( alpha = 0.5) + 
 # facet_wrap( ~ type, ncol = 1) + 
  ggtitle( paste0( "Hist of ", colnames(dat)[i] ))
plot








