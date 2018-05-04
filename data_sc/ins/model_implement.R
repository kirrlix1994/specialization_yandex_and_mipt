
library("RJDBC")
library("xgboost")

source("model_functions.R")




###### load data---
data_folder <- '../data_04_2018'
feat_table <- read.csv(file = glue("{data_folder}/FEATURE_MATRIX_NEW.csv"))
               
#colnames(feat_table) <-  gsub( pattern = "BAL.BAL.", x = colnames(feat_table), replacement = "BAL.") 
      
# 3 ids:  CLIENT_ID, CONTRACT_NUMBER, IQ_ID
feat_table1 <- insur_data_preprocess_new(feat_table)
feat_table_ids <-  feat_table1[[2]]
feat_table1 <- feat_table1[[1]]
feat_table1 %>% View()

# sapply( feat_table1, class)

#--------------
new_cols <-colnames(feat_table1)
colnames(feat_table1) <- str_trim(colnames(feat_table1))

old_cols <- colnames(feat_table_old )

new_cols <- str_trim(new_cols)
old_cols <- str_trim(old_cols)

extra_cols <- new_cols[ which(!new_cols %in% old_cols )]
used_cols <-  old_cols[ which(! old_cols %in% new_cols )]

#TRANS.TOTAL_MEAN/SUM_180
#SD_HF.GENDER
#MARITAL_STATUS
#EDUCATION_LEVEL
#APPOINTMENT_LEVEL
#OWN_CAR_FLAG
#APPOINTMENT_LEVEL
#LIM_MIN_POS_CHANGE ? 


# cols_to remove from model:
#  "CHANNEL_CC"  
#  "EVAL.KOL_CRD_14"   
#  "EVAL.DAYS_FROM_OPEN"    
#  "EVAL.GROUP"  
#  "STATUS."
#  "BAL.LAST_BAL"
#  "PH."
  
  
  
#"SD_HF.EMPLOYEE_FLAG_1" ? 
# SD_IQ.FL_PENS"
# SD_IQ.FL_SOC
# SD_IQ.FL_4P
# SD_IQ.FL_PENS_DOC
# FL_DOC_PROFIT
  

# intersect:
model_tmp_cols_list <- intersect(new_cols, old_cols)
length(model_tmp_cols_list)
length(new_cols)
length( old_cols)



###model------ 
TRAIN_MATR <- feat_table_old %>% select( one_of(model_tmp_cols_list), INS.TARGET)

TRAIN_MATR$INS.TARGET <- as.numeric(levels(TRAIN_MATR$INS.TARGET))[TRAIN_MATR$INS.TARGET]

library(xgboost)
xgb_train <- xgb.DMatrix( TRAIN_MATR %>%  select(-INS.TARGET) %>% as.matrix,
                          label =TRAIN_MATR$INS.TARGET)

TEST_MATR <- feat_table1 %>% select( one_of("CLIENT_ID", "CONTRACT_NUMBER", model_tmp_cols_list))
#TEST_MATR <- feat_table1[ ,which(colnames(feat_table1) %in% model_tmp_cols_list)]

# CHECK THAT COLNAMES ARE IDENTICAL
# for (i in (1:dim(TEST_MATR)[2])) 
# {
#   a[i] <- colnames(TRAIN_MATR)[i] == colnames(TEST_MATR)[i]
# }


xgb_test <- xgb.DMatrix(TEST_MATR %>% as.matrix)


xgb_model <- xgboost(params = params_to_test, nrounds = 20, seed1 = 12, seed2 = 45,
                     data = xgb_train )


feature_names <- colnames(TRAIN_MATR  %>% select(-INS.TARGET ))
tree_plot <- xgb.plot.tree(xgb_model, n_first_tree = 3, feature_names = feature_names)


PREDICTED_PROB_xgb <- round(predict(xgb_model, newdata = xgb_test),4)
hist( PREDICTED_PROB_xgb , breaks = 80, col = "purple")
abline(v = 0.169, col = 'red', lty=5)
abline(v = 0.193, col = 'green', lty=5)

##logit model ----
# use 61 stepwise forward features

forwards_cols_all <- readRDS( "LOGIT_forward_features_list")
TRAIN_MATR_2 <-  feat_table_old %>% 
  select( one_of( forwards_cols_all, "INS.TARGET" ))%>% 
  select(-SD_IQ.FL_MOBB_N, -LIM.LIM_MIN_NEG_CHANGE, 
         -TRANS.FLG_CATEGORY_0_MEAN_180)

TEST_MATR_2 <- feat_table1 %>% select( one_of(forwards_cols_all))

logit_model <-  glm( data = TRAIN_MATR_2, formula = as.formula(paste(target, "~ .")), 
                     family = binomial(link = "logit"))

summary(logit_model)

PREDICTED_PROB_logit <- round(predict(logit_model, newdata = TEST_MATR_2, type = "response"),4)
hist( PREDICTED_PROB_logit, breaks = 100, col = "green")


## output ---
file_with_preds <- glue("{data_folder}/INSUR_PREDICTIONS_DATA.csv")
file_with_preds_dir <- "Z:/Projects/insur/model_data_april/INSUR_PREDICTIONS_DATA.csv"

#file_with_preds_v2 <- glue("{data_folder}/INSUR_PREDICTIONS_DATA_v2.csv")
#file_with_preds_dir_v2 <-"Z:/Projects/insur/model_data_november/INSUR_PREDICTIONS_DATA_v2.csv"

#output_matr <- as.data.frame(cbind(feat_table_ids,PREDICTED_PROB1))
output_matr <- as.data.frame(cbind(feat_table_ids,PREDICTED_PROB_xgb, PREDICTED_PROB_logit))
dim(output_matr)
 
write.csv( x = output_matr, file = file_with_preds, row.names = FALSE) 
write.csv( x = output_matr, file = file_with_preds_dir,  row.names = FALSE) 

# write to sql base:
drv <- JDBC("oracle.jdbc.OracleDriver",
            classPath="C:/Users/Liksakov/Desktop/ojdbc7.jar", " ")

#PASSWORD <- "Binbank_way4_2"
PASSWORD <- "Binbank_way4_3"
UID = "LAVRENTIEV_AI"
#UID = "Gorbachev_SM"
con <- dbConnect(drv, user = UID, "jdbc:oracle:thin:@mos-way4db:1521:way4db", 
                 password = PASSWORD, host = "mos-way4db", port = 1521, 
                 dbname = "way4db" )

# test:
a <- dbGetQuery(con, "select count(*) from liks_ins_04_2018_preds")
dbWriteTable(con, "liks_ins_04_2018_preds",output_matr, overwrite=FALSE)


## SAVE model, plots and other stats:
saveRDS( object = xgb_model, file =  "XGB_FINAL_MODEL_april")
saveRDS( object = logit_model, file =  "LOGIT_FINAL_MODEL_april")





## test -----------------------------------------------------------------

# 272-P-79555129


ts <- feat_table1[c(2,nrow(feat_table1) -1),] 
#%>% select( one_of(model_tmp_cols_list))
#ts <- feat_table1[ 2,]
ts_matr <-  xgb.DMatrix( ts %>% as.matrix)
pr <- predict(xgb_model, newdata =ts_matr  )
pr 

#output_matr[2,]
#output_matr[53606,]

# train and test data distrib check:
all_data <- rbind( feat_table1  %>% 
                      select( one_of(model_tmp_cols_list)) %>%
                      mutate( data_type = "new"), 
                   feat_table_old %>% 
                      select( one_of(model_tmp_cols_list))   %>%
                      mutate( data_type = "old"))

for( i in (2:277))
{
  distr_plot <- ggplot(data = all_data, aes( x = ( all_data[,i] +1), fill = factor(data_type)))
  #distr_plot <- distr_plot +  geom_histogram(alpha = 0.6)
  distr_plot <- distr_plot +  geom_density(alpha = 0.6)
  distr_plot <- distr_plot + ggtitle( paste("Hist of:", colnames(all_data)[i]) )
  print(distr_plot)
  Sys.sleep(3)
  i = i +1
}




