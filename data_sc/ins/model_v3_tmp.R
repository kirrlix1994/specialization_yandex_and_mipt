

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
feat_table %<>% filter( !(feat_table$ID %in% dab_id_list) ) 
dim( feat_table) 
# - 529 
# - 326 only after clean

# 3 same ids: 
# 103530403_2017-03-16   
# 104343352_2017-03-30:

 tmp <-  feat_table %>% filter( ID == "103530403_2017-03-16")
 tmp[, grep("SD_HF", colnames(tmp))] %>% distinct()
 tmp[, grep("SD_IQ", colnames(tmp))] %>% distinct()
 
 sum( is.na( feat_table$SD_IQ.CL_INDUSTR))/ nrow( feat_table)
 # 25% na!
 
 
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
           CANC_N_BEFORE == 0, # COMM.  all: -396 obs only!
           CHANNEL == "GRAN") 
 nrow(feat_table) - nrow(feat_table_1)
 #table(feat_table_1$INS.TARGET )
 round( sum( feat_table_1$INS.TARGET == 1 )/ nrow( feat_table_1),2)
 # 6% of 1
 
 
#remove wrong features-----
 tmp <- feat_table_1 %>% colnames( )
 
 #feat_table_2 <- remove_ONE_CLASS_features( df = feat_table_1, max_class_percent = 0.999)
 feat_table_2 <- remove_ONE_CLASS_features( df = feat_table_1, max_class_percent = 0.95)
 dim( feat_table_2)
 
 tmp2 <- feat_table_2 %>% colnames( )
 
 dupl <- feat_table_2 %>% group_by(ID ) %>% summarise( n = n()) %>% filter( n > 1) 
 
  feat_table_3 <- feat_table_2 %>%
                  select( -ID , -CANC_SUM_AFTER, -CANC_N_AFTER )
        
 # remove bad obs----------
  feat_table_4 <- remove_obs(df = feat_table_3, 
                             col_idx = grep('SD_IQ', colnames(feat_table_3)),
                             omit_action_na = TRUE, omit_action_value =TRUE,
                             threshold =  0.2)
  # -1885 ( -100 "1")
  na_perc_sd <- hist( feat_table_4[[2]][,1], 100)
  feat_table_4 <- feat_table_4[[1]]
  sum(feat_table_4$INS.TARGET)
  
  feat_table_5 <- remove_obs(df = feat_table_4, 
                             col_idx = grep('TRANS', colnames(feat_table_4)),
                             omit_action_na = TRUE, omit_action_value =TRUE,
                             threshold =  0.95)  
  
  oneVAlue_perc_trans <- hist( feat_table_5[[2]][,2], 100)
  # at least all "1" has to be removed
  # - 1601 
   feat_table_5 <- feat_table_5[[1]]
   
   # remove y cols!
   # feat_table_5 %<>% select( -CANC_SUM_AFTER, -CANC_N_AFTER )
  
   feat_table_6 <- remove_obs(df = feat_table_5, 
                              col_idx = grep('BAL', colnames(feat_table_5)),
                              omit_action_na = TRUE, omit_action_value =TRUE,
                              threshold =  0.8)  
  oneVAlue_perc_trans <- hist( feat_table_6[[2]][,2], 100)
  # -169 
  feat_table_6 <- feat_table_6[[1]]
  

 
   t0 <- table( feat_table$INS.TARGET)
   t1 <- table( feat_table_1$INS.TARGET) 
   t2 <- table( feat_table_2$INS.TARGET) 
   t3 <- table( feat_table_3$INS.TARGET) 
   t4 <- table( feat_table_4$INS.TARGET) 
   t5 <- table( feat_table_5$INS.TARGET)
   t6 <- table( feat_table_6$INS.TARGET)
   
  H <- rbind( t0,t1,t2,t3,t4, t5, t6)
  H <- data.frame( H)
  colnames(H) <- c("neg_class_n", "pos_class_n")
  H %<>% mutate( rate =  round( pos_class_n/ (pos_class_n + neg_class_n), 3) )
  
  
   # fill NA values: 
   NA_list <-apply( feat_table_6, 2, function(x) round( sum( is.na(x))/nrow( feat_table_6) ,2) )
   NA_list %<>% sort(decreasing = TRUE )
   NA_list[1:10]
 
   #x[!x %in% c(4, 9, 10)]
   #FIll na with sample: 
   #cols_to_fill_na <- names( NA_list[1:9])
   # only facors( charectors) to make sample
   #cols_to_fill_na <- 
   #   cols_to_fill_na[ !cols_to_fill_na  %in% 
   #                     c("SD_IQ.CL_AMT_DOP", "SD_IQ.CL_CCY_DOP", "D_IQ.CL_CCY_ZP")]
            
    
   cols_to_fill_na_facotrs  <-  names(Filter(is.factor, feat_table_6))
   for( col in cols_to_fill_na_facotrs )
   {
     feat_table_6[, col] <-
       FILL_NA( column =  feat_table_6[, col], fill_type = "sample", seed = 11)
     cat( col, "\n")
   }
  
   
   # facors: work with them
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
                                    print_plot = TRUE, plot_names = FALSE, 
                                    corr_threshold = 0.99)
     
     
     dim(feat_table_7); dim(feat_table_8)
      
# distributions plot:: 
     
     # age : i= 136
     dat <- feat_table_8
     i <- 16
     dat$INS.TARGET %<>% as.factor()
     plot <- ggplot( data = dat, aes(x = (dat[,i]+1) , fill = INS.TARGET)) + 
       #geom_histogram(bins = 100 ) + 
       geom_density( alpha = 0.5) + 
       #facet_wrap( ~ INS.TARGET, ncol = 1) + 
       ggtitle( paste0( "Hist of ", colnames(dat)[i] ))
     plot
     
###modelling---------
     
     
     
     
     
      