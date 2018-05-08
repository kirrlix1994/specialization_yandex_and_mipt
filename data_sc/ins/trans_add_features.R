

  tmp <- trans %>% filter(CLIENT_ID == 100010543)
  
  tmp 
  
  tr_mcc <- trans %>% group_by(CLIENT_ID,TXN_MCC ) %>%
          summarise( n = n(), mcc_sum = sum(TXN_RUB_AMT), 
                     mcc_min = min(TXN_RUB_AMT), 
                     mcc_max = max(TXN_RUB_AMT))  %>%
          group_by(CLIENT_ID) %>%
          summarise( mcc_cnt = n(), n_tr = sum(n), mean = mean(mcc_sum), 
                     tr_mcc_ratio = n_tr/ mcc_cnt,
                     max = max(mcc_max), min = min(mcc_min), 
                     mcc_spread =min(mcc_min)/max(mcc_max))  %>%
          ungroup()
  
# CITY :  
  tr_city <- trans %>% group_by(CLIENT_ID,TXN_CT ) %>%
    summarise( n = n(), ct_sum = sum(TXN_RUB_AMT), 
               ct_min = min(TXN_RUB_AMT), 
               ct_max = max(TXN_RUB_AMT))  %>%
    group_by(CLIENT_ID) %>%
    summarise( ct_cnt = n(), n_tr = sum(n), mean = mean(ct_sum), 
               tr_ct_ratio = n_tr/ ct_cnt,
               max = max(ct_max), min = min(ct_min), 
               ct_spread =min(ct_min)/max(ct_max))  %>%
    ungroup()
                     
  # same with categories
  
  
        #  summarise( n = n(), mcc_cnt = length(unique(TXN_MCC)) )
  
  head(tr_mcc,100) %>% View() 
  tr_mcc_cnt <- ggplot(data = tr_mcc, aes(x = mcc_cnt )) 
  tr_mcc_cnt <- tr_mcc_cnt + geom_histogram(bins = 200, colour = "blue" )
  tr_mcc_cnt
  
  
  tr_mcc_spread <- ggplot(data = tr_mcc, aes(x = mcc_spread )) 
  tr_mcc_spread <- tr_mcc_spread+ geom_histogram(bins = 400, colour = "blue" )
  tr_mcc_spread 
  
  
  tr_mcc_tr_on_mcc <- ggplot(data = tr_mcc, aes(x = tr_mcc_ratio)) 
  tr_mcc_tr_on_mcc <- tr_mcc_tr_on_mcc + geom_histogram(bins = 400, colour = "blue" )
  tr_mcc_tr_on_mcc
  
  
  # city
  ct_cnt <- ggplot(data = tr_city, aes(x = ct_spread )) 
  ct_cnt <- ct_cnt + geom_histogram(bins = 200, colour = "blue" )
  ct_cnt
  
  
  # mcc features
  # 
  tmp %>% View()
  tmp <- trans %>% filter(CLIENT_ID == 100010543)
  client_trans_plot <- ggplot(data = tmp, aes(x = TXN_DT, y = TXN_RUB_AMT)) +
    geom_line(colour ="blue")
  client_trans_plot
  
  plot_trans_n_client <- function( client_vec)
  {
    tmp <- trans  %>% filter(CLIENT_ID %in% client_vec)
    ggplot(data = tmp, aes(x = TXN_DT, y = TXN_RUB_AMT, colour = fill(TXN_DIR))) + 
      geom_point(colour ="blue") + facet_wrap(~CLIENT_ID, nrow = 5)
  }
    
  L <- length(unique(trans$CLIENT_ID))
  

for( i in (1:1000))
{
  client_vec <- sample( unique(trans$CLIENT_ID), 5)
  plot_trans_n_client(client_vec)
  Sys.sleep(10)
}  
  
  
  #some other trans featues
  
  # ind_bonus
  # sum bonus
  # firs_trans_days
  # last trans days
   
  
  
 ###utltization -----
  
  utill_tab <- month_bal %>%
    inner_join( limits, by = c("CLIENT_ID","DT" ) ) %>%
    mutate( UT = if_else( LIM > 0, CRD_AMT/LIM, 0))  %>%
    select( -DLQ_AMT, -CRD_AMT, -DEP_AMT, -LIM) %>% 
    #filter(  as.numeric(DATE - DT) < 200 ) %>%
    group_by(CLIENT_ID, CONTRACT_NUMBER, DATE ) %>%
    arrange( CLIENT_ID, CONTRACT_NUMBER, DATE, desc(DT)) %>% 
    slice(1:6) %>% # leave last 6 month
    mutate( MON_AGO = row_number(), UTIL = "UT") %>% 
    select( -DT) %>% 
    unite( col = UT_MOM_AGO, c("MON_AGO", "UTIL"), sep = "_") %>% 
    spread( key = UT_MOM_AGO, value = UT) %>%
    data.frame()
  
  # good!
           
  utill_tab %>%
    
   
  
  
  
  
  