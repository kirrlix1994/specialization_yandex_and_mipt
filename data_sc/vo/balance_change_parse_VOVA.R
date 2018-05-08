## reading campaigns data ----

campaigns_file <- '../../bnb/bnk/projects/model/campaigns_2017-06-21_bal_12_coeff_0.056.csv'
cmpgn_df <- read.csv(campaigns_file,
                     sep = ';',
                     stringsAsFactors = FALSE)

## drop existing BALANCE columns and transforming date
cmpgn_df %<>%
  mutate(DATE_START = ymd(DATE_START)) %>%
  select(-matches('BAL|BALANCE'))

## read new balances
blnc_file_bb <- '../../data/new_dataset/модель_часть_2_балансы_ББ_изменения.csv'
blnc_chng_bb <- fread(blnc_file_bb,
                         sep = '>',
                         #nrows = 10,
                         stringsAsFactors = FALSE) %>%
  mutate(ST_BAL_DT = ymd_hms(ST_BAL_DT),
         DEP_AMT = as.numeric(gsub(',', '', DEP_AMT)), ##
         CRD_AMT = as.numeric(gsub(',', '', CRD_AMT)),
         DLQ_AMT = as.numeric(gsub(',', '', DLQ_AMT)))

## remove not unique values
blnc_chng_bb %<>%
  group_by(CONTRACT_REF, DEP_AMT, CRD_AMT, DLQ_AMT) %>%
  arrange(desc(ST_BAL_DT)) %>%
  mutate(N_ = 1:n()) %>%
  filter(N_ == 1) %>%                                                                                                                                                                                                                                     
  select(-N_)

blnc_file_bbkk <- '../../data/new_dataset/модель_часть_2_балансы_ББКК_изменения.csv'
blnc_chng_bbkk <- fread(blnc_file_bbkk,
                         sep = '>',
                         #nrows = 10,
                         stringsAsFactors = FALSE) %>%
  mutate(ST_BAL_DT = ymd_hms(ST_BAL_DT),
         DEP_AMT = as.numeric(gsub(',', '', DEP_AMT)), ##
         CRD_AMT = as.numeric(gsub(',', '', CRD_AMT)),
         DLQ_AMT = as.numeric(gsub(',', '', DLQ_AMT)))

## remove not unique values
blnc_chng_bbkk %<>%
  group_by(CONTRACT_REF, DEP_AMT, CRD_AMT, DLQ_AMT) %>%
  arrange(desc(ST_BAL_DT)) %>%
  mutate(N_ = 1:n()) %>%
  filter(N_ == 1) %>%
  select(-N_)

## keeping balances and debts
all_blnc <- rbind(blnc_chng_bb, blnc_chng_bbkk) %>%
  ungroup() %>%
  mutate(BAL = DEP_AMT - CRD_AMT - DLQ_AMT,
         DEBT = DLQ_AMT) %>%
  select(-DEP_AMT, -CRD_AMT, -DLQ_AMT)

rm(blnc_chng_bb, blnc_chng_bbkk)
### joining ----
all_blnc %<>% inner_join(cmpgn_df %>% select(CONTRACT_REF, DATE_START),
                         by = 'CONTRACT_REF')
## keep only rows in which changes occured before communication
all_blnc %<>% filter(ST_BAL_DT < DATE_START)

## divide into two data frames: one for balance, one for debt
## and filter unncecessary rows
just_blnc <- all_blnc %>% 
  select(-DEBT) %>%
  group_by(CONTRACT_REF, BAL, DATE_START) %>%
  arrange(desc(ST_BAL_DT)) %>%
  mutate(N_ = 1:n()) %>%
  filter(N_ == 1) %>%
  select(-N_) %>%
  ungroup()

just_debt <- all_blnc %>% 
  select(-BAL) %>%
  group_by(CONTRACT_REF, DEBT, DATE_START) %>%
  arrange(desc(ST_BAL_DT)) %>%
  mutate(N_ = 1:n()) %>%
  filter(N_ == 1) %>%
  select(-N_) %>%
  ungroup()

## creating agg summaries
source('helpers.R')

just_blnc %<>%
  group_by(CONTRACT_REF, DATE_START) %>%
  arrange(ST_BAL_DT) %>%
  mutate(BAL_CHNG = BAL - lag(BAL, n = 1,
                              default = NA),
         BAL_CHNG_SGN = sign(BAL_CHNG),
         BAL_NEG_CHNG = ifelse(BAL_CHNG_SGN == -1,
                               BAL_CHNG,
                               NA),
         BAL_POS_CHNG = ifelse(BAL_CHNG_SGN == 1,
                               BAL_CHNG,
                               NA)) %>%
  select(-BAL_CHNG, -BAL_CHNG_SGN, -BAL) %>%
  ungroup()


just_blnc2 <- createAggSummaries(just_blnc,
                           id_columns = c('CONTRACT_REF', 'DATE_START'),
                           agg_cols = list(c('BAL_NEG_CHNG', 'BAL_POS_CHNG'),
                                           c(7, 31, 90, 180), 
                                           c(),
                                           funs(MAX = max(., na.rm = TRUE),
                                                MIN = min(., na.rm = TRUE),
                                                N = sum(!is.na(.)),#length(., na.rm = TRUE),
                                                SUM = sum(., na.rm = TRUE))),
                           wide = TRUE)
#is.na(just_blnc) <- sapply(just_blnc, is.infinite) # replace Inf with NA
just_blnc2 %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) # replace NAs, inf with 0


just_debt %<>%
  group_by(CONTRACT_REF, DATE_START) %>%
  arrange(ST_BAL_DT) %>%
  mutate(DEBT_CHNG = DEBT - lag(DEBT, n = 1,
                              default = NA),
         DEBT_CHNG_SGN = sign(DEBT_CHNG),
         DEBT_NEG_CHNG = ifelse(DEBT_CHNG_SGN == -1,
                               DEBT_CHNG,
                               NA),
         DEBT_POS_CHNG = ifelse(DEBT_CHNG_SGN == 1,
                               DEBT_CHNG,
                               NA)) %>%
  select(-DEBT_CHNG, -DEBT_CHNG_SGN, -DEBT) %>%
  ungroup()


just_debt2 <- createAggSummaries(just_debt,
                                 id_columns = c('CONTRACT_REF', 'DATE_START'),
                                 agg_cols = list(c('DEBT_NEG_CHNG', 'DEBT_POS_CHNG'),
                                                 c(7, 31, 90, 180), 
                                                 c(),
                                                 funs(MAX = max(., na.rm = TRUE),
                                                      MIN = min(., na.rm = TRUE),
                                                      N = sum(!is.na(.)),#length(., na.rm = TRUE),
                                                      SUM = sum(., na.rm = TRUE))),
                                 wide = TRUE)
#is.na(just_blnc) <- sapply(just_blnc, is.infinite) # replace Inf with NA
just_debt2 %<>% mutate_if(is.numeric, funs(ifelse((is.na(.)) | (is.infinite(.)), 0, .))) # replace NAs, inf with 0

## most recent balances, debts ----
rcnt_blnc <- all_blnc %>%
  group_by(CONTRACT_REF, DATE_START) %>%
  summarise(BAL = BAL[which.max(ST_BAL_DT)],
            DEBT = DEBT[which.max(ST_BAL_DT)])

### join results ----
cmpgn_df2 <- cmpgn_df %>%
  left_join(just_blnc2, by = c('CONTRACT_REF', 'DATE_START')) %>%
  left_join(just_debt2, by = c('CONTRACT_REF', 'DATE_START')) %>%
  left_join(rcnt_blnc, by = c('CONTRACT_REF', 'DATE_START'))

write.table(cmpgn_df2,
            '../../bnb/bnk/projects/model/campaigns_2017-06-27_new_bal.csv',
            quote = FALSE,
            sep = ';',
            row.names = FALSE)
