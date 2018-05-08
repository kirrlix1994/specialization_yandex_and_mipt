library(magrittr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # for working with date objects
library(data.table) # for working with big tables
library(glue)
library(ggplot2)
library(plotly)


# write to sql base:
drv <- JDBC("oracle.jdbc.OracleDriver",
            classPath="C:/Users/Liksakov/Desktop/ojdbc7.jar", " ")

#PASSWORD <- "Binbank_way4_2"
PASSWORD <- "FP91g345"
#UID = "LAVRENTIEV_AI"
UID = "Gorbachev_SM"
con <- dbConnect(drv, user = UID, "jdbc:oracle:thin:@mos-way4db:1521:way4db", 
                 password = PASSWORD, host = "mos-way4db", port = 1521, 
                 dbname = "way4db" )

# test:
#a <- dbGetQuery(con, "select count(*) from rat__hist")

df <- read.csv('RAT_BASE.csv', sep = ";")

 df %<>% rename(comn_number=Номер.кампании,
                date_start=Дата.создания, 
                cl_id=Номер.заявки,
                fio=ФИО , 
                bith_dt=Дата.рождения ,
                phone=Телефон,
                cl_id2=Номер.заявки.1,
                set_1= X1.сет,
                seT_1_dt= Дата.1.сет ,
                set_2=  X2.сет,
                seT_2_dt=Дата.2.сета ,
                set_3= X3.сет ,
                seT_3_dt=Дата.3.сета ,
                dozhim_1=Дожим,
                dozhim_1_dt=Дата.дожима ,
                dozhim_2=Дожим.2,
                dozhim_2_dt=Дата.дожима.2 ,
                ) %>%
          mutate( date_start = as.Date(date_start, format = "%d.%m.%Y"),
                  fio = as.character(fio),
                  bith_dt = as.Date(bith_dt , format = "%d.%m.%Y"),
                  phone = paste0('+7',as.character(phone)),
                  set_1 = as.character( set_1),
                  seT_1_dt = as.Date(  seT_1_dt, format = "%d.%m.%Y"),
                  set_2 = as.character( set_2),
                  seT_2_dt = as.Date(seT_2_dt, format = "%d.%m.%Y"),
                  set_3 = as.character( set_3),
                  seT_3_dt = as.Date(  seT_3_dt, format = "%d.%m.%Y"),
                  dozhim_1 = as.character(dozhim_1),
                  dozhim_1_dt = as.Date( dozhim_1_dt, format = "%d.%m.%Y"),
                  dozhim_2 = as.character(dozhim_2),
                  dozhim_2_dt = as.Date(dozhim_2_dt, format = "%d.%m.%Y") )
 
 
dbWriteTable(con, "rat_base_liks", df, overwrite=FALSE)
 
 
## some analytix:

top_set1_list <- 
  df %>% select(set_1, cl_id2) %>%
  group_by(set_1) %>%
  summarise(n=n()) %>% 
  data.frame() %>%
  filter(n>300) %>% 
  as.data.frame()
 
tmp <-  df %>% select( set_1, set_2) %>%
   filter( set_1 %in% top_set1_list$set_1) %>%
   group_by(set_1, set_2) %>% 
   summarise( n = n()) %>% 
   left_join(  df %>% select( set_1, set_2) %>%
                 filter( set_1 %in% top_set1_list$set_1) %>%
                 group_by(set_1) %>% 
                 summarise(n_set = n()), by = 'set_1') %>%
  ungroup() %>%
  mutate( n_proc =  round(n/n_set, 4), 
          set_1 = as.factor(set_1))

# tmp %>% View()
p <- ggplot( tmp, aes(x=n_proc, fill = set_2)) + 
  geom_histogram() + 
  facet_wrap(~set_1)

ggplotly(p)


top_set2_list <- 
  df %>% select(set_2, cl_id2) %>%
  group_by(set_2) %>%
  summarise(n=n()) %>% 
  data.frame() %>%
  filter(n>300) %>% 
  as.data.frame()

tmp2 <-  df %>% select( set_2, set_3) %>%
  filter( set_2 %in% top_set2_list$set_2) %>%
  group_by(set_2, set_3) %>% 
  summarise( n = n()) %>% 
  left_join(  df %>% select( set_2, set_3) %>%
                filter( set_2 %in% top_set2_list$set_2) %>%
                group_by(set_2) %>% 
                summarise(n_set = n()), by = 'set_2') %>%
  ungroup() %>%
  mutate( n_proc =  round(n/n_set, 4), 
          set_2 = as.factor(set_2))

# tmp2 %>% View()
p2 <- ggplot( tmp2, aes(x=n, fill = set_3)) + 
  geom_histogram() + 
  facet_wrap(~set_2)

ggplotly(p2)


# сколько человек подключило после того, как им не подключили в set_1:
# df %>% View()
 

agree_status <- c("согласен подключить", 
                 "Услуга подключена с отложенным списанием ")

tmp3 <- df %>% filter( (!(set_1 %in% agree_status)) & 
                ( (set_2 %in% agree_status) |
                  (set_3 %in% agree_status) |
                  (dozhim_1 %in% agree_status) |
                  (dozhim_2 %in% agree_status) ) )

nraw(tmp3) 
tmp3 %>% View()
 
df %>% filter( ((set_1 %in% agree_status))) %>% nrow()
 
 








 