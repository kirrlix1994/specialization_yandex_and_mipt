library(data.table)
library(magrittr)
library(dplyr)
library(stringr)

# CHANGE WORKING DIRECTORY!
data <- read.csv(file = 'data.csv', sep=';', na.strings = 'N/A')

# data %>% View()
dim(data)
colnames(data)
# 0) mod data - change millions and thousads

# Change al
cols_to_trans_ind <- seq(2,12,2)
data_mod <- data.frame()
for ( i in cols_to_trans_ind){
   tmp <-  plyr:: mapvalues(data[,(i+1)], c('MLN', 'THD'), c(10^6, 10^3))
   tmp <- as.numeric(levels(tmp))[tmp]
   data_mod[(1:nrow(data)),i/2] <- data[,i]*tmp
   colnames(data_mod)[i/2] <- colnames(data)[i]
} 
# ADD last cols
for( i in seq(14,17,1)){
   data_mod[(1:nrow(data_mod)),(ncol(data_mod) +1)] <- data[,i]
}
colnames(data_mod)[ (7:10)] <- colnames(data)[seq(14,17,1)]

# check:
# data_mod %>% View()
tmp <- cbind(data[, c(5,4)] , data_mod[,2])
#tmp %>% View()

tmp2 <- cbind(data$qn08 , data_mod$qn08)
tmp2 %>% View()
sum( (na.omit(tmp2[,1]) - na.omit(tmp2[,2]))^2) 


log_transform <- function(x){
  x_up = as.numeric(quantile(x, 0.95, na.rm = TRUE))
  x_down = as.numeric(quantile(x, 0.05, na.rm = TRUE))
  M = 0.5*(x_up + x_down)
  S = 2*log(19)/(x_up - x_down)
  return( 1/(1 + exp(S*(M-x))))
}

# test:
x <- rnorm(n = 1000, 0,5)
logit_x <- log_transform(x)
hist(logit_x, breaks = 50, col = 'red')

# transformed values in [0,1] range
normalize_transform <- function(x){
  x_trans <- log_transform(x)
  mu <- mean(x_trans, na.rm = TRUE)
  sigma <- var(x_trans, na.rm = TRUE)^(0.5)
  return( 50*(x_trans - mu)/sigma )
}

# test
x <- rnorm(n = 1000, 0,5)
x_norm <- normalize_transform(x)
print( mean(x_norm))
print( var(x_norm)^(0.5))

# Apply logit and normalizaton transformation:
data_mod2 <-  as.data.frame(apply(data_mod,2, function(x) normalize_transform(x)))
data_mod2$PD <- data$PD

# Check: 
apply(data_mod2,2, function(x) mean(x,na.rm=TRUE))
apply(data_mod2,2, function(x) (var(x,na.rm=TRUE)^(0.5)))


# 1. One factor analysis:

# Choose factors ( from qn0 - qn10) wich satisfy  conditions:
# 1) APS( Скорректированный показатель статистической мощности) >= 0.3
# 2) Availability >= 0.8

# 2) ALL factors have Availability >= 0.8
na_perc_by_facor <- apply(data_mod2, 2, function(x) round( sum(!is.na(x))/length(x),3))
colnames(data_mod2)[ na_perc_by_facor < 0.8]

# 1) Calculate APS per each column:
PD_total <- sum(data_mod2$PD)

data_mod2 %>% View()

factor_PD <- function( df){
   colnames(df) <- c("Q", "PD")
   df %<>% arrange(Q)
   df$Q_rank <- round(seq(1, nrow(df), 1)/nrow(df),5)
   df$PD_cumm <- cumsum(df$PD)
   pd_total <- sum(df$PD)
   df$PD_perc <- round(df$PD_cumm/pd_total,5)
   
   # calculate area under plot(df$Q_rank, df$PD_perc ) curve!
   
}

df <- data_mod2 %>% select(qn01, PD)

plot(df$Q_rank, df$PD_perc )

K <- sum(data_mod2$PD)/nrow(data_mod)

df <- data_mod2 %>% select(qn01, PD)



















  











