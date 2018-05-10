library(data.table)
library(magrittr)
library(dplyr)
library(stringr)

# CHANGE WORKING DIRECTORY!
setwd("C:/Users/Kirill/Desktop/data science/CIB")
data <- read.csv(file = 'data.csv', sep=';', na.strings = 'N/A')

# data %>% View()
dim(data)

colnames(data)

data_mod <- data
cols_to_tr <-  seq(2,12,2)
for ( i in  cols_to_tr){
  #bob %>% mutate_if(is.factor, as.character) 
  #data_mod[,(i+1)] %<>% mutate_if(is.factor, as.character)
  # MLN = 1 , THD = 2
  #dim_col_tmp <-  (as.numeric(data[,(i+1)]))*(10^6 - 1000) + 1000
  for( j in (1:ncol(data))){
    range <- data[j,i+1]
    if( range == 'THD'){
     data_mod[j,i] = 1000*data[j,i] 
    }
    if( range == 'MLN'){
      data_mod[j,i] = 10^6*data[j,i] 
    }
    else{
      print(i)
      print(j)
      print( 'new value!')
    }
  }
}

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
  mu <- mean(x_trans)
  sigma <- var(x_trans)^(0.5)
  return( 50*(x_trans - mu)/sigma )
}

# test
x <- rnorm(n = 1000, 0,5)
x_norm <- normalize_transform(x)
print( mean(x_norm))
print( var(x_norm)^(0.5))



# One factor analysis:
data_cols <- colnames(data)
num_factors <- data_cols[which(substr(data_cols, 1,2) == 'qn')] 

data_mod <- data %>% 
  select( one_of(num_factors)) %>%
  as.data.frame()

data_mod %>% View()
data_mod <- apply(data_mod, 2, function(x) normalize_transform(x))

# What to do with NA?



















  











