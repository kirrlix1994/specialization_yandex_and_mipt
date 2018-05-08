
library(ggplot2)
library(magrittr)
library(dplyr)

#  setwd("C:/Users/Liksakov/Desktop/microsegm")

test_data <- read.csv('test_data_04_2018/TEST_PROCESSED.csv', sep = ',')
gr_data <- read.csv('test_data_04_2018/MICR_BASE_FINAL_04_2018.csv', sep=',')

# test_data %>% View()
# gr_data %>% View()

data <- test_data %>% 
   inner_join(gr_data, by = c("CONTRACT_REF","CLIENT_ID"))

data %>% View() 

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


pdf("FEAT_HIST_GROUPS.pdf", width = 10, height = 7 )
for( i in (5:122))
{
   dat <- data %>% 
      select( colnames(data)[i], SCEN, CGF, top_ind) %>% 
      filter( SCEN != 'not_sent') %>%
      data.frame()
  plot <- ggplot(data = dat, aes( x = dat[,1], fill = factor(SCEN))) + 
         geom_histogram() +
         facet_wrap(~factor( SCEN)) + 
         ggtitle( paste('Density of', colnames(dat)[1], 'by SCENARIO')) + 
         xlab('Feature')

 #print(i, "\n")
 print(plot)
}                      
dev.off()

pdf("FEAT_HIST_SENT.pdf", width = 10, height = 7 )
for( i in (5:122))
{
   dat <- data %>% 
      select( colnames(data)[i], SCEN, CGF, top_ind) %>% 
      data.frame()
   
   plot <- ggplot(data = dat, aes( x = dat[,1], fill = factor(top_ind))) + 
      geom_histogram() + 
         facet_wrap(~factor(top_ind)) + 
      ggtitle( paste('Hist of', colnames(dat)[1], 'by SENT flag')) + 
      xlab('Feature')
   
   #print(i, "\n")
   print(plot)
}                      
dev.off()


data %>% 
select(BAL, SCEN, top_ind, Uplift)  %>% 
group_by( top_ind) %>% 
summarise( mean(BAL)) %>% View()






