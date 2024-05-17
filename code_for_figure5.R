library(NHMM)
library(readxl)
library(ggplot2)
library(patchwork)
library(readr)

# getwd()
setwd("~/WorkSpace/yanl/nhmm-world/P~temp_s")
load("~/WorkSpace/yanl/nhmm-world/P~temp_s/PAC_temp_s_k5-10000-4-best.RData")
Trans_prob=OQQ(my.nhmm,FALSE)     #transition probability


#====================plot the state residence time========================#
library(ggplot2)
library(lubridate)
TimeRange <- seq.Date(from = as.Date("1881/01/01",format = "%Y/%m/%d"), 
                      by = "month", length.out = 1704)
plot0 = list()
k=1
temp_s0 = temp_s[1:1704,2] 

state= c(2, 1, 3, 5, 4)

for (i in 1:5){
  
  for (j in 1 : 5){
    
    tp=1/(1-Trans_prob[state[i], state[j],])
    
    dataf = data.frame(TimeRange,tp)
    plot0[[k]] = ggplot(dataf)+
      geom_line(aes(TimeRange, tp),size=0.3,color="black") +
      xlab("Year") + ylab('State residence time (months)') +
      geom_smooth(aes(TimeRange, tp), method = "loess", se = TRUE, 
                  color = "red", linetype = "solid",size = 0.5, span =0.75) +
      ggtitle(paste0('From state ',i,' to state ',j))
    k=k+1
  }
  
}


plot0[[1]] + plot0[[7]] + plot0[[13]] + plot0[[19]] + plot0[[25]] +
  plot_layout(ncol = 3)


ggsave("state_residence_time.tiff", device = "tiff", 
       width = 25, height = 15, units = "cm")

