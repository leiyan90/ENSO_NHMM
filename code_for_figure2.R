library(NHMM)
library(readxl)
library(ggplot2)
library(patchwork)
library(locfit)
library(nnet)
library(dplyr)
library(tidyr)
library(readr)
library(gridExtra)
library(patchwork)
library(grid)
library(lubridate)
library(tidyverse)


#========================= locfit regression  =============================
setwd("/Users/ryanyan/WorkSpace/yanl/nhmm-world/P~temp_s/state_vs_temp")

HS0 <- read_excel("Hidden_state_time_series_KAPLANSST.xlsx",sheet = "TIME SERIE")
HS = HS0[301:length(HS0$STATE),1:3]
state <- HS$STATE

state_01 <- matrix(data = NA, nrow = length(HS$MONTH), ncol = 5)
fitted_p <- matrix(data = NA, nrow = length(HS$MONTH), ncol = 5)

for (i in 1:5)
{
  
  
  for (j in 1 : length(HS$MONTH))
  {
    
    
    if (state[j] == i)
    {
      
      state_01[j,i] <- 1 
      
    } else {
      
      state_01[j,i] <- 0 
      
    }
    
    
  }
  
  
  
}

#write.table(state_01,"state_01.csv",sep=",")

time <- seq( 1, length(HS$MONTH), 1 )
y1 <- state_01[ , 1 ]
y2 <- state_01[ , 2 ]
y3 <- state_01[ , 3 ]
y4 <- state_01[ , 4 ]
y5 <- state_01[ , 5 ]


alpha_0 = 0.7
fit1 <- locfit(y1 ~ time, family = "binomial", deg = 1, alpha = alpha_0)
plot(fit1, get.data = F )
fitted_p[ , 1 ] <- fitted.values(fit1)

fit2 <- locfit(y2 ~ time, family = "binomial", deg = 1, alpha = alpha_0 )
plot(fit2, get.data = F )
fitted_p[ , 2 ] <- fitted.values(fit2)

fit3 <- locfit(y3 ~ time, family = "binomial", deg = 1, alpha = alpha_0 )
plot(fit3, get.data = F )
fitted_p[ , 3 ] <- fitted.values(fit3)

fit4 <- locfit(y4 ~ time, family = "binomial", deg = 1, alpha = alpha_0 )
plot(fit4, get.data = F )
fitted_p[ , 4 ] <- fitted.values(fit4)

fit5 <- locfit(y5 ~ time, family = "binomial", deg = 1, alpha = alpha_0 )
plot(fit5, get.data = F )
fitted_p[ , 5 ] <- fitted.values(fit5)


#=========================

monthly_csv <- as.matrix(read.table("GLB.Ts+dSST.csv", sep=',',header = TRUE))
a = monthly_csv[,c(-1,-14,-15,-16,-17,-18,-19)]
temp=c()

for (i in 1 : 143){
  temp[(12*(i-1)+1):(12*i)] =  as.numeric(a[i,])
}

time <- seq( 1, 1716, 1 )

alpha_1 <- (30*12)/1716
fit1 <- locfit(temp ~ time, deg = 1, alpha = alpha_1)
plot(fit1, get.data = F )
temp_s <- fitted.values(fit1)[13:1716]  

temp0 = temp[13:1716]
month0 <- seq(as.Date("1881/1/1"), as.Date("2022/12/1"), by = "month")


df3 <- data.frame(month = month0, temp0, temp_s, fitted_p[,4]*1.5-0.5,fitted_p[,5]*1.5-0.5,fitted_p[,3]*1.5-0.5,fitted_p[,1]*1.5-0.5,fitted_p[,2]*1.5-0.5)
colnames(df3) = c("month","Global Temperature","Global Smoothed Temperature","Classical El Niño","CP El Niño","Neutral","Mild La Niña","Classical La Niña")

df3_long <- pivot_longer(df3, cols = -month, names_to = "series", values_to = "value")

df3_long$series <- factor(df3_long$series, levels = c("Global Temperature","Global Smoothed Temperature",
                                                      "Classical El Niño","CP El Niño","Neutral","Mild La Niña","Classical La Niña"))

p4 = ggplot(df3_long, aes(x = month, y = value, color = series)) +
  geom_line(aes(size = ifelse(series == "Global Temperature", 0.4, 0.8))) + 
  scale_size_identity() +  
  scale_color_manual(values = c('Global Temperature' = '#ABD9E9', "Classical La Niña" = "#2C7BB6",
                                "Mild La Niña" = "gold", "Neutral" = "grey", "CP El Niño" = "black", 
                                "Classical El Niño" = "#FDAE61", 'Global Smoothed Temperature' = 'red')) +
  scale_y_continuous(
    name = "Global Temperature Anomaly (°C)",
    sec.axis = sec_axis(~ (. + 0.5) / 1.5, name = "Local Probabilities of Occurrence", breaks = seq(0, 1.5, by = 0.5))
  ) +
  labs(x = NULL) +
  theme(legend.position = c(0, 1), 
        legend.title = element_blank(), 
        legend.justification = c(0, 1), 
        text = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.02, 'cm'),    
        legend.spacing.y = unit(0.01, 'cm'),    
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(color = "black", size = 9),  
        axis.text.y = element_text(color = "black", size = 9)) +  
  scale_x_date(breaks = date_breaks("20 years"), date_labels = "%Y") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))

ggsave(filename = "5+2.tiff", plot = p4, width = 6, height = 4, dpi = 500)


