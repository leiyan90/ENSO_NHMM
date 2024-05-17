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
library(lubridate)
library(patchwork)
library(grid)
library(cowplot)



setwd("~/WorkSpace/yanl/nhmm-world/P~temp_s/temp_evolution")
#===============hidden states===================
library(readxl)
hds <- read_excel("Hidden_state_time_series_KAPLANSST.xlsx", sheet = "TIME SERIE")
hd = hds[301:2004, 1:3]
nino34 <- read_excel("nino34.xlsx", col_names = FALSE)[12:154,]

custom_order <- c('Classical La Niña', 'Mild La Niña', 'Neutral', 'CP El Niño', 'Classical El Niño')
hd$STATE <- factor(hd$STATE, levels = c(2, 1, 3, 5, 4), labels = custom_order, ordered = TRUE)
hd$YEAR_MONTH <- as.Date(paste(hd$YEAR, hd$MONTH, 1, sep="-"), "%Y-%m-%d")
hd$STATE_DIFF <- as.numeric(hd$STATE) - 3


###=======Nino_PAC========
#Nino_PAC <- read_csv("NIno_PAC_15-15.csv")[301:2004, ]
Nino_PAC <- read_csv("NIno_PAC_170-240.csv")[301:2004, ]
Nino_PAC_0 <- Nino_PAC %>%
  mutate(YEAR = year(time), 
         MONTH = month(time)) %>% 
  select(YEAR, MONTH, sst) %>% 
  rename(NINO_Pac = sst) 
hd <- hd %>%
  left_join(Nino_PAC_0, by = c("YEAR", "MONTH"))


###=======nino34========
colnames(nino34) <- c("YEAR", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
nino34_long <- nino34 %>%
  tidyr::pivot_longer(cols = -YEAR, names_to = "MONTH", values_to = "NINO34") %>%
  mutate(MONTH = match(MONTH, month.abb)) 

nino34_long$MONTH <- rep(1:12, times = nrow(nino34_long)/12)
hd <- hd %>%
  left_join(nino34_long, by = c("YEAR", "MONTH"))

head(hd)

#===========get the dominant states==============
# Function to get the dominant state
get_dominant_state <- function(states) {
  tab <- sort(table(states), decreasing = TRUE)
  if(length(tab) > 1 && tab[1] == tab[2]) {
    return(as.character(states[1])) 
  }
  return(as.character(names(tab[1]))) 
}

hd <- hd %>%
  mutate(PERIOD = if_else(MONTH >= 10, YEAR, YEAR - 1))

hd_filtered <- hd %>%
  filter(MONTH >= 10 | MONTH <= 2) 

hd_summary <- hd_filtered %>%
  group_by(PERIOD) %>%
  summarise(
    dominant_state = get_dominant_state(STATE), 
    nino34 = mean(NINO34, na.rm = TRUE), 
    nino_pac = mean(NINO_Pac, na.rm = TRUE), 
    .groups = 'drop'
  )


print(hd_summary)
unique(hd_summary$dominant_state)
hd_summary$dominant_state <- factor(hd_summary$dominant_state)
#write.csv(hd_summary,file="yearly_states.csv")



#==================INtensity===========

desired_order <- c("Classical La Niña", "Mild La Niña", "Neutral", "CP El Niño", "Classical El Niño")

hd_summary <- hd_summary %>%
  mutate(dominant_state = factor(dominant_state, levels = desired_order))

color_vector <- c("Classical La Niña" = "#2C7BB6", "Mild La Niña" = "#ABD9E9", 
                  "Neutral" = "#FFFFBF", "CP El Niño" = "#FDAE61", 
                  "Classical El Niño" = "#D7191C")

plot1 <- ggplot(hd_summary, aes(x = nino34, fill = dominant_state)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = color_vector) +
  labs(x = "Niño 3.4 index", y = "Density") +
  theme_minimal() +
  theme(legend.position = c(0.97, 0.97), 
        legend.justification = c("right", "top"), 
        legend.box.just = "right", 
        axis.text = element_text(size = 13),  
        axis.title = element_text(size = 15), 
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.margin = margin(), 
        legend.box.margin = margin(),
        legend.text = element_text(size = 12))+ 
  guides(fill = guide_legend(title = NULL))  

print(plot1)

ggsave(filename = "5desity.tiff", plot = plot1, width = 7, height = 5, dpi = 500)


# 5 subplots
library(ggplot2)
library(dplyr)

states <- unique(hd_summary$dominant_state)

y_range <- c(min(hd_summary$nino34, na.rm = TRUE), max(hd_summary$nino34, na.rm = TRUE))


for(state in states) {

  data_state <- dplyr::filter(hd_summary, dominant_state == state)

  p <- ggplot(data_state, aes(x = PERIOD, y = nino34)) +
    geom_point() +
    geom_smooth(method = "loess", span = 1) +
    labs(x = "Years", y = "Niño 3.4 index") +
    xlim(1880, 2022) +
    ylim(y_range) + 
    theme(axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14)) 
  
  print(p)

  ggsave(paste0("plot1_", state, ".tiff"), plot = p, width = 4, height = 3, dpi = 500)
}


