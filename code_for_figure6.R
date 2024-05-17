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
hd$YEAR_MONTH <- as.Date(paste(hd$YEAR, hd$MONTH, 1, sep="-"), "%Y-%m-%d")  # 创建一个日期列
hd$STATE_DIFF <- as.numeric(hd$STATE) - 3


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
write.csv(hd_summary,file="yearly_states.csv")

#================plot nino 34=======================
color_vector <- c("Classical La Niña" = "blue", "Mild La Niña" = "skyblue", "Neutral" = "black", "CP El Niño" = "orange", "Classical El Niño" = "red")
hd_summary$dominant_state <- factor(hd_summary$dominant_state, levels = c("Classical La Niña", "Mild La Niña", "Neutral", "CP El Niño", "Classical El Niño"))


la_nina_periods <- hd_summary %>%
  group_by(run_id = cumsum(dominant_state != lag(dominant_state, order_by = PERIOD) | is.na(lag(dominant_state, order_by = PERIOD)))) %>%
  filter(dominant_state %in%  c("Classical La Niña", "Mild La Niña")) %>%
  summarise(start = min(PERIOD), end = max(PERIOD), .groups = "drop") %>%
  filter(end - start >= 1)

p <- ggplot() +
  labs(
    x = "Year",
    y = "Nino 3.4 Index (℃)",
    fill = ""
  ) +
  scale_x_continuous(breaks = seq(min(hd_summary$PERIOD), max(hd_summary$PERIOD), by = 10)) +
  ylim(-2.5, 2.5) + # 设置y轴的范围
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black", size = 14), 
    axis.text.y = element_text(color = "black", size = 14),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.position = c(0.5, 0), 
    legend.justification = c(0.5, -0.25), 
    legend.direction = "horizontal", 
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 14), 
    legend.box = "horizontal" 
  )


print(p)

for (i in 1:nrow(la_nina_periods)) {
  p <- p + annotate("rect", xmin = la_nina_periods$start[i] - 0.5, 
                    xmax = la_nina_periods$end[i] + 0.5, 
                    ymin = -Inf, ymax = Inf, 
                    alpha = 0.5, fill = "grey") 
}

p <- p + geom_bar(data = hd_summary, aes(x = PERIOD, y = nino34, fill = dominant_state), 
                  stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = color_vector) +
  geom_hline(yintercept = -0.5, linetype = "dashed", color = 'black', size = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = 'black', size = 0.5)

print(p)

ggsave(filename = "temperal evolution_nino34-CP.tiff", plot = p, width = 12, height = 6, dpi = 500)


