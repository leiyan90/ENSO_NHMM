#==================plot the 25 plots===============#
TimeRange <- seq.Date(from = as.Date("1881/01/01",format = "%Y/%m/%d"), 
                      by = "month", length.out = 1704)
library(ggplot2)
library(patchwork)

plot0 = list()
k = 1
state = c(2, 1, 3, 5, 4)

for (i in 1:5) {
  for (j in 1:5) {
    tp = Trans_prob[state[i], state[j],]
    dataf = data.frame(TimeRange,tp)

    x_title <- if(k > 20) "Observation years" else ""
    y_title <- if(k %% 5 == 1) "Transition probabilities" else ""
    
    plot0[[k]] = ggplot(dataf) +
      geom_line(aes(TimeRange, tp), size=0.2, color="blueviolet", alpha = 1) +
      geom_smooth(aes(TimeRange, tp), method = "loess", se = FALSE, color = "red", linetype = "solid", size = 0.7) +
      ggtitle(paste0('From ', i,' to ', j)) +
      xlab(x_title) + ylab(y_title) +
      theme(
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14)
      )
    
    k = k + 1
  }
}


aa = wrap_plots(plot0, ncol = 5) 

ggsave("trans_prob_season3.tiff", aa, device = "tiff", width = 30, height = 30, units = "cm")

