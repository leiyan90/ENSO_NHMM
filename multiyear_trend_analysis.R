library(readxl)
# library(biwavelet)
library(locfit)

setwd("~/WorkSpace/yanl/nhmm-world/P~temp_s/multiyear-trend")


state_01 <- matrix(data = NA, nrow = 143, ncol = 2)
fitted_p <- matrix(data = NA, nrow = 143, ncol = 2)

yearly_states <- read_csv("yearly_states.csv")

time <- seq( 1, length(yearly_states$YEAR), 1 )
y1 <- yearly_states$index_nhmm
y2 <- yearly_states$index_subjective


# f1 = gcvplot(y1 ~ time, family = "binomial", deg = 1, alpha = seq(0.2,1.0,by=0.05))
# gcv(y1 ~ time, family = "binomial", deg = 1, alpha = 0.7)
# alpha_1 <- f1$alpha[which.min(f1$values)]
fit1 <- locfit(y1 ~ time, family = "binomial", deg = 1, alpha = 0.34)
plot(fit1, get.data = F )
fitted_p[ , 1 ] <- fitted.values(fit1)

# f2 = gcvplot(y2 ~ time, family = "binomial", deg = 1, alpha = seq(0.2,1.0,by=0.05))
# gcv(y2 ~ time, family = "binomial", deg = 1, alpha = 0.35)
# alpha_2 <- f2$alpha[which.min(f2$values)]
fit2 <- locfit(y2 ~ time, family = "binomial", deg = 1, alpha = 0.34)
plot(fit2, get.data = F )
fitted_p[ , 2 ] <- fitted.values(fit2)


crit(fit2) <- crit(fit2,cov=0.90)
aa = plot(fit2,band="global")
#write.table(fitted_p, "fitted_values_new.csv", sep=",")
fit2$critval

plot(fit2, band="global")

colnames(fitted_p) = c('NHMM_identified','Niño_3.4_identified')
fitted_p = as.data.frame(fitted_p)
fitted_p$year <- seq(as.Date("1880/1/1"), as.Date("2022/12/1"), by = "year")
df_long <- fitted_p %>%
  tidyr::pivot_longer(-year, names_to="methods", values_to="value")
str(fitted_p)

color_vector <- c("NHMM_identified" = "blue", 
                  "Niño_3.4_identified" = "red")

p <- ggplot(df_long, aes(x=year, y=value, color=methods)) +
  geom_rect(aes(xmin = as.Date("1992-1-1"), xmax = as.Date("2022-1-1"),
                ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, color = NA, show.legend = FALSE) +
  geom_line() +
  scale_x_date(limits = c(as.Date("1882-1-1"), as.Date("2022-1-1")),
               date_breaks = "20 years", date_labels = "%Y") +
  labs(x = NULL, y = "Local Probabilities of Occurrence", color = "") +
  theme_minimal() +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_blank(),
        text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"), 
        legend.text = element_text(size = 8)) +
  scale_color_manual(values = color_vector, 
                     breaks = c("NHMM_identified", "Niño_3.4_identified"),
                     guide = guide_legend(nrow = 2)) 




ggsave(filename = "multiyear_lania_trend.tiff", plot = p, width = 4, height = 3, dpi = 500)

