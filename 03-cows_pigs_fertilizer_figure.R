## Scripts to make cows, pigs, fertlizer figure
# 2016-08-23
# JJV


# 1. Load libraries
library('ggplot2')

# 2. Figure of cow, pigs, fertilizer use in Manitoba
# Cows: thousands, Fertlizer: thousand tons, Pigs: millions
cpf <- read.csv('cows fertilizer pigs.csv')
ggplot(cpf, aes(x = year, y = value, colour = variable)) + 
  geom_line(size = 2) + 
  labs(x = "Year", y = "", colour = "") + 
  scale_y_log10() + 
  scale_color_brewer(palette = "Dark2", 
                     breaks = c("Cow", "Pig", "Fert"), 
                     labels=c("Cattle (1000s) ", "Pigs (1,000,000s) ", "Fertilizer (1000 tons)")) + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.key = element_rect(colour = "white"))
ggsave(filename = "cows pigs fert.pdf", height = 5, width = 7, units = "in")

