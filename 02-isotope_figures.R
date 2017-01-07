## Scripts to make isotope figures
# 2016-08-23
# JJV


# Six steps to reproduce the figures and statistics in the paper


# 1. Load libraries
# 2. NH4 in precip
# 3. NO3 in precip
# 4. Lake and stream POM and DOM
# 5. Precip isotopes vs day of year
# 6. Precip isotopes vs day of year statistics


# 1. Load libraries
library('zoo')
library('dplyr')
library('reshape2')
library('ggplot2')
library('Kendall')


# 2. NH4 in precip
deltaNH4 <- read.csv('precip delta NH4.csv')
# use reorder to sort the Site based on median(d15NNH4)
ggplot(deltaNH4, aes(x = reorder(Site, d15NNH4, median), y = d15NNH4)) + 
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(aes(colour = Site), width = 0.2) + 
  coord_flip() +  
  labs(x = "", y = expression("\u03b4"^15*"N-NH"[4]^+{}*" (\u2030)"), colour="") + 
  scale_color_brewer(palette="Dark2", labels="") + 
  theme_bw() + 
  theme(legend.position="none") + scale_y_continuous(limits=c(-25,10))
ggsave(filename = "d15NNH4 precip boxplots.pdf", device = cairo_pdf, height = 5, width = 7, units = "in")


# 3. NO3 in precip
deltaNO3 <- read.csv('precip delta NO3.csv')
# use reorder to sort the Site based on median(d15NNH4)
ggplot(deltaNO3, aes(x = reorder(Site, d15NNO3, median), y = d15NNO3)) + 
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(aes(colour = Site), width = 0.2) + 
  coord_flip() +  
  labs(x = "", y = expression("\u03b4"^15*"N-NO"[3]^-{}*" (\u2030)"), colour="") + 
  scale_color_brewer(palette="Dark2", labels="") + 
  theme_bw() + 
  theme(legend.position="none") + scale_y_continuous(limits=c(-25,10))
ggsave(filename = "d15NNO3 precip boxplots.pdf", device = cairo_pdf, height = 5, width = 7, units = "in")


# 4. Lake and stream POM and DOM
lss <- read.csv('lake stream survey.csv')
ggplot(lss, aes(x = Sample.Type, y = d15N)) + 
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(aes(colour = Sample.Type), width = 0.2) + 
  facet_grid(Type ~ ., space = "free", scales = "free") +
  coord_flip() +
  labs(x = "", y = expression("\u03b4"^15*"N (\u2030)"), colour="") + 
  scale_color_brewer(palette="Dark2", labels="") + 
  theme_bw() + 
  theme(legend.position="none")+ scale_y_continuous(limits=c(-25,10))
ggsave(filename = "d15N lake stream boxplots.pdf", device = cairo_pdf, height = 5, width = 7, units = "in")


# 5. Precip isotopes vs day of year
# Have to adjust dates and calcuate day of year
precip_isotopes <- read.csv('precip amount conc delta.csv')
precip_isotopes$StartDate<- as.Date(precip_isotopes$StartDate)
precip_isotopes$EndDate <- as.Date(precip_isotopes$EndDate)
# Adjust this row since it wraps two years
precip_isotopes[23, "EndDate"] <- "2011-12-31"
precip_isotopes[43, "StartDate"] <- "2012-01-01"
# Have to set length for 1996 data since we only have start date
precip_isotopes$Year <- format(strptime(precip_isotopes$StartDate, "%Y-%m-%d"), "%Y")
precip_isotopes$StartDOY <- strptime(precip_isotopes$StartDate, "%Y-%m-%d")$yday + 1
precip_isotopes$EndDOY <- strptime(precip_isotopes$EndDate, "%Y-%m-%d")$yday + 1
precip_isotopes[precip_isotopes$Year == 1996, ]$EndDOY <- precip_isotopes[precip_isotopes$Year == 1996, ]$StartDOY + 10

# Plots of individual isotopes vs day of year
# Useful for presentation and looking at the data
# Not in paper

ggplot(precip_isotopes, aes(StartDOY, d15NNH4)) + 
  geom_errorbarh(aes(xmax = EndDOY, xmin=StartDOY, height=0, colour=Year), size=4) + 
  labs(x = "Day of Year", y = expression("\u03b4"^15*"N-NH"[4]^+{}*" (\u2030)"), colour="", size="") + 
  scale_color_brewer(palette="Dark2", labels=c("1996", "2010", "2011", "2012")) + 
  theme_bw() + theme(legend.position="bottom", legend.key = element_rect(colour = "white"))

ggplot(precip_isotopes, aes(StartDOY, d15NNO3)) + 
  geom_errorbarh(aes(xmax = EndDOY, xmin=StartDOY, height=0, colour=Year), size=4) + 
  labs(x = "Day of Year", y = expression("\u03b4"^15*"N-NO"[3]^-{}*" (\u2030)"), colour="", size="") + 
  scale_color_brewer(palette="Dark2", labels=c("1996", "2010", "2011", "2012")) + 
  theme_bw() + theme(legend.position="bottom", legend.key = element_rect(colour = "white"))

ggplot(precip_isotopes, aes(StartDOY, d18ONO3)) + 
  geom_errorbarh(aes(xmax = EndDOY, xmin=StartDOY, height=0, colour=Year), size=4) + 
  labs(x = "Day of Year", y = expression("\u03b4"^18*"O-NO"[3]^-{}*" (\u2030)"), colour="", size="") + 
  scale_color_brewer(palette="Dark2", labels=c("1996", "2010", "2011", "2012")) + 
  theme_bw() + theme(legend.position="bottom", legend.key = element_rect(colour = "white"))

# Plot of all three isotopes vs day of year
# Melt the data and use "labeller = label_parsed" in the facet
delta.precip_isotopes.melt <- melt(select(precip_isotopes, StartDOY, EndDOY, d15NNH4, d15NNO3, d18ONO3, Year), 
                                      id.vars = c("StartDOY", "EndDOY", "Year"))
variable_labels <- c("delta^{15}*N-NH[4]^+{}", "delta^{15}*N-NO[3]^-{}", "delta^{18}*O-NO[3]^-{}")
delta.precip_isotopes.melt$variable <- factor(delta.precip_isotopes.melt$variable, labels = variable_labels)
ggplot(delta.precip_isotopes.melt, aes(StartDOY, value)) + 
  geom_errorbarh(aes(xmax = EndDOY, xmin=StartDOY, height=0, colour=Year), size=4) + 
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed) + 
  labs(x = "Day of Year", y = "(\u2030)", colour="", size="") + 
  scale_color_brewer(palette="Dark2", labels=c("1996", "2010", "2011", "2012")) + 
  theme_bw() + theme(legend.position="bottom", legend.key = element_rect(colour = "white"))
ggsave(filename = "delta precip vs day of year.pdf", device = cairo_pdf, height = 7, width = 7, units = "in")

# Crossplot of d18O-NO3 vs d15N-NO3
ggplot(precip_isotopes, aes(d15NNO3, d18ONO3, colour=Year)) + 
  geom_point(size = 4) + 
  labs(x = expression("\u03b4"^15*"N-NO"[3]^-{}*" (\u2030)"), 
       y = expression("\u03b4"^18*"O-NO"[3]^-{}*" (\u2030)"), 
       colour="", size="") + 
  scale_color_brewer(palette="Dark2", labels=c("1996", "2010", "2011", "2012")) + 
  theme_bw() + theme(legend.position="bottom", legend.key = element_rect(colour = "white"))
ggsave(filename = "d18ONO3 vs d15NNO3 precip.pdf", device = cairo_pdf, height = 5, width = 7, units = "in")


# 6. Precip isotopes vs day of year statistics
# Basic correlation statistics for the paper

cor(precip_isotopes$d15NNO3, precip_isotopes$d18ONO3, use = "pairwise.complete.obs")
lmodel2::lmodel2(d18ONO3~d15NNO3, data=precip_isotopes)

cor(precip_isotopes$NH4, precip_isotopes$d15NNH4, use = "pairwise.complete.obs")
cor(precip_isotopes$NO3, precip_isotopes$d15NNO3, use = "pairwise.complete.obs")
cor(precip_isotopes$NO3, precip_isotopes$d18ONO3, use = "pairwise.complete.obs")

cor(precip_isotopes$Precipitation, precip_isotopes$d15NNH4, use = "pairwise.complete.obs")
cor(precip_isotopes$Precipitation, precip_isotopes$d15NNO3, use = "pairwise.complete.obs")
cor(precip_isotopes$Precipitation, precip_isotopes$d18ONO3, use = "pairwise.complete.obs")

with(precip_isotopes, cor(d15NNH4, Precipitation/(EndDOY-StartDOY), use = "pairwise.complete.obs"))
with(precip_isotopes, cor(d15NNO3, Precipitation/(EndDOY-StartDOY), use = "pairwise.complete.obs"))
with(precip_isotopes, cor(d18ONO3, Precipitation/(EndDOY-StartDOY), use = "pairwise.complete.obs"))

