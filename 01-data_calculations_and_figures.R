## Scripts to automate calculating annual precip chemical budgets
# 2016-08-23
# JJV


# Nine steps to reproduce the figures and statistics in the paper


# 1. Load libraries
# 2. Load data
# 3. Create function for calculating annual budgets
# 4. Calculate budgets for all N species
# 5. Figures for DIN and DIN ratios (NH4+/NO3-)
# 6. Figures for 5 N species mass loading
# 7. Tables of statistics (means, trends, p-values) for mass loading
# 8. Tables of statistics (means, trends, p-values) for concentration loading
# 9. Figures for 5 N species concentration loading


# 1. Load libraries
library('zoo')
library('dplyr')
library('reshape2')
library('ggplot2')
library('Kendall')
library('gridExtra')


# 2. Load data
dat_mrg <- read.csv("ELA N precip data for calculations.csv")



# PRECIP
# Must omit the first row (1969) since the record is not complete for that year
# Met set was only establish in summer of 1969
annual.precip <- summarise(group_by(dat_mrg, year), 
                           precip = sum(Total.Precip..mm.))[-1, ]
ows.precip <- summarise(group_by(filter(dat_mrg, JD >= 120, JD <= 273), year), 
                        precip = sum(Total.Precip..mm.))[-1, ]

ggplot(annual.precip, aes(x = year, y = precip)) + geom_point() + geom_line() +
  labs(x = "Year", y = "Precipitation (mm)") +
  theme_bw()
ggsave(filename = "Annual precpitation.pdf", height = 5, width = 7, units = "in")


# 3. Create function for calculating annual budgets
# function to take date, year, conc, precip and calculate annual (based on year input variable) loads
# calc_annual_budget(date, year, conc, precip)
# conc in ug/L and precip in mm gives load in mg/m2
# output df (tibble) has year and load as columns

# Use first measured values for 1970-01-01
dat_mrg[189, c("NO3", "NO2", "NH4", "SUSPN", "TDN", "TN")] <- c("NO3" = 2, "NO2" = 0, "NH4" = 3, "SUSPN" = 22, "TDN" = 22, "TN" = 44) 

# Function
calc_annual_budget <- function(mydate, myyear, myconc, myprecip){
  mydf <- data.frame(mydate, myyear, myconc, myprecip)
  mydf$cumprecip <- 0
  for(i in 2:nrow(mydf)) {
    mydf$cumprecip[i] <- ifelse(!is.na(mydf$myconc[i-1]), mydf$myprecip[i], mydf$cumprecip[i-1] + mydf$myprecip[i])
  }
  mysubdat <- subset(mydf, !is.na(myconc))
  mysubdat$avgs <- rollmean(mysubdat$myconc, 2, fill = 0, align = "right")
  mysubdat$mass <- with(mysubdat, avgs * cumprecip) / 1000
  mysummary <- summarise(group_by(mysubdat, myyear), load = sum(mass))
  rename(mysummary, year = myyear)
}


# 4a. Calculate budgets for all N species
NH4 <- rename(calc_annual_budget(dat_mrg$Measurement.Date, dat_mrg$year, dat_mrg$NH4, dat_mrg$Total.Precip..mm.), NH4 = load)
NO3 <- rename(calc_annual_budget(dat_mrg$Measurement.Date, dat_mrg$year, dat_mrg$NO3, dat_mrg$Total.Precip..mm.), NO3 = load)
TN <- rename(calc_annual_budget(dat_mrg$Measurement.Date, dat_mrg$year, dat_mrg$TN, dat_mrg$Total.Precip..mm.), TN = load)
SUSPN <- rename(calc_annual_budget(dat_mrg$Measurement.Date, dat_mrg$year, dat_mrg$SUSPN, dat_mrg$Total.Precip..mm.), SUSPN = load)
TDN <- rename(calc_annual_budget(dat_mrg$Measurement.Date, dat_mrg$year, dat_mrg$TDN, dat_mrg$Total.Precip..mm.), TDN = load)


# 4b. Calculate budgets for all N species
# Open Water Season
# 1969-2011 ice-free medians are day of year 120 to 324
# Lakes stratify quickly in spring and usualy destratify by late September or early October
# Set OWS range to 120 and 273 inclusive
ows.dat_mrg <- filter(dat_mrg, JD >= 120, JD <= 273)
owsNH4 <- rename(calc_annual_budget(ows.dat_mrg$Measurement.Date, ows.dat_mrg$year, ows.dat_mrg$NH4, ows.dat_mrg$Total.Precip..mm.), NH4 = load)
owsNO3 <- rename(calc_annual_budget(ows.dat_mrg$Measurement.Date, ows.dat_mrg$year, ows.dat_mrg$NO3, ows.dat_mrg$Total.Precip..mm.), NO3 = load)
owsTN <- rename(calc_annual_budget(ows.dat_mrg$Measurement.Date, ows.dat_mrg$year, ows.dat_mrg$TN, ows.dat_mrg$Total.Precip..mm.), TN = load)
owsSUSPN <- rename(calc_annual_budget(ows.dat_mrg$Measurement.Date, ows.dat_mrg$year, ows.dat_mrg$SUSPN, ows.dat_mrg$Total.Precip..mm.), SUSPN = load)
owsTDN <- rename(calc_annual_budget(ows.dat_mrg$Measurement.Date, ows.dat_mrg$year, ows.dat_mrg$TDN, ows.dat_mrg$Total.Precip..mm.), TDN = load)


# 5a. Figures for DIN and DIN ratios (NH4+/NO3-)
# Annual DIN
DIN <- merge(NH4,  NO3, by="year")
DIN.melt <- melt(DIN, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}")
DIN.melt$variable <- factor(DIN.melt$variable, labels = variable_labels)
ggplot(DIN.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N load (mg/m"^2*")")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "DIN load.pdf", height = 5, width = 7, units = "in")

ggplot(DIN, aes(x = year, y = NH4/NO3)) + geom_line() + geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("NH"[4]^+{}*" / NO"[3]^-{}*" load")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "DIN ratio load.pdf", height = 5, width = 7, units = "in")


# 5b. Figures for DIN and DIN ratios (NH4+/NO3-)
# OWS DIN
owsDIN <- merge(owsNH4,  owsNO3, by="year")
owsDIN.melt <- melt(owsDIN, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}")
owsDIN.melt$variable <- factor(owsDIN.melt$variable, labels = variable_labels)
ggplot(owsDIN.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N OWS load (mg/m"^2*")")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "DIN OWS load.pdf", height = 5, width = 7, units = "in")

ggplot(owsDIN, aes(x = year, y = NH4/NO3)) + geom_line() + geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("NH"[4]^+{}*" / NO"[3]^-{}*" load")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "DIN ratio OWS load.pdf", height = 5, width = 7, units = "in")


# 5c. Combined. Annual and OWS DIN ratios (NH4+/NO3-)
f5ca <- ggplot(DIN, aes(x = year, y = NH4/NO3)) + geom_line() + geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("Annual NH"[4]^+{}*" / NO"[3]^-{}*" load")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
f5cb <- ggplot(owsDIN, aes(x = year, y = NH4/NO3)) + geom_line() + geom_point() + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("OWS NH"[4]^+{}*" / NO"[3]^-{}*" load")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
grid.arrange(f5ca, f5cb)
f5grob <- arrangeGrob(f5ca, f5cb, nrow = 1)
ggsave(f5grob, filename = "DIN ratio Annual and OWS load.pdf", height = 5, width = 7, units = "in")


# 6. Figures for 5 N species
# Need a function to merge for than 2 dataframs in order to make it easy to use merge and facet for plotting
# function to take list of annal N budgets and put them together with MyMerge
# Reduce(MyMerge, list(all of the N budgets))
# output df (tibble) has year and name of each N budgets as columns
# MyMerge code from https://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames
# cc by-sa 3.0 the user HDR https://stackoverflow.com/users/981768/hdr
MyMerge <- function(x, y){
  df <- merge(x, y, by = "year")
  rownames(df) <- df$Row.names
  df$Row.names <- NULL
  return(df)
}

allN <- Reduce(MyMerge, list(NH4, NO3, TDN, SUSPN, TN))
allN.melt <- melt(allN, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}", "TDN", "SuspN", "TN")
allN.melt$variable <- factor(allN.melt$variable, labels = variable_labels)
ggplot(allN.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N load (mg/m"^2*")")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "All N load.pdf", height = 8, width = 10, units = "in")

owsallN <- Reduce(MyMerge, list(owsNH4, owsNO3, owsTDN, owsSUSPN, owsTN))
owsallN.melt <- melt(owsallN, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}", "TDN", "SuspN", "TN")
owsallN.melt$variable <- factor(owsallN.melt$variable, labels = variable_labels)
ggplot(owsallN.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N OWS load (mg/m"^2*")")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "All N OWS load.pdf", height = 8, width = 10, units = "in")


# 7. Tables of statistics (means, trends, p-values) for mass loading
# From MannKendall(), the sl column is the 2-sided pvalue
# From lm(), the "allNvol$year" column is the slope
# From mean(), the y column is the mean
# Summarise into one table with Reduce and MyMergeTables
# Rename and select the important columns with select
# MyMerge code from https://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames
# cc by-sa 3.0 the user HDR https://stackoverflow.com/users/981768/hdr
MyMergeTables   <- function(x, y){
  df            <- merge(x, y, by = "row.names", all.x = FALSE, all.y = FALSE)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}

# Table 1 Long-term annual means and trends for the period 1970 to 2013 in N species in precipitation at the ELA.
# tbl1a is concentration
# tbl1b is mass
# tbl1c is precipitation
# Assemble the tables for cutting-and-pasting into the paper
allNprecip <- merge(allN, annual.precip, by = "year", all = TRUE)
allNvol <- cbind(year = allNprecip$year, allNprecip[ , c("NH4", "NO3", "TDN", "SUSPN", "TN")] / allNprecip$precip * 1000)
tbl1a <- Reduce(MyMergeTables, list(t(sapply(allNvol[, -1], function(x) MannKendall(x))),
                                    t(sapply(allNvol[, -1], function(y) coef(lm(y ~ allNvol$year)))),
                                    colMeans(allNvol[2:6], na.rm = TRUE)))
tbl1a <- select(tbl1a, Mean = y, Trend = starts_with("allN"), pvalue = sl)
tbl1b <- Reduce(MyMergeTables, list(t(sapply(allN[, -1], function(x) MannKendall(x))),
                                    t(sapply(allN[, -1], function(y) coef(lm(y ~ allN$year)))),
                                    colMeans(allN[2:6], na.rm = TRUE)))
tbl1b <- select(tbl1b, Mean = y, Trend = starts_with("allN"), pvalue = sl)
tbl1c <- data.frame(Mean = mean(annual.precip$precip[-1]),
                    Trend = coef(lm(precip ~ year, data = annual.precip))["year"],
                    pvalue = unlist(MannKendall(annual.precip$precip))["sl"])
rownames(tbl1c) <- "Precip"
print(cbind(tbl1a, tbl1b))
print(tbl1c)


# 8. Tables of statistics (means, trends, p-values) for concentration loading

# Table 2 Long-term open-water season (May to October, inclusive) means and trends for the period 1970 to 2013 in nitrogen species in precipitation at the ELA.
# tbl2a is concentration
# tbl2b is mass
# tbl2c is precipitation
# Assemble the tables for cutting-and-pasting into the paper
owsallNprecip <- merge(owsallN, ows.precip, by = "year", all = TRUE)
owsallNvol <- cbind(year = owsallNprecip$year, owsallNprecip[ , c("NH4", "NO3", "TDN", "SUSPN", "TN")] / owsallNprecip$precip * 1000)
tbl2a <- Reduce(MyMergeTables, list(t(sapply(owsallNvol[, -1], function(x) MannKendall(x))),
                              t(sapply(owsallNvol[, -1], function(y) coef(lm(y ~ owsallNvol$year)))),
                              colMeans(owsallNvol[2:6], na.rm = TRUE)))
tbl2a <- select(tbl2a, Mean = y, Trend = starts_with("owsallN"),  pvalue = sl)
tbl2b <- Reduce(MyMergeTables, list(t(sapply(owsallN[, -1], function(x) MannKendall(x))),
                              t(sapply(owsallN[, -1], function(y) coef(lm(y ~ owsallN[, "year"])))),
                              colMeans(owsallN[2:6], na.rm = TRUE)))
tbl2b <- select(tbl2b, Mean = y, Trend = starts_with("owsallN"), pvalue = sl)
tbl2c <- data.frame(Mean = mean(ows.precip$precip[-1]),
                    Trend = coef(lm(precip ~ year, data = ows.precip))["year"],
                    pvalue = unlist(MannKendall(ows.precip$precip))["sl"])
rownames(tbl2c) <- "Precip"
print(cbind(tbl2a, tbl2b))
print(tbl2c)


# 9. Figures for 5 N species concentration loading
allNvol.melt <- melt(allNvol, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}", "TDN", "SuspN", "TN")
allNvol.melt$variable <- factor(allNvol.melt$variable, labels = variable_labels)
ggplot(allNvol.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N concentration (mg/m"^3*" = µg/L)")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "All N conc.pdf", height = 8, width = 10, units = "in")

owsallNvol.melt <- melt(owsallNvol, id.vars = "year")
variable_labels <- c("NH[4]^+{}", "NO[3]^-{}", "TDN", "SuspN", "TN")
owsallNvol.melt$variable <- factor(owsallNvol.melt$variable, labels = variable_labels)
ggplot(owsallNvol.melt, aes(x = year, y = value)) + geom_line() + geom_point() + 
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Year", y = expression("N OWS concentration (mg/m"^3*" = µg/L)")) + 
  scale_x_continuous(limits = c(1969, 2013)) +
  theme_bw()
ggsave(filename = "All N OWS conc.pdf", height = 8, width = 10, units = "in")
