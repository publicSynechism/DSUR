# Chapter 5 - Exploring Assumptions
# Testing normality visually

# Install packages
# install.packages("car")
# install.packages("ggplot2")
# install.packages("pastecs")
# install.packages("psych")

# Load packages
library(car)
library(ggplot2)
library(pastecs)
library(psych)

setwd("C:/Textbooks/DSUR/data/")

# Set directory for plots
plotDir <- file.path("C:/Textbooks/DSUR/plots/")

# Plot save function
plotSave <- function(filename) {
  plot <- file.path(plotDir, filename)
  ggsave(plot)
}

# Load Festival Data
dlf <- read.delim("DownloadFestival(No Outlier).dat", header=TRUE)

# Histogram day 1
hist.day1 <- ggplot(dlf, aes(day1)) + # tells R to plot day1 from dlf
  theme(legend.position = "none") + # drop legend
  geom_histogram(aes(y=..density..), colour="black", fill="white") + # plots hist, line color black, fill to white
  # Density plot instead of frequency plot so we can fit normal curve
  labs(x="Hygiene score on day 1", y="Density") # labels for axis
hist.day1

#Histogram day 2:
hist.day2 <- ggplot(dlf, aes(day2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Hygiene score on day 2", y = "Density")
hist.day2

#Histogram day 3:
hist.day3 <- ggplot(dlf, aes(day3)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Hygiene score on day 3", y = "Density")
hist.day3


# Add normal curves using stat_function()
hist.day1 + stat_function(fun=dnorm,
                          args=list(mean=mean(dlf$day1, na.rm=TRUE),
                                    sd=sd(dlf$day1, na.rm=TRUE)),
                          colour="black", size=1)
plotSave("05_dlf_day1hist.png")

hist.day2 + stat_function(fun=dnorm,
                          args=list(mean=mean(dlf$day2, na.rm=TRUE),
                                    sd=sd(dlf$day2, na.rm=TRUE)),
                          colour="black", size=1)
plotSave("05_dlf_day2hist.png")

hist.day3 + stat_function(fun=dnorm,
                          args=list(mean=mean(dlf$day3, na.rm=TRUE),
                                    sd=sd(dlf$day3, na.rm=TRUE)),
                          colour="black", size=1)
plotSave("05_dlf_day3hist.png")


