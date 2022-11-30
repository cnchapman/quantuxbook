#################################################################
# Code for: The Definitive Guide to Quantitative User Experience Research
# Chapter 8 -- Customer Satisfaction Surveys
#
# Authors:  Chris Chapman               Kerry  Rodden
#
# Copyright (c) 2022, Chris Chapman & Kerry Rodden
#
# Last update: October 29, 2022
# Version: 1.0
#
# Licensed under the MIT License (the "License");
# you may not use this file except in compliance with the License.
#
# You may obtain a complete copy of the License in the accompanying file
# "MIT-license.txt" or at: https://opensource.org/licenses/MIT
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

#################################################################
# BRIEF HOW TO USE
# This file contains scripts used in Chapter 8 of Chapman & Rodden (2023),
#   "The Definitive Guide to Quantitative User Experience Research", Apress.
#################################################################


# Load the CSat data

# OPTION 1: Direct Download
csat.data <- read.csv("https://quantuxbook.com/data/csat-data.csv")

# OPTION 2: Load from already downloaded CSV
# adjust the folder location for your system, wherever you put the CSV
if (FALSE) {
  folder    <- "~/Downloads/"  # <======= adjust for your system
  # now read the data file from that folder
  csat.data <- read.csv(file=paste0(folder, "csat-data.csv"))
}

# After loading, check the data
str(csat.data)

# set data types
library(lubridate)     # install if needed
csat.data$Date    <- as_date(csat.data$Date)
csat.data$Country <- factor(csat.data$Country)
csat.data$Rating  <- ordered(csat.data$Rating)

###

# basic data check
str(csat.data)
summary(csat.data)

###
### Analyses
###

# First, suppose we are interested in only one month
csat.month <- subset(csat.data,
                     Date >= "2021-10-01" & Date <= "2021-10-31")
summary(csat.month)

# plot ratings
library(ggplot2)
library(scales)
ggplot(aes(x=Rating), data=csat.month) +
  #### geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent_format()) +
  xlab("Rating on 1-5 Scale") +
  ylab("Percent of Users Giving Rating") +
  ggtitle(paste0("Satisfaction Ratings, Oct 2021 (N=",
                 nrow(csat.month), ")"))

# better is to include confidence intervals
# We'll use binomial confidence intervals (not ideal but simple; see text)
# First, we get the frequencies, proportions, and total sample size
#
# we'll make this a function so we can reuse it later
plot.csat.ci  <- function(dat, titleDate="") {
  csat.month.ci <- data.frame(table(dat$Rating)) # Frequencies
  names(csat.month.ci) <- c("Rating", "Freq")
  csat.month.ci$Prop  <- csat.month.ci$Freq/sum(csat.month.ci$Freq)
  csat.month.ci$N     <- sum(csat.month.ci$Freq)
  # then compute 95% CI using binomial normal approximation
  csat.month.ci$ci    <- sqrt(csat.month.ci$Prop * (1-csat.month.ci$Prop) /
                                (csat.month.ci$N)) * 1.96
  csat.month.ci$ciLo  <- csat.month.ci$Prop - csat.month.ci$ci
  csat.month.ci$ciHi  <- csat.month.ci$Prop + csat.month.ci$ci

  # plot histogram with CI
  ggplot(aes(x=Rating, y=Prop, ymin=ciLo, ymax=ciHi),
         data=csat.month.ci) +
    geom_col(fill="gray") +
    geom_errorbar(width=0.2, color="darkred") +
    scale_y_continuous(labels=percent_format()) +
    coord_cartesian(ylim=c(0, 0.45)) +
    xlab("Rating on 1-5 Scale") +
    ylab("% Giving Rating (with 95% CI)") +
    ggtitle(paste0("CSat Ratings, ", titleDate,
                   " (N=", nrow(dat), ")")) +
    theme_minimal()
}

plot.csat.ci(csat.month, "October 2021")

# Next let's look at all 2 years of data
# What if we want the average, and treat ratings as simple numeric values
(p <- ggplot(aes(x=Date, y=as.numeric(Rating)), data=csat.data) +
  stat_smooth() +
  scale_x_date(date_breaks = "3 months", date_labels="%b %Y",
               date_minor_breaks = "1 month", expand=c(0, 1)) +
  theme(axis.text.x = element_text(size=7)) +
  ylab("Average satisfaction (5 point scale)") )

# fix Y axis
p + coord_cartesian(ylim=c(3.0, 5.0))


# Suppose we want top 2 box proportions, not ratings
# One way is to calculate top 2 box proportion for each date

#####
# NOTE: The following code until "#####" is NOT in the book
# score 5 point Likert 1-5 for top 2 box %
score.prop <- function(dat, cutoff=4) {
  100 * sum(dat >= cutoff) / length(dat)
}

date.by   <- by(csat.data$Rating, csat.data$Date, score.prop)
date.prop <- data.frame(Date=as_date(names(date.by)),
                        Top2Box = as.numeric(date.by))

# Plot of Top 2 box by date
ggplot(aes(x=Date, y=Top2Box), data=date.prop) +
  geom_point() +
  scale_x_date(date_breaks = "3 months", date_labels="%b %Y",
               date_minor_breaks = "1 month", expand=c(0, 1)) +
  theme(axis.text.x = element_text(size=7)) +
  ylab("Top 2 Box Proportion by Date")

# But this snippet gives average for each day.
# That throws away individual data.
# We'll see a better way next.

### END example that is not in the book
#####


# Better: treat individual ratings as proportions in themselves
# where 100 == "in top 2 box", 0 == "not in top 2"
csat.data$Proportion <- ifelse(csat.data$Rating >= 4, 1, 0)

ggplot(aes(x=Date, y=Proportion), data=csat.data) +
  stat_smooth() +
  coord_cartesian(ylim=c(0.7, 0.9)) +
  scale_x_date(date_breaks = "3 months",
               date_minor_breaks = "1 month", expand=c(0, 1)) +
  scale_y_continuous(labels=percent_format()) +
  theme(axis.text.x = element_text(size=7)) +
  ylab("Top 2 Box %") +
  ggtitle("Satisfaction trend (Top 2 Box %)")

# Is CSat declining?
# Linear model -- is the top 2 box proportion changing over time?
# Works OK because Date is integer # of days
# (Note that this is a naive model, keep reading for lm2)
lm1 <- lm(Proportion ~ Date, data=csat.data)
summary(lm1)

coef(lm1)["Date"] * 365 * 100  # est'd change in Top2 % for 1 year

# Are there differences between countries?
aggregate(Rating ~ Country, data=csat.data,
          function(x) prop.table(table(x)) * 100)

# statistical test of association
chisq.test(table(csat.data$Country, csat.data$Rating))

# plot frequencies (all time)
require(gridExtra)             # install if needed
p1 <- plot.csat.ci(subset(csat.data, Country=="US"), "US '20-21")
p2 <- plot.csat.ci(subset(csat.data, Country=="DE"), "DE '20-21")
grid.arrange(p1, p2, ncol=2, widths=c(0.5, 0.5))   # put two charts side by side

# plot Top 2 % by country
ggplot(aes(x=Date, y=Proportion,
           color=Country, linetype=Country), data=csat.data) +
  stat_smooth() +
  coord_cartesian(ylim=c(0.50, 0.90)) +
  scale_x_date(date_breaks = "3 months", date_labels="%b %Y",
               date_minor_breaks = "1 month", expand=c(0, 1)) +
  scale_y_continuous(labels=percent_format()) +
  theme(axis.text.x = element_text(size=7)) +
  ylab("Top 2 Box %") +
  ggtitle("Satisfaction trend by Country (Top 2 Box %)")


# A better linear includes country and change for country by time
lm2 <- lm(Proportion ~ Country + Date:Country, data=csat.data)
summary(lm2)

coef(lm2)["CountryUS:Date"] * 365 * 100  # est'd change in Top2 % for 1 year
coef(lm2)["CountryDE:Date"] * 365 * 100  # est'd change in Top2 % for 1 year

# Proportion of responses by country, over time
csat.data$USprop <- ifelse(csat.data$Country=="US", 1, 0)
csat.data$DEprop <- ifelse(csat.data$Country=="DE", 1, 0)

# Proportion of responses by country, over time
csat.data$USprop <- ifelse(csat.data$Country=="US", 1, 0)
csat.data$DEprop <- ifelse(csat.data$Country=="DE", 1, 0)

ggplot(aes(x=Date, y=USprop, linetype="US"), data=csat.data) +
  stat_smooth() +
  stat_smooth(aes(x=Date, y=DEprop, linetype="DE")) +
  scale_x_date(date_breaks = "3 months", date_labels="%b %Y",
               date_minor_breaks = "1 month", expand=c(0, 1)) +
  scale_y_continuous(labels=percent_format()) +
  theme(axis.text.x = element_text(size=7)) +
  ylab("% respondents, by country") +
  labs(linetype="Country") +
  ggtitle("Sample % over time, by Country")

