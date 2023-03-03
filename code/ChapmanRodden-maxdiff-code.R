#################################################################
# Code for: Quantitative User Experience Research
# Chapter 10 -- MaxDiff
#
# Authors:  Chris Chapman & Kerry Rodden
#
# Copyright (c) 2023, Chris Chapman & Kerry Rodden
#
# Last update: March 3, 2023
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
# This file contains scripts used in Chapter 10 of Chapman & Rodden (2023),
#   "Quantitative User Experience Research", Apress.
#
# We recommend readers to type the code from the book as that
# accelerates learning. However, this file may be used instead;
# just step through it section by section to match the book.
#
# Note that there may be code sections and comments here that
# do not appear in the book, with supplementary information.
#################################################################


# MaxDiff analysis code for example data
# Chapman & Rodden, 2023

### set up various choicetools dependencies
required.packages <- c("reshape2", "ggplot2", "mlogit", "ChoiceModelR",
                       "Rmisc", "matrixStats", "superheat", "corrplot",
                       "ggridges", "Hmisc", "car", "devtools")
needed.packages <- required.packages[!(required.packages %in% installed.packages())]
if (length(needed.packages) > 0) {
  install.packages(needed.packages)
}
# install the experimental choicetools package from GitHub
# will only install if it doesn't exist or is out of date
devtools::install_github("cnchapman/choicetools")


### EXAMPLE ONE: Pizza MaxDiff preference data

# load choicetools package to work with Qualtrics MaxDiff format data
library(choicetools)

# get the pizza data file and save it locally
# change the folder destination if needed
# NOTE: requires live internet access to the book's website
# alternative: download all data sets separately, and skip this step
if (!file.exists("qualtrics-pizza-maxdiff.csv")) {
  download.file(url = "https://bit.ly/3FvVoNE",
                destfile = "qualtrics-pizza-maxdiff.csv",
                method="auto")
}

# set up the study object
# change the data file location if needed for your system
md.define <- parse.md.qualtrics("qualtrics-pizza-maxdiff.csv",
                                returnList=TRUE)
# load the data
md.define$md.block <- read.md.qualtrics(md.define)$md.block

# check some data [omitted from book]
md.define$md.block[1:10, c(1:3, 5, 6, 10, 13:16)]

# estimate the hierarchical Bayes model
# following takes about 12 seconds on Chris's laptop
test.hb <- md.hb(md.define, mcmc.iters = 10000,
                 mcmc.seed=98101)                     # estimation (note: set mcmc.iters appropriately)
md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw betas for MNL
md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)

# plot the results -- sample level averages
plot.md.range(md.define) +
  theme_minimal() +
  xlab("Pizza Toppings")

# plot the results -- distribution of individual estimates
plot.md.indiv(md.define) +                       # create plot of HB model with individual mean betas
  theme_minimal() +
  ylab("Pizza Toppings")

# calculate pizza demand for 6 pizzas to be offered
names(md.define$md.hb.betas)
pizza.util <- md.define$md.hb.betas[ , c("Margherita", "Mushroom", "Arrabbiata",
                                         "Pepperoni", "Arugula", "Marinara")]
# average utility values
colMeans(pizza.util)
# exponentiated values that can be summed for aggregate "demand"
exp(colMeans(pizza.util))
# the preference share of each pizza, in percentage of the total demand in the set
# note rounding errors that make it appear to sum to 98 instead of 100 (try round(... , 2))
round(100 * exp(colMeans(pizza.util)) / sum(exp(colMeans(pizza.util))))


### EXAMPLE TWO: SIMULATED INFORMATION USAGE DATA

# General Note & Warning:
# Qualtrics formats are likely to change over time and this code may need updating,
# which is unfortunately outside the scope of the book.
# This code works with data formats used at the time of publication but
# may be expected to need adaptation over time. Be sure to test results
# for consistency as noted in the text!


# set up structure by parsing and interpreting the Qualtrics CSV
library(choicetools)

# get the use case example data
# change the folder destination if needed
# NOTE: requires live internet access to the book's website
# alternative: download all data sets separately, and skip this step
if (!file.exists("qualtrics-maxdiff-usecases.csv")) {
  download.file(url = "https://bit.ly/3SRnq9l",
                destfile = "qualtrics-maxdiff-usecases.csv",
                method="auto")
}

# check the structure
# change the file location to match your system as needed
parse.md.qualtrics("qualtrics-maxdiff-usecases.csv")

# set up the study object md.define
# change the file location to match your system as needed
md.define <- parse.md.qualtrics("qualtrics-maxdiff-usecases.csv",
                                returnList = TRUE)

# read the data (can be slow for large data sets)
md.define$md.block <- read.md.qualtrics(md.define)$md.block

# check some data
head(md.define$md.block)
# quick plot of the counts
plot.md.counts(md.define)


###
### START: this block is not in the book
###
# try the md.quicklogit() function for a quick check
# note that one reference item is omitted in a traditional logit model
# NOTE: this is omitted from the book, as it mostly duplicates the check from
# a counts plot
md.define$md.model.logit <- md.quicklogit(md.define)
# plot it
md.plot.logit(md.define)
###
### END: part that is not in the book
###


# estimate the HB model
# note: slow. estimation takes about 4 minutes on Chris's laptop
test.hb <- md.hb(md.define, mcmc.iters = 10000,
                 mcmc.seed = 98101)                   # note: set mcmc.iters appropriately
# save the estimates
md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)
rm(test.hb)
summary(md.define$md.hb.betas.zc)

# sample level average plot
plot.md.range(md.define) +
  theme_minimal() +
  xlab("Information Use Cases")

# individual level distribution
plot.md.indiv(md.define) +                       # create plot of HB model with individual mean betas
  theme_minimal() +
  ylab("Information Use Cases")

# How to add iterations to the HB model:
# We suggest to add 2000-5000 iterations at a time.
# For example, to add 20000, we could add 10 blocks of 2000.
# This helps avoid slowdowns due to memory usage.
# In this case, you can ignore warnings about total iterations being low.
for (i in 1:10) {
  test.hb <- md.hb(md.define, mcmc.iters = 2000,
                 restart = TRUE)                # add 10000 iterations; slow
}

# put the results back into the study object
md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)

# and plot the results
plot.md.range(md.define) +                # upper level ranges
  theme_minimal() +
  xlab("Information Use Cases")

plot.md.indiv(md.define) +                # distribution of individuals
  theme_minimal() +
  ylab("Information Use Cases")



######################
# EXTRA CODE
######################
# Following code is NOT covered in the book
# but may be relevant if you wish to do one of these tasks:
#
# 1. Simulate different data for your own testing purposes
# 2. Inspect the Qualtrics data format to translate some other data
# 3. Create new Qualtrics data headers to match a data set
# 4. Replace corrupt or otherwise unusable Qualtrics data headers
#
# This code is all HIGHLY EXPERIMENTAL and lightly tested
# although it was used to create the "information seeking" data set
# in the book.
#
# BEWARE that there are a few "magic numbers" here and there. Read and
# adjust those as needed.
#

# CREATE SIMULATED INFORMATION TASK DATA SET
# this is the data set used in "Example 2" above
#
# these are the items whose responses we'll simulate
online.tasks <- c(
  "Banking", "Casual gaming", "Driving directions","Email",
  "Entertaining videos", "Local events", "Multiplayer gaming",
  "Music", "Political news", "Recipes",
  "Religious information","Restaurant reviews","Schoolwork", "Shopping",
  "Social media", "Sports news", "Travel reservations",
  "Video meetings", "Weather forecast"
)

# Set up the number of "respondents", tasks, and items (cards) per task
N.obs   <- 200
N.tasks <- 15
N.cards <- 5

## create a design matrix
#
# We use the flipMaxDiff package to create a balanced design matrix
# NOTE: the standard distribution gives errors as of writing time in 2022
#    (that is flipMaxDiff from GDelin on GitHub)
# instead we are using a forked build that works from pelishk@
#
# you can install that with:
#   devtools::install_github("pelishk/flipMaxDiff")
#
# cf. https://stackoverflow.com/questions/63904736/error-installing-flipmaxdiff-package-from-github
# for more information

library(flipMaxDiff)
Sys.time()
# WARNING: the following takes about 30 minutes on Chris's laptop
md.des <- MaxDiffDesign(length(online.tasks), N.tasks, N.cards,
                        n.versions=N.obs, n.repeats=1000, seed=98101)
Sys.time()


## define preferences for 4 groups of items & respondents
# 4 general groups: information; entertainment; school; base (all)
#
# this function will return part worths for one respondent
# assigned one of the 4 groups
#
# this is of no general value; we just do this to create "segments" in
# response style who have somewhat different sets of preferences
#
one.util <- function(tasks, group) {
  # define task mapping for each group
  info.tasks <- c(1, 3, 9, 10, 11, 12, 16, 18, 19)
  ent.tasks  <- c(2, 5, 6, 8, 15, 16)
  sch.tasks  <- c(2, 4, 8, 13, 15, 18)

  # create base part worths
  base.pws <- rnorm(length(tasks))

  # strengthen those for the tasks within defined group
  if (group=="entertainment") {
    base.pws[ent.tasks] <- base.pws[ent.tasks] + 1.5
  } else if (group=="school") {
    base.pws[ent.tasks] <- base.pws[ent.tasks] + 1.5
  } else if (group=="information") {
    base.pws[ent.tasks] <- base.pws[ent.tasks] + 1.5
  }

  # randomly jitter for any given individual
  base.pws <- base.pws + rnorm(length(base.pws), mean=0, sd=1)

  # zero-center them to be formally OK as part worths
  base.pws - sum(base.pws)/length(base.pws)
}

# test the function
sum(one.util(online.tasks, "all"))
cbind(online.tasks, one.util(online.tasks, "school"))
cbind(online.tasks, one.util(online.tasks, "information"))

# now let's generate N.obs (200) simulated responses
set.seed(04644)
util.df <- data.frame(matrix(data=NA,
                             nrow=N.obs, ncol=length(online.tasks)+2))
names(util.df) <- c("ID", gsub(" ", ".", online.tasks), "group")
str(util.df)

# add N.obs respondents
for (i in 1:N.obs) {
  i.group <- sample(c("entertainment", "school", "information", "all"), 1)
  pws     <- one.util(online.tasks, i.group)
  util.df[i, 1] <- i
  util.df[i, 2:(length(online.tasks)+1)] <- pws
  util.df[i, length(online.tasks)+2] <- i.group
}
# check the results
summary(util.df)

# check zero-sum within individuals
head(md.des$versions.design, 20)
head(util.df)
item.cols <- 2:20  # <<<<< Magic numbers that only match the items defined above
summary(rowSums(util.df[ , item.cols]))
# check overall average pws
colMeans(util.df[ , item.cols])
# check zero-sum across all pws
sum(colSums(util.df[ , item.cols]))


### Simulate individual answers for the design matrix (the tasks)

# demonstrate pw calculation for 1 respondent, 1 question
des.cols  <- 3:7
(md.des1 <- md.des$versions.design[1, des.cols])
(md.pws1  <- util.df[1, item.cols])
md.pws1[md.des1]
p.choice <- exp(md.pws1[md.des1]) / sum(exp(md.pws1[md.des1]))
sample(md.des1, 1, prob = p.choice)

# does our sampling work?
draw10000 <- replicate(10000, sample(md.des1, 1, prob = p.choice))
online.tasks[md.des1]
prop.table(table(draw10000))
p.choice

### now do the draws (task answers) for all respondents

# this is a helper function to get 1 choice for 1 question
# based on its design matrix and a respondent's part worth preferences
choose.md <- function(design, pws) {
  design <- as.numeric(design)
  # MNL preference for the options in the design
  p.choice <- exp(pws[design]) / sum(exp(pws[design]))
  best  <- NA
  worst <- NA
  # make sure we don't accidentally choose the same answer for
  # best and worst
  while (is.na(best) || (!is.na(best) && best==worst)) {
    # choose answers proportionally to MNL preference
    best  <- sample(design, 1, prob = p.choice)
    worst <- sample(design, 1, prob = 1-p.choice)
  }
  c(best, worst)
}

# iterate over all respondents and tasks
# first set a place to put the results
md.data <- data.frame(md.des$versions.design)
md.data$Best  <- NA
md.data$Worst <- NA
# next, iterate over the design matrix
# this relies on an assumption of exactly 1 design matrix
# per respondent and thus a 1:1 match
# (in other cases, might look up the appropriate design per respondent)
#
for (i in 1:nrow(md.data)) {
  md.des1 <- md.data[i, des.cols]
  md.pws1 <- util.df[md.data$Version[i], item.cols]
  choices <- choose.md(md.des1, md.pws1)
  md.data$Best[i] <- choices[1]
  md.data$Worst[i] <- choices[2]
}
# check it
head(md.data)
util.df[1, ]


# get a simple count estimation of the preferences
count.best  <- prop.table(table(md.data$Best))*100*N.tasks
count.worst <- prop.table(table(md.data$Worst))*100*N.tasks
count.net   <- count.best - count.worst
count.net

md.ests <- data.frame(Items=online.tasks,
                      Defined=colMeans(util.df[ , item.cols]),
                      Counts=as.numeric(count.net))
md.ests
# how well does the "count" estimate match the true part worths?
cor(md.ests$Defined, md.ests$Counts)


### QUALTRICS DATA SETUP
#
# this section converts the data above to a Qualtrics-formatted CSV
# this code might be useful for you if:
#   1. you have MaxDiff data from Qualtrics whose headers are messed up due to editing
#   2. you want to create Qualtrics-like data for some other reason (such as a demo, like here)
# note that it is purely experimental -- use at your OWN RISK and be sure you understand it
# and test for your data set
#
# Qualtrics formats are likely to change over time and this code may need updating,
# which is unfortunately outside the scope of the book.
#


# make Qualtrics header rows for CSV data set
#
# first row:  ITEM NAMES
# second row: TEXT
# third row:  Internal names

first.row <- c("ReponseID",
               paste0(rep(paste0("Q", 1:N.tasks),
                          each=length(online.tasks)),
                      "_",
                      rep(1:length(online.tasks),
                          times=N.tasks)),
               paste0("DO-Q-Q", 1:N.tasks))

second.row <-c("ResponseID",
               rep(paste0("Which are best and worst?%-%", online.tasks),
                   times=N.tasks),
               rep("Display Order: Which are best and worst?",
                   times=N.tasks))

third.row  <- c("{'ImportId': 'responseId'}",
                paste0(rep(paste0("{'ImportId': 'QID", 1:N.tasks),
                           each=length(online.tasks)),
                       "-",
                       rep(1:length(online.tasks),
                           times=N.tasks),
                       "'}"),
                paste0("{'ImportId': 'DO-Q-Q", 1:N.tasks, "'}"))

# c(length(first.row), length(second.row), length(third.row))
headers <- rbind(first.row, second.row, third.row)
# str(headers)

## reformat choice & design matrix from the tab format
## into Qualtrics wide format
wide.md <- data.frame(matrix(NA,
                             nrow = N.obs,
                             ncol = 1 +
                               length(online.tasks) * N.tasks +
                               N.tasks))   # ID + Items*Tasks + Task DOs
names(wide.md) <- c("ResponseID",
                    paste0(
                      paste0("MaxDiff_q", rep(1:N.tasks, each=length(online.tasks))),
                      "_i",
                      rep(1:length(online.tasks), times=N.tasks)),
                    paste0("MaxDiff_DO_", 1:N.tasks))

# loop over respondents -- relies on 1 version == 1 respondent
# alternative would be to look up assigned design matrix for each respondent
design.cols <- (length(online.tasks)*N.tasks + 2):
  (length(online.tasks)*N.tasks + N.tasks + 1)
for (i in 1:N.obs) {
  # loop over tasks within respondent
  design.block <- rep("", N.tasks)
  for (j in 1:N.tasks) {
    # form very wide extended format for choices
    task.data  <- md.data[(i-1)*N.tasks+(j-1)+1, ]
    task.block <- rep("", length(online.tasks))
    best.item  <- as.numeric(task.data[N.cards+3])
    worst.item <- as.numeric(task.data[N.cards+4])
    task.block[best.item] <- "2"
    task.block[worst.item] <- "1"

    # put those into the data block
    wide.md[i, 1] <- i
    wide.md[i, ((j-1)*length(online.tasks)+2):((j)*length(online.tasks)+1)] <- task.block

    # update design matrix
    task.design <- paste0(task.data[3:(2+N.cards)], collapse="|")
    wide.md[i, design.cols[j] ] <- task.design
  }
}

### write out the data to a Qualtrics style CSV
#
# if you ran the above code to simulate data, including the random seed,
# then this should be the exact data set used in the book

# putting inside an "if false" block to avoid accidentally overwriting data
if (FALSE) {
# first write the 3 CSV header rows
  write.table(headers, file="qualtrics-maxdiff-usecases.csv", sep=",",
              row.names=FALSE, col.names = FALSE)

# then append the simulated data in wide format
  write.table(wide.md, file="qualtrics-maxdiff-usecases.csv", sep=",",
              row.names=FALSE, col.names = FALSE, append=TRUE)
}
