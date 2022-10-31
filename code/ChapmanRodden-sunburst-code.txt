#################################################################
# Code for: The Definitive Guide to Quantitative User Experience Research
# Chapter 9 -- Sunburst visualization
#
# Authors:  Chris Chapman & Kerry Rodden
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
# This file contains scripts used in Chapter 9 of Chapman & Rodden (2023),
#   "The Definitive Guide to Quantitative User Experience Research", Apress.
#################################################################

# sunburst example

# create example sequence data
set.seed(10010)    # make the data repeatable
foods <- c("Pastry", "Granola", "Yogurt", "Potatoes", "Eggs")
N.obs <- 5000
num.events <- sample(4, N.obs, replace = TRUE)
table(num.events)

one.event <- function(len, dat, prob=((length(dat)+2):3),
                      replace=TRUE, sep="-") {
  event <- sample(dat, len, prob=prob, replace = replace)
  paste0(event, collapse=sep)
}

one.event(3, foods)
one.event(6, foods)

events <- sapply(num.events, one.event, dat=foods)
str(events)

library(car)
some(events)

events.freq <- data.frame(table(events))
head(events.freq)

library(sunburstR)
library(RColorBrewer)   # install if necessary
breakfastPalette <- brewer.pal(5, "Set1")
# breakfastPalette <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00")
sunburst(events.freq, colors = breakfastPalette)


### load data from R-book sequences
###
### in this section, we get the web log, sessionize it, and filter for
### HTML pages only
###
### For reference and more details, see R for Marketing Research and Analytics,
### 2nd ed., Chapter 14 (Chapman & Feit, 2019)
###

# get basic data
# this file starts after basic data processing noted in Chapman & Feit, Ch. 14
epa.df <- readRDS(gzcon(url("https://goo.gl/s5vjWz")))
head(epa.df)

# extract sessions
# 1. put DF in order of host and timestamp
epa.ordered <- epa.df[order(epa.df$host, epa.df$datetime), ]

# 2. get time differences between rows in minutes
epa.ordered$time.diff <-
  c(NA, as.numeric(
          epa.ordered$datetime[2:nrow(epa.ordered)] -
          epa.ordered$datetime[1:(nrow(epa.ordered)-1)],
        units="mins"))

# 3. determine new sessions, as being either:
# .. 1: host has changed since previous row
# .. 2: time difference exceeds session cutoff time of 15 minutes
session.time              <- 15   # exceed (mins) ==> new session
epa.ordered$newsession    <- NA   # is this row a new session?
epa.ordered$newsession[1] <- TRUE # row 1 is always a new session

epa.ordered$newsession[2:nrow(epa.ordered)]  <-
  ifelse(epa.ordered$host[2:nrow(epa.ordered)] !=
           epa.ordered$host[1:(nrow(epa.ordered)-1)], # hosts differ
         TRUE,                                        # => new session
         epa.ordered$time.diff[2:nrow(epa.ordered)] >=
           session.time )                 # else new if time exceeded

# 4. finalize session numbers & initial time differences
epa.ordered$session <- cumsum(epa.ordered$newsession)
epa.ordered$time.diff[epa.ordered$newsession] <- NA  # time NA for new

# 5. remove everything except HTML pages
epa.html <- epa.ordered[epa.ordered$pagetype=="html", ]

# 6. check a few to make sure they worked
epa.html[1:5, c(1, 13, 10)]

### sunburst processing
###
### In this section, we combine the session pages for sequential orders
### and count the occurrences for each order

# first, changes dashes in the page names to underscores (_),
# because "-" separates sequences in the sunburst data
epa.html$page <- gsub("-", "_", epa.html$page)

# now split epa.html$page into separate data frames for each unique session
epa.chunks <- split(epa.html$page, epa.html$session)
head(epa.chunks)

# assemble those pages into 1 sequence string for each chunk
# set maximum length to be 5 pages for tidier sunburst
epa.sequences <- data.frame(sequence=sapply(
      epa.chunks,
      function(x)
        paste0(x[1:min(length(x), 5)], collapse="-")))

epa.sequences[1:2, ]


# count the occurrences of each sequence
epa.sequences.freq <- data.frame(table(epa.sequences$sequence))
head(epa.sequences.freq, 3)
# how many have more than a single occurrence of the sequence?
table(epa.sequences.freq$Freq > 1)

### sunburst chart where frequency is greater than 1
library(sunburstR)
sunburst(data=subset(epa.sequences.freq, Freq > 1))

