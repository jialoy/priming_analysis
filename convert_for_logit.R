#!/usr/bin/env Rscript

## script to convert Stevie's data (coded) into format suitable for logit analysis
## expands each trial into X rows based on number of opportunities it has been coded for
## populates variables isDropped, isPronoun, isProper for each trial's rows appropriately
## based on coding of droppedNo, pronounNo, properNo respectively

library(tidyverse)
library(magrittr)

args = commandArgs(T)
inFile = args[1]
outFile = args[2]
data.ps.dir = read.csv(file=inFile)

data.ps.dir2 <- data.ps.dir %>%
  filter(!opp==0) %>%
  tidyr::uncount(opp) %>%
  mutate(isValid=1)

data.ps.dir3 <- data.ps.dir %>%
  filter(!opp==0)

## create a vector of 0s and 1s where 1s represent pronoun use on each trial and 0s represent other
data.ps.dir3$pro_ones = NULL
for (i in seq_len(nrow(data.ps.dir3))) {
  data.ps.dir3$pro_ones[i] = toString( rep ( c(0,1), times=c(data.ps.dir3$opp[i]-data.ps.dir3$pronounNo[i], data.ps.dir3$pronounNo[i]) ) )
}
pro_ones <- paste(data.ps.dir3$pro_ones, collapse= " ") %>%
  gsub(',', '', .) %>%
  gsub(' ', ',', .) %>%
  strsplit(., split=',') %>%
  unlist(.) 

data.ps.dir2$isPronoun = as.numeric(pro_ones)

## repeat for full NP use
data.ps.dir3$pp_ones = NULL
for (i in seq_len(nrow(data.ps.dir3))) {
  data.ps.dir3$pp_ones[i] = toString( rep ( c(1,0), times=c(data.ps.dir3$properNo[i], data.ps.dir3$opp[i]-data.ps.dir3$properNo[i]) ) )
}
pp_ones <- paste(data.ps.dir3$pp_ones, collapse= " ") %>%
  gsub(',', '', .) %>%
  gsub(' ', ',', .) %>%
  strsplit(., split=',') %>%
  unlist(.) 

data.ps.dir2$isProper = as.numeric(pp_ones)

## now create column representing dropped nouns based on cases that aren't pronoun or full NP
data.ps.dir2$isDropped = NULL
for (i in seq_len(nrow(data.ps.dir2))) {
  pro = data.ps.dir2$isPronoun
  pp = data.ps.dir2$isProper
  if (pro[i]==1 | pp[i]==1) {
    data.ps.dir2$isDropped[i] = 0
  } else {
    data.ps.dir2$isDropped[i] = 1
  }
}


data.ps.dir2$pro_ones = NULL
data.ps.dir2$pp_ones = NULL

write.csv(data.ps.dir2, file=sprintf('~/Desktop/priming/stevie/data/%s', outFile))
# data.ps.dir2 = data.ps.dir2[!data.ps.dir2$descImg=='P_desc_29',]

# data.ps.dir$droppedToAdd = rep(0, nrow(data.ps.dir))
# data.ps.dir$ppToAdd = data.ps.dir$isProper * 2
# data.ps.dir$nounType = data.ps.dir$ppToAdd + data.ps.dir$droppedToAdd + data.ps.dir$isPronoun