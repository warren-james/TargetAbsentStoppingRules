library(tidyverse)

#### pre-processing script for the data ####
# notes #
# key l = present 
# key r = absent


# Shift function #

# WHAT IS THIS USED FOR?
shift <- function(x,n){
  c(rep(NA, n),x[seq(length(x)-n)])
}

extract_participant_id <- function(filename) {
	#participant <- sub(".*bl", "", filename)
  participant <- substring(filename, 3)
  participant <- sub(".dat*.", "", participant)
  return(participant)
}

extract_difficulty <- function(d) {
	d$difficulty <- sub(".*v", "",d$name)
	d$difficulty <- sub(".jpg*.", "", d$difficulty)
	d$difficulty <- as.numeric(d$difficulty)
	return(d)
}

get_degrees <- function(x){
	((pi/x)*180)/(pi)
}


#### first, read in raw data to df ####


# create empty data.frame for data
df <- tibble(
	participant = character(), 
	group = character(),
	trial = numeric(), 
	block = numeric(),
    targ_pr = numeric(), 
    targ_side = character(), 
    name = character(),
    key = character(), 
    rt = numeric(), 
    message = character(), 
    difficulty = numeric(),
    block_type = character())

import_names <- c(
	"trial", 
	"block", 
	"targ_pr", 
	"targ_side", 
	"name", 
	"key", 
	"rt", 
	"message", 
	"difficulty", 
	"block_type")

#### retrieve SIBL data #####
block_files <- dir("../data/BLSI/Block/")
sine_files  <- dir("../data/BLSI/Sine/")

#  read in files
for (f in block_files) {
  	d <- read.csv(
  		paste("../data/BLSI/Block/", f, sep=""), 
  		sep = "\t", header = T)

	names(d) <-import_names

	# extract participant id number from filename
	d$participant <- extract_participant_id(f)
	
	# assign group factor
	d$group = "SIBL"

	# add to main dataframe
	df = bind_rows(df, d)
}

for (f in sine_files) {
	d = read.csv(
  		paste("../data/BLSI/sine/", f, sep=""), 
  		sep = "\t", header = T)
	
	names(d) <-import_names
	
	# extract participant id number from filename
	d$participant <- extract_participant_id(f)

	# extract difficulty 
	d <- extract_difficulty(d)

	# assign group factor
	d$group = "SIBL"	

	# add to main dataframe
	df = bind_rows(df, d)
}


#### retrieve RABL data ####
block_files <- dir("../data/BLRA/Block/")
random_files <- dir("../data/BLRA/Random/")

for (f in block_files) {
	d <- read.csv(
    	paste("../data/BLRA/Block/", f, sep=""), 
    	sep = "\t", header = T)
  
	names(d) <-import_names
  
	# extract participant id number from filename
	d$participant <- extract_participant_id(f)
  
	# assign group factor
	d$group = "RABL"

	# add to main dataframe
	df = bind_rows(df, d)
}

for (f in random_files) {
	d = read.csv(
    	paste("../data/BLRA/Random/", f, sep=""), 
    	sep = "\t", header = T)
  
	names(d) <-import_names
  
	# extract participant id number from filename
	d$participant <- extract_participant_id(f)
	
	# extract difficulty 
	d <- extract_difficulty(d)
	
	# assign group factor
	d$group = "RABL"

	# add to main dataframe
	df = bind_rows(df, d)
}

# tidy up
rm(d ,f, import_names, block_files,sine_files,random_files,shift)


#### now fix a few minor quirks in the data ####


# add site information
df$site <- "Aberdeen"
df$site[df$participant > 20] <- "Essex"

# recode participant numbers to make them all unique
pID <- as.factor(paste(df$participant, df$group))
levels(pID) <- seq(1, length(levels(pID)))
df$participant <- pID
rm(pID)

# convert things to factors and remove white space from names
df$targ_side <- as.factor(df$targ_side)
levels(df$targ_side) <- c("left", "right", "absent")

df$key <- as.factor(df$key)
levels(df$key) <- c("l", "r", "x")

df$message <- as.factor(df$message)

df$block_type <- as.factor(df$block_type)
levels(df$block_type) <- c("blocked", "random", "sinewave")

df$group = as.factor(df$group)
df$site <- as.factor(df$site)

# change Difficulty levels to the actual degree of variance #
df$difficulty <- get_degrees(df$difficulty)

# get information about correct judgements 
df$correct <- 0 
df$correct[df$key == "l" & df$targ_pr == 1] <- 1 
df$correct[df$key == "r" & df$targ_pr == 0] <- 1

# mark trials to be removed due to incorrect key press 
df$rt[df$key == "x"] <- NA


#### extract previous rt and accuracy ####


df$p_rt = NA
df$p_correct = NA

for (g in levels(df$group)) {
  for  (person in unique(df$participant[df$group == g])){
    for (bt in unique(df$block_type[df$group == g])) {
      for (blk in unique(df$block)) {
        # extract subset for person:block
        d <- filter(df, participant == person, block == blk, block_type == bt, group == g)
        if (nrow(d) > 0) {
          # add in previous reaction time 
          d$p_rt[2:nrow(d)] = d$rt[1:(nrow(d)-1)]
          # add in previous correct
          d$p_correct[2:nrow(d)] = d$correct[1:(nrow(d)-1)]
            
          # add back into main dataframe
          df[which(df$participant == person & df$group == g & df$block_type == bt & df$block == blk),] <- d
            
          rm(d)
            
        } else {
          rm(d)
        }
      }
    }
  }
}




rm(person, blk, bt, g)


#### save processed data! ####


# keep needed columns 
df <- select(df, 
	participant, 
	block_type, 
	block, 
	trial, 
	targ_pr, 
	key, 
	difficulty, 
	correct, 
	rt, 
	p_rt,
	p_correct,
	site, 
	group)

# save processed data file!
save(df, file = "scratch/processed_data.rda")