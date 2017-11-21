library(tidyverse)

#################################################
#### pre-processing script for the SIBL data ####
# notes #
# key l = present 
# key r = absent
#################################################

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

#################################################
# first, read in raw data to df
#################################################

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
rm(d ,f, import_names, block_files,sine_files)

#################################################
# now fix a few minor quirks in the data
#################################################

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
df$difficulty[df$difficulty == 1.5] <- 120
df$difficulty[df$difficulty == 1.8] <- 100
df$difficulty[df$difficulty == 2.3] <- 78
df$difficulty[df$difficulty == 2.8] <- 64
df$difficulty[df$difficulty == 3.3] <- 54
df$difficulty[df$difficulty == 3.8] <- 47
df$difficulty[df$difficulty == 4.3] <- 41


# get information about correct judgements 
df$correct <- 0 
df$correct[df$key == "l" & df$targ_pr == 1] <- 1 
df$correct[df$key == "r" & df$targ_pr == 0] <- 1



#################################################################

# mark trials to be removed due to incorrect key press 
df$rt[df$key == "x"] <- NA


#### add in PRT #### 

# this didn't work unique(all_data[c("block","Block_Type")])
people <- seq(max(as.numeric(all_data$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- all_data[all_data$participant == Subject,]
  nblocks <- unique(all_data$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PRT <- shift(data_this_block$RT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PRT_bl <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PRT_bl <- rbind(all_data_with_PRT_bl,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)




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
	site, 
	group)

# save processed data file!


