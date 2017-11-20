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

#### retrieve SIBL data ####
block_files <- dir("../data/BLSI/Block/")
sine_files  <- dir("../data/BLSI/Sine/")

# create empty data.frame for data
df <- tibble(
	participant = character(), 
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


for (f in block_files)
{
  	d <- read.csv(
  		paste("../data/BLSI/Block/", f, sep=""), 
  		sep = "\t", header = T)

	names(d) <-import_names

	# extract participant id number from filename
	d$participant <- extract_participant_id(f)
	
	# add to main dataframe
	df = bind_rows(df, d)
}


for (f in sine_files)
{
	d = read.csv(
  		paste("../data/BLSI/sine/", f, sep=""), 
  		sep = "\t", header = T)
	
	names(d) <-import_names
	
	# extract participant id number from filename
	d$participant <- extract_participant_id(f)
	# extract difficulty 
	d <- extract_difficulty(d)
	# add to main dataframe
	df = bind_rows(df, d)
}

# tidy up
rm(d ,f, import_names)

# add site information
df$site <- "Aberdeen"
df$site[df$participant > 20] <- "Essex"


# convert things to factors
df$participant <- as.factor(df$participant)
df$targ_side <- as.factor(df$targ_side)
df$key <- as.factor(df$key)
df$message <- as.factor(df$message)
df$block_type <- as.factor(df$block_type)


# add participant group info
df$group <- "SIBL"

# tidy
rm(block_files,sine_files)

#################################################################
#### this is as far as AC has checked! ####



#### retrieve RABL data ####
block_files <- dir("../data/BLRA/Block/")

random_files <- dir("../data/BLRA/Random/")


df2 <- tibble(
  participant = character(), 
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

# make the blocked data set #

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


for (f in block_files)
{
  d <- read.csv(
    paste("../data/BLRA/Block/", f, sep=""), 
    sep = "\t", header = T)
  
  names(d) <-import_names
  
  # extract participant id number from filename
  d$participant <- extract_participant_id(f)
  
  # add to main dataframe
  df2 = bind_rows(df2, d)
}


for (f in random_files)
{
  d = read.csv(
    paste("../data/BLRA/Random/", f, sep=""), 
    sep = "\t", header = T)
  
  names(d) <-import_names
  
  # extract participant id number from filename
  d$participant <- extract_participant_id(f)
  # extract difficulty 
  d <- extract_difficulty(d)
  # add to main dataframe
  df2 = bind_rows(df2, d)
}

# tidy up
rm(d ,f, import_names)

# add site information
df2$site <- "Aberdeen"
df2$site[df2$participant > 20] <- "Essex"

# stop participant numbers overlapping 
df2$participant <- as.numeric(df2$participant) + 20

# convert things to factors
df2$participant <- as.factor(df2$participant)
df2$targ_side <- as.factor(df2$targ_side)
df2$key <- as.factor(df2$key)
df2$message <- as.factor(df2$message)
df2$block_type <- as.factor(df2$block_type)


# add participant group info
df2$group <- "RABL"

# merge datasets 
df <- rbind(df,df2)

#tidy up again
rm(block_files,random_files, df2)

#####################################################################################
#### checked until here WJ ####
# pretty sure everything works above here, below may be a different story...

# get informtation about correct judgements 
all_data$correctT <- 0 
all_data$correctT[all_data$key == " l" & all_data$Target_pr == 1] <- 1 
all_data$correctT[all_data$key == " r" & all_data$Target_pr == 0] <- 1

# change Difficulty levels to the actual degree of variance #
all_data$Difficulty[all_data$Difficulty == 1.5] <- 120
all_data$Difficulty[all_data$Difficulty == 1.8] <- 100
all_data$Difficulty[all_data$Difficulty == 2.3] <- 78
all_data$Difficulty[all_data$Difficulty == 2.8] <- 64
all_data$Difficulty[all_data$Difficulty == 3.3] <- 54
all_data$Difficulty[all_data$Difficulty == 3.8] <- 47
all_data$Difficulty[all_data$Difficulty == 4.3] <- 41

# keep needed columns 
all_data <- select(all_data, "participant", "Block_Type", "block", "trial", "Target_pr", "key", "Difficulty", "correctT", "RT", "site", "Group")

# mark trials to be removed due to incorrect key press 
all_data$RT[all_data$key == " x"] <- NA

# sort out participants so they're numbered correctly 
temp <- all_data
temp$participant <- as.factor(temp$participant)

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





