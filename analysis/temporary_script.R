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
	participant <- sub(".*bl", "", filename)
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

# convert things to factors
df$participant <- as.numeric(df$participant)
df$targ_side <- as.factor(df$targ_side)
df$key <- as.factor(df$key)
df$message <- as.factor(df$message)
df$block_type <- as.factor(df$block_type)


# add site information
df$site <- "Aberdeen"
df$site[df$participant > 20] <- "Essex"

# add participant group info
df$group <- "SIBL"

# tidy
rm(block_files,sine_files)

#################################################################
# this is as far as AC has checked!



#### retrieve RABL data ####
blockFiles2 <- dir("data/BLRA/Block/")

randomFiles <- dir("data/BLRA/Random/")


df <- data.frame(participant=character(), trial=numeric(), block=numeric(),
                 Target_pr=numeric(), Targ_side=numeric(), Name=character(),
                 key=character(), RT=numeric(), Message=character(), Difficulty=numeric(),
                 Block_Type=character())

# make the blocked data set #
bldat2 <- df

for (f in blockFiles2)
{
  d = read.csv(paste("data/BLRA/Block/", f, sep=""), sep = "\t", header = T)
  names(d) = c("trial", "block", "Target_pr", "Target_side", "Name", "key", "RT", "Message", "Difficulty", "Block_Type")
  d$participant = f
  d$participant = sub(".*bl", "", d$participant)
  d$participant = sub(".dat*.", "", d$participant)
  bldat2 = rbind(bldat2, d)
}
rm(d)

# makes the random dataset #
radat <- df

for (f in randomFiles)
{
  d = read.csv(paste("data/BLRA/Random/", f, sep=""), sep = "\t", header = T)
  names(d) = c("trial", "block", "Target_pr", "Target_side", "Name", "key", "RT", "Message", "Difficulty", "Block_Type")
  d$participant = f
  d$participant = sub(".*ra", "", d$participant)
  d$participant = sub(".dat*.", "", d$participant)
  radat = rbind(radat, d)
}
rm(d)
rm(df)

radat$Difficulty = sub(".*v", "", radat$Name)
radat$Difficulty = sub(".jpg*.", "", radat$Difficulty)

RABL_data <- rbind(bldat2,radat)

RABL_data$Group <- "RABL"

# tidy up
rm(radat,bldat2,blockFiles2,randomFiles,f)

# prevent overlapping and add site info
RABL_data$participant <- as.numeric(RABL_data$participant)
RABL_data$site <- "Aberdeen"
RABL_data$site[RABL_data$participant > 20] <- "Essex"
RABL_data$participant <- RABL_data$participant + 20





#### combine all data into one set ####
all_data <- rbind(SIBL_data,RABL_data)

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




