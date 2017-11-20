library(tidyverse)

#### pre-processing script for the RABL data ####

# notes #
# key l = present 
# key r = absent

# Shift function #
shift <- function(x,n){
  c(rep(NA, n),x[seq(length(x)-n)])
}

# set up resultsFiles # 
blockFiles <- dir("data/BLRA/Block/")

randomFiles <- dir("data/BLRA/Random/")


df <- data.frame(participant=character(), trial=numeric(), block=numeric(),
                 Target_pr=numeric(), Targ_side=numeric(), Name=character(),
                 key=character(), RT=numeric(), Message=character(), Difficulty=numeric(),
                 Block_Type=character())

# make the blocked data set #
bldat <- df

for (f in blockFiles)
{
  d = read.csv(paste("data/BLRA/Block/", f, sep=""), sep = "\t", header = T)
  names(d) = c("trial", "block", "Target_pr", "Target_side", "Name", "key", "RT", "Message", "Difficulty", "Block_Type")
  d$participant = f
  d$participant = sub(".*bl", "", d$participant)
  d$participant = sub(".dat*.", "", d$participant)
  bldat = rbind(bldat, d)
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

# bind all the data together #
alldat <- rbind(radat,bldat)

# change Difficulty levels to the actual degree of variance #
alldat$Difficulty[alldat$Difficulty == 1.5] <- 120
alldat$Difficulty[alldat$Difficulty == 1.8] <- 100
alldat$Difficulty[alldat$Difficulty == 2.3] <- 78
alldat$Difficulty[alldat$Difficulty == 2.8] <- 64
alldat$Difficulty[alldat$Difficulty == 3.3] <- 54
alldat$Difficulty[alldat$Difficulty == 3.8] <- 47
alldat$Difficulty[alldat$Difficulty == 4.3] <- 41

# keep only the needed columns #
alldat = alldat[,c(11,10,2,1,3,6,8,9,7)]

# add something for correct trials #
alldat$correctT <- 0 
alldat$correctT[alldat$key == " l" & alldat$Target_pr == 1] <- 1 
alldat$correctT[alldat$key == " r" & alldat$Target_pr == 0] <- 1

alldat = alldat[,c(1,2,3,4,5,6,7,8,10,9)]

# remove wrong key presses
alldat$RT[alldat$key == " x"] <- NA

# set up separate data files so the loops below can recognise everything correctly #
bldat2 = alldat[alldat$Block_Type == " bl",]
radat2 = alldat[alldat$Block_Type == " ra",]

# ADD IN PRT #
# add in PRT for blocked #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- bldat2[bldat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
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

# for random #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- radat2[radat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PRT <- shift(data_this_block$RT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PRT_ra <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PRT_ra <- rbind(all_data_with_PRT_ra,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

all_data_with_PRT <- rbind(all_data_with_PRT_bl, all_data_with_PRT_ra)

rm(all_data_with_PRT_bl)
rm(all_data_with_PRT_ra)





# GET PCT #
# Get previous correctT for blocked #
people <- seq(max(as.numeric(alldat$participant)))
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- bldat2[bldat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PCT <- shift(data_this_block$correctT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PCT_bl <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PCT_bl <- rbind(all_data_with_PCT_bl,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

# Get previous correctT for random #
people <- seq(max(as.numeric(alldat$participant)))
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- radat2[radat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PCT <- shift(data_this_block$correctT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PCT_ra <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PCT_ra <- rbind(all_data_with_PCT_ra,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

# bind the data #
all_data_with_PCT <- rbind(all_data_with_PCT_bl, all_data_with_PCT_ra)

rm(all_data_with_PCT_bl)
rm(all_data_with_PCT_ra)

# make column to add onto alldata stuff # 
PCTdat <- all_data_with_PCT[,c(11)]

all_data <- cbind(all_data_with_PRT, PCTdat)
rm(all_data_with_PCT,all_data_with_PRT)

# TA or TP response  
all_data$TATP <- 0
all_data$TATP[all_data$key == " r"] <- "TA"
all_data$TATP[all_data$key == " l"] <- "TP"

# set Diff to a numeric value 
all_data$Difficulty <- as.numeric(all_data$Difficulty)

RABL_data <- all_data

RABL_data$participant <- as.numeric(RABL_data$participant)

#add which site it came from 
RABL_data$site <- "Aberdeen"
RABL_data$site[RABL_data$participant > 20] <- "Essex"

RABL_data$Group <- "RABL"

# clear everything but the data I want to keep
rm(list=ls()[!(ls() %in% c("RABL_data", "SIBL_data"))])


