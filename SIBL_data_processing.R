library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
#library(lmerTest) # gets p-values for lme's
library(dplyr)
#library(ez)
library(e1071)

#### generate SIBL data ####

# notes #
# key l = present 
# key r = absent

# Shift function #
shift <- function(x,n){
  c(rep(NA, n),x[seq(length(x)-n)])
}

# set up resultsFiles # 
blockFiles <- dir("data/BLSI/Block/")

sineFiles <- dir("data/BLSI/Sine/")


df <- data.frame(participant=character(), trial=numeric(), block=numeric(),
                 Target_pr=numeric(), Targ_side=numeric(), Name=character(),
                 key=character(), RT=numeric(), Message=character(), Difficulty=numeric(),
                 Block_Type=character())

# make the blocked data set #
bldat <- df

for (f in blockFiles)
{
  d = read.csv(paste("data/BLSI/Block/", f, sep=""), sep = "\t", header = T)
  names(d) = c("trial", "block", "Target_pr", "Target_side", "Name", "key", "RT", "Message", "Difficulty", "Block_Type")
  d$participant = f
  d$participant = sub(".*bl", "", d$participant)
  d$participant = sub(".dat*.", "", d$participant)
  bldat = rbind(bldat, d)
}
rm(d)

# makes the sine dataset #
sidat <- df

for (f in sineFiles)
{
  d = read.csv(paste("data/BLSI/sine/", f, sep=""), sep = "\t", header = T)
  names(d) = c("trial", "block", "Target_pr", "Target_side", "Name", "key", "RT", "Message", "Difficulty", "Block_Type")
  d$participant = f
  d$participant = sub(".*si", "", d$participant)
  d$participant = sub(".dat*.", "", d$participant)
  sidat = rbind(sidat, d)
}
rm(d)
rm(df)

sidat$Difficulty = sub(".*v", "", sidat$Name)
sidat$Difficulty = sub(".jpg*.", "", sidat$Difficulty)

# bind all the data together #
alldat <- rbind(sidat,bldat)

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


# removes outliers dependant on their sd within each condition
#condsd <- ddply(alldat, c("participant", "Difficulty", "Target_pr"), summarise, 
#                mm = mean(RT),
#                sdev = sd(RT))

#alldat = merge(alldat, condsd, by = c("participant", "Difficulty", "Target_pr"))

# bad trials should be handled here; make RT = NA #
alldat$RT[alldat$key == " x"] <- NA
#alldat$exlcude <- (alldat$mm + 4*alldat$sdev) # was there a reason this was commented out?
#alldat$RT[alldat$RT > alldat$exlcude] <- NA # removes data 4 sd's above the mean
#alldat$RT[alldat$RT > (mean(alldat$RT) + 4*sd(alldat$RT))] <- NA

alldat = alldat[,c(2,1,3,4,5,6,7,8,9,10)]

alldat = alldat[order(alldat$participant, alldat$Block_Type, alldat$block, alldat$trial),]

# set up separate data files so the loops below can recognise everything correctly #
bldat2 = alldat[alldat$Block_Type == " bl",]
sidat2 = alldat[alldat$Block_Type == " si",]

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

# for sine #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- sidat2[sidat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PRT <- shift(data_this_block$RT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PRT_si <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PRT_si <- rbind(all_data_with_PRT_si,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

all_data_with_PRT <- rbind(all_data_with_PRT_bl, all_data_with_PRT_si)

rm(all_data_with_PRT_bl)
rm(all_data_with_PRT_si)





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

# Get previous correctT for sine #
people <- seq(max(as.numeric(alldat$participant)))
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- sidat2[sidat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$PCT <- shift(data_this_block$correctT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_PCT_si <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_PCT_si <- rbind(all_data_with_PCT_si,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

# bind the data #
all_data_with_PCT <- rbind(all_data_with_PCT_bl, all_data_with_PCT_si)

rm(all_data_with_PCT_bl)
rm(all_data_with_PCT_si)

# make column to add onto alldata stuff # 
PCTdat <- all_data_with_PCT[,c(11)]

all_data <- cbind(all_data_with_PRT, PCTdat)



# Get previous TP #
# add in previous presence or absence for blocked #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- bldat2[bldat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$Ptargpres <- shift(data_this_block$Target_pr,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_Ptargpres_bl <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_Ptargpres_bl <- rbind(all_data_with_Ptargpres_bl,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)


# add in previous presence or absence for sine #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- sidat2[sidat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$Ptargpres <- shift(data_this_block$Target_pr,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_Ptargpres_si <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_Ptargpres_si <- rbind(all_data_with_Ptargpres_si,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)


# combine the datasets #
all_data_with_PTP <- rbind(all_data_with_Ptargpres_bl, all_data_with_Ptargpres_si)
rm(all_data_with_Ptargpres_bl, all_data_with_Ptargpres_si)

PTPdat = all_data_with_PTP[,c(11)]

all_data = cbind(all_data, PTPdat)


# Same, harder, easier? #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- bldat2[bldat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$Prediff <- shift(data_this_block$Difficulty,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_Prediff_bl <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_Prediff_bl <- rbind(all_data_with_Prediff_bl,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

# add in previous difficulty for sine #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- sidat2[sidat2$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #sepasites each block
    
    data_this_block$Prediff <- shift(data_this_block$Difficulty,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_Prediff_si <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_Prediff_si <- rbind(all_data_with_Prediff_si,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

all_data_with_Prediff <- rbind(all_data_with_Prediff_bl, all_data_with_Prediff_si)

# make column to add onto alldata stuff # 
Prediff <- all_data_with_Prediff[,c(11)]

all_data <- cbind(all_data, Prediff)


rm(all_data_with_Prediff_bl,all_data_with_Prediff_si)


# get the z scores # 
zdat <- ddply(alldat[alldat$correctT == 1,], c("participant", "Target_pr", "Difficulty", "Block_Type"), summarise, 
              mm = mean(RT), 
              sdev = sd(RT))

# z score calculation: (x - mean)/sd 
alldat <- merge(alldat, zdat, by = c("participant", "Target_pr", "Difficulty", "Block_Type"))

alldat$z_RT <- (alldat$RT - alldat$mm)/alldat$sdev

zbldat <- alldat[alldat$Block_Type == " bl",]
zsidat <- alldat[alldat$Block_Type == " si",]



# ADD IN PREVIOUS Z-SCORE #
# for blocked
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- zbldat[zbldat$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$zprev <- shift(data_this_block$z_RT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_zprev_bl <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_zprev_bl <- rbind(all_data_with_zprev_bl,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

# add in previous z-score for sine #
people <- seq(max(as.numeric(alldat$participant))) #This will need to be changed to the total number of participants...
count = 0 
n_to_shift <- 1

for(Subject in people){
  
  data_this_sub <- zsidat[zsidat$participant == Subject,]
  nblocks <- unique(data_this_sub$block)
  for (block_no in nblocks){
    
    count = count + 1 #just so we can ID the first time we run this - must be a better way than this
    
    data_this_block <- data_this_sub[data_this_sub$block == block_no,] #separates each block
    
    data_this_block$zprev <- shift(data_this_block$z_RT,n_to_shift)
    
    data_this_block <- data_this_block[-seq(n_to_shift),] #removes first n_to_shift trials in block
    
    
    
    if (count == 1)
    {
      all_data_with_zprev_si <- data_this_block #same as before, makes new data set
    }
    if (count > 1)
    {
      all_data_with_zprev_si <- rbind(all_data_with_zprev_si,data_this_block)
    }
    
  }
}
rm(data_this_block)
rm(data_this_sub)

all_data_with_zprev <- rbind(all_data_with_zprev_bl, all_data_with_zprev_si)

rm(all_data_with_zprev_bl,all_data_with_zprev_si)

# make column to add onto alldata stuff # 
PZdat <- all_data_with_zprev[,c(13,14)]

all_data <- cbind(all_data, PZdat)



# tidy the data 
all_data = all_data[complete.cases(all_data),]
alldat = alldat[complete.cases(alldat),]


rm(all_data_with_PCT,all_data_with_PRT,all_data_with_PTP)

# TA or TP response  
all_data$TATP <- 0
all_data$TATP[all_data$key == " r"] <- "TA"
all_data$TATP[all_data$key == " l"] <- "TP"

all_data = all_data[,c(2,4,5,6,1,3,9,12,10,11,13,14,15,16,17),]

# set Diff to a numeric value 
all_data$Difficulty <- as.numeric(all_data$Difficulty)

# need this for some reason, to convert a factor into a number otherwise R just gives the levels back
#as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#all_data$Prediff <- as.numeric.factor(all_data$Prediff)
all_data$Difficulty <- as.numeric(all_data$Difficulty)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

all_data$Prediff <- as.numeric.factor(all_data$Prediff)

# Change it so they're in categories
all_data$Prediff2[all_data$Prediff < all_data$Difficulty] <- "easier"
all_data$Prediff2[all_data$Prediff > all_data$Difficulty] <- "harder"
all_data$Prediff2[all_data$Prediff == all_data$Difficulty] <- "same"

all_data$Prediff <- all_data$Prediff2
all_data = all_data[,c(1:15)]


#### clean up ####
rm(sineFiles, blockFiles, zsidat, zdat, zbldat, sidat, sidat2, PZdat, condsd, bldat2, bldat, all_data_with_zprev, all_data_with_Prediff,f,block_no, nblocks, n_to_shift, Subject, Prediff, PTPdat, people, PCTdat,count)


SIBL_dat <- all_data
SI_alldat <- alldat

rm(alldat, all_data)
#enables combination of this data set with RABL dataset without overwriting participants
SIBL_dat$participant <- as.numeric(SIBL_dat$participant)
SIBL_dat$participant <- SIBL_dat$participant + 20

SIBL_dat$participant <- as.factor(SIBL_dat$participant)

  
