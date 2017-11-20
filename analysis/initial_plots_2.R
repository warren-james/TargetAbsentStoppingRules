library(reshape2)
library(tidyverse)

load("both_data_sets.RData")

#### sort out names of datasets ####
data_RABL <- RABL_data
data_SIBL <- SIBL_dat

rm(RABL_data, SIBL_dat)

#### sort out the factors #### 
# Target presence
data_RABL$Target_pr <- as.factor(data_RABL$Target_pr)

data_SIBL$Target_pr <- as.factor(data_SIBL$Target_pr)

# factorise Difficulty and drop 1.3 
data_SIBL <- data_SIBL[data_SIBL$Difficulty >2,]

data_RABL$Difficulty <- as.factor(data_RABL$Difficulty)

data_SIBL$Difficulty <- as.factor(data_SIBL$Difficulty)



# Participants
data_RABL$participant <- as.factor(data_RABL$participant)

#### plots ####

#### try some density plots ####
plot_RABLdens = ggplot(data_RABL, aes(RT, colour = Target_pr, fill = Target_pr)) + geom_density(alpha = 0.2)
plot_RABLdens = plot_RABLdens + facet_wrap(~Block_Type)

plot_SIBLdens = ggplot(data_SIBL, aes(RT, colour = Target_pr, fill = Target_pr)) + geom_density(alpha = 0.2)
plot_SIBLdens = plot_SIBLdens + facet_wrap(~Block_Type)

### RT over Difficulty and Target_pr ####
# set up the groups 
data_RABL$Group <- "RABL"
data_SIBL$Group <- "SIBL"
 
# group the data 
data_RABLSIBL <- rbind(data_RABL, data_SIBL)
data_RABLSIBL$Difficulty <- as.factor(data_RABLSIBL$Difficulty)


# get means using tidyverse version of ddply 
temp <- group_by(data_RABLSIBL, Group, Difficulty, Target_pr, Block_Type)
data_MeanRT <- summarise(temp, meanRT = mean(RT, na.rm=T),
                         sdev = sd(RT),
                         N = length(RT),
                         se = sdev/sqrt(N),
                         upper = meanRT + se,
                         lower = meanRT - se)

# tidy 
rm(temp)

# make some plots
plot_DiffBYTP = ggplot(data_MeanRT, aes(Difficulty, meanRT, colour = Block_Type))
plot_DiffBYTP = plot_DiffBYTP + geom_point()
plot_DiffBYTP = plot_DiffBYTP + geom_errorbar(aes(ymax = upper, ymin = lower))
plot_DiffBYTP = plot_DiffBYTP + facet_grid(Target_pr~Group)










