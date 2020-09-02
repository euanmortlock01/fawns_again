
##########################################################################################################################################################
# SECTION 1
# load in the data; download and extract raw data from google drive, set wd
# then run everthing until ##   END   ##

setwd("C:/Folder CONTAINING RAW FILES")
C9M = read.csv("C9_min.csv")
C30M = read.csv ("C30_min.csv")
C40M = read.csv ("C40_min.csv")
C8M = read.csv ("C8_min.csv")
#  not include C13 as data is off C13M = read.csv ("C13_min.csv")
C25M = read.csv ("C25_min.csv")
C57M = read.csv ("C57_min.csv")
# not include for now as very short C59M = read.csv ("C59_min.csv")
C65M = read.csv ("C65_min.csv")
# not include for now as very short C69M = read.csv ("C69_min.csv")
C74M = read.csv ("C74_min.csv")
C76M = read.csv ("C76_min.csv")
C9M$Age = C9M$days_after_capture + 6
C30M$Age = C30M$days_after_capture + 5
C40M$Age = C40M$days_after_capture + 3
C8M$Age = C8M$days_after_capture + 6
# C13M$Age = C13M$days_after_capture + 4
C25M$Age = C25M$days_after_capture + 4
C57M$Age = C57M$days_after_capture + 2
# C59M$Age = C59M$days_after_capture + 4
C65M$Age = C65M$days_after_capture + 2
# C69M$Age = C69M$days_after_capture + 6
C74M$Age = C74M$days_after_capture + 7
C76M$Age = C76M$days_after_capture + 3
all_data <- rbind(C9M, C30M, C40M, C8M, C25M, C57M, C65M,C74M, C76M)
##########################################################################################################################################################

# SECTION 2
# making the dataframes we want

# create binomial activity data, 0 is inactive, 1 is active
inactive <- subset(all_data, vedba <= 0.05) 
active <- subset(all_data, vedba >= 0.05)
inactive$state <- NA
inactive$state <- "0"
active$state <- NA
active$state <- "1"
all_data_1 <- rbind(inactive, active)
all_data_1$state <- as.numeric(all_data_1$state)
all_data_1$Tag <- as.factor(all_data_1$Tag)

# add column for periods of day
all_data_1$period <- "NA"

all_data_1$period <- ifelse(all_data_1$time_hour >= 4 & all_data_1$time_hour <= 6, "dawn",
                            ifelse(all_data_1$time_hour >= 21 & all_data_1$time_hour <= 23, "dusk",
                                   ifelse(all_data_1$time_hour > 6 & all_data_1$time_hour < 21, "day",
                                          ifelse(all_data_1$time_hour > 23 & all_data_1$time_hour < 4, "night", "night"))))
all_data_1$period <- as.factor(all_data_1$period)


# then if we sum tthe active minutes as below, we can
# calculate the number of active minutes per day per individual

active_minutes <- aggregate(all_data_1$state, by = list(all_data_1$Age, all_data_1$Tag), FUN = sum)
names(active_minutes) [1] <- "Age"
names(active_minutes) [2] <- "Tag"
names(active_minutes) [3] <- "active_mins"
active_minutes$Tag <- as.factor(active_minutes$Tag)


# calculate crepuscularity index

# create a new df with the mean vedba for the period, for each age day
crep_data_1 <- with(all_data_1,
            aggregate(vedba, by = list(Tag, period, Age), FUN = mean)
)
names(crep_data_1) <- c("Tag", "period", "Age", "mean_vedba")
# then draw out the dawn and dusk data seperately
dawn <- subset(crep_data_1, period == "dawn")
names(dawn)[4] <- "dawn_vedba"
dusk <- subset(crep_data_1, period == "dusk")
names(dusk)[4] <- "dusk_vedba"
# then we create a new df that has a column for one value, 
# the sum of the mean vedba for all periods of the day.
crep_data_2 <- with(crep_data_1,
            aggregate(`mean_vedba`, by = list(`Age`, `Tag`), FUN = sum)
)
names(crep_data_2) <- c("Age", "Tag", "day_vedba")
# then merge this new one with the seperate dawn and dusk frames
# so we can do our calculation and create the index.
dawncrep <- merge(crep_data_2, dawn, by = c("Tag", "Age"))
duskcrep <- merge(crep_data_2, dusk, by = c("Tag", "Age"))
dawncrep$crepindexdawn <- (dawncrep$`dawn_vedba`/dawncrep$`day_vedba`)
duskcrep$crepindexdusk <- (duskcrep$`dusk_vedba`/duskcrep$`day_vedba`)
# now we have the index in each seperate frame, we can put them both together again
names(dawncrep)[6] <- "crepindex"
names(duskcrep)[6] <- "crepindex"
dawncrep <- dawncrep[ -c(3,5) ]
duskcrep <- duskcrep[ -c(3,5) ]
crep_data_3 <- rbind(dawncrep, duskcrep)

##    END   ##

##########################################################################################################################################################

# SECTION 3
# plots, assorted

library(ggplot2)
library(ggridges)
library(mgcv)
library(gratia)
library(gamm4)

# relative mean vedba
ggplot(crep_data_1, aes(Age ,`mean_vedba`, fill = period, colour = period)) + 
  geom_smooth(method = "glm", se = FALSE) +
  geom_point()


# calculated crepuscularity index
ggplot(crep_data_3, aes(Age ,`crepindex`)) + 
  geom_smooth(method = "glm", se = FALSE) +
  geom_point()


# all individuals rhythm with vedba values, with GAMM
ggplot(all_data_1, aes(time, Tag, fill = Tag, height = vedba, colour = Tag)) + 
  geom_ridgeline() +
  facet_wrap(~Age)

ggplot(all_data_1, aes(time, Tag, fill = Tag, colour = Tag)) + 
  geom_density_ridges() +
  facet_wrap(~Age)

gam1 <- bam(state ~ s(time, bs = "cc") + s(Tag, time, bs = "re") + Age,
              family ="binomial", data = all_data_1)
summary(gam1)
gratia::draw(gam1)



# active minutes per day with age
ggplot(active_minutes, aes(x = `Age`, y = `active_mins`, fill = Tag, colour = Tag)) +
  geom_point() + 
  geom_smooth(method = "glm",se = FALSE) +
  labs(y = "Active mins in ageday (vedba > .05)")


# cyclical plot with raw VeDBA
ggplot(all_data_1, aes(time_hour, Tag, fill = Tag, height = vedba, colour = Tag)) + 
  geom_ridgeline() +
  facet_wrap(~Age)








