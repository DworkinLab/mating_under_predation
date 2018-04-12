#Courtship and Copulation changes under predation threat

## Set up files and packages needed
#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)
library(ggplot2)
# Bring in the data for mature females (copulation) and Immature females (courtship)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)



##Courtship Analysis


# Create unique Fly_ID for each individual with Box (treatment), date, replicate, and vial Number
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID

# Create Delta BP
courtship$deltaBP <- (courtship$BP.12.00.am - courtship$BP.8.00.Am)


# Change time (in HH:MM:SS) format to seconds (One had a value of -1, not sure, but changed to 0)
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)
courtship$startTimeSeconds[1339]
courtship$startTimeSeconds[1339] = 0


# Create new column of relative values for courtship start times (i.e so Observation all start at Time = 0)
courtship$relativeStartTimeSeconds <- (courtship$startTimeSeconds - courtship$Observation.Initiation)
courtship$relativeStartTimeSeconds

# New column for relative values of trial duration at end of behaviour
courtship$relativeTrial_latency_end <- (courtship$relativeStartTimeSeconds + courtship$court_duration)
courtship$relativeTrial_latency_end

# Need to get all courtship under 900 seconds (relative court duration)
#First, transition step for finding values ending abov 900
courtship$nineHundredTransition <- (900 - courtship$relativeTrial_latency_end)

# Second, if value for nineHundredTransition is negative, equate it to 900, all else stay as relativeTrial_latency_end
courtship$relativeTrialLatency900 <- ifelse(courtship$nineHundredTransition<0,900,courtship$relativeTrial_latency_end)

# Third, relative courtship duration (not including values over 900)
courtship$relativeCourtDuration <- (courtship$relativeTrialLatency900 - courtship$relativeStartTimeSeconds)

startLess900 <- subset(courtship, relativeCourtDuration>0)
summary(startLess900)

# Create a data frame for the predictor variables and for Fly_ID
pred_var_dat <- subset(courtship, select = c(Box, Date, Replicate, Vial_number, Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room,
                                             Observation.Initiation, Fly_ID, deltaBP))

# Make data frame only include unique values (i.e. remove duplicate rows)
pred_var_dat <- unique(pred_var_dat) 

#Rename Box to treatment type, and make characters to factor (to run later for plot(effect()))

pred_var_dat$Box <- ifelse(pred_var_dat$Box=="A", "Predator", "Control")

pred_var_dat$Box

pred_var_dat$Box <- factor(pred_var_dat$Box)

# Create a group by Fly_Id
FlyID <- group_by(startLess900,Fly_ID)
head(FlyID)

courtSum <- summarise(FlyID, sum = sum(relativeCourtDuration), count = n())
head(courtSum)
courtSum


courtship_for_analysis <- merge(x = pred_var_dat, y = courtSum, by.x="Fly_ID", by.y="Fly_ID")

with(courtship_for_analysis,
     boxplot(sum ~ Box))

with(courtship_for_analysis,
     boxplot(sum ~ Date))


### A simple version of the analysis
#Scale tempature, humidity and deltaBP
courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_for_analysis$HumCent <- scale(courtship_for_analysis$Humidity, scale = F)
courtship_for_analysis$BPCent <- scale(courtship_for_analysis$deltaBP, scale = F)

# Model for sum of time courting in 900 seconds
    #redone below for proportions of time spent courting
#courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

#Change time courting to proportion in 900 seconds
courtship_for_analysis$court_prop <- courtship_for_analysis$sum/900
courtship_model1 <- lmer(court_prop ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)
summary(courtship_model1)




# Model for number of courtship bouts in the 900 second observation window
courtship_model2 <- lmer(count ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

summary(courtship_model2)

plot(allEffects(courtship_model1))

plot(allEffects(courtship_model2))

plot(effect("Box", courtship_model1), main = "Male Time Courting of Immature Female in 900 Seconds",
     ylab = "Proportion of Time courting (sec)", xlab = "Treatment")

plot(effect("Box", courtship_model2), main = "Number of Male Courtship Bouts to Immature Female in 900 Seconds",
     ylab = "Courtship Bouts", xlab = "Treatment")


#### Copulation Analysis


#Similar to above for copulation now
summary(copulation)
dim(copulation)
head(copulation)
copulation$deltaBP <- (copulation$BP.12.00.am - copulation$BP.8.00.Am)
cop_data <- subset(copulation, select = c(Box, Date, Replicate, Vial.., Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room, Fly_ID, deltaBP))
cop_data <- unique(cop_data)
head(cop_data)

cop_data$Box <- ifelse(cop_data$Box=="C", "Predator", "Control")
cop_data$Box <- factor(cop_data$Box)
LatCop <- distinct(select(copulation, Cop_latency, Cop_Duration, Fly_ID))
head(LatCop)

copul_for_analysis <- merge(x = cop_data, y = LatCop, by.x="Fly_ID", by.y="Fly_ID")

copul_for_analysis$TempCent <- scale(copul_for_analysis$Temp, scale=F)
copul_for_analysis$HumCent <- scale(copul_for_analysis$Humidity, scale = F)
copul_for_analysis$BPCent <- scale(copul_for_analysis$deltaBP, scale = F)


### Simple linear models

copul_model1 <- lmer(Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)
summary(copul_model1)

copul_model2 <- lmer(Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)
summary(copul_model2)

with(copul_for_analysis, boxplot(Cop_latency ~ Box))

with(copul_for_analysis, boxplot(Cop_Duration ~ Box))

plot(allEffects(copul_model1))

plot(allEffects(copul_model2))

plot(effect("Box", copul_model1), main = "Mature Female Copulation Latency Rates with/without predator",
     ylab = "Copulation Latency (Sec)", xlab = "Treatment")

plot(effect("Box", copul_model2), main = "Mature Female Copulation Duration Rates with/without predator",
     ylab = "Copulation Duration (Sec)", xlab="Treatment")

#Removing all values with no copulation

LatCop2 <- distinct(select(copulation, Cop_latency, Cop_Duration, Fly_ID))
head(LatCop2)
LatCop2 <- LatCop2[!(LatCop2$Cop_latency==0),]
#LatCop2
#dim(LatCop2)
copul_for_analysis2 <- merge(x = cop_data, y = LatCop2, by.x="Fly_ID", by.y="Fly_ID")
copul_for_analysis2$TempCent <- scale(copul_for_analysis2$Temp, scale=F)
copul_for_analysis2$HumCent <- scale(copul_for_analysis2$Humidity, scale = F)
copul_for_analysis2$BPCent <- scale(copul_for_analysis2$deltaBP, scale = F)


copul_model12 <- lmer(Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis2)
copul_model22 <- lmer(Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis2)
summary(copul_model12)
summary(copul_model22)

plot(effect("Box", copul_model12), main = "Mature Female Copulation Latency Rates with/without predator, 0 removed",
     ylab = "Copulation Latency (Sec)", xlab = "Treatment")

plot(effect("Box", copul_model22), main = "Mature Female Copulation Duration Rates with/without predator, 0's removed",
     ylab = "Copulation Duration (Sec)", xlab="Treatment")

with(copul_for_analysis2, boxplot(Cop_latency ~ Box))

with(copul_for_analysis2, boxplot(Cop_Duration ~ Box))

# Copulation Count

#Count of mating for each
#byBox <- group_by(copul_for_analysis2, Box)
#byBox
#copCount <- summarise(byBox, count=n())
#copCount
#pie(copCount$count, labels = copCount$Box, radius = 1.0)

#Copulation Proportion:
head(copul_for_analysis)
copul_for_analysis$copulationSuccess <- ifelse(copul_for_analysis$Cop_latency==0, 0,1)
length(copul_for_analysis$Cop_Duration)


copprop_mod <- glm(copulationSuccess ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis, family = "binomial")
summary(copprop_mod)
plot(allEffects(copprop_mod))
plot(effect("Box", copprop_mod), main = "Copulation Proportions",
     ylab = "Copulation Proportion", xlab = "Treatment")

