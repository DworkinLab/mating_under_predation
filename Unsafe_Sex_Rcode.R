## Official Analysis: mating project

source("packages.R")

######################
## Courtship Analysis
courtship <- read.csv("Immature.csv",h=T)

#Remove Temp, Humidity and BP
courtship <- subset(courtship, select = 
                      -c(Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room ))


# Change time (in HH:MM:SS) format to seconds (One had a value of -1, not sure, but changed to 0)
# Start time of behaviours will be the time at end of courtship bout of full trial (trial_latency_behav_end minus the courtship duration)
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)

courtship$startTimeSeconds[1339] = 0

# Create new column of relative values for courtship start times (The start of observations are relative to the observation initiation of the certain vial (subtract observation initiation)
courtship$relativeStartTimeSeconds <- (courtship$startTimeSeconds - courtship$Observation.Initiation)

# New column for relative values of trial duration at end of behaviour (the length of behaviours)
courtship$relativeTrial_latency_end <- (courtship$relativeStartTimeSeconds + courtship$court_duration)

#Remove some unneeded columns:
courtship <- subset(courtship, select = 
                      -c(Day, start_time_program, Activity, 
                         trial_latency_behav_end, Video_Initition, 
                         Video_End, startTimeSeconds) )

# Need to get all courtship under 900 seconds (relative court duration)

#First, transition step for finding values ending abov 900
courtship$nineHundredTransition <- (900 - courtship$relativeTrial_latency_end)

# Second, if value for nineHundredTransition is negative, equate it to 900, all else stay as relativeTrial_latency_end
courtship$relativeTrialLatency900 <- ifelse(courtship$nineHundredTransition<0,
                                            900,
                                            courtship$relativeTrial_latency_end)

# Third, relative courtship duration (not including values over 900)
courtship$relativeCourtDuration <- (courtship$relativeTrialLatency900 - courtship$relativeStartTimeSeconds)

#Rename Box to treatment type, and make characters to factor (to run later for plot(effect()))

courtship$Treatment <- ifelse(courtship$Box=="A",
                              "Predator",
                              "Control")

courtship$Treatment <- factor(courtship$Treatment)

courtship <- subset(courtship, select =
                      -c(Box, court_duration, relativeTrial_latency_end, 
                         nineHundredTransition))

courtship2 <- subset(courtship, relativeCourtDuration>0)

courtship2 <- courtship2 %>%
  group_by(Date,Replicate,Vial_number,Observation.Initiation, Treatment) %>%
  summarise(Court_sum=sum(relativeCourtDuration), 
            count=n(), 
            court_prop=sum(relativeCourtDuration/900))

#The Models:

courtship_model1 <- lmer(court_prop ~ Treatment + Replicate + 
                           (1|Date), 
                         data=courtship2 )

corprop_eff <- effect("Treatment", courtship_model1)
corprop_eff <- as.data.frame(corprop_eff)
corprop_eff$Behaviour <- "Proportion Time Courting"
summary(courtship_model1)
car::Anova(courtship_model1)

gg_courtProp <- ggplot(corprop_eff, aes(x=Behaviour, y=fit, colour=Treatment))
gg_courtProp2 <- gg_courtProp + geom_point(stat="identity", 
                            position = position_dodge(.9), size=5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2) + 
  ylab("Proportion") + 
  xlab("") +
  ylim(0,1) +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_courtProp2)

courtship_model2 <- lmer(count ~ Treatment + Replicate + 
                           (1|Date), 
                         data = courtship2)

summary(courtship_model2)

corcount_eff<- effect("Treatment", courtship_model2)
corcount_eff <- as.data.frame(corcount_eff)
corcount_eff$Behaviour <- "Courtship Bouts"
summary(courtship_model2)
car::Anova(courtship_model2)

gg_courtcount <- ggplot(corcount_eff, aes(x=Behaviour, y=fit, colour=Treatment))
gg_courtcount2 <- gg_courtcount + geom_point(stat="identity", 
                  position = position_dodge(.9), size=5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2) + 
  ylab("Count") + 
  xlab("") +
  ylim(10,18) +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_courtcount2)

multiplot(gg_courtProp2, gg_courtcount2)




###############
## Copulation Analysis

copulation <- read.csv("Mature.csv",h=T)

#Change box to treatment and to Predator vs. Control
copulation$Treatment <- ifelse(copulation$Box=="C", 
                               "Predator", 
                               "Control")
copulation$Treatment <- factor(copulation$Treatment)

#Remove Temp, and other unneccessary columns)
copulation <- subset(copulation, select = 
                       -c(Box, File, Time_In, Cop_start, Cop_end, 
                          Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room))

#Copulation Proportion:

copulation$copulationSuccess <- ifelse(copulation$Cop_latency==0, 
                                       0, 1)

#Removing all values with no copulation (new data frame)
copulation2 <- copulation[!(copulation$Cop_latency==0),]

## Models:

#copulation proportion
copprop_mod <- glm(copulationSuccess ~ Treatment + Replicate + (1|Date),
                   data = copulation, family = "binomial")

copprop_eff <- effect("Treatment", copprop_mod)
copprop_eff <- as.data.frame(copprop_eff)
copprop_eff$Behaviour <- "Proportion Copulation"
summary(copprop_mod)
car::Anova(copprop_mod)

gg_copprop <- ggplot(copprop_eff, aes(x=Behaviour, y=fit, colour=Treatment))
gg_copprop2 <- gg_copprop + geom_point(stat="identity", 
                           position = position_dodge(.9), size=5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2) + 
  ylab("Proportion") + 
  xlab("") +
  ylim(0,1) +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_copprop2)

# Copulation Latency

copul_lat_mod <- lmer(Cop_latency ~ Treatment + Replicate + (1|Date), 
                      data = copulation2)

coplat_0_eff <- effect("Treatment", copul_lat_mod)
coplat_0_eff <- as.data.frame(coplat_0_eff)
coplat_0_eff$Behaviour <- "Copulation Latency"
summary(copul_lat_mod)
car::Anova(copul_lat_mod)
summary(copulation2)

gg_coplat <- ggplot(coplat_0_eff, aes(x=Behaviour, y=fit, colour=Treatment))
gg_coplat2 <- gg_coplat + geom_point(stat="identity", 
                        position = position_dodge(.9), size=5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2) + 
  ylab("Time (sec)") + 
  xlab("") +
  ylim(300,800) +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_coplat2)

# Copulation Duration:

copul_dur_Mod <- lmer(Cop_Duration ~ Treatment + Replicate + (1|Date), 
                      data = copulation2)

copdur_0_eff <- effect("Treatment", copul_dur_Mod)
copdur_0_eff <- as.data.frame(copdur_0_eff)
copdur_0_eff$Behaviour <- "Copulation Duration"
summary(copul_dur_Mod)
car::Anova(copul_dur_Mod)

gg_copdur <- ggplot(copdur_0_eff, aes(x=Behaviour, y=fit, colour=Treatment))
gg_copdur2 <- gg_copdur + geom_point(stat="identity", 
                        position = position_dodge(.9), size=5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2) + 
  ylab("Time (sec)") + 
  xlab("") +
  ylim(700,1000) +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_copdur2)


multiplot(gg_copdur2, gg_coplat2, gg_copprop2, cols=2)
