# Mating Under Predation R code and issues

dpylr recommended by Ian
```
install.packages("dplyr")
library(dplyr)
```

```
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
```
Bring in data from working directory (getwd())

```
barplot(copulation$Cop_latency, names.arg = copulation$Fly_ID)
barplot(copulation$Cop_Duration, names.arg = copulation$Fly_ID)
```
Quick view of both in copulation


```
summary(courtship)
```
need each fly with specific ID
```
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID
```
Need to keep times above 900 seconds for observation (15 minutes)

Make start time in seconds: currently in HH:MM:SS

Easiest method = trial end minus court duration
```
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)
courtship$startTimeSeconds
```
One value is negative, not sure why? changed to time 0 (delay in program starting vs. behaviour input)
```
courtship$startTimeSeconds[1339]
courtship$startTimeSeconds[1339] = 0
```

```
by_FlyID <- group_by(courtship$Fly_ID)
class(courtship$Fly_ID)
```
Error: says applied to character of class "character", which it is....
Needed library("dplyr") at start

```
by_FlyID <- group_by(courtship,Fly_ID)
by_FlyID
summary(by_FlyID)
```
Need values for less than 15 minutes (900 seconds)
- 1st: nothing that started after 900 seconds
```
lessBy_flyID <- subset(by_FlyID, startTimeSeconds < 900)
lessBy_flyID
summary(lessBy_flyID)
```

But incorrect: need relative start times: needs to be start time of behaviour (just found above in seconds) minus the observation initiation
```
courtship$relativeStartTimeSeconds <- (courtship$startTimeSeconds - courtship$Observation.Initiation)
courtship$relativeStartTimeSeconds
head(courtship)

lessBy_flyID2 <- subset(courtship, relativeStartTimeSeconds < 900)
head(lessBy_flyID2)
summary(lessBy_flyID2)
```

```
by_FlyID2 <- group_by(lessBy_flyID2,Fly_ID)
head(by_FlyID2)
```

```
courtSum <- summarise(by_FlyID2)
head(courtSum)
courtSum
```
- Output is just single group names

Appears to be a new file with each Fly_Id summed and the number of bouts counted
- should be only those that start the bout before the relative 900 seconds
- will include total times that go over the time limit (started before 900 but continued up to whenever
- find a way to cap this at 900 (the courtship duration + start time < 900 or something???)

```
courtSum <- summarise(by_FlyID2, sum = sum(court_duration), count = n())
head(courtSum)
courtSum

barplot(courtSum$sum, names.arg = courtSum$Fly_ID)
barplot(courtSum$count, names.arg = courtSum$Fly_ID)
```

Only values within 900 seconds

1) convrt start times to seconds (done above)

2) create relative start times (starttimeseconds - observationInitiation) Done above

3) relative trial latency at end (based on observation initiation corrected to T = 0 == relative start time -court duration)

4) 900 seconds - relative trial latency (transition step: anything negative = value over 900)

5) relative under 900 trial latency  = IF(900-relativetriallatency < 0, relative trial latency + (900 - relative trial latency), else relaitve trial latency)
  - With this one: worked in Excel that if value negative (distance from 900), that number is removed and the length at end now corrected to 900, or else the length is just the lenth
  
- This will need to be done with the 900 second limit to the start time from before.
-   Using lessBy_flyID2
```
lessBy_flyID2$relativeTrialLatency <- (lessBy_flyID$relativeStartTimeSeconds + lessBy_flyID2$court_duration)
Error in `$<-.data.frame`(`*tmp*`, "relativeTrialLatency", value = numeric(0)) : 
  replacement has 0 rows, data has 1693
```
Not sure the problem
= maybe first do all calculations in courtship
```
courtship$relativeTrialLatency <- (courtship$relativeStartTimeSeconds + courtship$court_duration)

courtship$nineHundredrelative <- (900 - courtship$relativeTrialLatency)

> courtship$relative900trialLatency <- if (courtship$nineHundredrelative<0) courtship$relativeTrialLatency+courtship$nineHundredrelative else courtship$relativeTrialLatency
Warning message:
In if (courtship$nineHundredrelative < 0) courtship$relativeTrialLatency +  :
  the condition has length > 1 and only the first element will be used
  
```

### Random code, myself and Ian

```
#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
barplot(copulation$Cop_latency, names.arg = copulation$Fly_ID)
barplot(copulation$Cop_Duration, names.arg = copulation$Fly_ID)
summary(courtship)
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)
courtship$startTimeSeconds[1339]
courtship$startTimeSeconds[1339] = 0
by_FlyID <- group_by(courtship,Fly_ID)
by_FlyID
summary(by_FlyID)
#lessBy_flyID <- group_by(by_FlyID, startTimeSeconds < 900)
#lessBy_flyID
#summary(lessBy_flyID)

#lessBy_flyID <- subset(by_FlyID, startTimeSeconds < 900)
#lessBy_flyID
#summary(lessBy_flyID)

courtship$relativeStartTimeSeconds <- (courtship$startTimeSeconds - courtship$Observation.Initiation)
courtship$relativeStartTimeSeconds
head(courtship)

lessBy_flyID2 <- subset(courtship, relativeStartTimeSeconds < 900)
head(lessBy_flyID2)
summary(lessBy_flyID2)

by_FlyID2 <- group_by(lessBy_flyID2,Fly_ID)
head(by_FlyID2)

courtSum <- summarise(by_FlyID2, sum = sum(court_duration), count = n())
head(courtSum)
courtSum

#Quick and cheap visualization
boxplot(courtSum$sum, names.arg = courtSum$Fly_ID)
boxplot(courtSum$count, names.arg = courtSum$Fly_ID)
boxplot(courtSum$sum/900, names.arg = courtSum$Fly_ID)

lessBy_flyID2$relativeTrialLatency <- (lessBy_flyID$relativeStartTimeSeconds + lessBy_flyID2$court_duration)

courtship$relativeTrialLatency <- (courtship$relativeStartTimeSeconds + courtship$court_duration)
courtship$nineHundredrelative <- (900 - courtship$relativeTrialLatency)

courtship$nineHundredrelative

courtship$relative900trialLatency <- if (courtship$nineHundredrelative<0) courtship$relativeTrialLatency+courtship$nineHundredrelative else courtship$relativeTrialLatency
courtship$relative900trialLatency


# Create a data frame for the predictor variables and for Fly_ID
pred_var_dat <- subset(courtship, select = c(Box, Date, Replicate, Vial_number, Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room,
                                             Observation.Initiation, Fly_ID))
# Make data frame only include unique values (i.e. remove duplicate rows)
pred_var_dat <- unique(pred_var_dat) 

# Currently courtSum is a dplyr data table (tbl_df). Not sure if it will merge properly with a regular data frame
# Seems to work
courtship_for_analysis <- merge(x = pred_var_dat, y = courtSum, by.x="Fly_ID", by.y="Fly_ID")

with(courtship_for_analysis,
     boxplot(sum ~ Box))

with(courtship_for_analysis,
     boxplot(sum ~ Date))


# A simple version of the analysis
courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + Humidity + BP.Room + (1|Date), 
                         data = courtship_for_analysis)
summary(courtship_model1)

plot(allEffects(courtship_model1))

plot(effect("Box", courtship_model1))

```

New Nicer set up of code

```
#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)

# Bring in the data for mature females (copulation) and Immautre females (courtship)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)

# Create unique Fly_ID for each individual with Box (treatment), date, replicate, and vial Number
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID

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

head(courtship)

startLess900 <- subset(courtship, relativeCourtDuration>0)
summary(startLess900)



# Create a data frame for the predictor variables and for Fly_ID
pred_var_dat <- subset(courtship, select = c(Box, Date, Replicate, Vial_number, Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room,
                                             Observation.Initiation, Fly_ID))
# Make data frame only include unique values (i.e. remove duplicate rows)
pred_var_dat <- unique(pred_var_dat) 

# Create a group by Fly_Id
FlyID <- group_by(startLess900,Fly_ID)
head(FlyID)

courtSum <- summarise(FlyID, sum = sum(relativeCourtDuration), count = n())
head(courtSum)
courtSum


# Currently courtSum is a dplyr data table (tbl_df). Not sure if it will merge properly with a regular data frame
# Seems to work
courtship_for_analysis <- merge(x = pred_var_dat, y = courtSum, by.x="Fly_ID", by.y="Fly_ID")

with(courtship_for_analysis,
     boxplot(sum ~ Box))

with(courtship_for_analysis,
     boxplot(sum ~ Date))


# A simple version of the analysis
courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + Humidity + BP.Room + (1|Date), 
                         data = courtship_for_analysis)
summary(courtship_model1)

plot(allEffects(courtship_model1))

plot(effect("Box", courtship_model1))
```

Random other version with maybe helpful things

#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)

# Bring in the data for mature females (copulation) and Immautre females (courtship)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
Courtship Analysis

# Create unique Fly_ID for each individual with Box (treatment), date, replicate, and vial Number
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID

# Create Delta BP (Barometric pressure difference 8 hours before experiment)
courtship$deltaBP <- (courtship$BP.12.00.am - courtship$BP.8.00.Am)

# Change time start of behaviour (in HH:MM:SS format) to seconds (One had a value of -1, not sure, but changed to 0)
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

head(courtship)

startLess900 <- subset(courtship, relativeCourtDuration>0)
summary(startLess900)

# Create a data frame for the predictor variables and for Fly_ID
pred_var_dat <- subset(courtship, select = c(Box, Date, Replicate, Vial_number, Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room,
                                             Observation.Initiation, Fly_ID, deltaBP))
# Make data frame only include unique values (i.e. remove duplicate rows)
pred_var_dat <- unique(pred_var_dat) 

# Create a group by Fly_Id
FlyID <- group_by(startLess900,Fly_ID)
head(FlyID)

courtSum <- summarise(FlyID, sum = sum(relativeCourtDuration), count = n())
head(courtSum)
courtSum


# Currently courtSum is a dplyr data table (tbl_df). Not sure if it will merge properly with a regular data frame
# Seems to work
courtship_for_analysis <- merge(x = pred_var_dat, y = courtSum, by.x="Fly_ID", by.y="Fly_ID")

with(courtship_for_analysis,
     boxplot(sum ~ Box))

with(courtship_for_analysis,
     boxplot(sum ~ Date))


# A simple version of the analysis

courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_for_analysis$HumCent <- scale(courtship_for_analysis$Humidity, scale = F)
courtship_for_analysis$BPCent <- scale(courtship_for_analysis$deltaBP, scale = F)

courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

courtship_model2 <- lmer(count ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)
summary(courtship_model1)
# Model for changes in sum of courtship duration in a 15 minute (900 seconds) window of observation from T=0 for each different vial
> summary(courtship_model1)
Linear mixed model fit by REML ['lmerMod']
Formula: sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1 | Date)
   Data: courtship_for_analysis

REML criterion at convergence: 1731.7

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-1.81892 -0.79151 -0.09858  0.86998  2.11869 

Random effects:
 Groups   Name        Variance Std.Dev.
 Date     (Intercept)     0      0.0   
 Residual             70912    266.3   
Number of obs: 128, groups:  Date, 6

Fixed effects:
            Estimate Std. Error t value
(Intercept)   466.97      61.22   7.628
BoxB           35.51      47.25   0.751
Replicate     -53.19      25.64  -2.075
TempCent      -48.93      65.01  -0.753
HumCent       -56.13      41.35  -1.358
BPCent        247.66     118.56   2.089

Correlation of Fixed Effects:
          (Intr) BoxB   Replct TmpCnt HumCnt
BoxB      -0.360                            
Replicate -0.839 -0.031                     
TempCent  -0.270 -0.032  0.332              
HumCent    0.250 -0.016 -0.286 -0.601       
BPCent    -0.116 -0.011  0.141  0.307 -0.793
summary(courtship_model2)
> summary(courtship_model2)
Linear mixed model fit by REML ['lmerMod']
Formula: count ~ Box + Replicate + TempCent + HumCent + BPCent + (1 |      Date)
   Data: courtship_for_analysis

REML criterion at convergence: 831.3

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-2.01612 -0.74077  0.03709  0.62407  2.14596 

Random effects:
 Groups   Name        Variance Std.Dev.
 Date     (Intercept)  3.095   1.759   
 Residual             43.660   6.608   
Number of obs: 128, groups:  Date, 6

Fixed effects:
            Estimate Std. Error t value
(Intercept)  15.4231     1.7056   9.043
BoxB         -0.5163     1.1738  -0.440
Replicate    -0.9174     0.6483  -1.415
TempCent      0.8865     2.3783   0.373
HumCent      -1.2041     1.5148  -0.795
BPCent        5.3338     4.6521   1.147

Correlation of Fixed Effects:
          (Intr) BoxB   Replct TmpCnt HumCnt
BoxB      -0.325                            
Replicate -0.754 -0.023                     
TempCent  -0.240 -0.025  0.226              
HumCent    0.227 -0.011 -0.202 -0.501       
BPCent    -0.097 -0.007  0.088  0.229 -0.770
plot(allEffects(courtship_model1))
AllEffects- courtship Sum

plot(effect("Box", courtship_model1))
plot(effect("Box", courtship_model2))

# Make total proportion of time courting

Copulation Analysis
summary(copulation)
dim(copulation)
head(copulation)
copulation$deltaBP <- (copulation$BP.12.00.am - copulation$BP.8.00.Am)
cop_data <- subset(copulation, select = c(Box, Date, Replicate, Vial.., Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room, Fly_ID, deltaBP))
cop_data <- unique(cop_data)
head(cop_data)

LatCop <- distinct(select(copulation, Cop_latency, Cop_Duration, Fly_ID))
head(LatCop)

copul_for_analysis <- merge(x = cop_data, y = LatCop, by.x="Fly_ID", by.y="Fly_ID")

copul_for_analysis$TempCent <- scale(copul_for_analysis$Temp, scale=F)
copul_for_analysis$HumCent <- scale(copul_for_analysis$Humidity, scale = F)
copul_for_analysis$BPCent <- scale(copul_for_analysis$deltaBP, scale = F)


copul_model1 <- lmer(Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)

copul_model2 <- lmer(Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)

summary(copul_model1)
summary(copul_model2)

with(copul_for_analysis, boxplot(Cop_latency ~ Box))
with(copul_for_analysis, boxplot(Cop_Duration ~ Box))

plot(allEffects(copul_model1))

plot(effect("Box", copul_model1))
plot(effect("Box", copul_model2))




