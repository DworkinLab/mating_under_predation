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






