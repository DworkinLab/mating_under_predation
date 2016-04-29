```
#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)

# Bring in the data for mature females (copulation) and Immautre females (courtship)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
```
# Courtship Analysis

```

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

```

```

# A simple version of the analysis

courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_for_analysis$HumCent <- scale(courtship_for_analysis$Humidity, scale = F)
courtship_for_analysis$BPCent <- scale(courtship_for_analysis$deltaBP, scale = F)

courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

courtship_model2 <- lmer(count ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)
```
```
summary(courtship_model1)
# Model for changes in sum of courtship duration in a 15 minute (900 seconds) window of observation from T=0 for each different vial
```
```
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
```
```
summary(courtship_model2)
```
```
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
```
```
plot(allEffects(courtship_model1))
```
![AllEffects- courtship Sum]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot-%20allEffects_courtshipSum.png)

```
plot(effect("Box", courtship_model1))
plot(effect("Box", courtship_model2))

# Make total proportion of time courting

```


# Copulation Analysis


```
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
```
