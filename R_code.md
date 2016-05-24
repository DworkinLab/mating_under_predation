#Courtship and Copulation changes under predation threat

## Set up files and packages needed
```
#install.packages("dplyr")
library(dplyr)
library(lme4)
library(effects)
```
```
# Bring in the data for mature females (copulation) and Immature females (courtship)
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
```
##Courtship Analysis
```
# Create unique Fly_ID for each individual with Box (treatment), date, replicate, and vial Number
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID
```
```
# Create Delta BP
courtship$deltaBP <- (courtship$BP.12.00.am - courtship$BP.8.00.Am)
```
```
# Change time (in HH:MM:SS) format to seconds (One had a value of -1, not sure, but changed to 0)
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)
courtship$startTimeSeconds[1339]
courtship$startTimeSeconds[1339] = 0
```
```
# Create new column of relative values for courtship start times (i.e so Observation all start at Time = 0)
courtship$relativeStartTimeSeconds <- (courtship$startTimeSeconds - courtship$Observation.Initiation)
courtship$relativeStartTimeSeconds
```
```
# New column for relative values of trial duration at end of behaviour
courtship$relativeTrial_latency_end <- (courtship$relativeStartTimeSeconds + courtship$court_duration)
courtship$relativeTrial_latency_end
```
```
# Need to get all courtship under 900 seconds (relative court duration)
#First, transition step for finding values ending abov 900
courtship$nineHundredTransition <- (900 - courtship$relativeTrial_latency_end)
# Second, if value for nineHundredTransition is negative, equate it to 900, all else stay as relativeTrial_latency_end
courtship$relativeTrialLatency900 <- ifelse(courtship$nineHundredTransition<0,900,courtship$relativeTrial_latency_end)
# Third, relative courtship duration (not including values over 900)
courtship$relativeCourtDuration <- (courtship$relativeTrialLatency900 - courtship$relativeStartTimeSeconds)
```
```
startLess900 <- subset(courtship, relativeCourtDuration>0)
summary(startLess900)
```
```
# Create a data frame for the predictor variables and for Fly_ID
pred_var_dat <- subset(courtship, select = c(Box, Date, Replicate, Vial_number, Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room,
                                             Observation.Initiation, Fly_ID, deltaBP))
# Make data frame only include unique values (i.e. remove duplicate rows)
pred_var_dat <- unique(pred_var_dat) 
```
```
#Rename Box to treatment type, and make characters to factor (to run later for plot(effect()))
pred_var_dat$Box <- ifelse(pred_var_dat$Box=="A", "Predator", "Control")
pred_var_dat$Box
pred_var_dat$Box <- factor(pred_var_dat$Box)
```
```
# Create a group by Fly_Id
FlyID <- group_by(startLess900,Fly_ID)
head(FlyID)
```
```
courtSum <- summarise(FlyID, sum = sum(relativeCourtDuration), count = n())
head(courtSum)
courtSum
```
```
# Currently courtSum is a dplyr data table (tbl_df). Not sure if it will merge properly with a regular data frame
# Seems to work
courtship_for_analysis <- merge(x = pred_var_dat, y = courtSum, by.x="Fly_ID", by.y="Fly_ID")

with(courtship_for_analysis,
     boxplot(sum ~ Box))
```
![Plot_sumcourtship.png]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20Bar%20Plot_sumcourtship.png)

```
with(courtship_for_analysis,
     boxplot(sum ~ Date))
```
![Plot_sum_by_date_not_informative]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20sum%20by%20date_useless.png)


### A simple version of the analysis
```
#Scale tempature, humidity and deltaBP
courtship_for_analysis$TempCent <- scale(courtship_for_analysis$Temp, scale=F)
courtship_for_analysis$HumCent <- scale(courtship_for_analysis$Humidity, scale = F)
courtship_for_analysis$BPCent <- scale(courtship_for_analysis$deltaBP, scale = F)
```

```
# Model for sum of time courting in 900 seconds

courtship_model1 <- lmer(sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

summary(courtship_model1)
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
(Intercept)   502.49      62.45   8.046
BoxPredator   -35.51      47.25  -0.751
Replicate     -53.19      25.64  -2.075
TempCent      -48.93      65.01  -0.753
HumCent       -56.13      41.35  -1.358
BPCent        247.66     118.56   2.089

Correlation of Fixed Effects:
            (Intr) BxPrdt Replct TmpCnt HumCnt
BoxPredator -0.404                            
Replicate   -0.846  0.031                     
TempCent    -0.289  0.032  0.332              
HumCent      0.233  0.016 -0.286 -0.601       
BPCent      -0.122  0.011  0.141  0.307 -0.793
```

```
# Model for number of courtship bouts in the 900 second observation window
courtship_model2 <- lmer(count ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)

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
(Intercept)  14.9068     1.7280   8.627
BoxPredator   0.5163     1.1738   0.440
Replicate    -0.9174     0.6483  -1.415
TempCent      0.8865     2.3783   0.373
HumCent      -1.2041     1.5148  -0.795
BPCent        5.3338     4.6521   1.147

Correlation of Fixed Effects:
            (Intr) BxPrdt Replct TmpCnt HumCnt
BoxPredator -0.359                            
Replicate   -0.759  0.023                     
TempCent    -0.254  0.025  0.226              
HumCent      0.217  0.011 -0.202 -0.501       
BPCent      -0.100  0.007  0.088  0.229 -0.770
```


```
plot(allEffects(courtship_model1))
```
![alleffetsmodel1]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20alleffects_court_sum.png)
```
plot(allEffects(courtship_model2))
```
![alleffectsmodel2]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20alleffects_court_count.png)

```
plot(effect("Box", courtship_model1), main = "Male Time Courting of Immature Female in 900 Seconds",
     ylab = "Time courting (sec)", xlab = "Treatment")
```
![court_sum]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20court_sum.png)

```
plot(effect("Box", courtship_model2), main = "Number of Male Courtship Bouts to Immature Female in 900 Seconds",
     ylab = "Courtship Bouts", xlab = "Treatment")
```
![court_count]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot-%20court_count2.png)

# Copulation Analysis

```
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
```
### Simple linear models

```
copul_model1 <- lmer(Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)
summary(copul_model1)
```
```
> summary(copul_model1)
Linear mixed model fit by REML ['lmerMod']
Formula: Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent +      (1 | Date)
   Data: copul_for_analysis

REML criterion at convergence: 2039.3

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.2829 -0.6276 -0.3544  0.3372  3.8892 

Random effects:
 Groups   Name        Variance Std.Dev.
 Date     (Intercept)      0     0.0   
 Residual             160489   400.6   
Number of obs: 142, groups:  Date, 6

Fixed effects:
            Estimate Std. Error t value
(Intercept)  263.453     90.713   2.904
BoxPredator   40.212     67.523   0.596
Replicate      3.726     36.116   0.103
TempCent     -28.806     86.581  -0.333
HumCent       62.495     56.581   1.105
BPCent       102.512    175.265   0.585

Correlation of Fixed Effects:
            (Intr) BxPrdt Replct TmpCnt HumCnt
BoxPredator -0.395                            
Replicate   -0.853  0.033                     
TempCent    -0.282 -0.008  0.339              
HumCent      0.079  0.025 -0.104 -0.523       
BPCent      -0.099  0.035  0.103  0.272 -0.745
```


```
copul_model2 <- lmer(Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis)
summary(copul_model2)
```
```
> summary(copul_model2)
Linear mixed model fit by REML ['lmerMod']
Formula: Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent +      (1 | Date)
   Data: copul_for_analysis

REML criterion at convergence: 2067.5

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.4292 -1.0166  0.3374  0.8474  1.6397 

Random effects:
 Groups   Name        Variance Std.Dev.
 Date     (Intercept)      0     0.0   
 Residual             197396   444.3   
Number of obs: 142, groups:  Date, 6

Fixed effects:
            Estimate Std. Error t value
(Intercept)   531.45     100.60   5.283
BoxPredator   -34.13      74.89  -0.456
Replicate     -17.03      40.05  -0.425
TempCent      -83.04      96.02  -0.865
HumCent        75.62      62.75   1.205
BPCent        -35.86     194.37  -0.184

Correlation of Fixed Effects:
            (Intr) BxPrdt Replct TmpCnt HumCnt
BoxPredator -0.395                            
Replicate   -0.853  0.033                     
TempCent    -0.282 -0.008  0.339              
HumCent      0.079  0.025 -0.104 -0.523       
BPCent      -0.099  0.035  0.103  0.272 -0.745

```

```
with(copul_for_analysis, boxplot(Cop_latency ~ Box))
```
![cop_lat]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20cop_lat.png)

```
with(copul_for_analysis, boxplot(Cop_Duration ~ Box))
```
![cop_dur]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20op_dur.png)

```
plot(allEffects(copul_model1))
```
![alleffects_lat]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20alleffects_cop_lat.png)


```
plot(allEffects(copul_model2))
```
![alleffects_dur]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20alleffects_cop_dur.png)

```
plot(effect("Box", copul_model1), main = "Mature Female Copulation Latency Rates with/without predator",
     ylab = "Copulation Latency (Sec)", xlab = "Treatment")
```

![plot_latency]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20cop_latency_effects.png)

```
plot(effect("Box", copul_model2), main = "Mature Female Copulation Duration Rates with/without predator",
     ylab = "Copulation Latency (Sec)", xlab="Treatment")
```

![plot_duration]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20Cop_duration_effects.png)



#Removing all values with no copulation
```
LatCop2 <- distinct(select(copulation, Cop_latency, Cop_Duration, Fly_ID))
head(LatCop2)
d<-d[!(d$A=="B" & d$E==0),]
LatCop2 <- LatCop2[!(LatCop2$Cop_latency==0),]
LatCop2
dim(LatCop2)
copul_for_analysis2 <- merge(x = cop_data, y = LatCop2, by.x="Fly_ID", by.y="Fly_ID")
copul_for_analysis2$TempCent <- scale(copul_for_analysis2$Temp, scale=F)
copul_for_analysis2$HumCent <- scale(copul_for_analysis2$Humidity, scale = F)
copul_for_analysis2$BPCent <- scale(copul_for_analysis2$deltaBP, scale = F)
copul_model12 <- lmer(Cop_latency ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis2)
copul_model22 <- lmer(Cop_Duration ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = copul_for_analysis2)
summary(copul_model12)
summary(copul_model22)
```

```
plot(effect("Box", copul_model12), main = "Mature Female Copulation Latency Rates with/without predator, 0 removed",
     ylab = "Copulation Latency (Sec)", xlab = "Treatment")
```
![0copLat]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20cop_lat_0.png)

```
plot(effect("Box", copul_model22), main = "Mature Female Copulation Duration Rates with/without predator, 0's removed",
     ylab = "Copulation Latency (Sec)", xlab="Treatment")
```
![0copdur]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20cop_dur_0.png)

```
with(copul_for_analysis2, boxplot(Cop_latency ~ Box))
```
![0boxcoplat]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20copBox_lat_0.png)
```
with(copul_for_analysis2, boxplot(Cop_Duration ~ Box))
```
![0boxcopdur]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20copBox_dur_0.png)


# Copulation Count
```
#Count of mating for each
byBox <- group_by(copul_for_analysis2, Box)
byBox
copCount <- summarise(byBox, count=n())
copCount
```

```
> copCount
Source: local data frame [2 x 2]

       Box count
    (fctr) (int)
1  Control    41
2 Predator    39
```

```
pie(copCount$count, labels = copCount$Box, radius = 1.0)
```

![copCountPie]
(https://github.com/PaulKnoops/mating_under_predation/blob/master/Rplot%20-%20countsts.png)

