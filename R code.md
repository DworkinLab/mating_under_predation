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
2) create relative start times (starttimeseconds - observationInitiation)
3) relative trial latency at end (based on observation initiation corrected to T = 0 == relative start time -court duration)
4) 900 seconds - relative trial latency (transition step: anything negative = value over 900)
5) relative under 900 trial latency  = IF(900-relativetriallatency < 0, relative trial latency + (900 - relative trial latency), else relaitve trial latency)
  - With this one: worked in Excel that if value negative (distance from 900), that number is removed and the length at end now corrected to 900, or else the length is just the lenth
  
- This will need to be done with the 900 second limit to the start time from before.






