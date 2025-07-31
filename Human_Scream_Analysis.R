#Import data frame: CHANGE THIS TO YOUR FILE PATH, WHERE YOU HAVE THE CSV 
data_all <- read.csv('~/Desktop/Ulab/data/Agnes_Data.csv')

# Look at the structure
str(data_all)
# Let's take a look at the column names
columns <- data.frame(colnames(data_all))

# And the first couple rows of data
head(data_all, 2)

summary(as.factor(data_all$StimType))


# Okay, so we know from the paper that the authors work with 6032 trials, but our data frame has 6240 rows (i.e., trials).
# First order of business is fixing this discrepancy. 

## TWO PRELIMINARY EXCLUSIONS ARE NECESSARY:
# More specifically, the authors said that the analyses reported in the paper excluded trials that "either (a) comprised duplicate stimulus pairs or (b) reflected premature responses (i.e., responses provided after the second stimulus had started playing but before it had finished)".
# According to supplementary file in the audio stimuli folder, the following stimuli were duplicate:
## Different_Vocalizer_21_b*
## Same_Vocalizer_4_a*

## The "SoundFile2" column identifies these, without the a and b letters and middle underscore.
## Let's exclude the rows where DifferentVocalizer_21 or SameVocalizer_4 occurs:
data_exclusions_stim <- subset(data_all, SoundFile2 != "DifferentVocalizer_21" & SoundFile2 != "SameVocalizer_4")
# Down to 6032 trials. This is good because we know, for the next exclusion factor, the authors say it resulted "in the exclusion of 489 trials out of 6,032 trials across all participants."

# NEXT, for the premature response exclusion, the authors included a column in their data that may help us figure out who meets this exclusion criteria: "Actual_Latency"
# It's possible that the negative values here represent trials where participants responded before the 2nd sound ended.
# Let's see how many negative values we have in this column (we're looking for 489!)
RT_exclusions <- subset(data_exclusions_stim, Actual_Latency < 0)
# Look at the number of rows in the data frame: 503 trials were classified as negative (not 489)

# Let's take an educated guess that the exclusions didn't apply to all trials below 0 (i.e., maybe there was some type of reasonable window applied)
RT_exclusions <- data.frame(RT_exclusions[order(RT_exclusions$Actual_Latency),]) # This will sort the negative values from most to least negative
RT_exclusions$check <- c(1:(nrow(RT_exclusions))) # This will tell us which negative value is actually #489
# Visually inspect the data, what do you see?

# It appears that the authors created a buffer window of 10 ms. That is, responses made within 10 ms of the 2nd scream ending were given the green light, all other responses made before the last 10 ms of the scream were excluded.
# Let's double-check this logic:
RT_exclusions <- subset(data_exclusions_stim, Actual_Latency < -10)
# Look at the number of rows in the data frame: 489

# Let's apply this exclusion to the data
data_exclusions_latency <- subset(data_exclusions_stim, Actual_Latency >= -10)

# Let's rename the data frame as something simpler
data <- data.frame(data_exclusions_latency)


# Cleaning up extra data frame that we won't need from here on out:
rm(data_exclusions_latency, data_exclusions_stim, RT_exclusions)


# Okay, it looks like "StimType" might be the column that differentiates between the 3 different trials, which corresponds to the SoundFile2 column.
# To check, let's see how many different numbers are in the StimType column- there should be 3 if these signify different trial types:
summary(as.factor(data$StimType))
# Good, yes it looks like there are 3 different numbers. 

# After visually inspecting the data for Subject 1, it appears that StimType numbers correspond to the following trial types:
# 0: Duration Modified
# 1: Same Vocalizer
# 2: Different Vocalizer
# This will be good to remember down the road in the analyses.


# # # # # # #       Effects of listener and vocalizer gender        # # # # # # #
# For the analyses you're interested in, we need to identify the following columns in the data set:
## Participant gender
## Vocalizer gender
## Response latency
## d': this is a little complicated, see my notes below on d' before calculating it.

# In signal detection, sensitivity (d', "d prime") is calculated with the following algorithm: 
##(1) the proportion of hits (correct “go” response for signal items) and false alarms (incorrect “go” response for noise items) are each converted to z-scores
#### ******** NOTE: Normally, to create z-scores (standardized scores) from a variable, you would subtract the mean of all data points from each individual data point, then divide those points by the standard deviation of all points. This would be accomplished using the scale() function: scale(A, center = TRUE, scale = TRUE). HOWEVER, z-scoring in the context of d' and signal detection theory is DIFFERENT- here we have to z-score against a standard, normal distribution (vs. our own data's distribution). To do this, we can use the function qnorm(). *************
## (2) a difference between the z-score values for hits and false alarms is d’.D-prime values of 0 or below indicate that subjects were either unable to discriminate any signal from noise or were not performing the task as instructed. 

# One more thing to note (may not apply to these analyses b/c I'm not sure if the authors did this)
# Calculating d' : need to exclude equal to or less than 0, which indicate that subjects were either unable to discriminate any signal from noise or were not performing the task as instructed. As such, blocks with sensitivity scores of 0 or below (0 is chance responding) are typically removed from the analysis. 
#**************THE AUTHORS DID NOT NOTE WHERE OR NOT THEY REMOVED PARTICIPANTS WITH D' <= 0 **************


# Okay, so the first thing we need to do when calculating d prime is calculate HIT and FALSE ALARM RATES:
# The authors said that they calculated hits and false alarms as: 
### •	Hit = correctly responding “Same” when the two screams were produced by the same vocalizer (i.e., Duration Modified and Same Vocalizer trials).
### •	False Alarm = incorrectly responding “Same” when two different vocalizers had produced the screams (i.e., Different Vocalizer trials)

# Okay, the "ACC" column convenient marks accurate (1) or inaccurate (0) trials for each subject.
# We need to sum the hits and false alarms for each subject and then divide this by the total number of trials each participant saw in each category.

# First, let's just check how many trials each participant has:
Sub_totalRowCount <- data.frame(table(data$Subject))
names(Sub_totalRowCount) <- c("Subject", "TotalRowCount")
summary(as.factor(Sub_totalRowCount$TotalRowCount))
# Okay, everyone has a different # of rows, i.e., trials, because the exclusions we did earlier applied to everyone differently. But most people have 57-58 trials

# NOW, to calculate HITS and FALSE ALARM RATES. We will subset by trial types first, then count number of trials.
# For HITS (need Duration Modified and Same Vocalizer trials):
DM_SV_trials <- subset(data, StimType == 0 | StimType == 1)
Sub_DMSVRowCount <- data.frame(table(DM_SV_trials$Subject))
names(Sub_DMSVRowCount) <- c("Subject", "DMSV_RowCount")
summary(as.factor(Sub_DMSVRowCount$DMSV_RowCount))

# install.packages("dplyr")
library(dplyr) # needed for %>% functions below
# Now let's count the # of hits they made:
Hit_total <- DM_SV_trials %>% group_by(Subject) %>% summarise(Hits = sum(ACC)) %>% select(Subject, Hits)
# Next, let's combine our data frames where the total trial counts and total hits made have just been calculated:
Hit_rates <- merge(Hit_total, Sub_DMSVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Duration Modified and Same Vocalizer trials:
Hit_rates$Hit_Rate <- c(Hit_rates$Hits / Hit_rates$DMSV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# Now let's do the same thing for false alarms. 
# For FALSE ALARMS (just need Different Vocalizer trials)
DV_trials <- subset(data, StimType == 2)
Sub_DVRowCount <- data.frame(table(DV_trials$Subject))
names(Sub_DVRowCount) <- c("Subject", "DV_RowCount")
summary(as.factor(Sub_DVRowCount$DV_RowCount))

# However, this time- we can't just sum the "ACC" column, because a False Alarm (or error) is signaled by a 0 in this column.
# Multiple ways to handle this, but I'll just subset the data again so that we have a new data frame that only contains the errors made for "Different Vocalizer" trials, and then count the number of rows per subject.
# Another way to do this would be to change the coding in the ACC column (e.g., 0 -> 1, and 1 -> 0)
DV_trials_FalseAlarms <- subset(DV_trials, ACC == 0)
FalseAlarm_total <- data.frame(table(DV_trials_FalseAlarms$Subject))
names(FalseAlarm_total) <- c("Subject", "FalseAlarms")

# Next, let's combine our data frames where the total trial counts and total false alarms made have just been calculated:
FalseAlarm_rates <- merge(FalseAlarm_total, Sub_DVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Different Vocalizer trials:
FalseAlarm_rates$FalseAlarm_Rate <- c(FalseAlarm_rates$FalseAlarms / FalseAlarm_rates$DV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# Inspect this data frame- here you can see that 3 subjects made no errors on these trials.
# These are marked as NAs, but really, they should be zeros.
FalseAlarm_rates[is.na(FalseAlarm_rates)] <- 0


# Now, let's look at these values side by side:
Rates <- merge(Hit_rates, FalseAlarm_rates, by= c("Subject"), all=T)
# Main thing we want to check is that there is no one with 100% for hits AND 100% for false alarms, which could imply that they are just hitting the "Same" key over and over again.
# However, a perfect score on hits (100%) and false alarms (0%) also poses a problem for z-scoring, where 0 equal "negative infinity" and 1 equals "positive infinity". Run the analyses below and see what happens to the z-score values.


# Once again, removing dataframes we no longer need:
rm(DM_SV_trials, DV_trials, DV_trials_FalseAlarms, FalseAlarm_rates, FalseAlarm_total, Hit_rates, Hit_total, Sub_DMSVRowCount, Sub_DVRowCount, Sub_totalRowCount)


# Now we will z-score the data using qnorm and calculate d' by subtracting the proportion of false alarms from hits:
Rates$Hit_RateZ <- qnorm(Rates$Hit_Rate)
Rates$FalseAlarm_RateZ <- qnorm(Rates$FalseAlarm_Rate)
Rates$Dprime  <- Rates$Hit_RateZ - Rates$FalseAlarm_RateZ

# To deal with the +/- infinity values:
# We can set the minimum % = 1/N where N is the number of trials used in the calculation of false alarms. This would be
Rates$Min <- (1 / Rates$DV_RowCount)
# We can set the maximum value for % = (N-1)/N where N is the number of trials used in the calculation of hits. This would be
Rates$Max <- ((Rates$DMSV_RowCount) - 1) / (Rates$DMSV_RowCount)
# Adapted from http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^

# This is how you would calculate the max and min values using the original total number of trials (after repeateded audio exclusions)
# Rates$Min <- (1 / 23)
# # We can set the maximum value for % = (N-1)/N where N is the number of trials used in the calculation of hits. This would be
# Rates$Max <- ((12 + 23) - 1) / (12 + 23)


# I'm not sure if this is what the authors did, but we should follow this convention and just make a note of it.
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS:
# Before replacing the infinity values with max and min values, let's save a tester variable where these values are replaced with NA's instead:
# Rates$Dprime2 <-  ifelse((Rates$Dprime == "-Inf" | Rates$Dprime == "Inf"), NA, Rates$Dprime)

# Now let's replace the rates that generated infinity values with the min and max values we just calculated, and then recalculate d prime.
Rates$FalseAlarm_Rate <-  ifelse((Rates$FalseAlarm_Rate == 0), Rates$Min, Rates$FalseAlarm_Rate)
Rates$Hit_Rate <-  ifelse((Rates$Hit_Rate == 1), Rates$Max, Rates$Hit_Rate)

Rates$Hit_RateZ <- qnorm(Rates$Hit_Rate)
Rates$FalseAlarm_RateZ <- qnorm(Rates$FalseAlarm_Rate)
Rates$Dprime  <- Rates$Hit_RateZ - Rates$FalseAlarm_RateZ

# Does the mean d' value match what the authors reported in their results? Let's use the "describe" function from the psych library.
# "The mean response accuracy across all participants and stimuli was 0.77 (SE = 0.01), corresponding to a mean d′ score = 1.63 (SE = 0.06)." 
#install.packages("psych")
library(psych)
describe(Rates$Dprime)
# This is very close!

# Maybe they excluded d primes that were less than zero? This is standard in the literature, but they did not report it, and I don't think they did judging from reported degrees of freedom. But let's just check it anyway, just in case.
Rates_nozero <- subset(Rates, Dprime > 0)
describe(Rates_nozero$Dprime)
describe(Rates_nozero$Dprime)$mean
describe(Rates_nozero$Dprime)$se
# The se's reported don't match, but now the mean values do!

# How many people have d' values at or below zero?
Dprime_zeros <- subset(Rates, Dprime <= 0)
nrow(Dprime_zeros) # 1 person
Dprime_zeros$Subject # 86 (might need this down the line)
# For now, to deal with this value, let's just replace it with NA so we don't lose the rest of the person's data in subsequent analyses.
Rates$Dprime <-  ifelse(Rates$Dprime <= 0, NA, Rates$Dprime)


rm(Dprime_zeros, Rates_nozero)


## Another sanity check here:
# They also report mean proportion correct for the entire sample (m= 0.77), which we can also calculate by summing the number of accurate responses / # of trials per participant:
Sub_totalRowCount <- data.frame(table(data$Subject))
names(Sub_totalRowCount) <- c("Subject", "TotalRowCount")
summary(as.factor(Sub_totalRowCount$TotalRowCount))

Acc_total <- data %>% group_by(Subject) %>% summarise(ACCtotal = sum(ACC)) %>% select(Subject, ACCtotal)

PercAcc <- merge(Acc_total, Sub_totalRowCount, by= c("Subject"), all=T)
PercAcc$PercAcc <- c(PercAcc$ACCtotal / PercAcc$TotalRowCount)

describe(PercAcc$PercAcc) 
# This matches their report value, which is great. However, it implies that we're doing something different when it comes to calculating d'.
# NEED TO CONTACT AUTHORS ABOUT THIS.

rm(Sub_totalRowCount, PercAcc, Acc_total)

####### 
####### Now we are ready for the actual analyses! 
####### 
####### 
####### 
####### 
####### 
####### Effects of listener and vocalizer gender
# FROM THEIR PAPER:
# Mixed factor ANOVAs were used to explore possible main effects and interactions relating to listener and vocalizer gender. 
# IVs: Vocalizer gender was used as a within-subjects factor and listener gender was used as a between-subjects factor, 
# DVs: d′ scores and mean response latencies used as output variables in two separate tests.

# For the analyses you're interested in, we need to identify the following columns in the data set:
head(data, 2)
## Participant gender - this is column "Sex"
## Vocalizer gender - "ScreamerSex"
## Response latency - "Actual_Latency"
## d': this is a little complicated, see my notes below on d' before calculating it.

summary(as.factor(data$ScreamerSex))
# According to the paper (table 1), there were more audio files with female screamers, so 0 must be female screamer.
# Let's create a factor variable so this is easier to remember
data$ScreamerSex_Factor <-  ifelse(data$ScreamerSex == 0, "Female_Screamer", "Male_Screamer")
summary(as.factor(data$ScreamerSex_Factor))

# Before we run the analyses, we need to make sure all our data is in the same place.
analyses <- merge(data, Rates, by=c("Subject"), all=T)

# To run a mixed-factor anova, we need to build our model so that it recognizes some observations come from the same person.
#install.packages("nlme")
library(nlme)

## FROM THEIR PAPER: MEAN RT AS OUTCOME

# Predicting MEAN response time- this is important, we need to calculate mean response times by screamer sex FIRST:
# We will do this for both RT variables since we're not sure which one they used.
Mean_RT <- analyses %>% group_by(Subject,ScreamerSex_Factor) %>% summarise(Mean_RT = mean(Stimulus.RT)) %>% select(Subject, ScreamerSex_Factor, Mean_RT)

Mean_ActualRT <- analyses %>% group_by(Subject,ScreamerSex_Factor) %>% summarise(Mean_ActualRT = mean(Actual_Latency)) %>% select(Subject, ScreamerSex_Factor, Mean_ActualRT)

# Let's double-check this worked correctly
test <- subset(analyses, Subject == 1)
with(test, tapply(test$Stimulus.RT, list(test$ScreamerSex_Factor), mean, na.rm = TRUE))
head(Mean_RT, 2)
# They match.
with(test, tapply(test$Actual_Latency, list(test$ScreamerSex_Factor), mean, na.rm = TRUE))
head(Mean_ActualRT, 2)
# They also match.
rm(test)

# Now, let's create a new data frame where we pull in the participant's sex.
SubSex1 <- subset(data_all, Trial == 1) # Creating a dataframe where there's only one row per person by simply subsetting by the row where the first trial occurs.
SubSex2 <- SubSex1[c("Subject", "Sex")] # Creating a new dataframe that only includes subject ID and subject sex.
Mean_RT_variables <- merge(Mean_RT, Mean_ActualRT, by=c("Subject","ScreamerSex_Factor"), all=T)
Mean_RT_analyses <- merge(Mean_RT_variables, SubSex2, by=c("Subject"), all=T) # Check out the new data frame, now we only have two rows per subject and a mean RT for each screamer sex.

# Removing extra data frames again
rm(Mean_RT, Mean_ActualRT, Mean_RT_variables, SubSex1, SubSex2)

# Sometimes there are discrepancies between anova models conducted in R and anova models conducted in SPSS. To make sure our models are working the same way, let's first make sure our IV's are factors.
str(Mean_RT_analyses)
# Screamer sex is a character string...let's turn it into a factor just in case.
Mean_RT_analyses$ScreamerSex_Factor <- as.factor(Mean_RT_analyses$ScreamerSex_Factor)
str(Mean_RT_analyses)

# Which RT variable should we use to replicate their analyses? Well, they report: 
# Main effect of vocalizer gender on mean response latencies (F(1, 102) = 16.18, p < 0.001, hp 2 = 0.137), 
# - > male screams (M= 1,120.63, SE = 57.44) 
# - > female screams (M = 972.64, SE = 43.93)
# So let's check to see if our mean values for screamer sex match these means.
with(Mean_RT_analyses, tapply(Mean_RT_analyses$Mean_RT, list(Mean_RT_analyses$ScreamerSex_Factor), mean, na.rm = TRUE))
with(Mean_RT_analyses, tapply(Mean_RT_analyses$Mean_ActualRT, list(Mean_RT_analyses$ScreamerSex_Factor), mean, na.rm = TRUE))
# It looks like the paper's reported values more closely match the mean actual RT value, so let's go with those.

# THEY FOUND:
# Main effect of vocalizer gender on mean response latencies (F(1, 102) = 16.18, p < 0.001), 
# Main effects of participant gender on mean response latencies (p = 0.064)
# Interaction between participant and vocalizer gender: (p = 0.498)

# Now let's test this model in TWO ways, using a combination of lme and the Anova function
GenderMod_rt <- lme(Mean_ActualRT ~ ScreamerSex_Factor * Sex, random= ~ScreamerSex_Factor|Subject, data = Mean_RT_analyses, method = "ML", na.action=na.omit)
anova(GenderMod_rt) # This is pretty close to the statistics they report.

# The sum of squares for anova in SPSS is set to 3, where as it's not in R for aov function. We can use the car package and the Anova function to override this.
# install.packages("car")
library(car)
# First a combination of the lm/aov function for our anova model:
GenderAOV_rt <- lm(aov(Mean_RT ~ Sex * ScreamerSex_Factor + Error(Subject/ScreamerSex_Factor), data = Mean_RT_analyses))
Anova(GenderAOV_rt, type = "III")



###################### 
######################
######################
###################### NOW, onto examining d' as an outcome
######################
######################

## FROM THEIR PAPER: D' AS OUTCOME
# Main effect of vocalizer gender on d′ scores (F(1, 102) = 4.10, p = 0.046, hp 2 = 0.039), male screams (M = 1.72, SE = 0.07) than female screams (M= 1.55, SE = 0.06).
# Main effects of participant gender on d′ scores (p = 0.802)
# Interaction between participant and vocalizer gender: (p = 0.492)

# Predicting d': model doesn't know how to deal because there is only one d' per person!!!
GenderMod_dprime <- lme(Dprime ~ Sex * ScreamerSex_Factor, random = ~1|ScreamerSex_Factor, data = analyses, method = "ML", na.action=na.omit)
anova(GenderMod_dprime)

# REPEATING D' ANALYSES WITH RAW DATA FOR TRIALS WITH FEMALE SCREAMERS
data_FV <- subset(data, ScreamerSex == 0)

# First, let's just check how many trials each participant has:
Sub_totalRowCount <- data.frame(table(data_FV$Subject))
names(Sub_totalRowCount) <- c("Subject", "TotalRowCount")
summary(as.factor(Sub_totalRowCount$TotalRowCount))

# Calculating HITS and FALSE ALARM RATES. We will subset by trial types first, then count number of trials.
DM_SV_trials <- subset(data_FV, StimType == 0 | StimType == 1)
Sub_DMSVRowCount <- data.frame(table(DM_SV_trials$Subject))
names(Sub_DMSVRowCount) <- c("Subject", "DMSV_RowCount")
summary(as.factor(Sub_DMSVRowCount$DMSV_RowCount))

# install.packages("dplyr")
library(dplyr) # needed for %>% functions below
# Now let's count the # of hits they made:
Hit_total <- DM_SV_trials %>% group_by(Subject) %>% summarise(Hits = sum(ACC)) %>% select(Subject, Hits)
# Next, let's combine our data frames where the total trial counts and total hits made have just been calculated:
Hit_rates <- merge(Hit_total, Sub_DMSVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Duration Modified and Same Vocalizer trials:
Hit_rates$Hit_Rate <- c(Hit_rates$Hits / Hit_rates$DMSV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# Now let's do the same thing for false alarms. 
# For FALSE ALARMS (just need Different Vocalizer trials)
DV_trials <- subset(data_FV, StimType == 2)
Sub_DVRowCount <- data.frame(table(DV_trials$Subject))
names(Sub_DVRowCount) <- c("Subject", "DV_RowCount")
summary(as.factor(Sub_DVRowCount$DV_RowCount))

# Summing the number of false alarms made
DV_trials_FalseAlarms <- subset(DV_trials, ACC == 0)
FalseAlarm_total <- data.frame(table(DV_trials_FalseAlarms$Subject))
names(FalseAlarm_total) <- c("Subject", "FalseAlarms")

# Next, let's combine our data frames where the total trial counts and total false alarms made have just been calculated:
FalseAlarm_rates <- merge(FalseAlarm_total, Sub_DVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Different Vocalizer trials:
FalseAlarm_rates$FalseAlarm_Rate <- c(FalseAlarm_rates$FalseAlarms / FalseAlarm_rates$DV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# People with no errors are marked as NAs, but really, they should be zeros.
FalseAlarm_rates[is.na(FalseAlarm_rates)] <- 0


# Now, let's look at these values side by side:
Rates_FV <- merge(Hit_rates, FalseAlarm_rates, by= c("Subject"), all=T)
# Main thing we want to check is that there is no one with 100% for hits AND 100% for false alarms, which could imply that they are just hitting the "Same" key over and over again.
# However, a perfect score on hits (100%) and false alarms (0%) also poses a problem for z-scoring, where 0 equal "negative infinity" and 1 equals "positive infinity". Run the analyses below and see what happens to the z-score values.


# Once again, removing dataframes we no longer need:
rm(DM_SV_trials, DV_trials, DV_trials_FalseAlarms, FalseAlarm_rates, FalseAlarm_total, Hit_rates, Hit_total, Sub_DMSVRowCount, Sub_DVRowCount, Sub_totalRowCount)


# Z-score the data using qnorm and calculate d' by subtracting the proportion of false alarms from hits:
Rates_FV$Hit_RateZ <- qnorm(Rates_FV$Hit_Rate)
Rates_FV$FalseAlarm_RateZ <- qnorm(Rates_FV$FalseAlarm_Rate)
Rates_FV$Dprime  <- Rates_FV$Hit_RateZ - Rates_FV$FalseAlarm_RateZ

# Dealing with the +/- infinity values:
# We can set the minimum % = 1/N where N is the number of trials used in the calculation of false alarms. This would be
Rates_FV$Min <- (1 / Rates_FV$DV_RowCount)
# We can set the maximum value for % = (N-1)/N where N is the number of trials used in the calculation of hits. This would be
Rates_FV$Max <- ((Rates_FV$DMSV_RowCount) - 1) / (Rates_FV$DMSV_RowCount)
# Adapted from http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^

# Now let's replace the rates that generated infinity values with the min and max values we just calculated, and then recalculate d prime.
Rates_FV$FalseAlarm_Rate <-  ifelse((Rates_FV$FalseAlarm_Rate == 0), Rates_FV$Min, Rates_FV$FalseAlarm_Rate)
Rates_FV$Hit_Rate <-  ifelse((Rates_FV$Hit_Rate == 1), Rates_FV$Max, Rates_FV$Hit_Rate)

Rates_FV$Hit_RateZ <- qnorm(Rates_FV$Hit_Rate)
Rates_FV$FalseAlarm_RateZ <- qnorm(Rates_FV$FalseAlarm_Rate)
Rates_FV$Dprime  <- Rates_FV$Hit_RateZ - Rates_FV$FalseAlarm_RateZ


# How many people have d' values at or below zero?
Dprime_zeros <- subset(Rates_FV, Dprime <= 0)
nrow(Dprime_zeros) # 2 people
Dprime_zeros$Subject # 18 and 86 (might need this down the line)
# For now, to deal with this value, let's just replace it with NA so we don't lose the rest of the person's data in subsequent analyses.
Rates_FV$Dprime_FV <-  ifelse(Rates_FV$Dprime <= 0, NA, Rates_FV$Dprime)


rm(Dprime_zeros)

##############
##############
##############
# REPEATING D' ANALYSES FOR TRIALS WITH MALE SCREAMERS
data_MV <- subset(data, ScreamerSex == 1)

# First, let's just check how many trials each participant has:
Sub_totalRowCount <- data.frame(table(data_MV$Subject))
names(Sub_totalRowCount) <- c("Subject", "TotalRowCount")
summary(as.factor(Sub_totalRowCount$TotalRowCount))

# Calculating HITS and FALSE ALARM RATES. We will subset by trial types first, then count number of trials.
DM_SV_trials <- subset(data_MV, StimType == 0 | StimType == 1)
Sub_DMSVRowCount <- data.frame(table(DM_SV_trials$Subject))
names(Sub_DMSVRowCount) <- c("Subject", "DMSV_RowCount")
summary(as.factor(Sub_DMSVRowCount$DMSV_RowCount))

# install.packages("dplyr")
library(dplyr) # needed for %>% functions below
# Now let's count the # of hits they made:
Hit_total <- DM_SV_trials %>% group_by(Subject) %>% summarise(Hits = sum(ACC)) %>% select(Subject, Hits)
# Next, let's combine our data frames where the total trial counts and total hits made have just been calculated:
Hit_rates <- merge(Hit_total, Sub_DMSVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Duration Modified and Same Vocalizer trials:
Hit_rates$Hit_Rate <- c(Hit_rates$Hits / Hit_rates$DMSV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# Now let's do the same thing for false alarms. 
# For FALSE ALARMS (just need Different Vocalizer trials)
DV_trials <- subset(data_MV, StimType == 2)
Sub_DVRowCount <- data.frame(table(DV_trials$Subject))
names(Sub_DVRowCount) <- c("Subject", "DV_RowCount")
summary(as.factor(Sub_DVRowCount$DV_RowCount))

# Summing the number of false alarms made
DV_trials_FalseAlarms <- subset(DV_trials, ACC == 0)
FalseAlarm_total <- data.frame(table(DV_trials_FalseAlarms$Subject))
names(FalseAlarm_total) <- c("Subject", "FalseAlarms")

# Next, let's combine our data frames where the total trial counts and total false alarms made have just been calculated:
FalseAlarm_rates <- merge(FalseAlarm_total, Sub_DVRowCount, by= c("Subject"), all=T)
# Finally, to calculate the hit rate, let's divide the number of hits made by the number of trials where hits were possible in the Different Vocalizer trials:
FalseAlarm_rates$FalseAlarm_Rate <- c(FalseAlarm_rates$FalseAlarms / FalseAlarm_rates$DV_RowCount)
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^


# People with no errors are marked as NAs, but really, they should be zeros.
FalseAlarm_rates[is.na(FalseAlarm_rates)] <- 0


# Now, let's look at these values side by side:
Rates_MV <- merge(Hit_rates, FalseAlarm_rates, by= c("Subject"), all=T)
# Main thing we want to check is that there is no one with 100% for hits AND 100% for false alarms, which could imply that they are just hitting the "Same" key over and over again.
# However, a perfect score on hits (100%) and false alarms (0%) also poses a problem for z-scoring, where 0 equal "negative infinity" and 1 equals "positive infinity". Run the analyses below and see what happens to the z-score values.


# Once again, removing dataframes we no longer need:
rm(DM_SV_trials, DV_trials, DV_trials_FalseAlarms, FalseAlarm_rates, FalseAlarm_total, Hit_rates, Hit_total, Sub_DMSVRowCount, Sub_DVRowCount, Sub_totalRowCount)


# Z-score the data using qnorm and calculate d' by subtracting the proportion of false alarms from hits:
Rates_MV$Hit_RateZ <- qnorm(Rates_MV$Hit_Rate)
Rates_MV$FalseAlarm_RateZ <- qnorm(Rates_MV$FalseAlarm_Rate)
Rates_MV$Dprime  <- Rates_MV$Hit_RateZ - Rates_MV$FalseAlarm_RateZ

# Dealing with the +/- infinity values:
# We can set the minimum % = 1/N where N is the number of trials used in the calculation of false alarms. This would be
Rates_MV$Min <- (1 / Rates_MV$DV_RowCount)
# We can set the maximum value for % = (N-1)/N where N is the number of trials used in the calculation of hits. This would be
Rates_MV$Max <- ((Rates_MV$DMSV_RowCount) - 1) / (Rates_MV$DMSV_RowCount)
# Adapted from http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
# POSSIBLY DEVIATION FROM AUTHORS' ANALYSIS ^^

# Now let's replace the rates that generated infinity values with the min and max values we just calculated, and then recalculate d prime.
Rates_MV$FalseAlarm_Rate <-  ifelse((Rates_MV$FalseAlarm_Rate == 0), Rates_MV$Min, Rates_MV$FalseAlarm_Rate)
Rates_MV$Hit_Rate <-  ifelse((Rates_MV$Hit_Rate == 1), Rates_MV$Max, Rates_MV$Hit_Rate)

Rates_MV$Hit_RateZ <- qnorm(Rates_MV$Hit_Rate)
Rates_MV$FalseAlarm_RateZ <- qnorm(Rates_MV$FalseAlarm_Rate)
Rates_MV$Dprime  <- Rates_MV$Hit_RateZ - Rates_MV$FalseAlarm_RateZ


# How many people have d' values at or below zero?
Dprime_zeros <- subset(Rates_MV, Dprime <= 0)
nrow(Dprime_zeros) # 2 people
Dprime_zeros$Subject # 18 and 86 (might need this down the line)
# For now, to deal with this value, let's just replace it with NA so we don't lose the rest of the person's data in subsequent analyses.
Rates_MV$Dprime_MV <-  ifelse(Rates_MV$Dprime <= 0, NA, Rates_MV$Dprime)


rm(Dprime_zeros)


##############
##############
##############

# Merging FV and MV data frames:
FV <- Rates_FV[c("Subject", "Dprime_FV")]
MV <- Rates_MV[c("Subject", "Dprime_MV")]

Dprime_byVocSex <- merge(FV, MV, by=c("Subject"))

#Now we need to turn it into a repeated measures dataframe, where every participant has two rows. To do that we'll use the gather function from the tidyr library.
# install.packages("tidyr")
library(tidyr)
head(Dprime_byVocSex)
Dprime_byVocSex_long <- gather(Dprime_byVocSex, Type, Dprime, Dprime_FV:Dprime_MV, factor_key=TRUE)
# And let's add a new column that makes the screamer sex "Male" or "Female" and that will make it easier to merge with our existing RT analyses data frame
head(Mean_RT_analyses)
Dprime_byVocSex_long$ScreamerSex_Factor <- ifelse(Dprime_byVocSex_long$Type=="Dprime_FV", "Female_Screamer", "Male_Screamer")


# Now let's merge this with our existing data frame, Mean_RT analyses
MeanRT_Dprime_analyses <- merge(Mean_RT_analyses, Dprime_byVocSex_long, by=c("Subject", "ScreamerSex_Factor"))



###################### NOW we're ready to try our model on dprime outcomes again
######################
######################

## FROM THEIR PAPER: D' AS OUTCOME
# Main effect of vocalizer gender on d′ scores (F(1, 102) = 4.10, p = 0.046, hp 2 = 0.039), male screams (M = 1.72, SE = 0.07) than female screams (M= 1.55, SE = 0.06).
# Main effects of participant gender on d′ scores (p = 0.802)
# Interaction between participant and vocalizer gender: (p = 0.492)

# Do our d' measures by vocalizer sex mirror theirs?
# male screams (M = 1.72, SE = 0.07) 
# female screams (M= 1.55, SE = 0.06)
with(MeanRT_Dprime_analyses, tapply(MeanRT_Dprime_analyses$Dprime, list(MeanRT_Dprime_analyses$ScreamerSex_Factor), mean, na.rm = TRUE))


# Now let's test this model in TWO ways, using a combination of lme and the Anova function
GenderMod_dprime <- lme(Dprime ~ ScreamerSex_Factor * Sex, random= ~ScreamerSex_Factor|Subject, data = MeanRT_Dprime_analyses, method = "ML", na.action=na.omit)
anova(GenderMod_dprime) # This is pretty close to the statistics they report.

# The sum of squares for anova in SPSS is set to 3, where as it's not in R for aov function. We can use the car package and the Anova function to override this.
# install.packages("car")
library(car)
# First a combination of the lm/aov function for our anova model:
GenderAOV_dprime <- lm(aov(Dprime ~ Sex * ScreamerSex_Factor + Error(Subject/ScreamerSex_Factor), data = MeanRT_Dprime_analyses))
Anova(GenderAOV_dprime, type = "III")
