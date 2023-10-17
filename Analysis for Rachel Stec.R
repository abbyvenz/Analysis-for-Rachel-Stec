# Determining if the (overall) test scores changed from Pre- to Post-Intervention:

## Reading in/naming the data
TestScores <- read.csv("https://raw.githubusercontent.com/abbyvenz/Data-for-Rachel-Stec/main/Pre%20and%20Post%20Test%20Scores-%20Urinary%20Catheter.csv")

## Getting summary statistics of the test scores (i.e., mean & SD)
install.packages('rstatix'); library(rstatix)
get_summary_stats(TestScores[,2:3], type = "mean_sd")

## Conducting a paired t-test to determine the difference & its significance 
t.test(TestScores$Post, TestScores$Pre, paired = TRUE) # = +1.8235 



# Identifying the difference (if any) in CAUTI Rate, Pre- vs. Post-Intervention:

## Reading in/naming the data
CAUTI <- read.csv("https://raw.githubusercontent.com/abbyvenz/Data-for-Rachel-Stec/main/CAUTI%20Rate%20Data.csv")

## Getting parameters for the analysis
total.CAUTI = sum(CAUTI$X..of.CAUTI[-7]) 
total.res.days = sum(CAUTI$Resident.Days[-7]) 

pre.CAUTI = sum(CAUTI$X..of.CAUTI[1:6])
pre.res.days = sum(CAUTI$Resident.Days[1:6]) 

post.CAUTI = sum(CAUTI$X..of.CAUTI[8:9]) 
post.res.days = sum(CAUTI$Resident.Days[8:9])

expected.post.ratio = post.res.days/(post.res.days + pre.res.days)
expected.post.CAUTI = total.CAUTI*expected.post.ratio

## Analyzing the difference in CAUTI Rates
dbinom(post.CAUTI, total.CAUTI, expected.post.ratio) # = .066295

total = 10*(sum(dbinom(post.CAUTI:total.CAUTI, total.CAUTI, expected.post.ratio))*2) # = 2.22298

pchisq(total, df = 2-1, lower.tail = FALSE) # = 0.1354



# Summarizing the Post-Intervention Nursing Survey, Graphically:

## Reading in/naming data
PostSurvey <- read.csv("https://raw.githubusercontent.com/abbyvenz/Data-for-Rachel-Stec/main/Post%20Intervention%20Nursing%20Survey.csv")

## Getting summary statistics of each question
get_summary_stats(PostSurvey[,2:8], type = "mean_sd")

## Stacking the data
Stacked.PostSurvey <- cbind(PostSurvey[1], stack(PostSurvey[2:8]))

## Graphing the data
library(ggplot2)

ggplot(Stacked.PostSurvey, aes(ind, values)) +
  scale_y_continuous(breaks = 1:7, labels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Agree", "Strongly agree")) +
  ylim("Strongly disagree", "Disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Agree", "Strongly agree") +
  ggtitle("Results of the Post-Intervention Survey, Measuring Nurses' Confidence and Self-Efficacy") +
  xlab("Question") +
  ylab("Self-Perceived Confidence (on a 7-point Likert Scale)") +
  geom_boxplot() + coord_flip() + geom_point()




