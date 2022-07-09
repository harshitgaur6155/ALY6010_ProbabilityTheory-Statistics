#---------------------- Week_4_Module_4_R-Script ----------------------#

print("Author : Harshit Gaur")
print("Week 4 Assignment - Module 4 R Pratice")

# Importing the packages.
listOfPackages <- c(
  "MASS",
  "dplyr", "tidyr", "plyr", "tidyverse", "RColorBrewer", "plotrix", "scales", "ggplot2",
  "data.table", "reshape", "gridExtra", "vtable", "moments", "ggpubr", "psych", "GGally"
)

for (package in listOfPackages) {
  if (package %in% rownames(installed.packages()) == FALSE)
    { install.packages(package) }
  
  # Importing the package.
  library(package, character.only = TRUE)
}

# Use & Display the 'cats' data set.
data(cats, package = "MASS")
View(cats)

# Print the structure of 'cats' data set
str(cats)
# Print the summary of 'cats' data set
summary(cats)
st(cats)

# Describe the summary of the data set by providing Descriptive Statistics.
View(describe(cats, skew = FALSE, quant = c(0.25, 0.75), IQR = TRUE))


#------------------- 1st Question -------------------#

# Filter the data set based on 'Sex (M/F)' and Make it a vector using 'Body weight (Bwt)' feature
male_cats_Bwt <- cats %>% filter(Sex == "M") %>% pull(Bwt)
female_cats_Bwt <- cats %>% filter(Sex == "F") %>% pull(Bwt)

# Describe the summary of both the Gender based vectors by providing Descriptive Statistics.
View(rbind
     ("Male" = describe(male_cats_Bwt, skew = FALSE, quant = c(0.25, 0.75), IQR = TRUE), 
     "Female" = describe(female_cats_Bwt, skew = FALSE, quant = c(0.25, 0.75), IQR = TRUE)))


#------------------- Exploratory Data Analysis -------------------#

# Check Normality using Density Graphs of all the uni-variates.
normality_male_bwt <- ggdensity(male_cats_Bwt, main = "Density plot of Male Cats' Bodyweight", xlab = "Male Cats' Bodyweight", fill = "#ffa514") +
                        stat_overlay_normal_density(color = 'red', linetype = 'dashed')

normality_female_bwt <- ggdensity(female_cats_Bwt, main = "Density plot of Female Cats' Bodyweight", xlab = "Female Cats' Bodyweight", fill = "#edf759") +
                          stat_overlay_normal_density(color = 'red', linetype = 'dashed')

grid.arrange(normality_male_bwt,normality_female_bwt)

# Check Normality using Shapiro-Wilks Test
format(shapiro.test(male_cats_Bwt)$p.value, scientific = FALSE)
format(shapiro.test(female_cats_Bwt)$p.value, scientific = FALSE)


#-------- Check Normality using Q-Q Plot of all the numeric features. --------#

# Function to plot graph
qq_plot <- function(numeric_feature, mainTitle) {
  qqnorm(numeric_feature, pch = 5, frame = TRUE, main = mainTitle)
  qqline(numeric_feature, col = "#ffa514", lwd = 2)
}

# Changing Plot Matrix Size to 1x2.
par(mfrow = c(2,1))

# Check Normality using Q-Q Plot of 'male_cats_Bwt' Features.
qq_plot(male_cats_Bwt, "Male Cats' Bodyweight Q-Q Plot")

# Check Normality using Q-Q Plot of 'female_cats_Bwt' Features.
qq_plot(female_cats_Bwt, "Female Cats' Bodyweight Q-Q Plot")

# Resetting Plot Matrix Size to 1x1.
par(mfrow = c(1,1))

# Check Skewness of the features 
skewness(male_cats_Bwt)
skewness(female_cats_Bwt)

# Check Kurtosis of the features 
kurtosis(male_cats_Bwt)
kurtosis(female_cats_Bwt)


# ----------- Two Sample Testing (t-Test) ----------- #

# Two-Sample t-test for 'Bodyweight of Male Cats vs Bodyweight of Female Cats'
ttest <- t.test(male_cats_Bwt, female_cats_Bwt, alternative = "two.sided")
format(ttest$p.value, scientific = FALSE)

# ----------- One Sample Testing (t-Test) ----------- #

# One-Sample t-test for 'Bodyweight of Male Cats vs Female Cats'
ttest <- t.test(male_cats_Bwt, female_cats_Bwt, alternative = "greater")
format(ttest$p.value, scientific = FALSE)


#------------------- 2nd Question -------------------#

# Average Sleeping Quality Scores BEFORE workshop (Pre-Meditation)
avg_scores_pre_med <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)

# Average Sleeping Quality Scores AFTER workshop (Post-Meditation)
avg_scores_post_med <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

# Grouping of the Meditation data.
sleeping_data <- data.frame(meditation_status = rep(c("pre", "post"), each = 10), 
                       sleep_score = c(avg_scores_pre_med,  avg_scores_post_med))

group_by(sleeping_data, meditation_status) %>% 
  dplyr::summarise(
    count = n(), 
    mean = mean(sleep_score, na.rm = TRUE), 
    sd = sd(sleep_score, na.rm = TRUE))

# Plot an Box Plot for 'Average Sleeping Score Pre and Post Meditation' using 'ggplot'
# ----------- Plot 1: Average Sleeping Score Pre vs Post Meditation ----------- #

par(mar = c(2,4,2,4))
ggplot(data = sleeping_data, aes(x = meditation_status, y = sleep_score, color = meditation_status)) +
  geom_boxplot(outlier.size = 3.5) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="steelblue") +
  labs(title = 'Box Plot of Average Sleeping Score Pre vs Post Meditation', x = 'Meditation Status', y = 'Average Sleeping Score') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Check Normality using Density Graphs.

# Computing the difference in between the sleeping average score of Pre- and Post- Meditation.
avg_sleep_score_diff <- with(sleeping_data, sleep_score[meditation_status == "pre"] - sleep_score[meditation_status == "post"])

par(mar = c(4,4,4,4))
ggdensity(avg_sleep_score_diff, main = "Density Plot of Difference in Average Sleeping Score", xlab = "Difference in Average Sleeping Score of Pre & Post Meditation", fill = "#40c7f7") +
  stat_overlay_normal_density(color = 'red', linetype = 'dashed')

# Check Normality using Shapiro-Wilks Test
format(shapiro.test(avg_sleep_score_diff)$p.value, scientific = FALSE)

par(mar = c(4,4,4,4))
# Check Normality using Q-Q Plot of 'male_cats_Bwt' Features.
qq_plot(avg_sleep_score_diff, "Q-Q Plot of Difference in Avg Sleeping Score of Pre- and Post- Meditation")


# ----------- Paired Testing (t-Test) ----------- #

# Paired t-test for 'Pre Meditation vs Post Meditation Sleep Score'
paired_ttest <- t.test(data = sleeping_data, sleep_score ~ meditation_status, paired = TRUE, alternative = "greater")
format(paired_ttest$p.value, scientific = FALSE)


ggplot(data = cats, aes(y = cats$Bwt, x = cats$Hwt)) +
  geom_point() +
  labs(title = "Scatter Plot of Cats' Bodyweight vs Heartweight", x = "Heart Weight", y = 'Body Weight') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) + 
  geom_smooth(method = "lm")

summary(lm(cats$Bwt ~ cats$Hwt))



