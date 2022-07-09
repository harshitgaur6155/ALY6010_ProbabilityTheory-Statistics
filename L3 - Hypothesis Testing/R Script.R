#---------------------- Week_3_Module_3_R-Script ----------------------#

print("Author : Harshit Gaur")
print("Week 3 Assignment - Module 3 R Pratice")

# Importing the packages.
listOfPackages <- c(
  "dplyr", "tidyr", "plyr", "tidyverse", "RColorBrewer", "plotrix", "scales", "ggplot2",
  "data.table", "reshape", "gridExtra", "vtable", "moments", "ggpubr", "psych", "GGally"
)

for (package in listOfPackages) {
  if (package %in% rownames(installed.packages()) == FALSE)
    { install.packages(package) }
  
  # Importing the package.
  library(package, character.only = TRUE)
}


# STEP 2: Import data set
# Note: Change the working directory as per the file's location.
setwd("/Users/HarshitGaur/Documents/Northeastern University/MPS Analytics/ALY 6010/Class 3/R Practice Assignment/")
pokemon_dataset <- read.csv("pokemons dataset.csv", header = TRUE)

# Display the data set.
View(pokemon_dataset)

# Print the structure of 'pokemons dataset.csv' data set
str(pokemon_dataset)
# Print the summary of 'pokemons dataset.csv' data set
summary(pokemon_dataset)
st(pokemon_dataset)

# Describe the summary of the data set by providing Descriptive Statistics.
View(describe(pokemon_dataset, skew = FALSE, quant = c(0.25, 0.75), IQR = TRUE))


#------------------- Data Cleaning -------------------#

# To check inconsistencies in the 'Primary Type - Character' feature of the data set.
unique(pokemon_dataset$Primary.Type)

# To check NA, NULL values in the data set.
sum(is.na(pokemon_dataset))
sum(is.null(pokemon_dataset))

# Replacing Empty Values in the features with the word 'NoName & NoType'
pokemon_dataset$Name2 <- gsub('^$', 'NoName', pokemon_dataset$Name2)
pokemon_dataset$Secondary.type <- gsub('^$', 'NoType', pokemon_dataset$Secondary.type)

# Check the duplicate values in a combination of 2 features.
duplicated(pokemon_dataset[,1:2])
# Retrieving the duplicated records from the data set.
pokemon_dataset[which(duplicated(pokemon_dataset[,1:2])),]
# Eliminating the duplicated records using the indexes provided by the above step.
pokemon_dataset <- pokemon_dataset %>% filter( !row_number() %in% 44)

# Removing 'NA, Missing Values' from the data set.
pokemon_dataset <- na.omit(pokemon_dataset)


#------------------- Exploratory Data Analysis -------------------#

# Check Normality using Density Graphs of all the univariates.
normality_attack <- ggdensity(pokemon_dataset$Attack, main = "Density plot of Attack", xlab = "Attack", fill = "#ffa514")
normality_defense <- ggdensity(pokemon_dataset$Defense, main = "Density plot of Defense", xlab = "Defense", fill = "#edf759")
normality_hp <- ggdensity(pokemon_dataset$HP, main = "Density plot of Hit Points", xlab = "Hit Points", fill = "#baf54c")
normality_spAttack <- ggdensity(pokemon_dataset$Sp.Attack, main = "Density plot of Special Attack", xlab = "Special Attack", fill = "#4cf5bd")
normality_spDefense <- ggdensity(pokemon_dataset$Sp.Defense, main = "Density plot of Special Defense", xlab = "Special Defense", fill = "#40c7f7")
normality_speed <- ggdensity(pokemon_dataset$Speed, main = "Density plot of Speed", xlab = "Speed", fill = "#d27afa")
normality_total <- ggdensity(pokemon_dataset$Total, main = "Density plot of Total Attributes", xlab = "Total Attributes", fill = "#eabdff")
grid.arrange(normality_attack,normality_defense,normality_hp,normality_spAttack,normality_spDefense,normality_speed,normality_total)

# Check Normality using Shapiro-Wilks Test
format(shapiro.test(pokemon_dataset$Attack)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$Defense)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$HP)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$Sp.Attack)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$Sp.Defense)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$Speed)$p.value, scientific = FALSE)
format(shapiro.test(pokemon_dataset$Total)$p.value, scientific = FALSE)
format(shapiro.test(log10(pokemon_dataset$Attack))$p.value, scientific = FALSE)

#-------- Check Normality using Q-Q Plot of all the numeric features. --------#

# Function to plot graph
qq_plot <- function(numeric_feature, mainTitle) {
  qqnorm(numeric_feature, pch = 5, frame = TRUE, main = mainTitle)
  qqline(numeric_feature, col = "#ffa514", lwd = 2)
}

# Changing Plot Matrix Size to 3x3.
par(mfrow = c(3,3))

# Check Normality using Q-Q Plot of 'Attack' Feature.
qq_plot(pokemon_dataset$Attack, "Attack")

# Check Normality using Q-Q Plot of 'Defense' Feature.
qq_plot(pokemon_dataset$Defense, "Defense")

# Check Normality using Q-Q Plot of 'HP' Feature.
qq_plot(pokemon_dataset$HP, "Horse Power")

# Check Normality using Q-Q Plot of 'Special Attack' Feature.
qq_plot(pokemon_dataset$Sp.Attack, "Special Attack")

# Check Normality using Q-Q Plot of 'Special Defense' Feature.
qq_plot(pokemon_dataset$Sp.Defense, "Special Defense")

# Check Normality using Q-Q Plot of 'Speed' Feature.
qq_plot(pokemon_dataset$Speed, "Speed")

# Check Normality using Q-Q Plot of 'Total Attributes' Feature.
qq_plot(pokemon_dataset$Total, "Total Attributes")

# Resetting Plot Matrix Size to 1x1.
par(mfrow = c(1,1))

# Check Skewness of the features 
skewness(pokemon_dataset$Attack)
skewness(pokemon_dataset$Defense)
skewness(pokemon_dataset$HP)
skewness(pokemon_dataset$Sp.Attack)
skewness(pokemon_dataset$Sp.Defense)
skewness(pokemon_dataset$Speed)
skewness(pokemon_dataset$Total)

# Check Skewness of the features 
kurtosis(pokemon_dataset$Attack)
kurtosis(pokemon_dataset$Defense)
kurtosis(pokemon_dataset$HP)
kurtosis(pokemon_dataset$Sp.Attack)
kurtosis(pokemon_dataset$Sp.Defense)
kurtosis(pokemon_dataset$Speed)
kurtosis(pokemon_dataset$Total)

# Describe the summary of the data set by providing Descriptive Statistics.
View(describe(pokemon_dataset, skew = FALSE, quant = c(0.25, 0.75), IQR = TRUE))

# Describe the summary of the data set by grouping
describeBy(pokemon_dataset, group = pokemon_dataset$Primary.Type)
describeBy(pokemon_dataset, group = pokemon_dataset$Secondary.type)


# ----------- Checking Normality of Features ----------- #

# Changing Plot Matrix Size to 3x3.
par(mfrow = c(2,2))

# Check Normality using Q-Q Plot of 'HP' and Log 10 of 'HP' Feature.
qq_plot(pokemon_dataset$HP, "Horse Power")

hp_log10 <- log10(pokemon_dataset$HP)
qq_plot(hp_log10, "Log 10 of Horse Power")

# Changing Plot Matrix Size to 3x3.
par(mfrow = c(1,1))

gd1 <- ggdensity(pokemon_dataset$HP, main = "Density Plot of Horse Power", xlab = "Horse Power Attribute", fill = "#eabdff")
gd2 <- ggdensity(hp_log10, main = "Density Plot of Log 10 (Horse Power)", xlab = "Log 10 (Horse Power) Attribute", fill = "#eabdff")

grid.arrange(gd1, gd2)


# ----------- One Sample Testing (t-Test) ----------- #

# Means of 'Attributes' according to Primary Type feature
groupPokemons <- pokemon_dataset %>% dplyr::group_by(pokemon_dataset$Primary.Type) %>% dplyr::summarise(
  mean_attack = mean(Attack),
  mean_defense = mean(Defense),
  mean_hp = mean(HP),
  mean_spAttack = mean(Sp.Attack),
  mean_spDefense = mean(Sp.Defense),
  mean_speed = mean(Speed),
  mean_total = mean(Total),
)

mean_Attack_WaterPokemon <- groupPokemons$mean_attack[groupPokemons$`pokemon_dataset$Primary.Type` == "WATER"]
mean_HP_WaterPokemon <- groupPokemons$mean_hp[groupPokemons$`pokemon_dataset$Primary.Type` == "WATER"]
mean_HP_PsychicPokemon <- groupPokemons$mean_hp[groupPokemons$`pokemon_dataset$Primary.Type` == "PSYCHIC"]
View(describe(groupPokemons))

# One-Sample t-test for 'Mean of Attack of Water type Pokemon'
t.test(pokemon_dataset$Attack, mu = mean_Attack_WaterPokemon, alternative = "greater", conf.level = .95)

# One-Sample t-test for 'Mean of Horse Power of Water type Pokemon'
t.test(pokemon_dataset$HP, mu = mean_HP_WaterPokemon, alternative = "greater")

# One-Sample t-test for 'Mean of Horse Power of Psychic type Pokemon'
t.test(pokemon_dataset$HP, mu = mean_HP_PsychicPokemon, alternative = "less")


# ----------- Two Sample Testing (t-Test) ----------- #

# Two-Sample t-test for 'Mean of Attack of Water vs Psychic type Pokemon'
t.test(pokemon_dataset$Attack[pokemon_dataset$Primary.Type == "WATER"], pokemon_dataset$Attack[pokemon_dataset$Primary.Type == "PSYCHIC"], alternative = "two.sided")

# Two-Sample t-test for 'Mean of Special Attack of Water vs Psychic type Pokemon'
t.test(pokemon_dataset$Sp.Attack[pokemon_dataset$Primary.Type == "WATER"], pokemon_dataset$Sp.Attack[pokemon_dataset$Primary.Type == "PSYCHIC"], alternative = "two.sided")


# ----------- Hypothesis Testing (P-Test) ----------- #

# Degree of Freedom
deg_of_freedom <- nrow(pokemon_dataset) - 1

# Population Mean (X Bar)
population_mean <- mean(pokemon_dataset$Attack)

# Standard Deviation of Sample (SD)
sd <- sd(pokemon_dataset$Attack)

# Test Statistics (t-score)
t_score <- (population_mean - mean_Attack_WaterPokemon) / (sd / sqrt(nrow(pokemon_dataset)))

# Hypothesis Testing (p-test)
pt(t_score, deg_of_freedom, lower.tail = FALSE)


  
  