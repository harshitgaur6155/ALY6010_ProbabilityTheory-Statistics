#---------------------- Week_2_Module_2_R-Script ----------------------#

print("Author : Harshit Gaur")
print("Week 2 Assignment - Module 2 R Pratice")

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
setwd("/Users/HarshitGaur/Documents/Northeastern University/MPS Analytics/ALY 6010/Class 2/R Practice Assignment/")
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

# Replacing Empty Values in the features with the word 'Unknown'
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
shapiro.test(pokemon_dataset$Attack)

# Check Normality using Q-Q Plot of all the numeric features.

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


# Plot an Grouped Line graph for attack using 'ggplot'
# ----------- Plot 1: Primary Type vs Summation of Attributes ----------- #

# Summation of 'Attributes' according to Primary Type feature
summationTable <- pokemon_dataset %>% group_by(pokemon_dataset$Primary.Type) %>% dplyr::summarise(
                    sum_attack = sum(Attack),
                    sum_defense = sum(Defense),
                    sum_hp = sum(HP),
                    sum_spAttack = sum(Sp.Attack),
                    sum_spDefense = sum(Sp.Defense),
                    sum_speed = sum(Speed),
                    sum_total = sum(Total),
                  )

# Converting the summation Table into a Data Frame.
summationTable <- data.frame((summationTable))

par(mar = c(4, 10, 8, 4) + 0.1)
ggplot(summationTable, aes(x = pokemon_dataset.Primary.Type, group = 1)) +
  geom_line(aes(y = sum_attack, color="Attack"), linetype="dotted",  size=1) +
  geom_line(aes(y = sum_defense, color="Defense"), linetype="longdash", size=1) +
  geom_line(aes(y = sum_hp, color="HP"), linetype="F1", size=1) +
  geom_line(aes(y = sum_spAttack, color="Special Attack"), linetype="F1", size=1) +
  geom_line(aes(y = sum_spDefense, color="Special Attack"), linetype="solid", size=1) +
  geom_line(aes(y = sum_speed, color="Speed"), linetype="twodash", size=1) +
  scale_color_manual(
    name = "Legends", 
    values = c("Attack" = "red", "Defense" = "blue", "HP" = "yellow", "Special Attack" = "orange", "Special Defense" = "Darkgreen", "Speed" = "magenta")) +
  theme(
    panel.background = element_rect("#c9f5e4"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)
  ) +
  labs(
    title = "Plot 2: Primary Type vs Summation of Attributes",
    x = "Primary Type",
    y = "Summation of Attributes"
  ) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#7F3D17", face = "bold"))


# Plot an Scatter Plot Matrix graph for 'Attributes' using 'pairs'
# ----------- Plot 2: Scatter Plot Matrix between Abilities (Attributes) ----------- #

pairs(~Attack + Defense + HP + Sp.Attack + Sp.Defense + Speed, pokemon_dataset, col=c("YELLOW", "RED"), pch = 5)


# Plot an Scatter Chart graph for 'Primary Type vs Attack' using 'ggplot'
# ----------- Plot 3: Scatter Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Primary.Type)) +
  geom_point() +
  labs(title = 'Scatter Plot of Primary Type vs Attack', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Plot an Jitter Chart graph for 'Primary Type vs Attack' using 'ggplot'
# ----------- Plot 4: Jittered Scatter Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Primary.Type)) +
  geom_jitter(position = position_jitterdodge()) +
  labs(title = 'Jitter Plot of Primary Type vs Attack', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Plot an Scatter Chart graph for 'Attack vs Defense (Grouped in 3 Types)' using 'ggplot'
# ----------- Plot 5: Scatter Plot of Attack vs Defense (Grouped in 3 Types) ----------- #
dgw_group <- filter(pokemon_dataset, Primary.Type %in% c("DRAGON", "WATER", "PSYCHIC"))

par(mar = c(2,4,2,4))
ggplot(data = dgw_group, aes(x = Attack, y = Defense, color = Primary.Type)) +
  geom_point() +
  labs(title = 'Scatter Plot of Attack vs Defense', x = 'Attack Attribute', y = 'Defense Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  geom_smooth(se = FALSE)


# Plot an Scatter Chart graph for 'Primary Type vs Attack varied with Defense' using 'ggplot'
# ----------- Plot 6: Scatter Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Defense)) +
  geom_point() +
  labs(title = 'Scatter Plot of Primary Type vs Attack varied with Defense', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Plot an Jitter Chart graph for 'Primary Type vs Attack varied with Defense' using 'ggplot'
# ----------- Plot 7: Jitter Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Defense)) +
  geom_jitter(position = position_jitterdodge()) +
  labs(title = 'Jitter Plot of Primary Type vs Attack varied with Defense', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Plot an Box Plot for 'Primary Type vs Attack' using 'ggplot'
# ----------- Plot 8: Box Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Primary.Type)) +
  geom_boxplot(outlier.size = 3.5) +
  labs(title = 'Box Plot of Primary Type vs Attack varied with Defense', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))


# Plot an Jitter Chart graph for 'Primary Type vs Attack varied with Defense' using 'ggplot'
# ----------- Plot 9: Jittered Box Plot of Primary Type vs Attack ----------- #

par(mar = c(2,4,2,4))
ggplot(data = pokemon_dataset, aes(x = Primary.Type, y = Attack, color = Primary.Type)) +
  geom_boxplot(outlier.size = 3.5, outlier.stroke = 2) +
  geom_jitter(position = position_jitterdodge()) +
  labs(title = 'Jitter Plot of Primary Type vs Attack', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))



d <- filter(pokemon_dataset, Primary.Type %in% c("WATER"))
ggplot(data = d, aes(y = Sp.Attack, x = Attack, color = Primary.Type)) +
  geom_point() +
  labs(title = 'Scatter Plot of Attack attribute vs Total', x = 'Primary Type', y = 'Attack Attribute') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) + 
  geom_smooth(method = "lm")

summary(lm(d$Attack ~ d$Sp.Attack))

