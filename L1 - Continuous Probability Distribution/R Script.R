#---------------------- Week_1_Module_1 R Script ----------------------#

print("Author : Harshit Gaur")
print("Week 1 Assignment - Module 1 R Pratice")

# Importing the packages.
listOfPackages <- c(
  "dplyr", "tidyr", "plyr", "tidyverse", "RColorBrewer", "plotrix", "scales", "ggplot2",
  "data.table", "reshape", "gridExtra", "vtable", "moments", "ggpubr"
)

for (package in listOfPackages) {
  if (package %in% rownames(installed.packages()) == FALSE)
    { install.packages(package) }
  
  # Importing the package.
  library(package, character.only = TRUE)
}


# STEP 2: Import data set
# Note: Change the working directory as per the file's location.
setwd("/Users/HarshitGaur/Documents/Northeastern University/MPS Analytics/ALY 6010/Class 1/Assignment/")
pokemon_dataset <- read.csv("pokemons dataset.csv", header = TRUE)

# Display the data set.
View(pokemon_dataset)

# Print the structure of 'pokemons dataset.csv' data set
str(pokemon_dataset)
# Print the summary of 'pokemons dataset.csv' data set
summary(pokemon_dataset)
st(pokemon_dataset)


#------------------- Data Cleaning -------------------#

# To check inconsistencies in the 'Primary Type - Character' feature of the data set.
unique(pokemon_dataset$Primary.Type)

# To check NaN, NULL values in the data set.
sum(is.na(pokemon_dataset))
sum(is.null(pokemon_dataset))

# Replacing Empty Values in the features with the word 'Unknown'
pokemon_dataset$Name2 <- gsub('^$', 'Unknown', pokemon_dataset$Name2)
pokemon_dataset$Secondary.type <- gsub('^$', 'Unknown', pokemon_dataset$Secondary.type)

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
normality_attack <- ggdensity(pokemon_dataset$Attack, main = "Density plot of Attack", xlab = "Attack")
normality_defense <- ggdensity(pokemon_dataset$Defense, main = "Density plot of Defense", xlab = "Defense")
normality_hp <- ggdensity(pokemon_dataset$HP, main = "Density plot of Hit Points", xlab = "Hit Points")
normality_spAttack <- ggdensity(pokemon_dataset$Sp.Attack, main = "Density plot of Special Attack", xlab = "Special Attack")
normality_spDefense <- ggdensity(pokemon_dataset$Sp.Defense, main = "Density plot of Special Defense", xlab = "Special Defense")
normality_speed <- ggdensity(pokemon_dataset$Speed, main = "Density plot of Speed", xlab = "Speed")
normality_total <- ggdensity(pokemon_dataset$Total, main = "Density plot of Total Attributes", xlab = "Total Attributes")
grid.arrange(normality_attack,normality_defense,normality_hp,normality_spAttack,normality_spDefense,normality_speed,normality_total)

# Check Normality using Shapiro-Wilks Test
shapiro.test(pokemon_dataset$Attack)

# Check Normality using Q-Q Plot of a single Feature.
qqnorm(pokemon_dataset$Attack); 
qqline(pokemon_dataset$Attack, col="green", lwd=2)

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

# Check if the factors of 'Primary Type' can be used for Frequency table.
factor(pokemon_dataset$Primary.Type)

# Create a Frequency Table
freqTable_primaryType <- count(pokemon_dataset, 'Primary.Type')
colnames(freqTable_primaryType) <- c("Primary Type", "Frequency")


# Plot an bar graph for 'Primary Type' using 'ggplot'
# ----------- Plot 1: Primary Type Distribution ----------- #

par(mar = c(2, 6, 3, 2) + 0.1)
ggplot(freqTable_primaryType)+
  geom_bar(
    mapping = aes(
      x = `Primary Type`, 
      y = Frequency
    ),
    stat="identity"
  ) +
  theme(panel.background = element_rect("#a7f4fc")) +
  labs(
    title = "Plot 1: Primary Type Distribution",
    x = "Primary Type",
    y = "Frequency Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, colour = "#7F3D17", face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=15)
  ) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(x = `Primary Type`, y = Frequency, label = Frequency), vjust=1.8, color = "white", size=4)


# Plot an Grouped Line graph for attack using 'ggplot'
# ----------- Plot 2: Primary Type vs Summation of Attributes ----------- #

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


# Plot an bar graph for 'Total Attributes' using 'ggplot'
# ----------- Plot 3: Total Attributes Distribution ----------- #

meanTable <- pokemon_dataset %>% group_by(pokemon_dataset$Primary.Type) %>% dplyr::summarise(
              mean_attack = mean(Attack),
              mean_defense = mean(Defense),
              mean_hp = mean(HP),
              mean_spAttack = mean(Sp.Attack),
              mean_spDefense = mean(Sp.Defense),
              mean_speed = mean(Speed),
              mean_total = mean(Total),
            )

par(mar = c(2, 6, 3, 2) + 0.1)
ggplot(meanTable)+
  geom_bar(
    mapping = aes(
      x = `pokemon_dataset$Primary.Type`, 
      y = mean_total
    ),
    stat="identity"
  ) +
  theme(panel.background = element_rect("#f2c8fa")) +
  labs(
    title = "Plot 5: Mean Total Attributes Distribution",
    x = "Primary Type",
    y = "Mean Total Attributes Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, colour = "#eb5e7a", face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18)
  ) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(x = `pokemon_dataset$Primary.Type`, y = mean_total, label = floor(mean_total)), vjust=1.8, color = "white", size=4)


# Plot an Scatter Plot Matrix graph for 'Attributes' using 'ggplot'
# ----------- Plot 4: Scatter Plot Matrix between Abilities (Attributes) ----------- #

pairs(~Attack + Defense + HP + Sp.Attack + Sp.Defense + Speed, pokemon_dataset, col=c("BLUE", "RED"))

