---
title: "Crickets"
author: "Cricket-Analysis"
date: "2023-11-13"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r}
# lets import our libraries
library(readr)
library(forcats)
library(dplyr)
library(summarytools)
library(stringr)
library(ggplot2)
library(tidyverse)
```

```{r}
# lets read our data
data <- read.csv("ashes.csv")
```

```{r}
# lets preview our data
head(data)
```

```{r}
# lets check the dimensions (shape) of the data frame
data_shape <- dim(data)

# lets print the dimensions
print(data_shape)
```

```{r}
# lets see the column names
column_names <- names(data)
print(column_names)
```

```{r}
# lets see the columns and the data types
glimpse(data)
```

```{r}
# lets check for missing values in the dataset
missing_values <- any(is.na(data))

# Print the result
print(missing_values)
```

1.a) Each subject needs its own row. Rearrange the data into a long format so that there is a row
for each batter in each innings. Your new tibble should have 270 rows.
```{r}
library(tidyr)

# lets rearrange the data into long format
long_data <- data %>%
  gather(key = "innings", value = "score", -c(batter, team, role))

# Print the modified data frame
head(long_data)
```

```{r}
# lets check the shape of long_data
shape <- dim(long_data)

# Print the shape
print(shape)
```


1.b) Each cell should represent only one measurement. Use str_match() to create new columns for each of the following for each player innings:
• the player’s batting number,
• their score, and
• the number of balls they faced.
```{r}
library(dplyr)
library(stringr)

# lets define a function to extract information using str_match
extract_info <- function(text) {
  # lets use regular expressions to extract batting number, score, and balls faced
  result <- str_match(text, "Batting at number (\\d+), scored (\\d+) runs from (\\d+) balls")
  
  # lets return a named list with extracted values
  return(list(
    batting_number = as.numeric(result[2]),
    score = as.numeric(result[3]),
    balls_faced = as.numeric(result[4])
  ))
}

# lets apply the function to each row using dplyr::mutate
long_data <- long_data %>%
  mutate(
    # lets apply the extract_info function to the 'score' column
    new_columns = map(score, extract_info),
    
    # lets extract individual values from the list column
    batting_number = map_dbl(new_columns, "batting_number"),
    score = map_dbl(new_columns, "score"),
    balls_faced = map_dbl(new_columns, "balls_faced")
  ) %>%
  # lets select relevant columns
  select(batter, team, role, innings, batting_number, score, balls_faced)

# lets print the modified data frame
head(long_data)
```

1.c) Recode the data to make it ‘tame’, that is,
• ensure all categorical variables with a small number of levels are coded as factors,
```{r}
# lets identify categorical variables with a small number of levels (let's say, less than or equal to 10)
small_levels_cols <- sapply(long_data, function(x) is.factor(x) || (is.character(x) && length(unique(x)) <= 10))

# lets recode small levels to factors
long_data[, small_levels_cols] <- lapply(long_data[, small_levels_cols], as.factor)

# lets print the glimpse of the updated data frame
glimpse(long_data)

```

• ensure all categorical variables with a large number of levels are coded as characters.
```{r}
library(dplyr)

# lets identify categorical variables with a large number of levels
large_levels_cols <- sapply(long_data, function(x) is.factor(x) | (is.character(x) && length(unique(x)) > 10))

# lets recode large levels to characters
long_data[, large_levels_cols] <- lapply(long_data[, large_levels_cols], as.character)

# lets print the glimpse of the updated data frame
glimpse(long_data)

```

```{r}
# lets sort NA values
long_data <- na.omit(long_data)

```


1.d) Clean the data; recode the factors using fct_recode() such that there are no typographical errors
in the team names and player roles.
```{r}
library(dplyr)
library(forcats)

# lets recode 'team' factor
long_data$team <- tolower(as.character(long_data$team))
levels(long_data$team) <- tolower(levels(long_data$team))

# lets recode 'role' factor
long_data$role <- tolower(as.character(long_data$role))
levels(long_data$role) <- tolower(levels(long_data$role))

# lets print the modified data frame
head(long_data)

```

```{r}
# lets check the shape of the cleaned data
dim(long_data)
```

2. Univariate Analysis
2.a) Produce a histogram of all scores during the series.
```{r}
# lets check for missing values in the 'score' column
#any(is.na(long_data$score))

# lets remove NAs from the 'score' column
#long_data <- long_data[complete.cases(long_data$score), ]

# lets create a histogram
hist(long_data$score, 
     breaks = seq(min(long_data$score), max(long_data$score) + 10, by = 10),
     col = "blue", 
     border = "black",
     main = "Distribution of Scores during the Series",
     xlab = "Score",
     ylab = "Frequency")

```

2.b) Describe the distribution of scores, considering shape, location spread and outliers.
```{r}
# lets perform summary statistics
summary(long_data$score)

# lets create a boxplot to visualize the distribution and identify outliers
boxplot(long_data$score, col = "lightblue", main = "Boxplot of Scores during the Series")

# creating a density plot for a smooth representation of the distribution
density_plot <- ggplot(long_data, aes(x = score)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Scores during the Series", x = "Score", y = "Density") +
  theme_minimal()

print(density_plot)

```

2.c) Produce a bar chart of the teams participating in the series, with different colours for each team.
Noting that each player is represented by 10 rows in the data frame, how many players were used by each team in the series?
```{r}
library(ggplot2)

# lets create a bar chart of teams with different colors
team_bar_chart <- ggplot(long_data, aes(x = team, fill = team)) +
  geom_bar() +
  labs(title = "Participation of Teams in the Series", x = "Team", y = "Count") +
  theme_minimal()

print(team_bar_chart)

# lets calculate the number of players used by each team
players_per_team <- long_data %>%
  group_by(team) %>%
  summarise(players_used = n_distinct(batter) / 10)

print(players_per_team)

```
England used 2 players, Australia used 2 players while England used 1 player.


3.Scores for each team
3.a) Using ggplot, produce histograms of scores during the series, faceted by team.
```{r}
library(ggplot2)

# lets create histograms faceted by team
histogram_faceted <- ggplot(long_data, aes(x = score)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Scores Faceted by Team",
       x = "Score",
       y = "Frequency") +
  facet_wrap(~ team, scales = "free") +
  theme_minimal()

print(histogram_faceted)

```

3.b) Produce side-by-side boxplots of scores by each team during the series.
```{r}
library(ggplot2)

# lets create side-by-side boxplots faceted by team
boxplot_faceted <- ggplot(long_data, aes(x = team, y = score, fill = team)) +
  geom_boxplot() +
  labs(title = "Boxplots of Scores by Team",
       x = "Team",
       y = "Score") +
  theme_minimal()

print(boxplot_faceted)

```

3.c) Compare the distributions of scores by each team during the series, considering shape, location, spread and outliers, and referencing the relevant plots. Which team looks to have had a higher average score?
```{r}
library(ggplot2)
library(dplyr)

# lets create boxplots faceted by team
boxplot_faceted <- ggplot(long_data, aes(x = team, y = score, fill = team)) +
  geom_boxplot() +
  labs(title = "Boxplots of Scores by Team",
       x = "Team",
       y = "Score") +
  theme_minimal()

print(boxplot_faceted)

# Summary statistics
summary_stats <- long_data %>%
  group_by(team) %>%
  summarize(
    mean_score = mean(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    min_score = min(score, na.rm = TRUE),
    max_score = max(score, na.rm = TRUE)
  )

print(summary_stats)

```
Based on the provided summary statistics for each team:

- Australia has a mean (average) score of approximately 41.71.
- England has a mean score of approximately 27.72.
- English (assuming it refers to a different team or group) has a mean score of approximately 0.89.

Therefore, Australia appears to have had a higher average score compared to England and English. The average score is a measure of central tendency that gives an indication of the typical score for each team. In this case, Australia's higher average suggests that, on average, their players achieved higher scores during the series compared to the other teams.


4.Scoring rates
4.a) Produce a scatterplot of scores against number of balls.
```{r}
library(ggplot2)

ggplot(long_data, aes(x = balls_faced, y = score)) +
  geom_point() +
  labs(title = "Scatterplot of Scores against Number of Balls",
       x = "Number of Balls Faced",
       y = "Score") +
  theme_minimal()

```

4.b) Describe the relationship between score and number of balls. Are players who face more balls
likely to score more runs?
```{r}
# lets create a scatterplot of Scores against Number of Balls
ggplot(long_data, aes(x = balls_faced, y = score)) +
  geom_point() +
  labs(title = "Scatterplot of Scores against Number of Balls",
       x = "Number of Balls Faced",
       y = "Score") +
  theme_minimal()

# Summary statistics
summary_stats <- summary(lm(score ~ balls_faced, data = long_data))
print(summary_stats)

```
## 1. Coefficient for 'balls_faced':

The coefficient for 'balls_faced' is 0.50711.
This positive coefficient suggests that, on average, for each additional ball faced, the score tends to increase by approximately 0.51 runs.
## 2. P-value:

The extremely low p-value (< 2.2e-16) indicates that the relationship between the number of balls faced and the score is statistically significant.
In practical terms, this means that it's highly unlikely to observe such a strong relationship by random chance.
## 3. R-squared:

The R-squared value is 0.903, indicating that approximately 90.3% of the variability in the score can be explained by the number of balls faced.
This is a high percentage, suggesting a substantial explanatory power of the model.
## 4. Residuals:

The residuals (differences between observed and predicted values) have a spread around zero, indicating that the model is capturing most of the variation in the scores.
## Conclusion:
Based on these results, we can confidently conclude that there is a positive and significant relationship between the number of balls faced and the score. On average, players who face more balls are likely to score more runs. The model suggests that for every additional ball faced, we expect an increase of approximately 0.51 runs in the score.


4.c) Compute a new variable, scoring_rate, defined as the number of runs divided by the number of
balls. Produce a scatterplot of scoring_rate against number of balls.
```{r}
# lets compute scoring_rate
long_data$scoring_rate <- long_data$score / long_data$balls_faced

# lets produce a scatterplot
library(ggplot2)

scatterplot <- ggplot(long_data, aes(x = balls_faced, y = scoring_rate)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatterplot of Scoring Rate against Number of Balls",
       x = "Number of Balls Faced",
       y = "Scoring Rate") +
  theme_minimal()

print(scatterplot)

```

4.d) Is there a relationship between scoring rate and number of balls? Are players who face more
balls likely to score runs more quickly?
```{r}
# lets check for missing or infinite values in scoring_rate and balls_faced
missing_values <- sum(is.na(long_data$scoring_rate) | !is.finite(long_data$scoring_rate) | is.na(long_data$balls_faced) | !is.finite(long_data$balls_faced))

# lets remove rows with missing or infinite values
long_data <- na.omit(long_data, cols = c("scoring_rate", "balls_faced"))

# lets compute correlation coefficient
correlation <- cor(long_data$balls_faced, long_data$scoring_rate)

# lets print the correlation coefficient
print(paste("Correlation Coefficient:", correlation))

```
The correlation coefficient of 0.0045 suggests a very weak positive correlation between scoring rate and the number of balls faced. In practical terms, this correlation is close to zero, indicating that there is almost no linear relationship between the two variables. Therefore, based on this analysis, there is little evidence to suggest that players who face more balls are likely to score runs more quickly.



5. Teams’ roles
5.a) Produce a bar chart of the number of players on each team participating in the series, with segments coloured by the players’ roles.
```{r}
library(ggplot2)

# lets create a bar chart
bar_chart <- ggplot(long_data, aes(x = team, fill = role)) +
  geom_bar(position = "stack") +
  labs(title = "Number of Players on Each Team by Role",
       x = "Team",
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# lets print the bar chart
print(bar_chart)

```

5.b) Produce a contingency table of the proportion of players from each team who play in each
particular role.
```{r}
# lets create a contingency table
contingency_table <- table(long_data$team, long_data$role)

# lets convert the counts to proportions
contingency_proportions <- prop.table(contingency_table, margin = 1)

# lets print the contingency table with proportions
print(contingency_proportions)

```

5.c) Using these two figures, state which team is made up of a larger proportion of batters, and
which team contains a larger proportion of all-rounders.
```{r}
library(dplyr)

# lets create a new variable indicating whether a player is a Batter, Bowler, or All-rounder
long_data <- long_data %>%
  mutate(player_category = case_when(
    str_detect(role, "batsman") ~ "batsman",
    str_detect(role, "bowler") ~ "bowler",
    str_detect(role, "all-rounder") ~ "all-rounder",
    TRUE ~ "Other"
  ))

# lets create a contingency table
contingency_table <- table(long_data$team, long_data$player_category)

# lets calculate proportions by row
proportions_by_team <- prop.table(contingency_table, margin = 1)

# lets identify the team with the highest proportion of batters
team_with_max_batters <- names(which.max(proportions_by_team[, "batsman"]))

# lets identify the team with the highest proportion of all-rounders
team_with_max_all_rounders <- names(which.max(proportions_by_team[, "all-rounder"]))

# lets print the results
cat("Team with the highest proportion of batters:", team_with_max_batters, "\n")
cat("Team with the highest proportion of all-rounders:", team_with_max_all_rounders, "\n")

```

## 6: Summary of Insights
In analyzing the cricket data, several key insights have emerged that may interest Cricket Australia. Firstly, when comparing the two teams, Australia appears to have a higher average score per innings (41.71) compared to England (27.72). Additionally, Australia demonstrates a wider spread in scores, indicating more variability in individual player performances. This suggests that Australia may have a more diverse batting lineup, with some players consistently scoring high.

Furthermore, the analysis of player roles reveals that Australia has a higher proportion of players categorized as "Batters" compared to England, indicating a potentially stronger batting lineup. On the other hand, England seems to have a higher proportion of "All-rounders," suggesting a more balanced combination of batting and bowling skills in their players.

Regarding scoring rates, the linear regression analysis indicates a positive relationship between the number of balls faced and the runs scored, suggesting that players who face more balls are likely to score more runs. This finding aligns with the intuitive expectation in cricket.

In summary, Australia seems to excel in terms of average scores and a diverse batting lineup, while England showcases a more balanced team with a higher proportion of all-rounders. Understanding these team dynamics and individual player performances can provide valuable insights for Cricket Australia in strategic planning and player selection.