#load libraries
library(tidyverse)
library(arules)
library(arulesViz)
library(knitr)
library(gridExtra)
library(lubridate)

#read the data
bb <- read.transactions("BreadBasket_DMS.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
bb
summary(bb)
glimpse(bb)

# Absolute Item Frequency Plot
itemFrequencyPlot(bb, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Relative Item Frequency Plot
itemFrequencyPlot(bb, topN=15, type="relative", col="lightcyan2", xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

# Load data 
bb_csv <- read.csv("BreadBasket_DMS.csv")

# Transactions per month
bb_csv %>%
  mutate(Month=as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Month, y=Transactions)) +
  geom_bar(stat="identity", fill="mistyrose2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per month") +
  theme_bw() 

# Transactions per weekday
bb_csv %>%
  mutate(WeekDay=as.factor(weekdays(as.Date(Date)))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme_bw() 

# Transactions per hour
bb_csv %>%
  mutate(Hour=as.factor(hour(hms(Time)))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(bb, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(bb, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(bb, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup0.5[i] <- length(apriori(bb, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}

# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
# Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
# Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(bb, parameter=list(sup=supportLevels[3], 
                                                   conf=confidenceLevels[5], target="rules"))

# Association rules
inspect(rules_sup1_conf50)

# Scatter plot
plot(rules_sup1_conf50, measure=c("support","lift"), shading="confidence")

# Graph
plot(rules_sup1_conf50, method="graph")

# Graph
plot(rules_sup1_conf50, method="graph", control=list(layout=igraph::in_circle()))

# Grouped matrix plot
plot(rules_sup1_conf50, method="grouped")
 
# another execution
# Apriori algorithm execution with a support level of 0.5% and a confidence level of 10%
rules_sup0.5_conf10 <- apriori(bb, parameter=list(sup=supportLevels[4], conf=confidenceLevels[9], target="rules"))

# Graph
plot(rules_sup0.5_conf10, method="graph", control=list(layout=igraph::in_circle()))

# Parallel coordinates plot
plot(rules_sup0.5_conf10, method="paracoord", control=list(reorder=TRUE))

# Grouped matrix plot
plot(rules_sup0.5_conf10, method="grouped")

# Scatter plot
plot(rules_sup0.5_conf10, measure=c("support","lift"), shading="confidence", jitter=0)
inspect(head(bb, 10))
