library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(marginaleffects)

# Read the dataset
sfo <- read_csv("SFO Passenger Survey Responses.csv")

# Explore the dataset
str(sfo)
head(sfo)

# Check for any missing values
sfo[rowSums(is.na(sfo))==0,]
nrow(sfo[is.na(sfo$good) | is.na(sfo$dirty) | is.na(sfo$wait) | is.na(sfo$lastyear) | is.na(sfo$usa), ]) 

# Split into train and test dataset
# sample <- sample.split(sfo$good, SplitRatio = 0.7)
# sfo_train  <- subset(sfo, sample == TRUE)
# sfo_test   <- subset(sfo, sample == FALSE)

# Change the features into correct type
str(sfo)

sfo$good <- as.factor(sfo$good)
levels(sfo$good)
table(sfo$good)

sfo$usa <- as.factor(sfo$usa)
levels(sfo$usa)
table(sfo$usa)

str(sfo)
summary(sfo)

# Exploration and Descriptive Statistics
xtabs(~ good + dirty, data = sfo)
table(sfo$dirty)

xtabs(~ good + wait, data = sfo)
table(sfo$wait)

xtabs(~ good + lastyear, data = sfo)
table(sfo$lastyear)

xtabs(~ good + usa, data = sfo)

ggpairs(sfo)

round(cor(sfo),2)

cor(sfo[sapply(sfo,is.numeric)])

# Categorical Features
good_freq <- table(sfo$good)
usa_freq <- table(sfo$usa)

good_df <- data.frame(good_freq)
usa_df <- data.frame(usa_freq)

good_df_freq <- good_df$Freq
names(good_df_freq) <- c("Not good", "Good")
good_df_freq

usa_df_freq <- usa_df$Freq
names(usa_df_freq) <- c("Other", "USA")
usa_df_freq

library(qcc)


pareto.chart(good_df_freq, main='Pareto Chart of Passenger Response')

pareto.chart(usa_df_freq, main='Pareto Chart of Passenger Destination', col=c("orange","yellow"))

ggplot(sfo, aes(x=dirty))+
  geom_bar(fill="cornflowerblue") +
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5) +
  labs(title = "Dirty Places", subtitle = "Frequency count of dirty", x="Dirty", y="Frequency")

ggplot(sfo, aes(y=wait))+
  geom_boxplot(fill="cornflowerblue", outlier.colour = "red")+
  labs(title = "Wait", subtitle = "Time duration in hours spent by a passenger at SFO", x="", y="Wait")

ggplot(sfo, aes(x=lastyear))+
  geom_boxplot(fill="cornflowerblue", outlier.colour = "red")+
  labs(title = "Wait", subtitle = "Time duration in hours spent by a passenger at SFO", y="", x="Wait")

ggplot(sfo, aes(x=lastyear))+
  geom_histogram(bins=70, fill="cornflowerblue", color="white") +
  labs(title = "Last Year Journeys", subtitle = "Count of flying out from SFO in past 12 months for a passenger", x="Journeys", y="Frequency")

# Numerical Features
cal_mode <- function(x){
  x_unique <- unique(x)
  x_unique[which.max(tabulate(match(x, x_unique)))]
}

cal_stats <- function(x){
  x_mean <- round(mean(x), 3)
  print(paste("Mean : ", x_mean))
  x_median <- round(median(x), 3)
  print(paste("Median : ", x_median))
  x_mode <- cal_mode(x)
  print(paste("Mode : ", x_mode))
}

cal_stats(sfo$dirty)
cal_stats(sfo$wait)
cal_stats(sfo$lastyear)

# Models and predictions

log.model1 <- glm(formula = good ~ dirty + wait + lastyear + usa, data = sfo, family = 'binomial')
summary(log.model1)

round(exp(log.model1$coefficients), 3)
round(exp(confint(log.model1)), 3)

log.model2 <- glm(formula = good ~ dirty + wait, data = sfo, family = 'binomial')
summary(log.model2)

# Converting log odds to odds ratio
round(exp(log.model2$coefficients), 3)
round(exp(confint(log.model2)), 3)

# Mean value of predictors
mean(sfo$dirty)
mean(sfo$wait)

# Odds of being passenger approved at mean values
exp(log.model2$coefficients[1] + 
      log.model2$coefficients[2]*mean(sfo$dirty) +
      log.model2$coefficients[3]*mean(sfo$wait))

# Predicted risk at mean values
1/(1+exp(-1*(log.model2$coefficients[1] + 
               log.model2$coefficients[2]*mean(sfo$dirty) +
               log.model2$coefficients[3]*mean(sfo$wait))))

# Marginal effects

marginaleffects(log.model2, newdata="mean")

# Compare with predicted risk at the means, but increasing wait by 1

1/(1+exp(-1*(log.model2$coefficients[1] + 
               log.model2$coefficients[2]*(mean(sfo$dirty)+1) +
               log.model2$coefficients[3]*(mean(sfo$wait)))))

1/(1+exp(-1*(log.model2$coefficients[1] + 
               log.model2$coefficients[2]*mean(sfo$dirty) +
               log.model2$coefficients[3]*(mean(sfo$wait)+1))))


sfo$good.predicted <- predict(log.model2, type='response')

sfo$good.predicted.class <- as.numeric(sfo$good.predicted > 0.50)

table(sfo$good, sfo$good.predicted.class, dnn=c('Observed', 'Predicted'))
plot(sfo$wait, sfo$dirty, 
     col=ifelse(sfo$good != sfo$good.predicted.class, '#b6252580', '#90909040'),
     cex=1.8, pch=19)

confusionMatrix(as.factor(sfo$good.predicted.class), sfo$good)

ggplot(sfo) + geom_bar(aes(y=wait, fill=good))

ggplot(sfo) + geom_bar(aes(y=wait, fill=good)) + facet_wrap(~usa)

ggplot(sfo, aes(x=usa, y=wait)) + geom_boxplot() + facet_wrap(~good) # this can be #option1

levels(sfo$usa) <- c("Other","USA")
levels(sfo$usa)
table(sfo$usa)

ggplot(sfo, aes(x=usa, y=wait)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.3,
              alpha = 0.3, 
              col = ifelse(sfo$good == 0, '#f8766c', '#01bfc4'))+
  labs(title = "Visualising 'good' with 'wait' and 'usa'",
     x="Destination", y="Wait")

ggplot(sfo, aes(x=usa, y=wait)) + 
  geom_boxplot() + 
  geom_point( alpha = 0.2, 
              col = ifelse(sfo$good == 0, '#b6252580', '#90909040'))+
  labs(title = "Last Year Journeys", 
       subtitle = "Count of flying out from SFO in past 12 months for a passenger", 
       x="Journeys", y="Frequency")

ggplot(sfo, aes(x=usa, ..count..)) +
  geom_bar(aes(fill=good), position = "dodge")

ggplot(sfo, aes(x=wait, ..count..)) +
  geom_bar(aes(fill=good), position = "stack")

ggplot(sfo, aes(x=good, y=wait)) +
  geom_bar(stat="identity")

#  facet_wrap()

xtabs(~wait+good+usa, sfo)

# ------------------
# Now move to converting some numerical into categories, see if it improves the performance

sfo

hist(sfo$dirty)

sfo$dirty.cat <- NA

sfo <- within(sfo, {
  dirty.cat <- NA
  dirty.cat[dirty == 0] <- "Clean"
  dirty.cat[dirty > 0] <- "Dirty"
})

sfo$dirty.cat <- as.factor(sfo$dirty.cat)

str(sfo)

hist(sfo$lastyear)

sfo$lastyear.cat <- cut(sfo$lastyear, breaks = c(-1,5,10,20,70), labels = c("Less than 5", "Between 5 to 10", "Between 10 to 20", "Between 20 to 70"))

table(sfo$lastyear.cat)

table(sfo$dirty.cat)

log.model3 <- glm(formula = good ~ dirty.cat + wait + lastyear.cat + usa, data = sfo, family = 'binomial')
summary(log.model3)

log.model4 <- glm(formula = good ~ dirty.cat + wait, data = sfo, family = 'binomial')
summary(log.model4)

log.model5 <- glm(formula = good ~ dirty.cat + wait + lastyear.cat, data = sfo, family = 'binomial')
summary(log.model5)

# Pie Chart with Percentages
slices <- c(2759, 892)
lbls <- c("USA", "Other")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Passenger Destination")