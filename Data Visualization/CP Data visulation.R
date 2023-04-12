library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
df <- read.csv("train.csv")
head(df)
summary(df)

#chceking the corelation
corr <- round(cor(df), 8)
print(ggcorrplot(corr))

df$blue <- as.factor(df$blue)
df$dual_sim <- as.factor(df$dual_sim)
df$four_g <- as.factor(df$four_g)
df$three_g<- as.factor(df$three_g)
df$touch_screen <- as.factor(df$touch_screen)
df$wifi <- as.factor(df$wifi)
df$price_range <- as.factor(df$price_range)

# Bar Chart Subplots
p1 <-  ggplot(df, aes(x=blue, fill=blue)) + theme_bw() + geom_bar() + ylim(0, 1050) + labs(title = "Bluetooth") + scale_x_discrete(labels = c('Not Supported','Supported'))
p2 <- ggplot(df, aes(x=dual_sim, fill=dual_sim)) + theme_bw() + geom_bar() + ylim(0, 1050) + labs(title = "Dual Sim") + scale_x_discrete(labels = c('Not Supported','Supported'))
p3 <- ggplot(df, aes(x=four_g, fill=four_g)) + theme_bw() + geom_bar() + ylim(0, 1050) + labs(title = "4 G") + scale_x_discrete(labels = c('Not Supported','Supported'))
print(grid.arrange(p1, p2, p3, nrow = 1))

print(prop.table(table(df$blue))) # cell percentages
print(prop.table(table(df$dual_sim))) # cell percentages
print(prop.table(table(df$four_g))) # cell percentages

# Bar Chart Subplots
p1 <-  ggplot(df, aes(x=price_range, y = battery_power, color=price_range)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) + labs(title = "Battery Power vs Price Range")
p2 <- ggplot(df, aes(x=price_range, y = mobile_wt, color=price_range)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) + labs(title = "Phone Weight vs Price Range")
p3 <- ggplot(df, aes(x=price_range, y = ram, color=price_range)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) + labs(title = "RAM vs Price Range")
print(grid.arrange(p1, p2, p3, nrow = 1))

data = data.frame(MagaPixels = c(df$fc, df$pc), Camera = rep(c("Front Camera", "Primary Camera"), c(length(df$fc), length(df$pc))))
print(ggplot(data, aes(MagaPixels, fill = Camera)) + geom_bar(position = 'identity', alpha = .5))

# Checking for Missing values
missing_values <- df %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
print(missing_values %>% ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) + geom_bar(stat="identity",fill="red")+coord_flip()+theme_bw())

f <- read.csv("train.csv")
#skewness
library(moments)
skewness(f)
hist(df$clock_speed, col='red')
