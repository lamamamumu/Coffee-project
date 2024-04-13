library(dplyr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(rpart)
library(rpart.plot)
library(caret)
library(ISLR)
library(rattle)

dat <- read.csv('C:/Users/nextn/Desktop/coffee/psd_coffee.csv')
dat$Country <- as.factor(dat$Country)
dat <- data.frame(dat)
names(dat)
str(dat)


totals_by_year <- dat %>%
  group_by(Year) %>%
  summarize(
    total_Arabica_Production = sum(Arabica.Production)/63,
    total_Bean_Exports = sum(Bean.Exports)/63,
    total_Bean_Imports = sum(Bean.Imports)/63,
    total_Beginning_Stocks = sum(Beginning.Stocks)/63,
    total_Domestic_Consumption = sum(Domestic.Consumption)/63,
    total_Ending_Stocks = sum(Ending.Stocks)/63,
    total_Exports = sum(Exports)/63,
    total_Imports = sum(Imports)/63,
    total_Other_Production = sum(Other.Production)/63,
    total_Production = sum(Production)/63,
    total_Roast_Ground_Exports = sum(Roast...Ground.Exports)/63,
    total_Roast_Ground_Imports = sum(Roast...Ground.Imports)/63,
    total_Robusta_Production = sum(Robusta.Production)/63,
    total_Roast_Ground_Domestic_Consumption = sum(Rst.Ground.Dom..Consum)/63,
    total_Soluble_Domestic_Consumption = sum(Soluble.Dom..Cons.)/63,
    total_Soluble_Imports = sum(Soluble.Imports)/63,
    total_Distribution = sum(Total.Distribution)/63,
    total_Supply = sum(Total.Supply)/63
  )
View(totals_by_year)
totals_by_country <- dat %>%
  group_by(Country) %>%
  summarize(
    total_Bean_Exports = sum(Bean.Exports),
    total_Bean_Imports = sum(Bean.Imports),
    total_Beginning_Stocks = sum(Beginning.Stocks),
    total_Domestic_Consumption = sum(Domestic.Consumption),
    total_Ending_Stocks = sum(Ending.Stocks),
    total_Exports = sum(Exports),
    total_Imports = sum(Imports),
    total_Other_Production = sum(Other.Production),
    total_Production = sum(Production),
    total_Roast_Ground_Exports = sum(Roast...Ground.Exports),
    total_Roast_Ground_Imports = sum(Roast...Ground.Imports),
    total_Robusta_Production = sum(Robusta.Production),
    total_Roast_Production = sum(Robusta.Production),
    total_Roast_Ground_Domestic_Consum = sum(Rst.Ground.Dom..Consum),
    total_Soluble_Domestic_Cons = sum(Soluble.Dom..Cons.),
    total_Soluble_Exports = sum(Soluble.Exports),
    total_Soluble_Imports = sum(Soluble.Imports),
    total_Supply = sum(Total.Supply),
    total_Distribution = sum(Total.Distribution),
    total_Arabica_Production = sum(Arabica.Production)
  )
View(totals_by_country)
summary(totals_by_year)
summary(totals_by_country)

library (EnvStats)
rosnerTest(totals_by_country$total_Exports,alpha = 0.01,k=7)
rosnerTest(totals_by_country$total_Imports,alpha = 0.01,k=7)
rosnerTest(totals_by_country$total_Production,alpha = 0.01,k=7)
rosnerTest(totals_by_country$total_Domestic_Consumption,alpha = 0.01,k=7)

totals_by_year %>%
  tail(63) %>%
  ggplot( aes(x=Year, y=total_Exports)) +
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="green4", size=4) +
  labs(title='Total Export Line Chart',
       x='Year',
       y='The amount of export')+
  theme_minimal()

totals_by_year %>%
  tail(22) %>%
  ggplot( aes(x=Year, y=total_Imports)) +
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="red2", size=4) +
  labs(title='Total Import Line Chart',
       x='Year',
       y='The amount of import')+
  theme_minimal()

totals_by_year %>%
  tail(63) %>%
  ggplot( aes(x=Year, y=total_Production)) +
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="red2", size=4) +
  labs(title='Total Production Line Chart',
       x='Year',
       y='The amount of production stock')+
  theme_minimal()


Most_Exports <- totals_by_country %>%
  dplyr::arrange(desc(total_Exports)) %>%
  head(7) %>%
  dplyr::select(Country, total_Exports)


Most_Imports <- totals_by_country %>%
  dplyr::arrange(desc(total_Imports)) %>%
  head(7) %>%
  dplyr::select(Country,total_Imports)

Most_Production <- totals_by_country %>%
  dplyr::arrange(desc(total_Production)) %>%
  head(7) %>%
  dplyr::select(Country,total_Production)

Most_Domestic_Consumption <- totals_by_country %>%
  dplyr::arrange(desc(total_Domestic_Consumption)) %>%
  head(7) %>%
  dplyr::select(Country,total_Domestic_Consumption)


ggplot(dat, aes(x = Exports, y = Production)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Exports vs Production",
       x = "Exports",
       y = "Production")

ggplot(dat, aes(x = Total.Distribution, y = Production)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Distribution vs Production",
       x = "Distribution",
       y = "Production")

ggplot(Most_Exports, aes(x = reorder(Country, total_Exports), y = total_Exports)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.0f", total_Exports)), position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.2) +
  labs(title = "Top 7 Countries by Total Exports", x = "Country", y = "Total Exports") +
  theme_minimal() +
  coord_flip()


marketshare_Exports <- totals_by_country%>%mutate(Market_Share = round(total_Exports/sum(total_Exports)*100,1))%>%
  add_row(Market_Share = 23.2, Country = "Others")%>%
  arrange(desc(Market_Share))%>%
  head(7)%>%
  dplyr::select(Country, Market_Share)


colnames(marketshare_Exports) <- c('Country', 'Market_Share')
ggplot(marketshare_Exports, aes(x = '', y = Market_Share, fill = Country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(Market_Share, '%')),
            fontface = 'bold',
            position = position_stack(vjust = 0.5), 
            color = 'white', size = 5) +
  theme_void()

ggplot(Most_Imports, aes(x = reorder(Country, total_Imports), y = total_Imports)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = sprintf("%.0f", total_Imports)), position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.2) +
  labs(title = "Top 7 Countries by Total Imports", x = "Country", y = "Total Imports") +
  theme_minimal() +
  coord_flip()


marketshare_Imports <- totals_by_country%>%mutate(Market_Share = round(total_Imports/sum(total_Imports)*100,1))%>%
  add_row(Market_Share = 25.2, Country = "Others")%>%
  arrange(desc(Market_Share))%>%
  head(7)%>%
  dplyr::select(Country, Market_Share)


colnames(marketshare_Imports) <- c('Country', 'Market_Share')
ggplot(marketshare_Imports, aes(x = '', y = Market_Share, fill = Country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(Market_Share, '%')),
            fontface = 'bold',
            position = position_stack(vjust = 0.5), 
            color = 'white', size = 5) +
  theme_void()

ggplot(Most_Production, aes(x = reorder(Country, total_Production), y = total_Production)) +
  geom_bar(stat = "identity", fill = "green4") +
  geom_text(aes(label = sprintf("%.0f", total_Production)), position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.2, color = "white") +
  labs(title = "Top 7 Countries by Total Production", x = "Country", y = "Total Production") +
  theme_minimal() +
  coord_flip()


marketshare_Production <- totals_by_country%>%mutate(Market_Share = round(total_Production/sum(total_Production)*100,1))%>%
  add_row(Market_Share = 34.5, Country = "Others")%>%
  arrange(desc(Market_Share))%>%
  head(7)%>%
  dplyr::select(Country, Market_Share)


colnames(marketshare_Production) <- c('Country', 'Market_Share')
ggplot(marketshare_Production, aes(x = '', y = Market_Share, fill = Country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(Market_Share, '%')),
            fontface = 'bold',
            position = position_stack(vjust = 0.5), 
            color = 'white', size = 5) +
  theme_void()

ggplot(Most_Domestic_Consumption, aes(x = reorder(Country, total_Domestic_Consumption), y = total_Domestic_Consumption)) +
  geom_bar(stat = "identity", fill = "yellow3") +
  geom_text(aes(label = sprintf("%.0f", total_Domestic_Consumption)), position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.2) +
  labs(title = "Top 7 Countries by Total Domestic_Consumption", x = "Country", y = "Total Domestic_Consumption") +
  theme_minimal() +
  coord_flip()


marketshare_Domestic_Consumption <- totals_by_country%>%mutate(Market_Share = round(total_Domestic_Consumption/sum(total_Domestic_Consumption)*100,1))%>%
  add_row(Market_Share = 34, Country = "Others")%>%
  arrange(desc(Market_Share))%>%
  head(7)%>%
  dplyr::select(Country, Market_Share)


colnames(marketshare_Domestic_Consumption) <- c('Country', 'Market_Share')
ggplot(marketshare_Domestic_Consumption, aes(x = '', y = Market_Share, fill = Country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(Market_Share, '%')),
            fontface = 'bold',
            position = position_stack(vjust = 0.5), 
            color = 'white', size = 5) +
  theme_void()

library (ggcorrplot)
corr <- cor(dat[, c(6,7,8,9,10,12,20)])
ggcorrplot(corr,type = "lower", lab = T,lab_size = 3,method='square')

names(totals_by_year)
model1<-lm(total_Production ~ total_Robusta_Production+total_Arabica_Production+total_Other_Production, data = totals_by_year)
par(mfrow=c(2,2))
plot(model1)

model2<-lm(total_Exports ~ total_Bean_Exports+total_Roast_Ground_Exports, data = totals_by_year)
par(mfrow=c(2,2))
plot(model2)

model3<-lm(total_Domestic_Consumption ~ total_Soluble_Domestic_Consumption+total_Roast_Ground_Domestic_Consumption, data = totals_by_year)
par(mfrow=c(2,2))
plot(model3)

model4<-lm(total_Imports ~ total_Bean_Imports+total_Roast_Ground_Imports, data = totals_by_year)
par(mfrow=c(2,2))
plot(model4)

set.seed(1234)
index<-sample(1:nrow(totals_by_year),nrow(totals_by_year)*0.7)
data_train<-totals_by_year[index,]
data_test<-totals_by_year[-index,]
tree<-rpart(total_Production~.,data = totals_by_year)
fancyRpartPlot(tree)
rpart.plot(tree)

rpart.rules(tree)
summary(tree)

plotcp(tree)
tree$cptable

pruned_tree<-prune(tree,cp=0.01561658 )
fancyRpartPlot(pruned_tree)
rpart.plot(pruned_tree)
summary(pruned_tree)


pred<-predict(pruned_tree,data_test)
rmse<-function(actual,predict){
  sqrt(mean((actual-predict)^2))
}
rmse(data_test$total_Production,pred)

