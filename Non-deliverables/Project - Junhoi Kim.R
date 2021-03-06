
library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(e1071)
library(caret)
library(viridis)

beer = read.csv("D:\\MSDS\\DS 6306\\MSDS_6306_Doing-Data-Science\\Unit 8 and 9 Case Study 1\\beers.csv",header = TRUE)
brwr = read.csv("D:\\MSDS\\DS 6306\\MSDS_6306_Doing-Data-Science\\Unit 8 and 9 Case Study 1\\Breweries.csv",header = TRUE)
summary(beer)
summary(brwr)

head(beer)
head(brwr)


##### Q1.How many breweries are present in each state?
brwr_1 <- brwr %>% group_by(State) %>% summarize(count = n())
ggplot(brwr_1,aes(x= reorder(State,-count),count))+
  scale_fill_viridis_c() +
  labs(x="State", y = "Number of Brewers") +
  geom_bar(stat ="identity")


##### Q2.Merge beer data with the breweries data
merge_df <- merge(beer, brwr, by.x = "Brewery_id", by.y = "Brew_ID", all.y = TRUE)   
head(merge_df)

##### Q3.Address the missing values in each column
beer %>% summarise(across(everything(), ~ sum(is.na(.))))
brwr %>% summarise(across(everything(), ~ sum(is.na(.))))

##### Q4.Compute the median alcohol content and international bitterness unit for each state
median_ABV <- merge_df %>% 
  group_by(State) %>% 
  summarize(median = median(ABV))
final_median_ABV <- na.omit(median_ABV)
ggplot(final_median_ABV,aes(x= reorder(State,-median),median))+
  scale_fill_viridis_c() +
  labs(x="State", y = "Median ABV") +
  geom_bar(stat ="identity")

median_IBU <- merge_df %>% 
  group_by(State) %>% 
  summarize(median = median(IBU))
final_median_IBU <- na.omit(median_IBU)
ggplot(final_median_IBU,aes(x= reorder(State,-median),median))+
  scale_fill_viridis_c() +
  labs(x="State", y = "Median IBU") +
  geom_bar(stat ="identity")


##### Q5.Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
merge_df_ABV = merge_df %>% filter(!is.na(ABV)) %>% 
  group_by(State) %>% 
  summarize(maxABV = max(ABV), count = n()) %>% 
  arrange(desc(maxABV), desc(maxABV)) %>%  print(n=10)

merge_df_IBU = merge_df %>% filter(!is.na(IBU)) %>% 
  group_by(State) %>% 
  summarize(maxIBU = max(IBU), count = n()) %>% 
  arrange(desc(maxIBU), desc(maxIBU)) %>%  print(n=10)

##### Q6.Comment on the summary statistics and distribution of the ABV variable
summary(merge_df$ABV)

merge_nm_df = merge_df %>% filter(!is.na(ABV))
merge_df_final = merge_nm_df %>% filter(!is.na(IBU))

quantile(merge_df_final$ABV)

ggplot(merge_df_final, aes(x=ABV)) + geom_histogram()
ggplot(merge_df_final, aes(x=ABV)) + geom_density()


##### Q7.relationship between the bitterness of the beer and its alcoholic content?
del_outliers <- subset(merge_df_final, ABV<.1) 
plot(del_outliers$ABV,del_outliers$IBU, pch = 1,xlab = "ABV",ylab = "IBU")

ggplot(del_outliers, aes(x=ABV, y=IBU)) +
  geom_point() +
  geom_smooth(method=loess, se=FALSE) + 
  labs(x="ABV", y = "IBU") 


##### Q8

##### Q9

