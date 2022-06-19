library(tidyr)
library(ggplot2)
library(plyr)

Beers = read.csv(file.choose(),header = TRUE)
Breweries = read.csv(file.choose(),header = TRUE)

head(Beers)
head(Breweries)

# Q1:Breweries in each state
Breweries %>% ggplot(aes(x=State)) + geom_bar() +
  labs(title="Breweries by state", x ="State",y="Number of Breweries")


# Q2:Merge beers and breweries
Beer_Brew <- merge(x=Beers,y=Breweries,by.x="Brewery_id",by.y="Brew_ID",all=TRUE)

head(Beer_Brew)
tail(Beer_Brew)
dim(Beer_Brew)
# Q3:Address the missing values in each column
Beer_Brew_clean = Beer_Brew %>% dplyr::filter(!is.na(Name.x) & !is.na(Beer_ID) & !is.na(ABV))
dim(Beer_Brew_clean)


# Q4:Compute the median alcohol content and international bitterness unit for each state. 
#Plot a bar chart to compare.

#ABV
Beer_Brew_clean %>% ggplot(aes(x=State, y=ABV)) + geom_bar(stat="summary") +
  labs(title="abv by state", x ="State",y="ABV")

meds <- ddply(Beer_Brew_clean, .(State), summarise, med = median(ABV))

Beer_Brew_clean %>% ggplot(aes(x=reorder(State,ABV,median), y=ABV)) + geom_boxplot() +
  labs(title="ABV by State", x ="State",y="ABV")+
  geom_text(data = meds, aes(x = State, y = med, label = med)) 
#IBU
Beer_Brew_clean2 = Beer_Brew %>% dplyr::filter(!is.na(Name.x) & !is.na(Beer_ID) & !is.na(IBU))
meds <- ddply(Beer_Brew_clean2, .(State), summarise, med = median(IBU))

Beer_Brew_clean2 %>% ggplot(aes(x=reorder(State,IBU,median), y=IBU)) + geom_boxplot() +
  labs(title="IBU by State", x ="State",y="IBU")+
  geom_text(data = meds, aes(x = State, y = med, label = med), 
            size = 3, vjust = -1.5)
# Q5:Which state has the maximum alcoholic (ABV) beer? Kentucky
# Which state has the most bitter (IBU) beer? Maine

# Q6:Comment on the summary statistics and distribution of the ABV variable.
Beer_Brew_clean %>% ggplot(aes(x=ABV)) + geom_boxplot() + coord_flip() + labs(title="Total ABV",y="ABV")
summary(Beer_Brew_clean)
# Q7:Is there an apparent relationship between the bitterness of the beer and its alcoholic content? 
#   Draw a scatter plot.  
#   Make your best judgment of a relationship and EXPLAIN your answer.
Beer_Brew_clean = Beer_Brew %>% dplyr::filter(!is.na(Name.x) & !is.na(Beer_ID) & !is.na(ABV) & !is.na(IBU))

Beer_Brew_clean %>% ggplot(aes(x=ABV, y=IBU))+ geom_point() +
labs(title="ABV vs IBU", x ="ABV",y="IBU") +  stat_smooth(method="lm")


#####################################################################################

# Testing other metrics
Beer_Brew_clean = Beer_Brew %>% dplyr::filter(!is.na(Name.x) & !is.na(Style) & !is.na(ABV))
meds <- ddply(Beer_Brew_clean, .(Style), summarise, med = median(ABV))

Beer_Brew_clean %>% ggplot(aes(x=reorder(Style,ABV,median), y=ABV)) + geom_boxplot() +
  labs(title="ABV by Style", x ="Style",y="ABV")+
  geom_text(data = meds, aes(x = Style, y = med, label = med), 
            size = 3, vjust = -1.5)



