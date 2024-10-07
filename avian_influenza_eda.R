
  
#Installing packages 

install.packages("readr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggmap")
install.packages("reshape2")
install.packages("maps")
install.packages("mapdata")
install.packages("here")

#Loading packages

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("ggmap")
library("knitr")
library("reshape2")
library("maps")
library("mapdata")
library("here")


#Loading data and checking data quality
avian_influenza_data <- read.csv(here("avian_influenza.csv"), header = TRUE)

#Checking data structure
str(avian_influenza_data)

#Checking first few data 
head(avian_influenza_data)

#Checking column names
colnames(avian_influenza_data)


#Data Cleaning decisions

#Decided to drop the X_id column as it is just a repetition of row names 
avian_influenza_data <- subset(avian_influenza_data, select = -X_id)

#Checking for duplicated data
duplicate_avian_inf_data <- avian_influenza_data[duplicated(avian_influenza_data),]
View(duplicate_avian_inf_data)

#Checking the duplicate data in another way
dupe_avian_data <- sum(duplicated(avian_influenza_data))
print(dupe_avian_data)

#Reconfirming that its duplicated rows counted 
nrow(avian_influenza_data[duplicated(avian_influenza_data), ])

#Selecting non-duplicated data for analysis
avian_flu <- unique(avian_influenza_data)

#NB:Important to note that the sum of unique value and duplicated value equals the total number of the data set

#Checking new selected data
head(avian_flu)
str(avian_flu)

#Checking for missing data 
sum(is.na(avian_flu))
which(is.na(avian_flu))

#Checking where exactly the missing variables are
colSums(is.na(avian_flu))
#Based on this information, we can assume that at least 68 sightings dont have county allocated to them. This is important to note depending on the analysis to be done.


#Distribution of targeted bird species

#Grouping the Bird species and renaming column appropiately
bird_spp_distribution <- avian_flu %>% 
  group_by(Common_Name) %>%
  count(target_H5_HPAI)

print(head(bird_spp_distribution))

#Arranging the dataset based on the frequency of Bird species captured 
bird_spp_distribution <- bird_spp_distribution %>%
  rename(times_captured = n)  %>%
  select(Common_Name, times_captured) %>%
  arrange(desc(times_captured))

#Selecting the top 15 birds captured and targeted for the H5N1 strain 
top_15_bird_spp <- bird_spp_distribution[1:15, ]

ggplot(top_15_bird_spp, aes(x = reorder(Common_Name, times_captured), y=times_captured))+
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  geom_text(aes(label = times_captured), hjust = +1.5, colour = "black")+
  labs(title="Top 15 Bird species captured in Ireland from 1980-2020", x="Bird Species", y="Number captured")




#Distribution of bird species over the years

#Grouping the data set by year 
birds_per_year <- avian_flu %>% 
  group_by(Year) %>%
  count(target_H5_HPAI)

head(birds_per_year)

#This gave me a count of captured birds, both the positives and negative counts were separated , making plotting for me a little difficult 

#Grouping the data, taking the count of the positives but also taking the count of the total rows to account for both positive and negative counts.

distribution_of_birds_per_year <- avian_flu %>%
  group_by(Year) %>%
  summarise(positive_cases = sum(target_H5_HPAI, na.rm = TRUE), count_rows = n()) %>%
  rename(total_cases = count_rows) %>%
  mutate(negative_cases = total_cases-positive_cases)

head(distribution_of_birds_per_year)

#Reshaping the data frame to make it more plottable

plot_bird_per_year <- melt(distribution_of_birds_per_year, id.vars = "Year", measure.vars = c("positive_cases", "negative_cases"), variable.name = "H5N1_Target",  value.name = "total_cases")

ggplot(plot_bird_per_year, aes(x= Year, y= total_cases, fill = H5N1_Target))+
  geom_bar(stat = "identity", position = "stack")+
  labs(title = "The distribution of birds captured from 1980-2020 ", x = "Year", y = "Number of birds captured" )+
  theme(plot.title = element_text(hjust = 0.5))



#Distribution of bird spp across state a.k.a Provinces

state_bird_distribution <- avian_flu %>%
  group_by(State)%>%
  count(target_H5_HPAI)

head(state_bird_distribution)

ggplot(state_bird_distribution, aes(x = State, y = n, fill = as.factor(target_H5_HPAI))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "red"), name = "H5N1_Target", labels = c("positive_cases", "negative_cases")) +
  labs(title = "The distribution of birds captured across Provinces", x = "State", y = "Number of birds captured") 


#Distribution by months
monthly_bird_distribution <- avian_flu %>%
  group_by(Month) %>%
  count(target_H5_HPAI) %>%
  mutate(Month = case_when(
    Month == "1" ~ "January",
    Month == "2" ~ "February",
    Month == "3" ~ "March",
    Month == "4" ~ "April",
    Month == "5" ~ "May",
    Month == "6" ~ "June",
    Month == "7" ~ "July",
    Month == "8" ~ "August",
    Month == "9" ~ "September",
    Month == "10" ~ "October",
    Month == "11" ~ "November",
    Month == "12" ~ "December",
  )) %>%
  mutate(Month = factor(Month, levels = month.name))

#Plot of monthly bird distirbution
ggplot(monthly_bird_distribution, aes(x = Month, y = n, fill = as.factor(target_H5_HPAI))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "red"), name = "H5N1_Target", labels = c("negative_cases", "positive_cases")) +
  labs(title = "The distribution of birds captured across Months", x = "Month", y = "Number of birds captured") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Distribution map of birds captured

bird_long_lat <- avian_flu %>%
  select(Common_Name, Latitude,Longitude, target_H5_HPAI)


point_colors <- ifelse(bird_long_lat$target_H5_HPAI == 1, "red", "blue")


map("worldHires", "Ireland", fill = TRUE, col = "lightblue", bg = "lightblue", xlab = "Longitude", ylab = "Latitude")
title(main = "Map of Ireland showing birds captured", col.main = "black", font.main = 4)
points( bird_long_lat$Longitude, bird_long_lat$Latitude,col= point_colors, pch=16, cex = 0.7)
legend("bottomright", legend = c("Positive", "Negative"), col = c("red", "blue"), pch = 16, title = "HPA1 Target", cex = 0.5)

#Adding the provinces manually 
provinces_ireland <- data.frame(
  province = c("Munster", "Leinster", "Connacht", "Ulster"),
  latitude = c(52.4, 53.3, 53.8, 54.6),   
  longitude = c(-8.5, -6.5, -9.0, -7.3)   
)
points(provinces_ireland$longitude, provinces_ireland$latitude, col = "darkred", pch = 17, cex = 1.5)  # Plot province points
text(provinces_ireland$longitude, provinces_ireland$latitude, labels = provinces_ireland$province, pos = 1.5, cex = 0.5, col = "black", font = 4) 



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  