rm(list=ls())

# Base R tasks

# 1. Download the food_coded.csv file

# 2. Load the CSV file into your R environment.

# Open the `codebook_food.docx` file for guidance.

# 3. Extract the first 95 rows.

head(food_coded, 95)

# 4. Look at the following variables using both name and column index/number.

# * GPA
#  * calories_chicken
#  * drink
#  * fav_cuisine
#  * father_profession
#  * mother_profession

# using column name
data1 <- food_coded[, c("GPA", "calories_chicken", "drink", "fav_cuisine", "father_profession", "mother_profession")]
# using column number

which( colnames(food_coded)=="drink" )
which( colnames(food_coded)=="fav_cuisine" )
which( colnames(food_coded)=="father_profession" )
which( colnames(food_coded)=="mother_profession" )

data2 <-food_coded[, c(1, 4, 16, 26, 25, 45)]

#  5. Create a new variable for how healthy each person feels but convert the scale from 1 to 10 to 1 to 100.
print(food_coded$healthy_feeling)

food_coded$healthy_feeling_converted <- food_coded$healthy_feeling * 10
print(food_coded$healthy_feeling_converted)

# 6. Filter to students who are female and have GPAs that are above 3.0.

data3 <- food_coded[food_coded$Gender == 1, ]
data3 <- data3[data3$GPA > 3.0, ]

# 7. Find the mean and standard deviation for the following variables, and summarize them in a data frame.

# * chicken_calories
# * tortilla_calories
# * turkey_calories
# * waffle_calories

data4 <- food_coded[, c("calories_chicken", "tortilla_calories", "turkey_calories", "waffle_calories")]

data4_mean <- sapply(data4, mean, na.rm= TRUE)
data4_sd <-sapply(data4, sd, na.rm= TRUE)
data4_final <- data.frame(rbind(data4_mean, data4_sd))

# 8. Summarize GPA and weight within the gender and cuisine variables.

food_coded_rep <- food_coded
food_coded_rep$GPA <- as.numeric(food_coded_rep$GPA)
food_coded_rep$weight <- as.numeric(food_coded_rep$weight)

agg1 <- aggregate(GPA ~ Gender, food_coded_rep, function(x) c(mean = mean(x), n = length(x)))
agg2 <- aggregate(weight ~ Gender, food_coded_rep, function(x) c(mean = mean(x), n = length(x)))
agg3 <- aggregate(GPA ~ cuisine, food_coded_rep, function(x) c(mean = mean(x), n = length(x)))
agg4 <- aggregate(weight ~ cuisine, food_coded_rep, function(x) c(mean = mean(x), n = length(x)))


# Tidyverse tasks

# 1. Download the facebook-fact-check.csv
# 2. Load the CSV file into your R environment.
# 3. Extract the last 500 rows.
# Hint: Check out the [top_n() page](https://rdrr.io/github/YTLogos/dplyr/man/top_n.html) to figure out how to extract the last 500 rows instead of the first 500 rows.

library(tidyverse)

facebook %>% top_n(-500)

# 4. Look at the even-numbered column indices only. Identify them by name.

facebook1<-facebook %>% select(2,4,6,8,10,12)
colnames(facebook1)


# 5. Using `mutate`, create a new variable called `post_type_coded` that renames each post type to the following:
  
# * link = 1
# * photo = 2
# * text = 3
# * video = 4
# Hint: look up case_when within tidyverse. You can also use if_else

facebook2 <- # saving changes to diamonds as a new object
  facebook %>% # original datase
  mutate(post_type_coded = NA)

case_when(facebook$`Post Type`  == "link" ~ 1,
          facebook$`Post Type`  == "photo" ~ 2,
          facebook$`Post Type`  == "text" ~ 3,
          facebook$`Post Type`  == "video" ~ 4)%>% 
  table


# 6. Arrange page names in reverse order.

facebook3 <- facebook %>% arrange(desc(Page))

# 7. Find the mean and standard deviation for the following variables, and summarize them.

# * share_count
# * reaction_count
# * comment_count

facebook4_mean<- facebook %>% summarize_at(c("share_count", "reaction_count", "comment_count"), mean, na.rm=TRUE)
facebook4_sd<- facebook %>% summarize_at(c("share_count", "reaction_count", "comment_count"), sd, na.rm=TRUE)  

# 8. Summarize the mean and standard deviations in Question 7 with the "mainstream" values in the `category` variable.

facebook5_mainstream_mean <- facebook %>%
  filter(facebook$Category == "mainstream") %>%
  group_by(Category) %>%
  summarize_at(c("share_count", "reaction_count", "comment_count"), mean, na.rm=TRUE)


facebook5_mainstream_sd <- facebook %>%
  filter(facebook$Category == "mainstream") %>%
  group_by(Category) %>%
  summarize_at(c("share_count", "reaction_count", "comment_count"), sd, na.rm=TRUE)
