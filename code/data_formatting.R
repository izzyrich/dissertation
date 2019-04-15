# extract data for each year and format for statistics 

# import packages 
library(readr)
library(tidyverse)

# TOTAL AREA ----

# import data 1989 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1989.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

delete89 <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df89 <- bind_rows(
  delete89 %>% select(label, class = class0, pixels = number0),
  delete89 %>% select(label, class = class1, pixels = number1),
  delete89 %>% select(label, class = class2, pixels = number2),
  delete89 %>% select(label, class = class3, pixels = number3),
  delete89 %>% select(label, class = class4, pixels = number4),
  delete89 %>% select(label, class = class5, pixels = number5),
  delete89 %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1989)
  

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

# import data 1990 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1990.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

delete90 <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df90 <- bind_rows(
  delete90 %>% select(label, class = class0, pixels = number0),
  delete90 %>% select(label, class = class1, pixels = number1),
  delete90 %>% select(label, class = class2, pixels = number2),
  delete90 %>% select(label, class = class3, pixels = number3),
  delete90 %>% select(label, class = class4, pixels = number4),
  delete90 %>% select(label, class = class5, pixels = number5),
  delete90 %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1990)


# import data 1991 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1991.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

delete91 <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df91 <- bind_rows(
  delete91 %>% select(label, class = class0, pixels = number0),
  delete91 %>% select(label, class = class1, pixels = number1),
  delete91 %>% select(label, class = class2, pixels = number2),
  delete91 %>% select(label, class = class3, pixels = number3),
  delete91 %>% select(label, class = class4, pixels = number4),
  delete91 %>% select(label, class = class5, pixels = number5),
  delete91 %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1991)


# import data 1992 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1992.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df92 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1992)


# import data 1993 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1993.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df93 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1993)


# import data 1994 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1994.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df94 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1994)


# import data 1995 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1995.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df95 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1995)

# import data 1996 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1996.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df96 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1996)


# import data 1997 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1997.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df97 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1997)

# import data 1998 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1998.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df98 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1998)

# import data 1999 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_1999.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df99 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 1999)

# import data 2000 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2000.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df00 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2000)

# import data 2001 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2001.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df01 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2001)

# import data 2002 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2002.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df02 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2002)

# import data 2003 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2003.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df03 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2003)

# import data 2004 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2004.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df04 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2004)

# import data 2005 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2005.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df05 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2005)

# import data 2006 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2006.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df06 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2006)


# import data 2007 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2007.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df07 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2007)

# import data 2008 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2008.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df08 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2008)

# import data 2009 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2009.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df09 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2009)

# import data 2010 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2010.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df10 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2010)

# import data 2011 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/classified_2011.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1', 
                            'class2', 'number2', 
                            'class3', 'number3', 
                            'class4', 'number4', 
                            'class5', 'number5', 
                            'class6', 'number6'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)
new$class2 <- gsub("[^0-9.-]", "", new$class2)
new$number2 <- gsub("[^0-9.-]", "", new$number2)
new$class3 <- gsub("[^0-9.-]", "", new$class3)
new$number3 <- gsub("[^0-9.-]", "", new$number3)
new$class4 <- gsub("[^0-9.-]", "", new$class4)
new$number4 <- gsub("[^0-9.-]", "", new$number4)
new$class5 <- gsub("[^0-9.-]", "", new$class5)
new$number5 <- gsub("[^0-9.-]", "", new$number5)
new$class6 <- gsub("[^0-9.-]", "", new$class6)
new$number6 <- gsub("[^0-9.-]", "", new$number6)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)
new$number2 <- as.numeric(new$number2)
new$number3 <- as.numeric(new$number3)
new$number4 <- as.numeric(new$number4)
new$number5 <- as.numeric(new$number5)
new$number6 <- as.numeric(new$number6)

# get sum of all regions for each class and input into excel -----
sum(new$number0,na.rm=TRUE)
sum(new$number1,na.rm=TRUE)
sum(new$number2,na.rm=TRUE)
sum(new$number3,na.rm=TRUE)
sum(new$number4,na.rm=TRUE)
sum(new$number5,na.rm=TRUE)
sum(new$number6,na.rm=TRUE)

trial <- new %>%
  dplyr::select(-c(.geo, `system:index`))

new_df11 <- bind_rows(
  trial %>% select(label, class = class0, pixels = number0),
  trial %>% select(label, class = class1, pixels = number1),
  trial %>% select(label, class = class2, pixels = number2),
  trial %>% select(label, class = class3, pixels = number3),
  trial %>% select(label, class = class4, pixels = number4),
  trial %>% select(label, class = class5, pixels = number5),
  trial %>% select(label, class = class6, pixels = number6)
) %>%
  mutate(area = pixels*30) %>%
  mutate(year = 2011)

final_area <- rbind(new_df89, new_df90, new_df91, new_df92, new_df93, new_df94,
                    new_df95, new_df96, new_df97, new_df98, new_df99, new_df00,
                    new_df01, new_df02, new_df03, new_df04, new_df05, new_df06,
                    new_df07, new_df08, new_df09, new_df10, new_df11)

class_options <- c("1", "2", "3")

final <- final_area %>%
  filter(class %in% class_options)

write.csv(final, file = "data/detailed_area.csv")

# TOTAL TRANSITION ----

# import data 89 to 90 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 89 to 90 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 89 to 90 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 89 to 90 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 89 to 90 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 89 to 90 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/89_90_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_8990_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1990") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 90 to 91 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 90 to 91 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")


# import data 90 to 91 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")


# import data 90 to 91 2 to 3 ---- 
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")


# import data 90 to 91 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")


# import data 90 to 91 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/90_91_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9091_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1991") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")


# import data 91 to 92 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 91 to 92 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 91 to 92 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 91 to 92 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 91 to 92 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 91 to 92 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/91_92_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9192_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1992") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 92 to 93 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 92 to 93 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 92 to 93 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 92 to 93 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 92 to 93 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 92 to 93 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/92_93_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9293_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1993") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 93 to 94 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 93 to 94 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 93 to 94 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 93 to 94 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 93 to 94 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 93 to 94 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/93_94_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9394_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1994") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 94 to 95 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 94 to 95 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 94 to 95 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 94 to 95 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 94 to 95 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 94 to 95 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/94_95_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9495_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1995") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 95 to 96 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 95 to 96 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 95 to 96 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 95 to 96 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 95 to 96 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 95 to 96 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/95_96_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9596_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1996") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 96 to 97 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 96 to 97 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 96 to 97 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 96 to 97 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 96 to 97 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 96 to 97 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/96_97_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9697_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1997") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 97 to 98 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 97 to 98 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 97 to 98 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 97 to 98 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 97 to 98 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 97 to 98 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/97_98_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9798_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1998") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 98 to 99 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 98 to 99 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 98 to 99 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 98 to 99 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 98 to 99 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 98 to 99 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/98_99_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9899_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "1999") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 99 to 00 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 99 to 00 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 99 to 00 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 99 to 00 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 99 to 00 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 99 to 00 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/99_00_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_9900_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2000") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 00 to 01 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 00 to 01 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 00 to 01 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 00 to 01 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 00 to 01 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 00 to 01 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/00_01_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0001_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2001") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 01 to 02 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 01 to 02 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 01 to 02 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 01 to 02 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 01 to 02 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 01 to 02 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/01_02_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0102_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2002") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 02 to 03 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 02 to 03 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 02 to 03 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 02 to 03 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 02 to 03 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 02 to 03 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/02_03_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0203_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2003") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 03 to 04 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 03 to 04 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")


# import data 03 to 04 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")


# import data 03 to 04 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")


# import data 03 to 04 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")


# import data 03 to 04 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/03_04_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0304_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2004") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")


# import data 04 to 05 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")


# import data 04 to 05 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 04 to 05 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 04 to 05 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 04 to 05 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 04 to 05 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/04_05_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0405_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2005") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 05 to 06 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 05 to 06 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 05 to 06 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 05 to 06 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 05 to 06 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 05 to 06 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/05_06_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0506_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2006") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 06 to 07 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 06 to 07 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 06 to 07 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 06 to 07 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 06 to 07 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 06 to 07 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/06_07_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0607_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2007") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 07 to 08 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 07 to 08 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 07 to 08 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 07 to 08 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 07 to 08 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 07 to 08 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/07_08_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0708_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2008") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 08 to 09 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 08 to 09 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 08 to 09 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 08 to 09 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 08 to 09 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 08 to 09 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/08_09_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0809_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2009") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 09 to 10 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 09 to 10 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")

# import data 09 to 10 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")

# import data 09 to 10 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")

# import data 09 to 10 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")

# import data 09 to 10 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/09_10_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_0910_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2010") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")

# import data 10 to 11 1 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_1to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_1 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "1") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "3")

# import data 10 to 11 1 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_1to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_2 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "2") %>%
  mutate(previous_class = "1") %>%
  mutate(current_class = "2")


# import data 10 to 11 2 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_2to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_3 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "3") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "1")


# import data 10 to 11 2 to 3 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_2to3.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_4 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "4") %>%
  mutate(previous_class = "2") %>%
  mutate(current_class = "3")


# import data 10 to 11 3 to 1 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_3to1.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_5 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "5") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "1")


# import data 10 to 11 3 to 2 ----
data <- read_csv("~/Documents/Edinburgh Year 4/dissertation/data/10_11_3to2.csv")

# format ----
new <- data %>%
  separate(groups, into = c('class0', 'number0', 
                            'class1', 'number1'), sep = ",") 

new$class0 <- gsub("[^0-9.-]", "", new$class0)
new$number0 <- gsub("[^0-9.-]", "", new$number0)
new$class1 <- gsub("[^0-9.-]", "", new$class1)
new$number1 <- gsub("[^0-9.-]", "", new$number1)

new$number0 <- as.numeric(new$number0)
new$number1 <- as.numeric(new$number1)

# get sum of all regions for transition and input into excel -----
sum(new$number1,na.rm=TRUE)

transition_1011_6 <- new %>%
  dplyr::select(-c(.geo, `system:index`, class0, class1, number0)) %>%
  mutate(area = number1*30) %>%
  mutate(year = "2011") %>%
  mutate(transition = "6") %>%
  mutate(previous_class = "3") %>%
  mutate(current_class = "2")


final_transition <- rbind(transition_8990_1,transition_8990_2, transition_8990_3, 
                          transition_8990_4, transition_8990_5, transition_8990_6,
                          transition_9091_1, transition_9091_2, transition_9091_3,
                          transition_9091_4, transition_9091_5, transition_9091_6,
                          transition_9192_1, transition_9192_2, transition_9192_3,
                          transition_9192_4, transition_9192_5, transition_9192_6,
                          transition_9293_1, transition_9293_2, transition_9293_3,
                          transition_9293_4, transition_9293_5, transition_9293_6,
                          transition_9394_1, transition_9394_2, transition_9394_3,
                          transition_9394_4, transition_9394_5, transition_9394_6,
                          transition_9495_1, transition_9495_2, transition_9495_3,
                          transition_9495_4, transition_9495_5, transition_9495_6,
                          transition_9596_1, transition_9596_2, transition_9596_3,
                          transition_9596_4, transition_9596_5, transition_9596_6,
                          transition_9697_1, transition_9697_2, transition_9697_3,
                          transition_9697_4, transition_9697_5, transition_9697_6,
                          transition_9798_1, transition_9798_2, transition_9798_3,
                          transition_9798_4, transition_9798_5, transition_9798_6,
                          transition_9899_1, transition_9899_2, transition_9899_3,
                          transition_9899_4, transition_9899_5, transition_9899_6,
                          transition_9900_1, transition_9900_2, transition_9900_3,
                          transition_9900_4, transition_9900_5, transition_9900_6,
                          transition_0001_1, transition_0001_2, transition_0001_3,
                          transition_0001_4, transition_0001_5, transition_0001_6,
                          transition_0102_1, transition_0102_2, transition_0102_3,
                          transition_0102_4, transition_0102_5, transition_0102_6,
                          transition_0203_1, transition_0203_2, transition_0203_3,
                          transition_0203_4, transition_0203_5, transition_0203_6,
                          transition_0304_1, transition_0304_2, transition_0304_3,
                          transition_0304_4, transition_0304_5, transition_0304_6,
                          transition_0405_1, transition_0405_2, transition_0405_3,
                          transition_0405_4, transition_0405_5, transition_0405_6,
                          transition_0506_1, transition_0506_2, transition_0506_3,
                          transition_0506_4, transition_0506_5, transition_0506_6,
                          transition_0607_1, transition_0607_2, transition_0607_3,
                          transition_0607_4, transition_0607_5, transition_0607_6,
                          transition_0708_1, transition_0708_2, transition_0708_3,
                          transition_0708_4, transition_0708_5, transition_0708_6,
                          transition_0809_1, transition_0809_2, transition_0809_3,
                          transition_0809_4, transition_0809_5, transition_0809_6,
                          transition_0910_1, transition_0910_2, transition_0910_3,
                          transition_0910_4, transition_0910_5, transition_0910_6,
                          transition_1011_1, transition_1011_2, transition_1011_3,
                          transition_1011_4, transition_1011_5, transition_1011_6)

colnames(final_transition)[colnames(final_transition) == "number1"] <- "pixels"

write.csv(final_transition, file = "data/detailed_transition.csv")


