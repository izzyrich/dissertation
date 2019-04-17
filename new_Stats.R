# new stats and data vis script cause you kinda went crazy before let's be honest

# load packages ----
library(readr)
library(tidyverse)
library(lme4)
library(MuMIn)
library(lmerTest)
library(segmented)

# load data ----
detailed_area <- read_csv("data/detailed_area.csv") %>%
  dplyr::select(-c("X1"))
colnames(detailed_area)[colnames(detailed_area) == "label"] <- "cell"
detailed_area$class <- factor(detailed_area$class)
detailed_area$cell <- factor(detailed_area$cell)

detailed_transition <- read_csv("data/detailed_transition.csv") %>%
  dplyr::select(-c("X1"))
colnames(detailed_transition)[colnames(detailed_transition) == "label"] <- "cell"
detailed_transition$cell <- factor(detailed_transition$cell)
detailed_transition$transition <- factor(detailed_transition$transition)

# apply bigger grid
detailedA <-  detailed_area %>%
  mutate(grid = if_else(cell == 8 | cell == 9 | cell == 10 | cell == 11 | cell == 23 |
                          cell == 24 | cell == 25 | cell == 26 | cell == 27 |
                          cell == 38 | cell == 39 | cell == 40 | cell == 41 |
                          cell == 42 | cell == 43 | cell == 53 | cell == 54 | cell == 55 | cell == 56, "NW", 
                        if_else(cell == 3 | cell == 4 | cell == 5 | cell == 6 | cell == 7 |
                                  cell == 19 | cell == 20 | cell == 21 | cell == 22 |
                                  cell == 35 | cell == 36 | cell == 37 | cell == 49 |
                                  cell == 50 | cell == 51 | cell == 52, "SW", 
                                if_else(cell == 64 | cell == 65 | cell == 66 | cell == 67 | cell == 68 |
                                          cell == 69 | cell == 79 | cell == 80 | cell == 81 |
                                          cell == 82 | cell == 83 | cell == 84 | cell == 85 |
                                          cell == 86 | cell == 87 | cell == 88 | cell == 89 | cell == 94 | 
                                          cell == 95 | cell == 96 | cell == 97 | cell == 98 | cell == 99 |
                                          cell == 100 | cell == 101 | cell == 102 | cell == 103 | cell == 104, "C",
                                        if_else(cell == 113 | cell == 114 | cell == 115 | cell == 116 | cell == 117 |
                                                  cell == 118 | cell == 119 | cell == 120 | cell == 128 |
                                                  cell == 129 | cell == 130 | cell == 131 | cell == 132 |
                                                  cell == 133 | cell == 134 | cell == 143 | cell == 144 | cell == 145 | 
                                                  cell == 146 | cell == 147 | cell == 158 | cell == 159 | cell == 160 |
                                                  cell == 161 | cell == 162 | cell == 173 | cell == 174 | cell == 175 | cell == 176, "NE",
                                                if_else(cell == 108 | cell == 109 | cell == 110 | cell == 111 | cell == 112 |
                                                          cell == 121 | cell == 122 | cell == 123 | cell == 124 |
                                                          cell == 125 | cell == 126 | cell == 127 | cell == 136 |
                                                          cell == 137 | cell == 138 | cell == 139 | cell == 140 | cell == 141 | 
                                                          cell == 142 | cell == 151 | cell == 152 | cell == 153 | cell == 154 |
                                                          cell == 155 | cell == 156 | cell == 157 | cell == 166 | cell == 167 | cell == 168 |
                                                          cell == 169 | cell == 170 | cell == 171 | cell == 172 | cell == 183 | cell == 184 | cell == 185 |
                                                          cell == 186, "SE", "NA")))))) 

# apply bigger grid
detailedT <- detailed_transition %>%
  mutate(grid = if_else(cell == 8 | cell == 9 | cell == 10 | cell == 11 | cell == 23 |
                          cell == 24 | cell == 25 | cell == 26 | cell == 27 |
                          cell == 38 | cell == 39 | cell == 40 | cell == 41 |
                          cell == 42 | cell == 43 | cell == 53 | cell == 54 | cell == 55 | cell == 56, "NW", 
                        if_else(cell == 3 | cell == 4 | cell == 5 | cell == 6 | cell == 7 |
                                  cell == 19 | cell == 20 | cell == 21 | cell == 22 |
                                  cell == 35 | cell == 36 | cell == 37 | cell == 49 |
                                  cell == 50 | cell == 51 | cell == 52, "SW", 
                                if_else(cell == 64 | cell == 65 | cell == 66 | cell == 67 | cell == 68 |
                                          cell == 69 | cell == 79 | cell == 80 | cell == 81 |
                                          cell == 82 | cell == 83 | cell == 84 | cell == 85 |
                                          cell == 86 | cell == 87 | cell == 88 | cell == 89 | cell == 94 | 
                                          cell == 95 | cell == 96 | cell == 97 | cell == 98 | cell == 99 |
                                          cell == 100 | cell == 101 | cell == 102 | cell == 103 | cell == 104, "C",
                                        if_else(cell == 113 | cell == 114 | cell == 115 | cell == 116 | cell == 117 |
                                                  cell == 118 | cell == 119 | cell == 120 | cell == 128 |
                                                  cell == 129 | cell == 130 | cell == 131 | cell == 132 |
                                                  cell == 133 | cell == 134 | cell == 143 | cell == 144 | cell == 145 | 
                                                  cell == 146 | cell == 147 | cell == 158 | cell == 159 | cell == 160 |
                                                  cell == 161 | cell == 162 | cell == 173 | cell == 174 | cell == 175 | cell == 176, "NE",
                                                if_else(cell == 108 | cell == 109 | cell == 110 | cell == 111 | cell == 112 |
                                                          cell == 121 | cell == 122 | cell == 123 | cell == 124 |
                                                          cell == 125 | cell == 126 | cell == 127 | cell == 136 |
                                                          cell == 137 | cell == 138 | cell == 139 | cell == 140 | cell == 141 | 
                                                          cell == 142 | cell == 151 | cell == 152 | cell == 153 | cell == 154 |
                                                          cell == 155 | cell == 156 | cell == 157 | cell == 166 | cell == 167 | cell == 168 |
                                                          cell == 169 | cell == 170 | cell == 171 | cell == 172 | cell == 183 | cell == 184 | cell == 185 |
                                                          cell == 186, "SE", "NA")))))) 

# DFs for Q1
# before and after in km2 per cell 
questiononeSUC <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1991 | year == 1992 | year == 1993 | year == 1994) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(area = mean(area)/1000) 


questiononeEUA <- detailedA %>%
  dplyr::filter(year == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005 | year == 2006) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2001 | year == 2002 | year == 2003, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(area = mean(area)/1000) 

# DFs for Q2
# before and after 
questiontwoSUC <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1992 | year == 1993 | year == 1994 | year == 1995) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991 | year == 1992, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(area = mean(area)/1000) 

# for EUA
questiontwoEUA <- detailedT %>%
  dplyr::filter(year == 2002 | year == 2003 | year == 2004 | year == 2005 | year == 2006 | year == 2007) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2002| year == 2003 | year == 2004, "first", "second")) %>%
  dplyr::select(-c(year) )%>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(area = mean(area)/1000) 


# DFs for Q3 ----
questionthreeASUC <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1991 | year == 1995 | year == 1996 | year == 1997) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(area = mean(area)/1000) 


questionthreeAEUA <- detailedA %>%
  dplyr::filter(year == 2001 | year == 2002 | year == 2003 | year == 2007 | year == 2008 | year == 2009) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2001 | year == 2002 | year == 2003, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(area = mean(area)/1000) 

questionthreeTSUC <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1992 | year == 1996 | year == 1997 | year == 1998) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991 | year == 1992, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(area = mean(area)/1000) 

questionthreeTEUA <- detailedT %>%
  dplyr::filter(year == 2002 | year == 2003 | year == 2004 | year == 2008 | year == 2009 | year == 2010) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2002| year == 2003 | year == 2004, "first", "second")) %>%
  dplyr::select(-c(year) )%>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(area = mean(area)/1000) 

# Q1 -- Here we want to see the effect of SPE on LUC areas -
# is a change of area significant within the three years following SPE event??
# here we want to account for the variation across Latvia BUT we don't want to 
# statistically examine the differences -- why? cause it is clear that regions
# will experience different things, but that's more likely to be due to other 
# things than the SPE event!!

# Abandoned SUC ----
questiononeSUC1 <- questiononeSUC %>%
  dplyr::filter(class == 1) 

questiononeSUC1$grid <- factor(questiononeSUC1$grid)
questiononeSUC1$cell <- factor(questiononeSUC1$cell)
questiononeSUC1$before_after <- factor(questiononeSUC1$before_after)

# model
abandonedSUC <- lmer(area ~ before_after + (1|grid), data = questiononeSUC1)
summary(abandonedSUC)

r.squaredGLMM(abandonedSUC)

# Graph relationship
plot(area ~ before_after, data = questiononeSUC1)

# check assumptions
plot(abandonedSUC)
qqnorm(resid(abandonedSUC)) 
qqline(resid(abandonedSUC)) 


# Abandoned EUA ----
questiononeEUA1 <- questiononeEUA %>%
  dplyr::filter(class == 1) 

questiononeEUA1$grid <- factor(questiononeEUA1$grid)
questiononeEUA1$cell <- factor(questiononeEUA1$cell)
questiononeEUA1$before_after <- factor(questiononeEUA1$before_after)

# model
abandonedEUA <- lmer(area ~ before_after + (1|grid), data = questiononeEUA1)
summary(abandonedEUA)

r.squaredGLMM(abandonedEUA)

# Graph relationship
plot(area ~ before_after, data = questiononeEUA1)

# check assumptions
plot(abandonedEUA)
qqnorm(resid(abandonedEUA)) 
qqline(resid(abandonedEUA)) 

# Extensive SUC ----
questiononeSUC2 <- questiononeSUC %>%
  dplyr::filter(class == 2) 

questiononeSUC2$grid <- factor(questiononeSUC2$grid)
questiononeSUC2$cell <- factor(questiononeSUC2$cell)
questiononeSUC2$before_after <- factor(questiononeSUC2$before_after)

# model
extensiveSUC <- lmer(area ~ before_after + (1|grid), data = questiononeSUC2)
summary(extensiveSUC)

r.squaredGLMM(extensiveSUC)

# Graph relationship
plot(area ~ before_after, data = questiononeSUC2)

# check assumptions
plot(extensiveSUC)
qqnorm(resid(extensiveSUC)) 
qqline(resid(extensiveSUC)) # meets assumptions if logged 


# Extensive EUA ----
questiononeEUA2 <- questiononeEUA %>%
  dplyr::filter(class == 2) 

questiononeEUA2$grid <- factor(questiononeEUA2$grid)
questiononeEUA2$cell <- factor(questiononeEUA2$cell)
questiononeEUA2$before_after <- factor(questiononeEUA2$before_after)

# model
extensiveEUA <- lmer(area ~ before_after + (1|grid), data = questiononeEUA2)
summary(extensiveEUA)

r.squaredGLMM(extensiveEUA)

# Graph relationship
plot(area ~ before_after, data = questiononeEUA2)

# check assumptions
plot(extensiveEUA)
qqnorm(resid(extensiveEUA)) 
qqline(resid(extensiveEUA)) 

# Intensive SUC ----
questiononeSUC3 <- questiononeSUC %>%
  dplyr::filter(class == 3) 

questiononeSUC3$grid <- factor(questiononeSUC3$grid)
questiononeSUC3$cell <- factor(questiononeSUC3$cell)
questiononeSUC3$before_after <- factor(questiononeSUC3$before_after)

# model
intensiveSUC <- lmer(area ~ before_after + (1|grid), data = questiononeSUC3)
summary(intensiveSUC)

r.squaredGLMM(intensiveSUC)

# Graph relationship
plot(area ~ before_after, data = questiononeSUC3)

# check assumptions
plot(intensiveSUC)
qqnorm(resid(intensiveSUC)) 
qqline(resid(intensiveSUC))  


# Intensive EUA ----
questiononeEUA3 <- questiononeEUA %>%
  dplyr::filter(class == 3) 

questiononeEUA3$grid <- factor(questiononeEUA3$grid)
questiononeEUA3$cell <- factor(questiononeEUA3$cell)
questiononeEUA3$before_after <- factor(questiononeEUA3$before_after)

# model
intensiveEUA <- lmer(area ~ before_after + (1|grid), data = questiononeEUA3)
summary(intensiveEUA)

r.squaredGLMM(intensiveEUA)

# Graph relationship
plot(area ~ before_after, data = questiononeEUA3)

# check assumptions
plot(intensiveEUA)
qqnorm(resid(intensiveEUA)) 
qqline(resid(intensiveEUA)) # meets assumptions if logged 

# Q2 A--I: EUA ----
questiontwoEUA1 <- questiontwoEUA %>%
  dplyr::filter(transition == 1)

questiontwoEUA1$grid <- factor(questiontwoEUA1$grid)
questiontwoEUA1$cell <- factor(questiontwoEUA1$cell)
questiontwoEUA1$before_after <- factor(questiontwoEUA1$before_after)

atoiEUA <- lmer(area ~ before_after + (1|grid), data = questiontwoEUA1)
summary(atoiEUA)

plot(area ~ before_after, data = questiontwoEUA1)

r.squaredGLMM(atoiEUA)

ggplot(questiontwoEUA1,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiEUA)
qqnorm(resid(atoiEUA)) 
qqline(resid(atoiEUA)) 

# E to I EUA ----
questiontwoEUA4 <- questiontwoEUA %>%
  dplyr::filter(transition == 4)

questiontwoEUA4$grid <- factor(questiontwoEUA4$grid)
questiontwoEUA4$cell <- factor(questiontwoEUA4$cell)
questiontwoEUA4$before_after <- factor(questiontwoEUA4$before_after)

atoiEUA <- lmer(area ~ before_after + (1|grid), data = questiontwoEUA4)
summary(atoiEUA)

plot(area ~ before_after, data = questiontwoEUA1)

r.squaredGLMM(atoiEUA)

ggplot(questiontwoEUA1,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiEUA)
qqnorm(resid(atoiEUA)) 
qqline(resid(atoiEUA)) 

# Q2 I to A: SUC ----
questiontwoSUC5 <- questiontwoSUC %>%
  dplyr::filter(transition == 5)

questiontwoSUC5$grid <- factor(questiontwoSUC5$grid)
questiontwoSUC5$cell <- factor(questiontwoSUC5$cell)
questiontwoSUC5$before_after <- factor(questiontwoSUC5$before_after)

itoaSUC <- lmer(area ~ before_after + (1|grid), data = questiontwoSUC5)
summary(itoaSUC)

plot(area ~ before_after, data = questiontwoEUA1)

r.squaredGLMM(itoaSUC)

ggplot(questiontwoSUC5,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoaSUC)
qqnorm(resid(itoaSUC)) 
qqline(resid(itoaSUC)) 

# Q2 I to E: SUC ----
questiontwoSUC6 <- questiontwoSUC %>%
  dplyr::filter(transition == 6)

questiontwoSUC6$grid <- factor(questiontwoSUC6$grid)
questiontwoSUC6$cell <- factor(questiontwoSUC6$cell)
questiontwoSUC6$before_after <- factor(questiontwoSUC6$before_after)

itoeSUC <- lmer(area ~ before_after + (1|grid), data = questiontwoSUC6)
summary(itoeSUC)

plot(area ~ before_after, data = questiontwoEUA1)

r.squaredGLMM(itoeSUC)

ggplot(questiontwoSUC6,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoeSUC)
qqnorm(resid(itoeSUC)) 
qqline(resid(itoeSUC)) 

# Q3 lag Abandoned SUC ----
questionthreeSUC1 <- questionthreeASUC %>%
  dplyr::filter(class == 1) 

questionthreeSUC1$grid <- factor(questionthreeSUC1$grid)
questionthreeSUC1$cell <- factor(questionthreeSUC1$cell)
questionthreeSUC1$before_after <- factor(questionthreeSUC1$before_after)

# model
abandonedlagSUC <- lmer(area ~ before_after + (1|grid), data = questionthreeSUC1)
summary(abandonedlagSUC)

r.squaredGLMM(abandonedlagSUC)

# Graph relationship
plot(area ~ before_after, data = questionthreeSUC1)

# check assumptions
plot(abandonedlagSUC)
qqnorm(resid(abandonedlagSUC)) 
qqline(resid(abandonedlagSUC)) 


# Abandoned EUA ----
questionthreeEUA1 <- questionthreeAEUA %>%
  dplyr::filter(class == 1) 

questionthreeEUA1$grid <- factor(questionthreeEUA1$grid)
questionthreeEUA1$cell <- factor(questionthreeEUA1$cell)
questionthreeEUA1$before_after <- factor(questionthreeEUA1$before_after)

# model
abandonedlagEUA <- lmer(area ~ before_after + (1|grid), data = questionthreeEUA1)
summary(abandonedlagEUA)

r.squaredGLMM(abandonedlagEUA)

# Graph relationship
plot(area ~ before_after, data = questionthreeEUA1)

# check assumptions
plot(abandonedlagEUA)
qqnorm(resid(abandonedlagEUA)) 
qqline(resid(abandonedlagEUA)) 

# Extensive SUC ----
questionthreeSUC2 <- questionthreeASUC %>%
  dplyr::filter(class == 2) 

questionthreeSUC2$grid <- factor(questionthreeSUC2$grid)
questionthreeSUC2$cell <- factor(questionthreeSUC2$cell)
questionthreeSUC2$before_after <- factor(questionthreeSUC2$before_after)

# model
extensivelagSUC <- lmer(area ~ before_after + (1|grid), data = questionthreeSUC2)
summary(extensivelagSUC)

r.squaredGLMM(extensivelagSUC)

# Graph relationship
plot(area ~ before_after, data = questionthreeSUC2)

# check assumptions
plot(extensivelagSUC)
qqnorm(resid(extensivelagSUC)) 
qqline(resid(extensivelagSUC)) # meets assumptions if logged 


# Extensive EUA ----
questionthreeEUA2 <- questionthreeAEUA %>%
  dplyr::filter(class == 2) 

questionthreeEUA2$grid <- factor(questionthreeEUA2$grid)
questionthreeEUA2$cell <- factor(questionthreeEUA2$cell)
questionthreeEUA2$before_after <- factor(questionthreeEUA2$before_after)

# model
extensivelagEUA <- lmer(area ~ before_after + (1|grid), data = questionthreeEUA2)
summary(extensivelagEUA)

r.squaredGLMM(extensivelagEUA)

# Graph relationship
plot(area ~ before_after, data = questionthreeEUA2)

# check assumptions
plot(extensivelagEUA)
qqnorm(resid(extensivelagEUA)) 
qqline(resid(extensivelagEUA)) 

# Intensive SUC ----
questionthreeSUC3 <- questionthreeASUC %>%
  dplyr::filter(class == 3) 

questionthreeSUC3$grid <- factor(questionthreeSUC3$grid)
questionthreeSUC3$cell <- factor(questionthreeSUC3$cell)
questionthreeSUC3$before_after <- factor(questionthreeSUC3$before_after)

# model
intensivelagSUC <- lmer(area ~ before_after + (1|grid), data = questionthreeSUC3)
summary(intensivelagSUC)

r.squaredGLMM(intensivelagSUC)

# Graph relationship
plot(area ~ before_after, data = questionthreeSUC3)

# check assumptions
plot(intensivelagSUC)
qqnorm(resid(intensivelagSUC)) 
qqline(resid(intensivelagSUC))  


# Intensive EUA ----
questionthreeEUA3 <- questionthreeAEUA %>%
  dplyr::filter(class == 3) 

questionthreeEUA3$grid <- factor(questionthreeEUA3$grid)
questionthreeEUA3$cell <- factor(questionthreeEUA3$cell)
questionthreeEUA3$before_after <- factor(questionthreeEUA3$before_after)

# model
intensivelagEUA <- lmer(area ~ before_after + (1|grid), data = questionthreeEUA3)
summary(intensivelagEUA)

r.squaredGLMM(intensivelagEUA)

# Graph relationship
plot(area ~ before_after, data = questionthreeEUA3)

# check assumptions
plot(intensivelagEUA)
qqnorm(resid(intensivelagEUA)) 
qqline(resid(intensivelagEUA)) # meets assumptions if logged 

# Q2 A--I: EUA ----
questionthreetEUA1 <- questionthreeTEUA %>%
  dplyr::filter(transition == 1)

questionthreetEUA1$grid <- factor(questionthreetEUA1$grid)
questionthreetEUA1$cell <- factor(questionthreetEUA1$cell)
questionthreetEUA1$before_after <- factor(questionthreetEUA1$before_after)

atoilagEUA <- lmer(area ~ before_after + (1|grid), data = questionthreetEUA1)
summary(atoiEUA)

plot(area ~ before_after, data = questionthreetEUA1)

r.squaredGLMM(atoiEUA)

ggplot(questionthreetEUA1,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiEUA)
qqnorm(resid(atoiEUA)) 
qqline(resid(atoiEUA)) 

# E to I EUA ----
questionthreetEUA4 <- questionthreeTEUA %>%
  dplyr::filter(transition == 4)

questionthreetEUA4$grid <- factor(questionthreetEUA4$grid)
questionthreetEUA4$cell <- factor(questionthreetEUA4$cell)
questionthreetEUA4$before_after <- factor(questionthreetEUA4$before_after)

atoilagEUA <- lmer(area ~ before_after + (1|grid), data = questionthreetEUA4)
summary(atoilagEUA)

plot(area ~ before_after, data = questionthreetEUA1)

r.squaredGLMM(atoilagEUA)

ggplot(questionthreetEUA1,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoilagEUA)
qqnorm(resid(atoilagEUA)) 
qqline(resid(atoilagEUA)) 

# Q2 I to A: SUC ----
questionthreetSUC5 <- questionthreeTSUC %>%
  dplyr::filter(transition == 5)

questionthreetSUC5$grid <- factor(questionthreetSUC5$grid)
questionthreetSUC5$cell <- factor(questionthreetSUC5$cell)
questionthreetSUC5$before_after <- factor(questionthreetSUC5$before_after)

itoalagSUC <- lmer(area ~ before_after + (1|grid), data = questionthreetSUC5)
summary(itoalagSUC)

plot(area ~ before_after, data = questionthreetEUA1)

r.squaredGLMM(itoaSUC)

ggplot(questionthreetSUC5,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoaSUC)
qqnorm(resid(itoaSUC)) 
qqline(resid(itoaSUC)) 

# Q2 I to E: SUC ----
questionthreetSUC6 <- questionthreeTSUC %>%
  dplyr::filter(transition == 6)

questionthreetSUC6$grid <- factor(questionthreetSUC6$grid)
questionthreetSUC6$cell <- factor(questionthreetSUC6$cell)
questionthreetSUC6$before_after <- factor(questionthreetSUC6$before_after)

itoelagSUC <- lmer(area ~ before_after + (1|grid), data = questionthreetSUC6)
summary(itoelagSUC)

plot(area ~ before_after, data = questionthreetEUA1)

r.squaredGLMM(itoelagSUC)

ggplot(questionthreetSUC6,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoelagSUC)
qqnorm(resid(itoelagSUC)) 
qqline(resid(itoelagSUC)) 

# Q3 part b ----
abandonedseglag <- detailedA %>%
  dplyr::select(-c(pixels)) %>%
  filter(class == "1") %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)

abandonedlm <- lm(year_total ~ year + grid, data = abandonedlag) 
summary(abandonedlm)

abandonedmod <- segmented(abandonedlm, seg.Z = ~year, psi = list(year = c(1991,2004)))
summary(abandonedmod)

extensiveseglag <- detailedA %>%
  dplyr::select(-c(pixels)) %>%
  filter(class == "2") %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)

extensivelm <- lm(year_total ~ year + grid, data = extensiveseglag) 
summary(extensivelm)

extensivemod <- segmented(extensivelm, seg.Z = ~year, psi = list(year = c(1991,2004)))
summary(extensivemod)

intensiveseglag <- detailedA %>%
  dplyr::select(-c(pixels)) %>%
  filter(class == "3") %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)

intensivelm <- lm(year_total ~ year + grid, data = intensiveseglag) 
summary(intensivelm)

intensivemod <- segmented(intensivelm, seg.Z = ~year, psi = list(year = c(1991,2004)))
summary(intensivemod)

itoaseglag <- detailedT %>%
  dplyr::select(-c(pixels, previous_class, current_class, cell)) %>%
  filter(transition == "5") %>%
  dplyr::select(-c(transition)) %>%
  na.omit() %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)

itoalm <- lm(year_total ~ year + grid, data = itoaseglag) 
summary(itoalm)

itoamod <- segmented(itoalm, seg.Z = ~year, psi = list(year = c(1991,2004)))
summary(itoamod)

itoeseglag <- detailedT %>%
  dplyr::select(-c(pixels, previous_class, current_class, cell)) %>%
  filter(transition == "6") %>%
  dplyr::select(-c(transition)) %>%
  na.omit() %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)


itoelm <- lm(year_total ~ year + grid, data = itoeseglag) 
summary(itoelm)

itoemod <- segmented(itoelm, seg.Z = ~year, psi = list(year = c(1993,2004)))
summary(itoemod)

etoiseglag <- detailedT %>%
  dplyr::select(-c(pixels, previous_class, current_class, cell)) %>%
  filter(transition == "4") %>%
  dplyr::select(-c(transition)) %>%
  na.omit() %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)


etoilm <- lm(year_total ~ year + grid, data = etoiseglag) 
summary(etoilm)

etoimod <- segmented(etoilm, seg.Z = ~year, psi = list(year = c(1993,2004)))
summary(etoimod)

atoiseglag <- detailedT %>%
  dplyr::select(-c(pixels, previous_class, current_class, cell)) %>%
  filter(transition == "1") %>%
  dplyr::select(-c(transition)) %>%
  na.omit() %>%
  group_by(year, grid) %>%
  summarise(year_total = sum(area)/1000)


atoilm <- lm(year_total ~ year + grid, data = atoiseglag) 
summary(atoilm)

atoimod <- segmented(atoilm, seg.Z = ~year, psi = list(year = c(1992,2004)))
summary(atoimod)

