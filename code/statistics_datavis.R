# statistics for diss

# load packages
library(readr)
library(tidyverse)
library(lme4)
library(MuMIn)
library(nlme)
library(stargazer)
library(sjPlot)
library(ggeffects)
library(scales)
library(gridExtra)
library(broom)
library(lmerTest)
library(segmented)
library(effects)

# theme from coding club tutorial
theme_marine <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype="blank"))
}

# import datasests + format ----
areaset <- read_csv("data/area.csv")
areaset$landuse <- factor(areaset$landuse)
areaset$year <- factor(areaset$year)

transition <- read_csv("data/transition.csv")

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
# before and after in km2
# Comment from Isla: You had a mistake in the filter call - you have to spell out the 'year ==' bit each time
questiononeSUC <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1992 | year == 1993) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(average = mean(area)/1000) 

# attempt at percent cover 
# Comment from Isla: so I think for percent cover here you want to devide by 
# the area of a total cell, rather than the sum of the area of the particular land cover
questiononeSUCNEW <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1992 | year == 1993) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, cell) %>%
  mutate(totalper = sum(area)) %>%
  ungroup() %>%
  mutate(percent_cover = area*100/totalper)

# attempt at percent cover by grid 
# Comment from Isla: I don't know that it makes sense to do the percent cover by your "regions"
# that you are calling grid 
questiononeSUCNEWbygrid <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1992 | year == 1993) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid) %>%
  mutate(totalper = sum(area)) %>%
  ungroup() %>%
  dplyr::select(-c(cell)) %>%
  group_by(grid,class) %>%
  mutate(total = sum(area)) %>%
  dplyr::select(-c(area)) %>%
  distinct() %>%
  mutate(percent_cover = total*100/totalper)

# before and after EUA 
questiononeEUA <- detailedA %>%
  dplyr::filter(year == 2002 | year == 2003 | year == 2005 | year == 2006) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2002 | year == 2003, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell) %>%
  summarise(average = mean(area)/1000) 

# before and after transition - dates are different cause its transition to the start of the next year
questiontwoSUC <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1993 | year == 1994) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(average = mean(area)/1000) 

# for EUA
questiontwoEUA <- detailedT %>%
  dplyr::filter(year == 2003 | year == 2004 | year == 2006 | year == 2007) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2003 | year == 2004, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(average = mean(area)/1000) 

# ACTUALLY Q1 ----
# looking at the change in land-use ACROSS the country before and after SPE events 
# categories of 3 years before and 3 years after 
# group by class and get the mean area for each period 
# is there a signal caused by the SPE event? 
# separate model for each land use type for each event - 6 models 
# cell as is now can't be random effect -- there is only one value in each cell! 
# SO need to make bigger cells that the smaller cells can be nested into - can
# do this manually by aggregating cells 
# mean ~ before_after + (1|cell/big_cell) - model for each class for each land-use type
# delete all 0s besides the ones that have a value for the other time period - need those

# Q1: Abandoned:SUC ----

# attempt at following coding tutorial for data vis
#questiononeSUC1 <- questiononeSUC %>%
#  dplyr::filter(class == 1) %>%
#  group_by(before_after, grid, cell) %>%
#  summarise(maxaverage = max(average)) %>%
#  group_by(before_after, grid, cell) %>%
#  summarise(mean = mean(maxaverage))

questiononeSUC1 <- questiononeSUC %>%
  dplyr::filter(class == 1)

questiononeSUC1$grid <- factor(questiononeSUC1$grid)
questiononeSUC1$cell <- factor(questiononeSUC1$cell)
questiononeSUC1$before_after <- factor(questiononeSUC1$before_after)

# model
abandonedSUC <- lmer(average ~ before_after + (1|grid), data = questiononeSUC1)
summary(abandonedSUC)

# Question from Isla: Are there replicates within each cell?  If not then I don't think there should be a cell
# random effect, just the grid one.

# Graph relationship
plot(average ~ before_after, data = questiononeSUC1)

#hist(questiononeSUC1$average)
ggplot(questiononeSUC1,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
qqnorm(resid(abandonedSUC)) 
qqline(resid(abandonedSUC)) # meets assumptions!!!!

r.squaredGLMM(abandonedSUC)  # 0.006821604 0.8151366
# Comment from Isla: The first number is the pseudo R2 - which is a very small amount of variance
# explained by the fixed effect - 0.68% of the variance

# effects into df 
abandonedSUCef <- effect("before_after", abandonedSUC)
abandonedSUCdf <- as.data.frame(abandonedSUCef)
abandonedSUCdf <- abandonedSUCdf %>%
  mutate(class = "abandoned")

# effect size - the effect sizes are pretty much the same
ggplot(abandonedSUCdf, aes(x = before_after , y = fit)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = 0.2) 

# thinking about data vis
# Set a clean theme for the graphs
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

(fe.effects <- plot_model(abandonedSUC, show.values = TRUE))
(re.effects <- plot_model(abandonedSUC, type = "re", show.values = TRUE))

# predictions - pretty much the same
ggpredict(abandonedSUC, terms = c("before_after")) %>% plot()

ggpredict(abandonedSUC, terms = c("before_after", "grid", "cell"), type = "re") %>% plot()

# figure
(ggplot(questiononeSUC1,aes(x=before_after,y=average))+
    geom_point()+
    geom_line(aes(group=cell))+  ## connect subjects with a line
    facet_grid(.~grid) )                     ## squash together

# Comment from Isla: You could model this by putting region in as a fixed effect in your model above

# statistical model
abandonedSUC_region <- lm(average ~ before_after + grid, data = questiononeSUC1)
summary(abandonedSUC_region)

# trying coding club tutorial
abandonedSUCdf <- tidy(abandonedSUC)

mm.abandoned <- expand.grid(before_after = c("first", "second"), mean = 0)

mmSUCa <- model.matrix(terms(abandonedSUC), mm.abandoned)

mm.abandoned$mean <- mm %*% fixef(abandonedSUC)

# Q1 Extensive: SUC ----
questiononeSUC2 <- questiononeSUCNEW %>%
  dplyr::filter(class == 2)

questiononeSUC2$grid <- factor(questiononeSUC2$grid)
questiononeSUC2$cell <- factor(questiononeSUC2$cell)
questiononeSUC2$before_after <- factor(questiononeSUC2$before_after)

# statistical model
extensiveSUC <- lmer(percent_cover ~ before_after + (1|grid), data = questiononeSUC2)
summary(extensiveSUC)

r.squaredGLMM(extensiveSUC)

#hist(questiononeSUC2$totalper)
ggplot(questiononeSUC2,aes(x=totalper))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(extensiveSUC)
qqnorm(resid(extensiveSUC)) 
qqline(resid(extensiveSUC)) # meets assumptions!!!!

extensiveSUCef <- effect("before_after", extensiveSUC)
extensiveSUCdf <- as.data.frame(extensiveSUCef) 
extensiveSUCdf <- extensiveSUCdf %>%
  mutate(class = "extensive")


# Q1 Extensive: EUA ----
questiononeEUA2 <- questiononeEUA %>%
  dplyr::filter(class == 2)

questiononeEUA2$grid <- factor(questiononeEUA2$grid)
questiononeEUA2$cell <- factor(questiononeEUA2$cell)
questiononeEUA2$before_after <- factor(questiononeEUA2$before_after)

extensiveEUA <- lmer(average ~ before_after + (1|grid), data = questiononeEUA2)
summary(extensiveEUA)

r.squaredGLMM(extensiveEUA)

#hist(questiononeEUA2$average)
ggplot(questiononeEUA2,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(extensiveEUA)
qqnorm(resid(extensiveEUA)) 
qqline(resid(extensiveEUA)) # meets assumptions!!!!

# Q1 Intensive: SUC ----
questiononeSUC3 <- questiononeSUCNEW %>%
  dplyr::filter(class == 3)

questiononeSUC3$grid <- factor(questiononeSUC3$grid)
questiononeSUC3$cell <- factor(questiononeSUC3$cell)
questiononeSUC3$before_after <- factor(questiononeSUC3$before_after)

intensiveSUC <- lmer(area ~ before_after + (1|grid), data = questiononeSUC3)
summary(intensiveSUC)

r.squaredGLMM(intensiveSUC)

#hist(questiononeSUC3$area)
ggplot(questiononeSUC3,aes(x=area))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(intensiveSUC)
qqnorm(resid(intensiveSUC)) 
qqline(resid(intensiveSUC)) # meets assumptions!!!!

intensiveSUCef <- effect("before_after", intensiveSUC)
intensiveSUCdf <- as.data.frame(intensiveSUCef)
intensiveSUCdf <- intensiveSUCdf %>%
  mutate(class = "intensive")

ggplot(intensiveSUCdf, aes(x = before_after , y = fit, fill = "#68228B")) + 
  geom_bar(stat="identity") + 
  theme(legend.background = element_blank()) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = 0.2) 


# Q1 Intensive: EUA ----
questiononeEUA3 <- questiononeEUA %>%
  dplyr::filter(class == 3)

questiononeEUA3$grid <- factor(questiononeEUA3$grid)
questiononeEUA3$cell <- factor(questiononeEUA3$cell)
questiononeEUA3$before_after <- factor(questiononeEUA3$before_after)

intensiveEUA <- lmer(average ~ before_after + (1|grid), data = questiononeEUA3)
summary(intensiveEUA)

r.squaredGLMM(intensiveEUA)

#hist(questiononeEUA3$average)
ggplot(questiononeEUA3,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(intensiveEUA)
qqnorm(resid(intensiveEUA)) 
qqline(resid(intensiveEUA)) # meets assumptions!!!!

# Q1 Abandoned: EUA ----
questiononeEUA1 <- questiononeEUA %>%
  dplyr::filter(class == 1) 

questiononeEUA1$grid <- factor(questiononeEUA1$grid)
questiononeEUA1$cell <- factor(questiononeEUA1$cell)
questiononeEUA1$before_after <- factor(questiononeEUA1$before_after)

abandonedEUA <- lmer(average ~ before_after + (1|grid), data = questiononeEUA1)
summary(abandonedEUA)

plot(average ~ before_after, data = questiononeEUA1)

r.squaredGLMM(abandonedEUA)
plot(abandonedEUA)
plot(questiononeEUA$average)
qqnorm(resid(abandonedEUA)) 
qqline(resid(abandonedEUA))

# combine effect sizes
sucarea <- rbind(abandonedSUCdf, extensiveSUCdf, intensiveSUCdf)

# data vis attempt
(Q1figure <- ggplot(sucarea, aes(x = before_after, y = fit, fill = class)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
    scale_fill_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    scale_colour_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    geom_errorbar(aes(ymin=fit-se, ymax=fit+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    theme_bw() +
    ylab(expression("Area" ~ (m^{2}))) +
    xlab("\nYear")  +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 15, face = "plain"),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),   
          axis.line.y.left = element_line(colour = "black"),
          legend.title = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
ggsave(file ="images/Q1barfig.png", Q1figure, dpi = 2000)

# ACTUALLY Q2 ----
# pick transitions that fit with my story and the results of Q1 
# separate model for each event and transition - same as before 

# Q2 A--I: SUC ----
questiontwoSUC1 <- questiontwoSUC %>%
  dplyr::filter(transition == 1)

questiontwoSUC1$grid <- factor(questiontwoSUC1$grid)
questiontwoSUC1$cell <- factor(questiontwoSUC1$cell)
questiontwoSUC1$before_after <- factor(questiontwoSUC1$before_after)

atoiSUC <- lmer(average ~ before_after + (1|grid), data = questiontwoSUC1)
summary(atoiSUC)

r.squaredGLMM(atoiSUC)

#hist(questiontwoSUC1$average)
ggplot(questiontwoSUC1,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiSUC)
qqnorm(resid(atoiSUC)) 
qqline(resid(atoiSUC)) # meets assumptions!!!!

# Q2 A--I: EUA ----
questiontwoEUA1 <- questiontwoEUA %>%
  dplyr::filter(transition == 1)

questiontwoEUA1$grid <- factor(questiontwoEUA1$grid)
questiontwoEUA1$cell <- factor(questiontwoEUA1$cell)
questiontwoEUA1$before_after <- factor(questiontwoEUA1$before_after)

atoiEUA <- lmer(average ~ before_after + (1|grid), data = questiontwoEUA1)
summary(atoiEUA)

plot(average ~ before_after, data = questiontwoEUA1)

r.squaredGLMM(atoiEUA)

#hist(questiontwoEUA1$average)
ggplot(questiontwoEUA1,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiEUA)
qqnorm(resid(atoiEUA)) 
qqline(resid(atoiEUA)) # meets assumptions!!!!

# Q2 E -- I: SUC ----
questiontwoSUC4 <- questiontwoSUC %>%
  dplyr::filter(transition == 4)

questiontwoSUC4$grid <- factor(questiontwoSUC4$grid)
questiontwoSUC4$cell <- factor(questiontwoSUC4$cell)
questiontwoSUC4$before_after <- factor(questiontwoSUC4$before_after)

etoiSUC <- lmer(average ~ before_after + (1|grid), data = questiontwoSUC4)
summary(etoiSUC)

plot(average ~ before_after, data = questiontwoSUC4)

r.squaredGLMM(etoiSUC)

#hist(questiontwoSUC1$average)
ggplot(questiontwoSUC1,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(atoiSUC)
qqnorm(resid(atoiSUC)) 
qqline(resid(atoiSUC)) # meets assumptions!!!!

# Q2 E -- I: EUA ----
questiontwoEUA4 <- questiontwoEUA %>%
  dplyr::filter(transition == 4)

questiontwoEUA4$grid <- factor(questiontwoEUA4$grid)
questiontwoEUA4$cell <- factor(questiontwoEUA4$cell)
questiontwoEUA4$before_after <- factor(questiontwoEUA4$before_after)

etoiEUA <- lmer(average ~ before_after + (1|grid), data = questiontwoEUA4)
summary(etoiEUA)

r.squaredGLMM(etoiEUA)

#hist(questiontwoEUA4$average)
ggplot(questiontwoEUA4,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(etoiEUA)
qqnorm(resid(etoiEUA)) 
qqline(resid(etoiEUA)) # meets assumptions!!!!


# Q2 I to E: SUC ----
questiontwoSUC6 <- questiontwoSUC %>%
  dplyr::filter(transition == 6)

questiontwoSUC6$grid <- factor(questiontwoSUC6$grid)
questiontwoSUC6$cell <- factor(questiontwoSUC6$cell)
questiontwoSUC6$before_after <- factor(questiontwoSUC6$before_after)

itoeSUC <- lmer(average ~ before_after + (1|grid), data = questiontwoSUC6)
summary(itoeSUC)

r.squaredGLMM(itoeSUC)

#hist(questiontwoSUC6$average)
ggplot(questiontwoSUC6,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoeSUC)
qqnorm(resid(itoeSUC)) 
qqline(resid(itoeSUC)) # meets assumptions!!!!

# Q2 I to E: EUA ----
questiontwoEUA6 <- questiontwoEUA %>%
  dplyr::filter(transition == 6)

questiontwoEUA6$grid <- factor(questiontwoEUA6$grid)
questiontwoEUA6$cell <- factor(questiontwoEUA6$cell)
questiontwoEUA6$before_after <- factor(questiontwoEUA6$before_after)

itoeEUA <- lmer(average ~ before_after + (1|grid), data = questiontwoEUA6)
summary(itoeEUA)

r.squaredGLMM(itoeEUA)

#hist(questiontwoEUA6$average)
ggplot(questiontwoEUA6,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoeEUA)
qqnorm(resid(itoeEUA)) 
qqline(resid(itoeEUA)) # meets assumptions!!!!


# Q2 I to A: SUC ----
questiontwoSUC5 <- questiontwoSUC %>%
  dplyr::filter(transition == 5)

questiontwoSUC5$grid <- factor(questiontwoSUC5$grid)
questiontwoSUC5$cell <- factor(questiontwoSUC5$cell)
questiontwoSUC5$before_after <- factor(questiontwoSUC5$before_after)

itoaSUC <- lmer(average ~ before_after + (1|grid), data = questiontwoSUC5)
summary(itoaSUC)

r.squaredGLMM(itoaSUC)

#hist(questiontwoSUC5$average)
ggplot(questiontwoSUC5,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoaSUC)
qqnorm(resid(itoaSUC)) 
qqline(resid(itoaSUC)) # meets assumptions!!!!

# Q2 I to A: EUA ----
questiontwoEUA5 <- questiontwoEUA %>%
  dplyr::filter(transition == 5)

questiontwoEUA5$grid <- factor(questiontwoEUA5$grid)
questiontwoEUA5$cell <- factor(questiontwoEUA5$cell)
questiontwoEUA5$before_after <- factor(questiontwoEUA5$before_after)

itoaEUA <- lmer(average ~ before_after + (1|grid), data = questiontwoEUA5)
summary(itoaEUA)

r.squaredGLMM(itoaEUA)

#hist(questiontwoEUA5$average)
ggplot(questiontwoEUA5,aes(x=average))+geom_histogram()+facet_grid(~before_after)+theme_bw()
plot(itoaEUA)
qqnorm(resid(itoaEUA)) 
qqline(resid(itoaEUA)) # meets assumptions!!!!


# ACTUALLY Q3 ----
# pick transitions/classes carefully -- if there's loads of zeros probably shows that it isn't important
# moving window analysis where you have same model, same before time period but new afters
# 1 & 2 years after, 2 & 3 years after etc. 
# new model for each window and each transition/class 
# break point analysis? 

# Q3 abandoned

abandonedlag2 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1994 | year == 1995) %>%
  dplyr::filter(class == "1") %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000)

abandonedlag2$grid <- factor(abandonedlag2$grid)
abandonedlag2$cell <- factor(abandonedlag2$cell)
abandonedlag2$before_after <- factor(abandonedlag2$before_after)


abandonedlag2mod<- lmer(average ~ before_after + (1|grid), data = abandonedlag2)
summary(abandonedlag2mod)

plot(average ~ before_after, data = abandonedlag2)

r.squaredGLMM(abandonedlag2mod)

abandonedlag3 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1996 | year == 1997) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(class == 1)

abandonedlag3$grid <- factor(abandonedlag3$grid)
abandonedlag3$cell <- factor(abandonedlag3$cell)
abandonedlag3$before_after <- factor(abandonedlag3$before_after)


abandonedlag3mod <- lmer(average ~ before_after + (1|grid), data = abandonedlag3)
summary(abandonedlag3mod)

r.squaredGLMM(abandonedlag3mod)


extensivelag2 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1994 | year == 1995) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(class == 2)

extensivelag2$grid <- factor(extensivelag2$grid)
extensivelag2$cell <- factor(extensivelag2$cell)
extensivelag2$before_after <- factor(extensivelag2$before_after)


extensivelag2mod <- lmer(average ~ before_after + (1|grid), data = extensivelag2)
summary(extensivelag2mod)

r.squaredGLMM(extensivelag2mod)

extensivelag3 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1996 | year == 1997) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000)  %>%
  dplyr::filter(class == 2)

extensivelag3$grid <- factor(extensivelag3$grid)
extensivelag3$cell <- factor(extensivelag3$cell)
extensivelag3$before_after <- factor(extensivelag3$before_after)


extensivelag3mod<- lmer(average ~ before_after + (1|grid), data = extensivelag3)
summary(extensivelag3mod)

r.squaredGLMM(extensivelag3mod)

intensivelag2 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1994 | year == 1995) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(class == 3)

intensivelag2$grid <- factor(intensivelag2$grid)
intensivelag2$cell <- factor(intensivelag2$cell)
intensivelag2$before_after <- factor(intensivelag2$before_after)


intensivelag2mod <- lmer(average ~ before_after + (1|grid), data = intensivelag2)
summary(intensivelag2mod)

r.squaredGLMM(intensivelag2mod)

intensivelag3 <- detailedA %>%
  dplyr::filter(year == 1989 | year == 1990 | year == 1996 | year == 1997) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1989 | year == 1990, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, class, cell)%>%
  summarise(average = mean(area)/1000)  %>%
  dplyr::filter(class == 3)

intensivelag3$grid <- factor(intensivelag3$grid)
intensivelag3$cell <- factor(intensivelag3$cell)
intensivelag3$before_after <- factor(intensivelag3$before_after)


intensivelag3mod<- lmer(average ~ before_after + (1|grid), data = intensivelag3)
summary(intensivelag3mod)

r.squaredGLMM(intensivelag3mod)

# this gives different results!
atoiEUAprelag1 <- detailedT %>%
  dplyr::filter(year ==  2001 | year == 2002 | year == 2005 | year == 2006 ) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2001 | year == 2002, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell) %>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "1")

atoiEUAprelag1$grid <- factor(atoiEUAprelag1$grid)
atoiEUAprelag1$cell <- factor(atoiEUAprelag1$cell)
atoiEUAprelag1$before_after <- factor(atoiEUAprelag1$before_after)

atoiEUAprelag1mod<- lmer(average ~ before_after + (1|grid), data = atoiEUAprelag1)
summary(atoiEUAprelag1mod)

r.squaredGLMM(atoiEUAprelag1mod)



atoiEUAlag1 <- detailedT %>%
  dplyr::filter(year == 2003 | year == 2004 | year == 2008 | year == 2009) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2003 | year == 2004, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "1")

atoiEUAlag1$grid <- factor(atoiEUAlag1$grid)
atoiEUAlag1$cell <- factor(atoiEUAlag1$cell)
atoiEUAlag1$before_after <- factor(atoiEUAlag1$before_after)

atoiEUAlag1mod<- lmer(average ~ before_after + (1|grid), data = atoiEUAlag1)
summary(atoiEUAlag1mod)

r.squaredGLMM(atoiEUAlag1mod)


atoiEUAlag2 <- detailedT %>%
  dplyr::filter(year == 2003 | year == 2004 | year == 2010 | year == 2011) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 2003 | year == 2004, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell, .drop = FALSE)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "1")

atoiEUAlag2$grid <- factor(atoiEUAlag2$grid)
atoiEUAlag2$cell <- factor(atoiEUAlag2$cell)
atoiEUAlag2$before_after <- factor(atoiEUAlag2$before_after)

atoiEUAlag2mod<- lmer(average ~ before_after + (1|grid), data = atoiEUAlag2)
summary(atoiEUAlag2mod)

r.squaredGLMM(atoiEUAlag2mod)

itoaSUClag1 <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1995 | year == 1996) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "5")


itoaSUClag1$grid <- factor(itoaSUClag1$grid)
itoaSUClag1$cell <- factor(itoaSUClag1$cell)
itoaSUClag1$before_after <- factor(itoaSUClag1$before_after)

itoaSUClag1mod<- lmer(average ~ before_after + (1|grid), data = itoaSUClag1)
summary(itoaSUClag1mod)

r.squaredGLMM(atoiEUAlag1mod)


itoaSUClag2 <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1997 | year == 1998) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "5")


itoaSUClag2$grid <- factor(itoaSUClag2$grid)
itoaSUClag2$cell <- factor(itoaSUClag2$cell)
itoaSUClag2$before_after <- factor(itoaSUClag2$before_after)

itoaSUClag2mod<- lmer(average ~ before_after + (1|grid), data = itoaSUClag2)
summary(itoaSUClag2mod)

r.squaredGLMM(atoiEUAlag2mod)


itoeSUClag1 <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1995 | year == 1996) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "6")


itoeSUClag1$grid <- factor(itoeSUClag1$grid)
itoeSUClag1$cell <- factor(itoeSUClag1$cell)
itoeSUClag1$before_after <- factor(itoeSUClag1$before_after)

itoeSUClag1mod<- lmer(average ~ before_after + (1|grid), data = itoeSUClag1)
summary(itoeSUClag1mod)

r.squaredGLMM(itoeSUClag1mod)


itoeSUClag2 <- detailedT %>%
  dplyr::filter(year == 1990 | year == 1991 | year == 1997 | year == 1998) %>%
  dplyr::select(-c(pixels)) %>%
  mutate(before_after = ifelse(year == 1990 | year == 1991, "first", "second")) %>%
  dplyr::select(-c(year)) %>%
  group_by(before_after, grid, transition, cell)%>%
  summarise(average = mean(area)/1000) %>%
  dplyr::filter(transition == "6")


itoeSUClag2$grid <- factor(itoeSUClag2$grid)
itoeSUClag2$cell <- factor(itoeSUClag2$cell)
itoeSUClag2$before_after <- factor(itoeSUClag2$before_after)

itoeSUClag2mod<- lmer(average ~ before_after + (1|grid), data = itoeSUClag2)
summary(itoeSUClag2mod)

r.squaredGLMM(itoeSUClag2mod)



# Break point attempt
abandonedlag <- detailedA %>%
  dplyr::select(-c(pixels)) %>%
  filter(class == "1") %>%
  group_by(year) %>%
  summarise(year_total = sum(area)/1000)

(startfiga <- ggplot(abandonedlag, aes(year, y = year_total)) 
  + geom_line()
  + labs(x = "Time (year)", y = "Total area (km2)"))


abandonedlm <- lm(year_total ~ year, data = abandonedlag) 
summary(abandonedlm)


segmented.mod <- segmented(abandonedlm, seg.Z = ~year, psi=1991)
summary(segmented.mod)


plot(year,year_total, pch=16, ylim=c(1989,2011))
plot(segmented.mod, add=T)




abandonedseg <- segmented(abandonedlm,
                          seg.Z = ~ year,
                          psi = list(year = c(1991, 2004)))

summary(abandonedseg)

# breakpoints
abandonedseg$psi

slope(abandonedseg)

abandonedfit <- fitted(abandonedseg)
abandonedmodel <- data.frame(year = abandonedlag$year, area = abandonedfit)

ggplot(abandonedmodel, aes(x = year, y = area)) + geom_line()

abandonedlines <- abandonedseg$psi[, 1]

abandonedslopes <- coef(abandonedseg)

abandonedb0 <- coef(abandonedseg)[[1]]
abandonedb1 <- coef(abandonedseg)[[2]]
abandonedc1 <- coef(abandonedseg)[[2]] + coef(abandonedseg)[[3]]
abandonedbreak1 <- abandonedseg$psi[[3]]
abandonedc0 <- abandonedb0 + abandonedb1 * abandonedbreak1 - abandonedc1 * abandonedbreak1
abandonedd1 <- coef(abandonedseg)[[4]] + abandonedc1
abandonedbreak2 <- abandonedseg$psi[[4]]
abandonedd0 <- abandonedc0 + abandonedc1 * abandonedbreak2 - abandonedd1 * abandonedbreak2

(startfiga <- ggplot(abandonedlag, aes(year, y = year_total)) +
    geom_line() +
    geom_line(data = abandonedmodel, aes(x = year, y = area), colour = "blue") +
    geom_abline(aes(intercept = abandonedb0, slope = abandonedb1, 
                colour = 'pre SUC')) +
    geom_abline(aes(intercept = abandonedc0, slope = abandonedc1, 
                colour = 'post')) +
    geom_abline(aes(intercept = abandonedd0, slope = abandonedd1, 
                colour = "post2")) +
    geom_vline(xintercept = abandonedlines, linetype = "dashed") +
    labs(x = "Time (year)", y = "Total area (km2)"))

# data vis
area_graph <- summarySE(detailed_area, measurevar="area", groupvars=c("year", "class"))

levels(area_graph$class)[levels(area_graph$class)=="1"] <- "Abandoned"
levels(area_graph$class)[levels(area_graph$class)=="2"] <- "Extensive"
levels(area_graph$class)[levels(area_graph$class)=="3"] <- "Intensive"

(Q1figure <- ggplot(area_graph, aes(x = year, y = area, fill = class)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
    scale_fill_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    scale_colour_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    geom_errorbar(aes(ymin=area-ci, ymax=area+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    theme_bw() +
    ylab(expression("Area" ~ (m^{2}))) +
    xlab("\nYear")  +
    scale_y_continuous(trans = log_trans(),
      breaks = trans_breaks("log2", function(x) 2^x),
      labels = trans_format("log2", math_format(2^.x))) + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 15, face = "plain"),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),   
          axis.line.y.left = element_line(colour = "black"),
          legend.title = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
ggsave(file ="images/Q1barfig.png", Q1figure, dpi = 2000)

area1 <- area %>%
  filter(landuse == "1")

(area1fig <- ggplot(area1, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#8B4500", fill = "#8B4500", alpha = 0.4) +
    ylab(expression("Area" ~ (m^{2}))) + 
    labs(x = "", title = "a. Abandoned") +
    theme_marine())

area2 <- area %>%
  filter(landuse == "2")

(area2fig <- ggplot(area2, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#228B22", fill = "#228B22", alpha = 0.4) +
    labs(x = "Year", y = " ", title = "b. Extensive") +
    theme_marine())

area3 <- area %>%
  filter(landuse == "3")

(area3fig <- ggplot(area3, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#68228B", fill = "#68228B", alpha = 0.4) +
    labs(x = "", y = "", title = "c. Intensive") +
    theme_marine())

# combine to a panel
area_panel <- arrangeGrob(area1fig, area2fig, area3fig, ncol = 3, widths = c(1, 1, 1))
ggsave(file = "images/Q1panelfig.png", area_panel, dpi = 2000, width = 12)

# panel figure 
trans1 <- transition %>%
  filter(transition == "1")

(trans1fig <- ggplot(trans1, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#68228B", fill = "#68228B", alpha = 0.4) +
    ylab(expression("Area" ~ (m^{2}))) +
    labs(x = "", title = "a. Abandoned to intensive") +
    theme_marine())

trans2 <- transition %>%
  filter(transition == "2")

(trans2fig <- ggplot(trans2, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#228B22", fill = "#228B22", alpha = 0.4) +
    labs(x = "", y = " ", title = "b. Abandoned to extensive") +
    theme_marine())

trans3 <- transition %>%
  filter(transition == "3")

(trans3fig <- ggplot(trans3, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#8B4500", fill = "#8B4500", alpha = 0.4) +
    labs(x = "", y = " ", title = "c. Extensive to abandoned") +
    theme_marine())

trans4 <- transition %>%
  filter(transition == "4")

(trans4fig <- ggplot(trans4, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#68228B", fill = "#68228B", alpha = 0.4) +
    ylab(expression("Area" ~ (m^{2}))) + 
    labs(x = "", title = "d. Extensive to intensive") +
    theme_marine())

trans5 <- transition %>%
  filter(transition == "6")

(trans5fig <- ggplot(trans5, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#228B22", fill = "#228B22", alpha = 0.4) +
    labs(x = "Year", y = " ", title = "e. Intensive to extensive") +
    theme_marine())

trans6 <- transition %>%
  filter(transition == "5")

(trans6fig <- ggplot(trans6, aes(x = year, y = area)) +
    geom_point(colour = "black", size = 1) +
    geom_smooth(method = "lm", colour = "#8B4500", fill = "#8B4500", alpha = 0.4) +
    labs(x = "", y = "", title = "f. Intensive to abandoned") +
    theme_marine())

# combine to a panel
row1 <- grid.arrange(trans1fig, trans2fig, trans3fig, ncol = 3, widths = c(1, 1, 1))
row2 <- grid.arrange(trans4fig, trans5fig, trans6fig, ncol = 3, widths = c(1, 1, 1))
panel <-arrangeGrob(row1, row2, nrow = 2)
ggsave(file = "images/Q2panelfig.png", panel, dpi = 2000, width = 12, height = 6)

transition_graph <- summarySE(detailed_transition, measurevar="area", groupvars=c("year", "transition"))

levels(transition_graph$transition)[levels(transition_graph$transition)=="1"] <- "Abandoned to intensive"
levels(transition_graph$transition)[levels(transition_graph$transition)=="2"] <- "Abandoned to extensive"
levels(transition_graph$transition)[levels(transition_graph$transition)=="3"] <- "Extensive to abandoned"
levels(transition_graph$transition)[levels(transition_graph$transition)=="4"] <- "Extensive to intensive"
levels(transition_graph$transition)[levels(transition_graph$transition)=="5"] <- "Intensive to abandoned"
levels(transition_graph$transition)[levels(transition_graph$transition)=="6"] <- "Intensive to extensive"

(Q2figure <- ggplot(transition_graph, aes(x = year, y = area, fill = transition)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
    scale_fill_manual(values = c("#68228B", "#458B00", "#8B7355", "#9A32CD", "#66CD00", "#CDAA7D")) +
    scale_colour_manual(values = c("#68228B", "#458B00", "#8B7355", "#9A32CD", "#66CD00", "#CDAA7D")) +
    geom_errorbar(aes(ymin=area-ci, ymax=area+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    theme_bw() +
    ylab(expression("Area" ~ (m^{2}))) +
    xlab("\nYear") + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 15, face = "plain"),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),   
          axis.line.y.left = element_line(colour = "black"),
          legend.title = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
ggsave(file ="images/Q2barfig.png", Q2figure, dpi = 2000)

(Q3afigure <- ggplot(area_graph, aes(x = year, y = area, fill = class)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
    scale_fill_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    scale_colour_manual(values = c("#8B4500", "#228B22", "#68228B")) +
    geom_errorbar(aes(ymin=area-ci, ymax=area+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    theme_bw() +
    geom_vline(aes(xintercept = 1991),               
               colour = "red", linetype = "dashed", size=1) +
    geom_vline(aes(xintercept = 2004),  
               colour = "red", linetype = "dashed", size=1) +
    ylab(expression("Area" ~ (m^{2}))) +
    xlab("\nYear")  +
    scale_y_continuous(trans = log_trans(),
                       breaks = trans_breaks("log2", function(x) 2^x),
                       labels = trans_format("log2", math_format(2^.x))) + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 15, face = "plain"),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),   
          axis.line.y.left = element_line(colour = "black"),
          legend.title = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
ggsave(file = "images/Q3afig.png", Q3afigure, dpi = 2000)

(Q3bfigure <- ggplot(transition_graph, aes(x = year, y = area, fill = transition)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black") +
    scale_fill_manual(values = c("#68228B", "#458B00", "#8B7355", "#9A32CD", "#66CD00", "#CDAA7D")) +
    scale_colour_manual(values = c("#68228B", "#458B00", "#8B7355", "#9A32CD", "#66CD00", "#CDAA7D")) +
    geom_errorbar(aes(ymin=area-ci, ymax=area+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    geom_vline(aes(xintercept = 1992),               
               colour = "red", linetype = "dashed", size=1) +
    geom_vline(aes(xintercept = 2005),                   
               colour = "red", linetype = "dashed", size=1) +
    theme_bw() +
    ylab(expression("Area" ~ (m^{2}))) +
    xlab("\nYear") + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 15, face = "plain"),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),   
          axis.line.y.left = element_line(colour = "black"),
          legend.title = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
ggsave(file = "images/Q3bfig.png", Q3bfigure, dpi = 2000)
