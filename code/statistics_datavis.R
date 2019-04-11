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
library(Rmisc)
library(scales)
library(gridExtra)

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

areaset[is.na(areaset)] <- 0
areaset$landuse <- factor(areaset$landuse)
areaset$year <- factor(areaset$year)

detailed_area <- read_csv("data/detailed_area.csv") %>%
  dplyr::select(-c("X1"))
colnames(detailed_area)[colnames(detailed_area) == "label"] <- "cell"
detailed_area$class <- factor(detailed_area$class)
detailed_area$cell <- factor(detailed_area$cell)
detailed_area[is.na(detailed_area)] <- 0

transition <- read_csv("data/transition.csv")

detailed_transition <- read_csv("data/detailed_transition.csv") %>%
  dplyr::select(-c("X1"))
colnames(detailed_transition)[colnames(detailed_transition) == "label"] <- "cell"
detailed_transition$cell <- factor(detailed_transition$cell)
detailed_transition$transition <- factor(detailed_transition$transition)
detailed_transition[is.na(detailed_transition)] <- 0

# Q1: Is there quantifiable, country-scale land-use change following SPE
# events in Latvia? ----
# looking at the change in area with time -- per class means class is fixed effect
# account for year and cell as random effects to account for spatial/temporal autocorrelation

# histogram
hist(detailed_area$area) # not normal - left skewed due to lots of zeros -- use poisson?

# model
mod1REML <- lmer(area ~ I(year - 1988) + class + (1|year) + (1|cell), data = detailed_area)
summary(mod1REML) # best model
mod1 <- lmer(area ~ I(year - 1988) + class + (1|year) + (1|cell), data = detailed_area, REML = FALSE)
summary(mod1) # best model
mod1b <- lmer(area ~ I(year - 1988) + class + (cell|year), data = detailed_area, REML = FALSE)
summary(mod1b) # could not converge 
mod1c <- lmer(area ~ I(year - 1988) + class + (cell|year) + (1|year) + (1|cell), data = detailed_area, REML = FALSE)
summary(mod1c) # could not converge 
mod1d <- lmer(area ~ I(year - 1988) + class + (1|year), data = detailed_area, REML = FALSE)
summary(mod1d)
mod1e <- lmer(area ~ I(year - 1988) + class + (1|cell), data = detailed_area, REML = FALSE)
summary(mod1e)
mod1f <- lmer(area ~ I(year - 1988) + class + event + (1|year) + (1|cell), data = detailed_area, REML = FALSE)
summary(mod1f) 


r.squaredGLMM(mod1) # 0.5892351 / 0.743847
r.squaredGLMM(mod1b)
r.squaredGLMM(mod1c)
r.squaredGLMM(mod1d) # 0.5905421 / 0.5963272
r.squaredGLMM(mod1e) # .5894802 / 0.737027
r.squaredGLMM(mod1f) # 0.5912443/ 0.7437002

AICc(mod1, mod1d, mod1e,mod1f)
# mod1   7 298154.3
# mod1d  6 301025.1
# mod1e  6 298296.8
# mod1f  8 298148.2 -- improves model slightly, may not be worth the extra term

mod1n <- lmer(area ~ 1 + (1|year) + (1|cell), data = detailed_area, REML = FALSE)
summary(mod1n) 

anova(mod1, mod1n) # statistically different 

# visualise 
stargazer(mod1REML, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Set a clean theme for the graphs
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Visualises random effects 
(re.effects <- plot_model(mod1REML, type = "re", show.values = TRUE))


# To see the estimate for our fixed effects
(fe.effects <- plot_model(mod1REML, show.values = TRUE))

# predictions
ggpredict(mod1REML, terms = c("year")) %>% plot()

ggpredict(mod1REML, terms = c("year", "class"), type = "re") %>% plot()

# assumptions 
plot(mod1) # not homoscedastic it seems
qqnorm(resid(mod1)) 
qqline(resid(mod1)) # a bit skewed at the ends

# linearity 
# Error in xy.coords(x, y, xlabel, ylabel, log) : 
# 'x' and 'y' lengths differ
Linearity1 <- plot(resid(mod1REML), area)

# homoscedasity - fake levene's test 
detailed_area$mod1res<- residuals(mod1REML) #extracts the residuals and places them in a new column in our original data table

detailed_area$absmodel <-abs(detailed_area$mod1res) #creates a new column with the absolute value of the residuals

detailed_area$res2 <- detailed_area$mod1res^2 #squares the absolute values of the residuals to provide the more robust estimate

Levene.Model.res <- lm(res2 ~ year + class + cell, data=detailed_area) #ANOVA of the squared residuals

anova(Levene.Model.res) #displays the results -- all below 0.05 so not equal variances!

# normal distribution 
require("lattice")
qqmath(mod1REML, id=0.05) # - some violation 

# need to transform
modREML1b <- lmer(sqrt(area) ~ I(year - 1988) + class + (1|year) + (1|cell), data = detailed_area)
qqmath(modREML1b, id=0.05) # doesn't improve


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

# Q2: Are the strength and direction of land-use change different among extensive,
# intensive and abandoned land-use types? ----

# histogram
hist(detailed_transition$area) # left skewed due to lots of zeros -- use poisson? 

# model
mod2REML <- lmer(area ~ I(year - 1989) + transition + (1|year) + (1|cell), data = detailed_transition)
summary(mod2REML)
mod2 <- lmer(area ~ I(year - 1989) + transition + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2)
mod2b <- lmer(area ~ I(year - 1989) + transition + (cell|year), data = detailed_transition, REML = FALSE)
summary(mod2b) # could not converge
mod2c <- lmer(area ~ I(year - 1989) + transition + (cell|year) + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2c) # could not converge
mod2d <- lmer(area ~ I(year - 1989) + transition + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2d)
mod2e <- lmer(area ~ I(year - 1989) + transition + (1|year), data = detailed_transition, REML = FALSE)
summary(mod2e)
mod2f <- lmer(area ~ I(year - 1989) + transition + event + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2f)
mod2g <- lmer(area ~ I(year - 1989) + transition + previous_class + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2g)
mod2h <- lmer(area ~ I(year - 1989) + transition + previous_class + current_class + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2g)
mod2i <- lmer(area ~ I(year - 1989) + transition +  current_class + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2h)
mod2j <- lmer(area ~ I(year - 1989) + previous_class + current_class + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2j)

r.squaredGLMM(mod2) # 0.3306943 0.4201315
r.squaredGLMM(mod2b)
r.squaredGLMM(mod2c)
r.squaredGLMM(mod2d) # 0.3309036 0.4030753
r.squaredGLMM(mod2e) # 0.3309036 0.3473395
r.squaredGLMM(mod2f) #0.3314983 0.4201238
r.squaredGLMM(mod2g) # 0.3329322 0.422532
r.squaredGLMM(mod2h) # 0.3353278 0.4259556
r.squaredGLMM(mod2i) # 0.3343533 0.4243897
r.squaredGLMM(mod2j) # 0.0009591529 0.087430343

AICc(mod2, mod2d, mod2e,mod2f, mod2g, mod2h, mod2i, mod2j)
# mod2  10 337641.7 # this makes most sense
# mod2d  9 338051.2
# mod2e  9 339272.5
# mod2f 11 337642.7
# mod2g 11 337583.6
# mod2h 12 337513.1 # this may be best -- but is it worth the two extra terms?? 
# mod2i 11 337542.5
# mod2j  7 345288.2

mod2n <- lmer(area ~ 1 + (1|year) + (1|cell), data = detailed_transition, REML = FALSE)
summary(mod2n) 

anova(mod2, mod2n) # statistically different 

# visualise 
stargazer(mod2REML, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Set a clean theme for the graphs
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Visualises random effects 
(re.effects <- plot_model(mod2REML, type = "re", show.values = TRUE))


# To see the estimate for our fixed effects
(fe.effects <- plot_model(mod2REML, show.values = TRUE))

# predictions
ggpredict(mod2REML, terms = c("year")) %>% plot()

predictions <- ggpredict(mod2REML, terms = c("year", "transition"), type = "re") 

(predplot2 <- ggplot(predictions, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE) + 
    labs(x = "\nYear since 1989", y = "Predicted area \n") + 
  scale_y_continuous(limits = c(0, 51000))) 

# assumptions 
plot(mod2) # not homoscedastic it seems
qqnorm(resid(mod2)) 
qqline(resid(mod2)) # very skewed at right end

# linearity 
# Error in xy.coords(x, y, xlabel, ylabel, log) : 
# 'x' and 'y' lengths differ
Linearity2 <- plot(resid(mod2REML), area) 

# homoscedasity - fake levene's test 
detailed_transition$mod2res<- residuals(mod2REML) #extracts the residuals and places them in a new column in our original data table

detailed_transition$absmodel <-abs(detailed_transition$mod2res) #creates a new column with the absolute value of the residuals

detailed_transition$res2 <- detailed_transition$mod2res^2 #squares the absolute values of the residuals to provide the more robust estimate

Levene.Model.res2 <- lm(res2 ~ year + transition + cell, data=detailed_transition) #ANOVA of the squared residuals

anova(Levene.Model.res2) #displays the results -- all below 0.05 so not equal variances!

# normal distribution 
require("lattice")
qqmath(mod1REML, id=0.05) # - some violation 

# need to transform
modREML2b <- lmer(sqrt(area) ~ I(year - 1989) + transition + (1|year) + (1|cell), data = detailed_transition)
qqmath(modREML2b, id=0.05) # doesn't improve

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

# Q3: Is there a time lag between socio-economic events and the occurrence of land-use change? 
# Does this differ between land-use type?
mod3 <- lm(years_since ~ event + class + 1, data = detailed_area)
summary(mod3)


mod3b <- lm(years_since ~ event + transition + 1, data = detailed_transition)
summary(mod3b)

# check assumptions
hist(detailed_area$years_since) # skewed 
shapiro.test(area$years_since) # 0 p-value .0524 - if use full dataset, can't get value
bartlett.test(area$years_since, area$event) # p-value 0.0018
bartlett.test(area$years_since, area$landuse) # p-value 1 both are basically factors

hist(detailed_transition$years_since) # skewed 
shapiro.test(transition$years_since) # 0 p-value .00028 - if use full dataset, can't get value
bartlett.test(transition$years_since, transition$event) # p-value 5.603e-05
bartlett.test(transition$years_since, transition$transition) # p-value 1 both are basically factors

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
