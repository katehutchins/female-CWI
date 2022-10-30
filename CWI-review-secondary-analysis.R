#Data and code for paper "Treating Exertional Heat Stroke: Limited Understanding of the Female Response to Cold Water Immersion."

#Authors: Kate P Hutchins, Geoffrey M Minett, Ian B Stewart

#Lemire secondary analysis
#Completed September 2022


library(ggplot2)
library(tidyverse)
library(car)

setwd("~/Desktop")

d <- read.csv("Lemire.csv")

d$Sex <- as.factor(d$Sex)

#create LBM variable
d$LBM <- (1/d$SA.LBM) * (d$absolute.SA*10000)


Female <- subset(d,Sex=="F")
Male <- subset(d, Sex=="M")

#correlations tables
cor(d[, c('Cooling.Rate', 'absolute.SA', 'LBM', 'SA.LBM')])
cor(Female[, c('Cooling.Rate', 'absolute.SA', 'LBM', 'SA.LBM')])
cor(Male[, c('Cooling.Rate', 'absolute.SA', 'LBM', 'SA.LBM')])

#correlations with result
cor.test(d$Cooling.Rate, d$SA.LBM) #0.69 [.49,.81], p=.001
cor.test(Female$Cooling.Rate, Female$SA.LBM) #0.32 [-.67,.90], p=.403
cor.test(Male$Cooling.Rate, Male$SA.LBM) #0.26 [-.26,.77], p=.474
cor.test(d$Cooling.Rate, d$LBM) #-0.57 [-.75,-.30], p=.010
cor.test(Female$Cooling.Rate, Female$LBM) #0.17 [-.55,.74], p=.656 
cor.test(Male$Cooling.Rate, Male$LBM) #-0.36 [-.72,.22], p=.307

#bootstrap correlation CI
library(confintr)
ci_cor(
  Male$Cooling.Rate, 
  Male$SA.LBM,
  probs = c(0.025, 0.975),
  method = c("pearson"),
  type = c("bootstrap"),
  boot_type = c("bca"),
  R = 1000,
  seed = NULL
)

library(confintr)
ci_cor(
  Female$Cooling.Rate, 
  Female$SA.LBM,
  probs = c(0.025, 0.975),
  method = c("pearson"),
  type = c("bootstrap"),
  boot_type = c("bca"),
  R = 1000,
  seed = NULL
)

#regressions
fitall <- lm(Cooling.Rate ~ absolute.SA + LBM + Sex, data=d)
summary(fitall)
confint(fitall)

fitall <- lm(Cooling.Rate ~ SA.LBM, data=d)
summary(fitall)
confint(fitall)

fitfemale <- lm(Cooling.Rate ~ SA.LBM, data=Female)
summary(fitfemale)
confint(fitfemale)

fitmale <- lm(Cooling.Rate ~ SA.LBM, data=Male)
summary(fitmale)
confint(fitmale)
plot(fitmale)

#plots
ggplot(d, aes(SA.LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")
ggplot(Female, aes(SA.LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")
ggplot(Male, aes(SA.LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")
ggplot(d, aes(LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")
ggplot(Female, aes(LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")
ggplot(Male, aes(LBM, Cooling.Rate)) + geom_point() + geom_smooth(method = "lm")


#plot Cooling Rate by LBM
p_LBM <- ggplot(d, aes(LBM, Cooling.Rate)) +
  geom_point(size = 3, aes(shape=factor(Sex))) + 
  stat_smooth(method = 'lm', aes(linetype = Sex, colour = Sex), se = FALSE) +
  scale_colour_manual(values=c(1,1)) +
  scale_shape_manual(values=c(1, 16)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "hidden",
        plot.tag = element_text()) +
  xlab("Lean Body Mass (kg)") +
  ylab("Cooling Rate (°C/min)") +
  labs(tag = "A") +
  theme(legend.title = element_blank())

p_LBM


#plot Cooling Rate by absolute surface area
p_SA <- ggplot(d, aes(absolute.SA, Cooling.Rate)) +
  geom_point(size = 3, aes(shape=factor(Sex))) + 
  stat_smooth(method = 'lm', aes(linetype = Sex, colour = Sex), se = FALSE) +
  scale_colour_manual(values=c(1,1)) +
  scale_shape_manual(values=c(1, 16)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "hidden",
        plot.tag = element_text()) +
  xlab(expression(paste("Absolute Surface Area (","m",""^2,")",))) +
  ylab("Cooling Rate (°C/min)") +
  labs(tag = "B") +
  theme(legend.title = element_blank()) 

p_SA


#plot Cooling Rate by surface area-to-lean body mass ratio
p_SA.LBM <- ggplot(d, aes(SA.LBM, Cooling.Rate)) +
  geom_point(size = 3, aes(shape=factor(Sex))) + 
  stat_smooth(method = 'lm', aes(linetype = Sex, colour = Sex), se = FALSE) +
  scale_colour_manual(values=c(1,1)) +
  scale_shape_manual(values=c(1, 16)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "hidden",
        plot.tag = element_text()) +

  xlab(expression(paste("Surface Area-to-Lean Body Mass Ratio (","cm",""^2,"/kg",")",))) +
  ylab("Cooling Rate (°C/min)") +
  labs(tag = "C") +
  theme(legend.title = element_blank()) 

p_SA.LBM



#plots arranged
library(ggpubr)
combined_plot <- ggarrange (p_LBM, p_SA, p_SA.LBM, 
                            ncol = 1, nrow = 3, 
                            common.legend = TRUE,
                            legend = "bottom")
combined_plot


ggsave("lemire_panel_figure.jpeg", plot=combined_plot, dpi = 600, units = "in", width = 6, height = 9)

