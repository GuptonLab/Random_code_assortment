##This is for Caroline's blot analysis and graph making.

#Load the required packages for the analysis
require(tibble)
require(ggplot2)
require(tidyverse)
require(reshape2)
require(cowplot)

#read in the data; already formed as a tibble when using tidyverse.
dat_ <- read_csv("D:/Fabio/Book1.csv")

#turn the data into long format. If gather didn't work, using reshape2 is more powerful.
dat1 = gather(dat_,key = treatment, value = intensity, -id_pair)
dat1

#plot the dataz
p1 <- ggplot(dat1,aes(x=treatment,y=intensity)) + geom_boxplot() + geom_point() + geom_line(aes(group = id_pair, color = factor(id_pair)),size = 0.75) + scale_x_discrete(limits=c("Untreated","500","900"))


#create a density plot
p2 <- ggplot(dat1,aes(x=intensity,fill=treatment)) + geom_density(alpha=0.5)
p2
#create a histogram plot
p3 <- ggplot(dat1,aes(x=intensity,color=treatment,fill=treatment)) + geom_histogram(bins = 5,alpha=0.5,position = "dodge")
p3

#
#plot all of the 3 together; this requires the cowplot package.
plot_grid(p1,p2,p3, align = "v")


#perform t.tests between all of the data points. We will do a post-hoc correction using benjamini-hochberg.
test_1 <- t.test(dat_$`500`,dat_$`900`,paired = TRUE)
test_2 <- t.test(dat_$Untreated, dat_$`900`, paired = TRUE)
test_3 <- t.test(dat_$Untreated,dat_$`500`, paired = TRUE)

#perform the correction
p.adjust(c(test_1$p.value, test_2$p.value, test_3$p.value), method = "hochberg")


#Just for funsies: power analysis to see how many n we would need to detect an effect size of ~0.94
require(pwr)
pwr.t.test(d = 0.94, sig.level = 0.05, power = 0.8, type ="paired",alternative = "two.sided")
pwr.t.test(d=0.94,sig.level = 0.05, power = 0.8, type = "paired", alternative = "two.sided")


mean_u <- mean(dat_$Untreated)
sd_u <- sd(dat_$Untreated)

mean_t <- mean(dat_$`500`)
sd_t <- sd(dat_$`500`)

#always comment your code
pooled_sd = sqrt((sd_u^2+sd_t^2)/2)
effect_size = (.46-.66)/pooled_sd


