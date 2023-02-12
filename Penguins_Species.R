install.packages("palmerpenguins")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("GGally")
install.packages("corrr")
install.packages("ggpubr")
install.packages("fitdistrplus")
install.packages("lattice")  
install.packages("reshape2")
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrr)
library(ggpubr)
library(fitdistrplus)
library(lattice)
library(reshape2)

data("penguins")
penguins = na.omit(penguins)

my.student.number = 000000000
set.seed(my.student.number)
my.penguins = penguins[sample(nrow(penguins), 100),]

# Bar Plot - Species by Island
ggplot(my.penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# Bar Plot - Species by sex
ggplot(my.penguins, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# Scatter Plot - Flipper Length vs Body Mass
ggplot(data = my.penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) 

# Best
# Scatter Plot - Bill Length vs Bill Depth
ggplot(data = my.penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2)  +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

# Scatter Plot - Flipper Length vs Body Mass
ggplot(data = my.penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2)  +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

# Best
# Scatter Plot - Flipper Length vs Bill Length
ggplot(data = my.penguins, aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2)  +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

# Main
# Population
# Scatter Plot with Ellipses
##Flipper Length vs Body Mass
f_bm <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
f_bm +labs(title="Plot of length \n by dose",
        x ="Dose (mg)", y = "Teeth length")
##Flipper Length vs Bill Length
f_bl <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Flipper Length vs Bill Depth
f_bd <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Body Mass vs Bill Length
bm_bl <- ggplot(data = my.penguins, aes(x = body_mass_g, y = bill_length_mm, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Body Mass vs Bill Depth
bm_bd <- ggplot(data = my.penguins, aes(x = body_mass_g, y = bill_depth_mm, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Bill Length vs Bill Depth
bl_bd <- ggplot(data = my.penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = species), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
## Scatter Plot in one page
ggarrange(f_bm + labs(title="Flipper Length vs Body Mass", x ="Flipper Length (mm)", y = "Body Mass (g)"), 
          f_bl + labs(title="Flipper Length vs Bill Length", x ="Flipper Length (mm)", y = "Bill Length (mm)"),
          f_bd + labs(title="Flipper Length vs Bill Depth", x ="Flipper Length (mm)", y = "Bill Depth (mm)"),
          bm_bl + labs(title="Body Mass vs Bill Length", x ="Body Mass (g)", y = "Bill Length (mm)"),
          bm_bd + labs(title="Body Mass vs Bill Depth", x ="Body Mass (g)", y = "Bill Depth (mm)"),
          bl_bd + labs(title="Bill Length vs Bill Depth", x ="Bill Length (mm)", y = "Bill Depth (mm)"),
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "right")

# Histogram example: flipper length by species
hist_bl <- ggplot(data = my.penguins, aes(x = bill_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

hist_bd <- ggplot(data = my.penguins, aes(x = bill_depth_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

ggarrange(hist_bl, hist_bd,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

# Fitting Distribution for Bill Length
## Subset
ade <- subset(x = my.penguins, subset = species == "Adelie", select = c(species, island, bill_length_mm, bill_depth_mm, sex, year))
chi <- subset(x = my.penguins, subset = species == "Chinstrap", select = c(species, island, bill_length_mm, bill_depth_mm, sex, year))
gen <- subset(x = my.penguins, subset = species == "Gentoo", select = c(species, island, bill_length_mm, bill_depth_mm, sex, year))
##Normal
norm_bl <- fitdist(my.penguins$bill_length_mm, "norm") 
##Exponential
exp_bl <- fitdist(my.penguins$bill_length_mm, "exp") 
## Summary
summary(norm_bl)
summary(exp_bl)
## Plot graph
par(mfrow = c(2, 2))
plot.legend <- c("Normal","Exponential")
denscomp(list(norm_bl,exp_bl), legendtext = plot.legend)
qqcomp(list(norm_bl,exp_bl), legendtext = plot.legend)
cdfcomp(list(norm_bl,exp_bl), legendtext = plot.legend)
ppcomp(list(norm_bl,exp_bl), legendtext = plot.legend)

# Fitting Distribution for Bill Depth
##Normal
norm_bd <- fitdist(gen$bill_depth_mm, "norm") 
##Exponential
exp_bd <- fitdist(gen$bill_depth_mm, "exp") 
## Summary
summary(norm_bd)
summary(exp_bd)
## Plot graph
par(mfrow = c(2, 2))
plot.legend <- c("Normal","Exponential")
denscomp(list(norm_bd,exp_bd), legendtext = plot.legend)
qqcomp(list(norm_bd,exp_bd), legendtext = plot.legend)
cdfcomp(list(norm_bd,exp_bd), legendtext = plot.legend)
ppcomp(list(norm_bd,exp_bd), legendtext = plot.legend)

# Normal Distribution two plot
## Normal distribution - Bill Length
dnorm_bl <- ggplot(data = my.penguins, aes(x = bill_length_mm)) +
  stat_function(fun = dnorm, args = list(38.95, 2.53),
                aes(colour = "Adelie"), size = 1) +
  stat_function(fun = dnorm, args = list(49.45, 3.23),
                aes(colour = "Chinstrap"), size = 1) +
  stat_function(fun = dnorm, args = list(47.62, 2.80),
                aes(colour = "Gentoo"), size = 1) +
  scale_x_continuous(name = "Probability", breaks = seq(25, 70, 5),
                     limits=c(25, 70)) +
  scale_y_continuous(name = "Frequency") +
  labs(colour = "Species") +
  theme(legend.text = element_text(size=10)) +
  ggtitle("Normal Distribution of Bill Length") +
  theme(plot.title = element_text(hjust = 0.5))
## Normal distribution - Bill Depth
dnorm_bd <- ggplot(data = my.penguins, aes(x = bill_depth_mm)) +
  stat_function(fun = dnorm, args = list(18.20, 1.01),
                aes(colour = "Adelie"), size = 1) +
  stat_function(fun = dnorm, args = list(18.78, 1.30),
                aes(colour = "Chinstrap"), size = 1) +
  stat_function(fun = dnorm, args = list(14.97, 0.98),
                aes(colour = "Gentoo"), size = 1) +
  scale_x_continuous(name = "Probability", breaks = seq(5, 30, 5),
                     limits=c(5, 30)) +
  scale_y_continuous(name = "Frequency") +
  labs(colour = "Species") +
  theme(legend.text = element_text(size=810)) +
  ggtitle("Normal Distribution of Bill Depth") +
  theme(plot.title = element_text(hjust = 0.5))
## Plot two graph
dnorm_arr <- ggarrange(dnorm_bl, dnorm_bd,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
annotate_figure(dnorm_arr, top = text_grob("Normal Function Curves of Probabilities", face = "bold", size = 14))

# Confidence Intervals
## Bill Length
## Choose data
data <- ade
#data <- chi
#data <- gen

##Mean
sample.mean <- mean(data$bill_length_mm)
print(paste("mean:", sample.mean))
##Standard error of mean
sample.n <- length(data$bill_length_mm)
sample.sd <- sd(data$bill_length_mm)
sample.se <- sample.sd/sqrt(sample.n)
print(paste("standard error of mean:",sample.se))
##t-score
alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(paste("t-score:", t.score))
#Margin error
margin.error <- t.score * sample.se
print(paste("Margin error:", margin.error))
#Confidence Interval
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(paste("Confidence Interval:", lower.bound, "< x <", upper.bound))

## Bill Depth
## Choose data
data <- ade
#data <- chi
#data <- gen

##Mean
sample.mean <- mean(data$bill_depth_mm)
print(paste("mean:", sample.mean))
##Standard error of mean
sample.n <- length(data$bill_depth_mm)
sample.sd <- sd(data$bill_depth_mm)
sample.se <- sample.sd/sqrt(sample.n)
print(paste("standard error of mean:",sample.se))
##t-score
alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(paste("t-score:", t.score))
#Margin error
margin.error <- t.score * sample.se
print(paste("Margin error:", margin.error))
#Confidence Interval
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(paste("Confidence Interval:", lower.bound, "< x <", upper.bound))

# Likelihood
data <- ade
#data <- chi
#data <- gen

normalF <- function(parvec) {
  # Log of likelihood of a normal distribution
  # parvec[1] - mean
  # parvec[2] - standard deviation
  # x - set of observations. Should be initialized before MLE
  sum ( -0.5* log(parvec[2]) - 0.5*(x - parvec[1])^2/parvec[2] )
}
normalF(data$bill_length_mm)

# Sexing
##Subset
ade <- subset(x = my.penguins, subset = species == "Adelie", select = c(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year))
chi <- subset(x = my.penguins, subset = species == "Chinstrap", select = c(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year))
gen <- subset(x = my.penguins, subset = species == "Gentoo", select = c(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year))

b_s <- ggplot(data = my.penguins, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = sex), alpha = 0.5, position = "identity") +
  ggtitle("Histogram of Body Mass by Sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Body Mass (g)") +
  theme(legend.text = element_text(size=11))

bd_s <- ggplot(data = my.penguins, aes(x = bill_depth_mm)) +
  geom_histogram(aes(fill = sex), alpha = 0.5, position = "identity") +
  ggtitle("Histogram of Bill Depth by Sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Bill Depth (mm)") +
  theme(legend.text = element_text(size=11))

ggarrange(b_s, bd_s,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

ah <- ggplot(data = ade, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = sex), alpha = 0.5, position = "identity") +
  ggtitle("Adelie") +
  theme(plot.title = element_text(hjust = 0.5))

ch <- ggplot(data = chi, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = sex), alpha = 0.5, position = "identity") +
  ggtitle("Chinstrap") +
  theme(plot.title = element_text(hjust = 0.5))

gh <- ggplot(data = gen, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = sex), alpha = 0.5, position = "identity") +
  ggtitle("Gentoo") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(ah, ch, gh,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

# By species By sex
ggplot(my.penguins, aes(x = body_mass_g,
                     y = bill_depth_mm)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"), 
                     na.translate = FALSE) 

# Scatter Plot with Ellipses
##Flipper Length vs Body Mass
f_bm <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = body_mass_g, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
f_bm +labs(title="Plot of length \n by dose",
           x ="Dose (mg)", y = "Teeth length")
##Flipper Length vs Bill Length
f_bl <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = bill_length_mm, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Flipper Length vs Bill Depth
f_bd <- ggplot(data = my.penguins, aes(x = flipper_length_mm, y = bill_depth_mm, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Body Mass vs Bill Length
bm_bl <- ggplot(data = my.penguins, aes(x = body_mass_g, y = bill_length_mm, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Body Mass vs Bill Depth
bm_bd <- ggplot(data = my.penguins, aes(x = body_mass_g, y = bill_depth_mm, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
##Bill Length vs Bill Depth
bl_bd <- ggplot(data = my.penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = sex)) +
  geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = sex), 
               alpha = 0.2) +
  theme(legend.text = element_text(size=10))
## Scatter Plot in one page
ggarrange(f_bm + labs(title="Flipper Length vs Body Mass", x ="Flipper Length (mm)", y = "Body Mass (g)"), 
          f_bl + labs(title="Flipper Length vs Bill Length", x ="Flipper Length (mm)", y = "Bill Length (mm)"),
          f_bd + labs(title="Flipper Length vs Bill Depth", x ="Flipper Length (mm)", y = "Bill Depth (mm)"),
          bm_bl + labs(title="Body Mass vs Bill Length", x ="Body Mass (g)", y = "Bill Length (mm)"),
          bm_bd + labs(title="Body Mass vs Bill Depth", x ="Body Mass (g)", y = "Bill Depth (mm)"),
          bl_bd + labs(title="Bill Length vs Bill Depth", x ="Bill Length (mm)", y = "Bill Depth (mm)"),
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "right")
# Hypothesis Testing
#Mean
p_female <- subset(x = my.penguins, subset = sex == "female", select = c(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year))
p_male <- subset(x = my.penguins, subset = sex == "male", select = c(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year))

female_avg_b <- mean(p_female$body_mass_g)
male_avg_b <- mean(p_male$body_mass_g)

female_avg_b + (male_avg_b - female_avg_b)/2

#t-value
t.test(p_female$body_mass_g, mu = 4200, alternative = 'greater')

penguins_flipper_length_by_sex= my.penguins %>%
  group_by(species,sex) %>%
  summarise(bill_depth_mm= mean(bill_depth_mm),           #summarising
            bill_length_mm= mean(bill_length_mm),         #on basis
            flipper_length_mm=mean(flipper_length_mm),    #of mean
            body_mass_g=mean(body_mass_g))
ggplot(penguins_flipper_length_by_sex, aes(x=sex, y=flipper_length_mm,fill=sex))+   #plotting flipper length with respect to sex
  geom_col(position = position_dodge())

#Trial Hypothesis Testing
#splitting the dataset
my.penguins_male = my.penguins[my.penguins$sex == "male",] #male data set
my.penguins_female= my.penguins[my.penguins$sex == "female",] #female data set
#counter for comparison and calculating probability as if gone through vector space time complexity would be more
countGreaterMale<-0  #counter of male penguins having more flipper length
countGreaterFemale<-0 # counter for female penguins having more flipper length

# loop initiation for comparison
for (male in my.penguins_male$body_mass_g){ #outer loop for male 
  for (female in my.penguins_female$body_mass_g){ #nested loop for female as we have to consider all combinations
    if (male>=female){                #comparison between flipper length
      countGreaterMale<- countGreaterMale+1  #increaing counter if true
    }
    else{
      countGreaterFemale<-countGreaterFemale+1 #increasing counter if not true
    }
    
  }
}
#countGreaterMale #total times male penguins having more flipper length
#countGreaterFemale #total times female penguins having more flipper length
probfemale<- 1 - (countGreaterFemale/(countGreaterMale+countGreaterFemale)) #probability of males having more flipper length than females.
print("probability of female having lighter bodymass than male:")
probfemale

#answer2
print("null hypotheis that penguin's body mass are 3900g on average:")
my.penguins_female = my.penguins[my.penguins$sex == "female",] #male data set
meanBodyMass_female= mean(my.penguins_female$body_mass_g) #mean of the flipper length
print("mean:") #printing the value
meanBodyMass_female
varianceBodyMass_female=var(my.penguins_female$body_mass_g) #variance of the flipper length 
print("Variance:")
varianceBodyMass_female
length_female=length(my.penguins_female$body_mass_g) #total no. of male penguins
print("length of the dataset of male:")
length_female
Tvalue_female<-(meanBodyMass_female-200)/(sqrt(varianceBodyMass_female/length_female)) #t value of the null hypothesis
print("T value:")
Tvalue_female
Pvalue_female<-pt(Tvalue_female,(length_female-1)) #pvalue of the null hypothesis
print("P value:")
Pvalue_female
print("As the p value is greater than 0.05. the hypothesis stands true")


countHappen <- 0
countNot <- 0
for (body_mass in my.penguins_female$body_mass_g){ #outer loop for male 
  if(body_mass <= 3900){
    countHappen <- countHappen + 1
  }
  else{
    countNot <- countNot + 1
  }
}

prob_happen = countHappen/(countHappen+countNot)
prob_happen

binom.test(100, 333, p = 0.63)

# Two-sample t-test
bartlett.test(bill_depth_mm ~ sex, data = my.penguins)
t.test(bill_depth_mm ~ sex, data = my.penguins, var.equal = TRUE)

# Island and species
ggplot(my.penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()+
  theme(plot.title = element_text(size = 30))

# Island Box plot
#bwplot(value ~ variable, data_long)

#penguins_long <- melt(ade, id = "island")
#head(penguins_long)        

#ggplot(penguins_long, aes(x = variable, y = value, color = island)) +  # ggplot function
  #geom_boxplot()
#flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm"

ade_fl <- ggplot(ade, aes(x=island, y=flipper_length_mm, color=island)) +
  geom_boxplot() + scale_color_brewer(palette="Dark2")
ade_bm <- ggplot(ade, aes(x=island, y=body_mass_g, color=island)) +
  geom_boxplot() + scale_color_brewer(palette="Dark2")
ade_bl <- ggplot(ade, aes(x=island, y=bill_length_mm, color=island)) +
  geom_boxplot() + scale_color_brewer(palette="Dark2")
ade_bd <- ggplot(ade, aes(x=island, y=bill_depth_mm, color=island)) +
  geom_boxplot() + scale_color_brewer(palette="Dark2")

ggarrange(ade_fl + labs(title="Flipper Length", y ="Flipper Length (mm)"),
          ade_bm + labs(title="Body Mass", y ="Body Mass (g)"),
          ade_bl + labs(title="Bill Length", y ="Bill Length (mm)"), 
          ade_bd + labs(title="Bill Depth", y ="Bill Depth (mm)"),
          ncol = 4, nrow = 1, common.legend = TRUE, legend = "right")

# Correlation every variables
my.penguins %>%
  GGally::ggpairs(aes(color = sex),
                  columns = c("flipper_length_mm", "body_mass_g", 
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Trials
# Correlation every variables
ade %>%
  GGally::ggpairs(aes(color = island),
                  columns = c("flipper_length_mm", "body_mass_g", 
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))


my.penguins %>%
  GGally::ggpairs(aes(color = species),
                  columns = c("flipper_length_mm", "body_mass_g", 
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Correlation with everything but only one panel
ggpairs(my.penguins,
        columns = c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm"), 
        aes(color = species, alpha = 0.5),
        upper = list(continuous = "blankDiag"),
        lower = list(continuous = "smooth"),
        diag = list(continuous = "blankDiag")) 

ggpairs(my.penguins,
        columns = c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm"), 
        aes(color = species, alpha = 0.5),
        upper = list(continuous = "blankDiag"),
        diag = list(continuous = "blankDiag")) 

ggpairs(my.penguins,
        columns = c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm"), 
        aes(color = sex, alpha = 0.5),
        lower = list(continuous = "smooth"))

#  Correlation table
penguins_corr <- my.penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  correlate() %>%
  rearrange()
penguins_corr

# PCA Data
my.penguins %>% 
  dplyr::select(body_mass_g, ends_with("_mm")) %>% 
  tidyr::drop_na() %>% 
  scale() %>% 
  prcomp() %>%  
  .$rotation








# Trial
barplot(table(my.penguins$sex))

boxplot(my.penguins$bill_length_mm ~ my.penguins$species, xlab="Species", ylab="Bill Length", col="forestgreen")
boxplot(my.penguins$bill_length_mm ~ my.penguins$island, xlab="Island", ylab="Bill Length", col="forestgreen")

# Histogram
# Comparison of Body Mass between Male and Female (All Species) - Histogram
par(mfrow=c(1,2))
hist(my.penguins$body_mass_g[my.penguins$sex == "male"], xlim = c(2000, 6000),
     main = 'Body Mass Distribution of Male Penguins', xlab = 'Body Mass (g)', col = "purple")
hist(my.penguins$body_mass_g[my.penguins$sex == "female"], xlim = c(2000, 6000), 
     main = 'Body Mass Distribution of Female Penguins', xlab = 'Body Mass (g)', col = "chocolate")

# Comparison of Body Mass between Male and Female (Adelie) - Histogram
par(mfrow=c(1,2))
hist(my.penguins$body_mass_g[my.penguins$sex == "male" & my.penguins$species == "Adelie"], xlim = c(2000, 6000),
     main = 'Body Mass Distribution of Male Adelie', xlab = 'Body Mass (g)', col = "purple")
hist(my.penguins$body_mass_g[my.penguins$sex == "female" & my.penguins$species == "Adelie"], xlim = c(2000, 6000), 
     main = 'Body Mass Distribution of Female Adelie', xlab = 'Body Mass (g)', col = "chocolate")

# Comparison of Body Mass between Male and Female (Chinstrap) - Histogram
par(mfrow=c(1,2))
hist(my.penguins$body_mass_g[my.penguins$sex == "male" & my.penguins$species == "Chinstrap"], xlim = c(2000, 6000),
     main = 'Body Mass Distribution of Male Chinstrap', xlab = 'Body Mass (g)', col = "purple")
hist(my.penguins$body_mass_g[my.penguins$sex == "female" & my.penguins$species == "Chinstrap"], xlim = c(2000, 6000), 
     main = 'Body Mass Distribution of Female Chinstrap', xlab = 'Body Mass (g)', col = "chocolate")

# Comparison of Body Mass between Male and Female (Gentoo) - Histogram
par(mfrow=c(1,2))
hist(my.penguins$body_mass_g[my.penguins$sex == "male" & my.penguins$species == "Gentoo"], xlim = c(2000, 6000),
     main = 'Body Mass Distribution of Male Gentoo', xlab = 'Body Mass (g)', col = "purple")
hist(my.penguins$body_mass_g[my.penguins$sex == "female" & my.penguins$species == "Gentoo"], xlim = c(2000, 6000), 
     main = 'Body Mass Distribution of Female Gentoo', xlab = 'Body Mass (g)', col = "chocolate")


# Box Plot
boxplot(my.penguins$bill_length_mm ~ my.penguins$species, xlab="Species", ylab="Bill Length", col="forestgreen")
boxplot(my.penguins$bill_length_mm ~ my.penguins$island, xlab="Island", ylab="Bill Length", col="forestgreen")




# Comparison of Body Mass between Male and Female - Box plot
par(mfrow=c(1,2))
boxplot(my.penguins$body_mass_g[my.penguins$sex == "male"])
boxplot(my.penguins$body_mass_g[my.penguins$sex == "female"])

# Quantile values of Body Mass between Male and Female
quantile(my.penguins$body_mass_g[my.penguins$sex == "male"], type=6)
quantile(my.penguins$body_mass_g[my.penguins$sex == "female"], type=6)









