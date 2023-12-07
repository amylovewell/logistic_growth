#wd set on positcloud
growth_data <- read.csv("experiment1.csv")

#Set the initial population
N0 <- exp(6.927845) 

#Set the growth rate
r <- 0.009944 

#Set the carrying capacity
K <- 6e+10 

#QUESTION 2

#Setting t to find the population size at 4980 minutes
t <- 4980

#Making exponential and logistic functions
population_exponential <- N0 * exp(r * t)
population_logistic <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))


print(paste("Population size at t =", t, "minutes according to the exponential growth model:", population_exponential))
#"Population size at t = 4980 minutes according to the exponential growth model: 3.27698164369414e+24"

print(paste("Population size at t =", t, "minutes according to the logistic growth model:", population_logistic))
#"Population size at t = 4980 minutes according to the logistic growth model: 59999999999.9989"


#QUESTION 3

#This makes a logistic growth function
logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}


#This makes an exponential growth function
exponential_fun <- function(t) {
  
  N <- (N0*exp(r*t))
  
  return(N)
  
}


#This installs and loads the packages used to make the figure
#install.packages("ragg")
#install.packages("ggplot2")
library(ragg)
library(ggplot2)

#This is the code for the plot with both exponential and logistic curves
exponentiallogisticplot <- ggplot(aes(t, N), data = growth_data) +
  
  #This adds the logistic growth curve
  stat_function(fun = logistic_fun, aes(color = "Logistic Growth")) +
  
  #This adds the exponential growth curve
  geom_function(fun = exponential_fun, aes(color = "Exponential Growth")) +
  
  #This labels the axis and title
  xlab("Time(minutes)") +
  ylab("Number of cells") +
  ggtitle("Line graph comparing growth in an exponential and logistic model")+
  
  #This sets the theme and sets the colours for the curves
  theme_bw() +
  scale_color_manual(values = c("red", "blue"))+
  
  #This sets the limits of the y axis to be able to view both curves
  ylim(c(0, 10e+10))

#This saves the image as a .png file with the measurements to view the whole plot
agg_png("/cloud/project/exponentiallogisticplot.png", 
        width = 30, height = 15, units = "cm", res = 600, scaling = 1.4)
exponentiallogisticplot
dev.off()
