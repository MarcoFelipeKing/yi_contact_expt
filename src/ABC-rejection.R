# Script to perform ABC analysis on the data and to fit the gradient model

# Define a function for a recurrence relation x_{n+1}=x_n+lam*(cs*af-x[i+1])
# where a is a constant.
#
# The function is defined by the recurrence relation
# x_{n+1}=x_n-beta*af*x_n-1
#
# 7/07/22 - remember that it's surface concentration we're measuring, not finger. So it's the reverse of the formula

#load packages ----------------------------------------------------------------
pacman::p_load(dplyr, tidyr, ggplot2)

gradient_model <- function(parameters) {
beta <- parameters[1]
cf <- parameters[2]
af <- parameters[3]
x <- rep(0, 6)

for (i in seq(1, 6)) { # FIXME define the correct equation- You measure surface intensity in each oval. So you need to predict that - not the finger concentration.
  # x[i + 1] <- cf - beta * x[i] * af
  # x[i + 1] <- beta*cfx[i]+(cf*af-x[i])
}
  return(x %>% unlist())
}

# Load in the data
dY<-vroom::vroom("data/Yi.data.longformat.20191121_censoring_handled.csv") %>% janitor::clean_names()

## Calculate the mean and standard deviation of raw_int_d data grouping by contact_number and surface 
dY_grouped<-dY %>% 
filter(gloves=="N") %>%
group_by(contact_number, surface) %>% 
summarise(mean_raw_int_d = mean(raw_int_d), sd_raw_int_d = sd(raw_int_d))

#Define Euclidean distance function between gradientModel and experimental data ----------------------------------------------------------------
#This is just for plastic,
distance_data_function <- function(x) {
    y <- dY_grouped %>% filter(surface=="Plastic") %>% select(mean_raw_int_d)%>%as.matrix()
    #drop contact_number column
    #y <- rbind(0,y)
    #add 0 to the first row of y
    y <- y[,2:ncol(y)]
    
    
    s <- dY_grouped %>% filter(surface=="Plastic") %>% select(sd_raw_int_d)%>%as.matrix()
    #drop contact_number column
   # s <- rbind(0,s)
    #add 0 to the first row of y
    s <- s[,2:ncol(s)]
    return(abs(sum((x - y), na.rm = TRUE)))
}

#Number of simulations ----------------------------------------------------------------
n <- 1e3

#Define a data.frme of n parameter draws for lam, cs and cf ----------------------------------------------------------------
parameters <- data.frame(
beta = runif(n = n, min = 0, max = 1), # transfer efficiency
cf = runif(n = n, min = 69977, max = 34516017), # surface concentrations
af = runif(n = n, min = 1.0, max = 2.9) # finger area
)


# Simulate n gradient models in serial and store the results in a data.frame
# gradientModels <- data.frame(gradientModel(parameters)) 

# listargs <- split(parameters,1:n)

# lapply(listargs,gradient_model)%>%bind_rows()

# For every row of parameters run the recurrence function gradientModel and store the results in a data.frame ----------------------------------------------------------------
predictions <- apply(parameters, 1, gradient_model)

# transpose predictions
predictions <- t(predictions)

# Calculate the distance for each gradient model in the data.frame gradientModels ----------------------------------------------------------------
distances <- apply(predictions, 1, distance_data_function)

# arrange predictions in ascending order of distance
predictions <- predictions[order(distances), ]


# Select the first m=50 rows of predictions
m = 50
predictions <- predictions[1:m, ]

# Convert predictions to a data.frame
predictions <- data.frame(predictions)

# pivot longer ready for plotting
predictions <- predictions %>%
pivot_longer(1:6) %>% 
mutate(contact = rep(0:5,m)) %>%
select(-name)

# Using ggplot2, plot the first 1000 rows of predictions using a geom_ribbon ----------------------------------------------------------------
y <- c(0.0, 1.29, 2.22, 2.39, 2.45, 2.09) # plastic mean
s <- c(0.0, 0.216, 0.521, 0.565, 0.848, 1.04) # plastic st

ribbon_data <- predictions%>%
group_by(contact) %>%
summarise(ymin = mean(value)-1.96*sd(value)/sqrt(m), 
ymax = mean(value)+1.96*sd(value)/sqrt(m))

mean_predictions <- predictions%>%
group_by(contact) %>%
summarise(ymean = mean(value))

ggplot() +
geom_point(data = data.frame(x=c(0:5),y=y,s=s),aes(x=x,y=y))+
geom_errorbar(data = data.frame(x=c(0:5),y=y,s=s), aes(x=x, y=y, width=s, ymin=y+s, ymax=y-s), size=1.05)+
geom_line(data = predictions[1:6, ], aes(x=contact,y=value),size=1.1) + # Best prediction 
geom_line(data= mean_predictions, aes(x=contact,y=ymean),colour="#a51e1e") + # Note that using the mean_prediction is worse than using the best.
geom_ribbon(data= ribbon_data ,aes(x=contact,ymin=ymin, ymax=ymax), alpha=0.2, fill="#a51e1e")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  xlab("Contact nubmer") +
  ylab("Concentration") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "none")->a

# Plot marginal density plots of the chosen parameter sets using ggplot2 ----------------------------------------------------------------

## First arrange the parameter dataframe by distances
parameters <- parameters[order(distances), ]

## select top 1000 parameter sets
parameters <- parameters[1:1000, ]

## Convert parameters to a data.frame
parameters <- data.frame(parameters)

## pivot longer ready for plotting  (note that the parameter sets are now arranged by distance)
parameters <- parameters %>%
pivot_longer(1:3)

## Create density plots for the chosen parameters
parameters%>%
# mutate(name=case_when(name="lam"~"lambda",
# name="cs"~"Surface concentration",
# name="af"~"Contact area"))%>%
ggplot()+
geom_histogram(aes(x=value,y=..density..),fill="#a51e1e",alpha=0.6,bins=15)+
facet_grid(~name, scales="free")+
  xlab("Parameter value")+
  ylab("Density")+
  theme(legend.position = "none") +
   hrbrthemes::theme_ipsum() +
  theme(legend.position = "none")->b

## place the density plots with the predictions using ggarrange and add label

ggpubr::ggarrange(a,b,nrow=2)->fig_arranged

## save the figure using 300 dpi
ggsave(file="images/parameter_distributions.png", plot=fig_arranged, width=8, height=8, units="in", dpi=300)


# Plot cumulative distribution of distances using ggplot2 ----------------------------------------------------------------

distances%>%
as_tibble()%>%
ggplot()+
geom_density(aes(x=value))+
  xlab("Distance")+
  ylab("Density")+
  theme(legend.position = "none") +
   hrbrthemes::theme_ipsum() +
  theme(legend.position = "none")

#Count number of distances below a threshold of 0.01
distances%>%
as_tibble%>%
arrange(value)
filter(value<0.01)%>%
nrow()
