library(tidyverse)
library(stringr)

#Read the results of the BRVRP and the instances
data <- read.csv("alpha-test.csv")
instance.path = "/home/bill/Doc/br-analysis/instances"
instance.list <- list.files(instance.path, pattern = ".*\\.vrp", full.names=TRUE)

i = 50

#Get instance name
instance.name = tail(unlist(str_split(instance.list[i], "/")), n= 1)

#Extract the optimal value from the instance file
a <- read_lines(instance.list[i])[2]
optimal <- as.numeric(stringr::str_extract(a, "(\\d+)(?!.*\\d)")) #Match the last number

#Get one instance
instance <- data %>%
	filter(instance == instance.name) %>% 
	filter(alpha != 1E-4) %>%
	mutate(alpha = round(alpha, digits = 2)) %>% 
	mutate(alpha = as.factor(alpha))

#Plot the distribution of the solutions wrt the geometric distribution parameter alpha
#showing the optimal value for the instance
ggplot(instance, aes(x = alpha, y = cost))+
	ylab("Cost")+
	geom_boxplot()+
	ggtitle("Solutions of the BRVRP w.r.t. alpha")+
	geom_hline(yintercept = optimal) + 
	annotate("text", 2, optimal, vjust = -1, label = "Optimal")