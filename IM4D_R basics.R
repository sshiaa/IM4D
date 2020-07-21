##################################
# Dummy for Dummies---Lecture 0#
# R basics #
# Shawn SHI #
##################################

#setwork directory
setwd("D:/Dropbox/SOE0330/LaTex/R course/spatial analysis")
getwd()

#setwd("/Users/Desktop/IM4D/") #This is how it looks like if you are a Mac user

#install package
#install.packages("rmarkdown")

#use that package
#library(ggplot2)

#assignment
x <- 3         # Assignment
x              # Evaluate the expression and print result
z <- x + 17*y  # Assignment

x = 3         # Assignment
x              # Evaluate the expression and print result
z = x + 17*y  # Assignment

#vector
v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)
v <- 1:7         # same as c(1,2,3,4,5,6,7)  
v <- rep(0, 77)  # repeat zero 77 times: v is a vector of 77 zeroes
v <- rep(1:3, times=2) # Repeat 1,2,3 twice  
v <- rep(1:10, each=2) # Repeat each element twice  
v <- seq(10,20,2) # sequence: numbers between 10 and 20, in jumps of 2  

#math
sum(v1)      # The sum of all elements
mean(v1)     # The average of all elements
sd(v1)       # The standard deviation
cor(v1,v1*5) # Correlation between v1 and v1*5 

#data frame
dfr1 <- data.frame( ID=1:4,
                    
                    FirstName=c("John","Jim","Jane","Jill"),
                    
                    Female=c(F,F,T,T), 
                    
                    Age=c(22,33,44,55) )



#plots
plot(x=1:10, y=rep(5,10), pch=19, cex=3, col="dark red") #pch is the point symbol shape, cex is the point size
points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799")
points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3))

#creat network

#veritice=nodes
#links=edges

rm(list = ls())
library(igraph)
g1 <- graph( edges=c(1, 2, 2, 3, 3, 1), n=3, directed=F ) #one way of double way
plot(g1) # A simple plot of the network - we'll talk more about plots later


g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10)
plot(g2)   

# When the edge list has vertex names, the number of nodes is not needed
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
plot(g3)

# In named graphs we can specify isolates by providing a list of their names.
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  
plot(g4)

#Erdos-Renyi random graph model
er <- sample_gnm(n=100, m=40) #(¡®n¡¯ is number of nodes, ¡®m¡¯ is the number of edges).
plot(er, vertex.size=6, vertex.label=NA)  

#readin network from file
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)

library(igraph)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
plot(net, edge.arrow.size=.4,vertex.label=NA)

#clear
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

#R markdown example

#ctrl+enter to run the data
#ctrl+z to undo the last step

#some resource: 
# help window
# https://rstudio.com/resources/cheatsheets/
# https://geocompr.robinlovelace.net/
# https://cengel.github.io/R-spatial/
# https://www.r-graph-gallery.com/ggplot2-package.html
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# If you have a question, it's probably been asked before on StackOverflow/StackExchange!





