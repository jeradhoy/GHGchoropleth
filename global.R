###########
# Descritpion: preprocesses co2 data and defines several functions for use in application
# 			  This script runs before ui.R and server.R, and objects are available for use in the environments of both scripts
# Author: Jerad Hoy
# Modified: 7/6/2016
###############

library(leaflet)

getColorsPerCapita <- function(goals, x){
    colors <- structure(
      perCapitaPalette[cut(goals[,1], densityBreaks1)],
      names = tolower(rownames(goals))
    )
    return(colors[[x]])
}

getColorsTotal <- function(goals, x){
    colors <- structure(
    totalPalette[cut(goals[,1], densityBreaks2)],
    names = tolower(rownames(goals))
    )
    return(colors[[x]])
}

# The state names that come back from the maps package's state database has
# state:qualifier format. This function strips off the qualifier.
getStateName <- function(id) {
  strsplit(id, ":")[[1]][1]
}

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
    sep="", collapse=" ")
}

substrLeft <- function(x, n){
    substr(x, 1, nchar(x)-n)
}

#Includes district of columbia in state name vector
new.state.names <- c(state.name[1:8],"District of Columbia", state.name[9:50])


states.co2 = c()


all.states.co2 <- read.csv(file="CO2FFC_2012.csv", header=FALSE)[4:309,3:25]

for (i in 1:length(new.state.names)){
    states.co2[[new.state.names[i]]] <- all.states.co2[(1:6+6*(i-1)),]
    colnames(states.co2[[i]]) <- c(1990:2012)
    rownames(states.co2[[i]]) <- c("Total", paste(read.csv(file="CO2FFC_2012.csv", header=FALSE)[5:9,2]))
}

pop <- read.csv(file="NST-EST2014-01_thousand.csv")[9:59,4:8]
rownames(pop) <- new.state.names
colnames(pop) <- c(2010, 2011, 2012, 2013, 2014)



state.data <- data.frame(goals = numeric(51), newGoal = numeric(51), co2PerCapita = numeric(51), co2PerCapitaNew = numeric(51), total = numeric(51), commSlider = numeric(51), industSlider = numeric(51), residSlider = numeric(51), transSlider = numeric(51), elecSlider = numeric(51), totalNew = numeric(51), row.names=new.state.names)

for (state in rownames(state.data)) {
    state.data[state,5:10] <- c(states.co2[[state]][,"2012"])
    state.data[state,c("goals", "newGoal")] <- 28-(1-states.co2[[state]][1,"2012"]/states.co2[[state]][1,"2005"])*100
}

state.data_2005 <- data.frame(total = numeric(51), row.names=new.state.names)

for (state in rownames(state.data_2005)) {
    state.data_2005[state,1] <- c(states.co2[[state]][1,"2005"])
}

usGoal <- 28-(1-sum(state.data[c(-1,-12),5])/sum(state.data_2005[c(-1,-12),]))*100




state.data$co2PerCapita <- state.data[,5]/pop[,"2012"]*1e6
state.data$co2PerCapitaNew <- state.data[,5]/pop[,"2012"]*1e6
state.data$totalNew <- state.data$total

state.data <- round(state.data, digits=4)

densityBreaks <- c(-100, seq(0.001, 32.5), seq(34.5, 52, 2), seq(52, 117, 5))

densityBreaks1 <- c(-100, seq(0.001, 32.5), seq(34.5, 52, 2), seq(52, 117, 5))
densityBreaks2 <- c(-100, seq(0.001, 198, 2), seq(200, 240, 10), seq(260, 680, 20))

colfunc <- colorRampPalette(c("white","yellow", "red", "darkred","black"))

perCapitaPalette <- c("#66CD00", c(colfunc(length(densityBreaks1)-1)))
totalPalette <- c("#66CD00", c(colfunc(length(densityBreaks2)-1)))

