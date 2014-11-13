density = c(
  "Alabama" = 27,
  "Arizona" = 52,
  "Arkansas" = 45,
  "California" = 23,
  "Colorado" = 35,
  "Connecticut" = 29,
  "Delaware" = 32,
  "District of Columbia" = 0,
  "Florida" = 38,
  "Georgia" = 44,
  "Idaho" = 33,
  "Illinois" = 33,
  "Indiana" = 20,
  "Iowa" = 16,
  "Kansas" = 23,
  "Kentucky" = 18,
  "Louisiana" = 40,
  "Maine" = 14,
  "Maryland" = 37,
  "Massachusetts" = 38,
  "Michigan" = 32,
  "Minnesota" = 41,
  "Mississippi" = 39,
  "Missouri" = 21,
  "Montana" = 21,
  "Nebraska" = 26,
  "Nevada" = 34,
  "New Hampshire" = 46,
  "New Jersey" = 43 ,
  "New Mexico" = 34,
  "New York" = 44,
  "North Carolina" = 40,
  "North Dakota" = 11,
  "Ohio" = 28,
  "Oklahoma" = 36,
  "Oregon" = 48,
  "Pennsylvania" = 32,
  "Rhode Island" = 14 ,
  "South Carolina" = 52,
  "South Dakota" = 35,
  "Tennessee" = 39,
  "Texas" = 39,
  "Utah" = 27,
  "Vermont" = 0,
  "Virginia" = 38,
  "Washington" = 72,
  "West Virginia" = 29,
  "Wisconsin" = 34,
  "Wyoming" = 19
)

# Breaks we'll use for coloring
densityBreaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
# Construct break ranges for displaying in the legend
densityRanges <- data.frame(
  from = head(densityBreaks, length(densityBreaks)-1),
  to = tail(densityBreaks, length(densityBreaks)-1)
)

# Eight colors for eight buckets
palette <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
             "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
# Assign colors to states
colors <- structure(
  palette[cut(density, densityBreaks)],
  names = tolower(names(density))
)

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




#####################
####################

states.co2 = c()

all.states.co2 <- read.csv(file="CO2FFC_2012.csv", header=FALSE)[4:309,3:25]


new.state.names <- c(state.name[1:8],"District of Columbia", state.name[9:50])



for (i in 1:length(new.state.names)){
    states.co2[[new.state.names[i]]] <- all.states.co2[(1:6+6*(i-1)),]
    colnames(states.co2[[i]]) <- c(1990:2012)
    rownames(states.co2[[i]]) <- c("Total", paste(read.csv(file="CO2FFC_2012.csv", header=FALSE)[5:9,2]))
}

state.goals <- read.csv(file="state_Goals.csv", header=TRUE, row.names=1)

