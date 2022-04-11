

##########################################
######### CAMERON FIRE COVID-19 ##########
##########################################


###   Kristen Marshall, PhD, MPH   ###
###         2019 EISO, CDPHE       ###


# 4-5-21

install.packages(c("ggplot2","tidyr","data.table","dplyr","stringr","igraph","RColorBrewer","network"))
library(ggplot2)
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(igraph)
library(RColorBrewer)
library(network)

setwd("~/Desktop/Data")
fire <- read.csv("Cameron_Network_Data_DeID.csv",header=TRUE,as.is=TRUE)



#############
#   Clean   #
#############

# Separate exposures to new columns
fire2 <- fire %>% separate(Intra.crew.Contacts..all., 
                                 c("C1", "C2","C3", "C4", "C5", "C6", "C7","C8",
                                   "C9","C10","C11","C12"))


#################
#   Edge list   #
#################

# Subset and convert wide to long
firetemp <- subset(fire2, select=c("KM.ID","C1", "C2","C3", "C4", "C5", "C6", "C7","C8",
                                      "C9","C10","C11","C12"))
fireedge <- reshape(firetemp, 
                    direction = "long",
                    varying = list(names(firetemp)[2:13]),
                    v.names = "Value",
                    idvar = "KM.ID")

# Clean - delete time var and remove duplicate rows
fireedge <- fireedge[-2]
fireedge <- unique(fireedge)

# Delete value=NA (they replicated somehow); n=441 pairs
fireedge <- fireedge[!is.na(fireedge$Value),]

# Order by system_id
fireedge <-fireedge[order(fireedge$KM.ID),]

# Most frequent node?
sort(table(fireedge$Value))
# All nodes in C-201; Oregon gorup (largest group, all connected to each other)

# Remove duplicate rows
fireedge <- unique(fireedge) #no duplicates
fireedge <- fireedge[!duplicated(t(apply(fireedge,1,sort))),] #duplicates removed; n=251

# Edge lists complete


#####################
#   Network graph   #
#####################


# Basic graph #

fgraph <- graph.data.frame(fireedge,directed=FALSE)
# Plot basic graph
coords = layout_with_fr(fgraph)
fgraph <- simplify(fgraph)
plot(fgraph, layout=coords, vertex.label=NA, vertex.size=10)
# Optional .graphml export
write.graph(graph=fgraph, format="graphml", file="~/Desktop/Data/fgraphall_040521.graphml")


# Graph with node attributes #

fireedgetest <- fireedge
colnames(fireedgetest)[1] <- "system_id" # rename columns

# Check to see which "Value" are not present in "system_id"
fcheck <- as.data.table(fireedgetest$Value %in% fireedgetest$system_id) # a lot are missing
# Need to get all values in "Value" over to "system_id" side to be able to assign node attributes
fireedgev <- subset(fireedgetest, select=c("Value","system_id")) # subset
colnames(fireedgev)[2] <- "Value" # rename columns
colnames(fireedgev)[1] <- "system_id" # rename columns
fireedgev$system_id <- as.integer(fireedgev$system_id) # make sure classes match between ds
fireedgetest$Value <- as.integer(fireedgetest$Value) # make sure classes match between ds
fireedge2 <- rbind(fireedgetest, fireedgev, all=TRUE) # merge
fireedge2 <- unique(fireedge2) # n=503 -> n=442
fireedge2 <- fireedge2[(fireedge2$system_id != 1),] # remove wonky row at the end if present
fireedge2 <- fireedge2[!(fireedge2$system_id ==1 & fireedge2$Value ==1), ] # remove wonky row at the end if present
# n=441

# Recheck to see which "value are not present in "system_id"
fcheck <- as.data.table(fireedge2$Value %in% fireedge2$system_id) # all true

# Create graph - insert whichever edge list you want to use
# Graph must be undirected due to data entry methods
fgraph <- graph.data.frame(fireedge2,directed=FALSE)
# Plot basic graph
coords = layout_with_fr(fgraph)
fgraph <- simplify(fgraph)
plot(fgraph, layout=coords, vertex.label=NA, vertex.size=10)
# Optional .graphml export
write.graph(graph=fgraph, format="graphml", file="~/Desktop/Data/fgraphall_040621.graphml")

# Get node attributes for IDs present in fgraph
fnodes <- fire[(fire$KM.ID %in% fireedge2$system_id),] # only keep what is in ds
# Pull node attributes 
fnodes2 <- fire[(fire$KM.ID %in% fireedge2$Value),]
# Merge and make new ds "nodes"
fnodess <- rbind(fnodes, fnodes2, all=TRUE)

# Rename and subset
colnames(fnodess)[1] <- "system_id" 
fnodess <- subset(fnodess, select=c("system_id","Date.collected..COVID...MM.DD.YYYY.","Crew","Camp","Intra.crew.Contacts..all.","WGS.Cluster",
                                    "Sex","Ethnicity","Race","Staff.Address.State"))
fnodess <- fnodess[!(fnodess$system_id ==1 & fnodess$Crew == "TRUE"), ] # remove wonky row at the end if present

# Need nodes table to be same length as system_id in edge list
edgenode <- subset(fireedge2, select=c("system_id")) # subset just "from" nodes in edge list
fnodesnew <- merge(fireedge2,fnodess, all=TRUE) # merge
fnodesnew <- unique(fnodesnew) # delete duplicates
fnodesnew$system_id %in% fireedge2$system_id # all true
# Code blank and NA in node attributes to "unknown" (do for all attributes with missing data)
attach(fnodesnew)
fnodesnew$Crew[Crew == "-"] <- "Unknown"
fnodesnew$Crew[Crew == "Not on list"] <- "Unknown"
fnodesnew$Camp[Camp == ""] <- "Unknown"
fnodesnew$Sex[Sex == ""] <- "Unknown"
fnodesnew$Ethnicity[Ethnicity == ""] <- "Unknown"
fnodesnew$Race[Race == ""] <- "Unknown"
fnodesnew$WGS.Cluster[WGS.Cluster == ""] <- "Not sequenced"
fnodesnew$Camp[Camp == ""] <- "Unknown"
fnodesnew$Staff.Address.State[Staff.Address.State == ""] <- "Unknown"
fnodesnew <- subset(fnodesnew, select=c("system_id","Date.collected..COVID...MM.DD.YYYY.","Crew","Camp","Intra.crew.Contacts..all.","WGS.Cluster",
                                        "Sex","Ethnicity","Race","Staff.Address.State"))
# Delete duplicate node names
fnodesnew <- unique(fnodesnew) # delete duplicates

# Secondary graph with node attributes attached
fg <- graph_from_data_frame(fireedge2, directed=FALSE, vertices=fnodesnew)
plot(fg, layout=coords, vertex.label=NA, vertex.size=10)
# Optional .graphml export
write.graph(graph=fg, format="graphml", file="~/Desktop/Data/fgraphall_att_040621.graphml")








