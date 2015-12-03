# Sample script for processing and visualizing the benchmark results.
# 
# The script aggregates the data and transforms it to a wide table suited for visualization.
# The aggregration takes the **average** time required by the individual steps in the phases. 
# For multiple runs, the script takes the **minimum** value.
#
# The basic workflow for the script is the following:
# * load the file from the CSV
# * filter the data
# * convert the data from long table to wide table (better suited for processing)
# * aggregate the data
# * convert the data to long table (better suited for visualization)
# * draw the plots

library("reshape2")
library("plyr")

source("plot.R")

results = read.csv("results.csv")

# filtering for time values
times = subset(results, Metric == "time")

# convert nanoseconds to seconds
times$Value = times$Value / 10^9
# replace underscore with space in tool names
times$Tool = gsub('_', ' ', times$Tool)

# transform long table to wide table
times.plot = times
times.plot = rename(times.plot, c("Value"="time"))

# plot decoration
scenario = "Batch"
# the scenario is set to "Batch" on purpose
modelsizes.repair = data.frame(Scenario = "Batch", Artifact = 2^(0:14), Triples = c("4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"))
modelsizes = do.call(rbind, list(modelsizes.repair))

levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")

benchmark.plot.by.case(times.plot, "Batch", modelsizes, levels.cases, "read", "read phase")
