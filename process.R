# Sample script for visualizing the execution times of the phases in the benchmark.
# Processes the results file, aggregate the data and transform it to a wide table suited for visualization.
# The aggregration takes the **average** time required by the individual steps in the phases.
# For multiple runs, the script takes the **minimum** value.
#
# The basic workflow for the script is the following:
# * load the file from the CSV
# * convert the data from long table to wide table (better suited for processing)
# * filter and aggregate the data
# * convert the data to long table which (better suited for visualization)

library("reshape2")
library("plyr")

results = read.csv("results.csv")

#results.transform = function(results) {
# filtering for time values
times = subset(results, Metric == "Time")

# convert nanoseconds to seconds
times$Value = times$Value / 10^9
# replace underscore with space in tool names
times$Tool = gsub('_', ' ', times$Tool)

# long table to wide table
times.wide = dcast(times,
                   Scenario + Tool + Run + Case + Artifact + Metric ~ Phase,
                   value.var = "Value")

# calculate aggregated values
times.derived = times.wide
times.derived$Read.and.Check = times.derived$Read + times.derived$Check

# summarize for each value (along the **Iteration** attribute) using a columnwise function
times.aggregated.iteration = ddply(
  .data = times.derived,
  .variables = c("Scenario", "Tool", "Run", "Case", "Artifact", "Metric"),
  .fun = colwise(mean)
)

# summarize for each value (along the **Run** attribute) using the fr function
times.aggregated.runs = ddply(
  .data = times.aggregated.iteration,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Metric"),
  .fun = colwise(median)
)

# melt data to
times.plot = melt(
  data = times.aggregated.runs,
  id.vars = c("Tool", "Artifact", "Scenario", "Case"),
  measure.vars = c("Read", "Check", "Read.and.Check"),
  variable.name = "Phase",
  value.name = "time"
)

times.plot

# remove the . characters from the phasename
print(plottimes)
plottimes$PhaseName = gsub('\\.', ' ', plottimes$PhaseName)
#print(plottimes)

plottimes = plottimes[!is.na(plottimes$time), ]




# plottimes.mix = results.transform(results.mix)
plottimes.individual = results.transform(results.individual)

# constants
#modelsize.batch = data.frame(Scenario = "Batch", Size = 2^(0:14), Triples = c("4.7k", "7.9k", "20.6k", "41k", "89.4k", "191.8k", "374.1k", "716.5k", "1.5M", "2.8M", "5.7M", "11.5M", "23M", "45.9M", "92.3M"))
modelsize.batch = data.frame(Scenario = "Batch", Size = 4^(0:7), Triples = c("4.7k", "20.6k", "89.4k", "374.1k", "1.5M", "5.7M", "23M", "92.3M"))
# "5k", "9.3k", "19.9k", "44.6k", "85.7k", "191.6k", "373.1k", "752.8k", "1.5M", "3M", "5.8M", "11.6M", "23.3M", "46.5M", "93M"
modelsize.inject = data.frame(Scenario = "Inject", Size = 4^(0:7), Triples = c("5k", "19.9k", "85.7k", "373.1k", "1.5M", "5.8M", "23.3M", "93M"))
# "4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"
modelsize.repair = data.frame(Scenario = "Repair", Size = 4^(0:7), Triples = c("4.9k", "19.8k", "85.4k", "372.1k", "1.5M", "5.8M", "23.2M", "92.8M"))

modelsizes = do.call(rbind, list(modelsize.batch, modelsize.inject, modelsize.repair))

query.mix.batch = c("ConnectedSegments-PosLength-RouteSensor-SemaphoreNeighbor-SwitchSensor-SwitchSet")
query.mix = c("RouteSensor-ConnectedSegments-PosLength-SemaphoreNeighbor-SwitchSensor-SwitchSet")
levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")
