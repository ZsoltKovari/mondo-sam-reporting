# Sample script for processing the benchmark results.
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
# * convert the data to long table which (better suited for visualization)

library("reshape2")
library("plyr")

process.results = function(results) {
  # filtering for time values
  times = subset(results, Metric == "Time")
  
  # convert nanoseconds to seconds
  times$Value = times$Value / 10^9
  # replace underscore with space in tool names
  times$Tool = gsub('_', ' ', times$Tool)
  
  # transform long table to wide table
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
  
  # melt data to a wide table
  times.plot = melt(
    data = times.aggregated.runs,
    id.vars = c("Scenario", "Tool", "Case", "Artifact", "Metric"),
    measure.vars = c("Read", "Check", "Read.and.Check"),
    variable.name = "Phase",
    value.name = "time"
  )
  
  # remove the . characters from the phasename
  times.plot$Phase = gsub('\\.', ' ', times.plot$Phase)
  times.plot
}
