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

# modelsizes
modelsize.batch  = data.frame(Scenario = "Batch",  Artifact = 2^(0:14), Triples = c("4.7k", "7.9k", "20.6k", "41k", "89.4k", "191.8k", "374.1k", "716.5k", "1.5M", "2.8M", "5.7M", "11.5M", "23M", "45.9M", "92.3M"))
modelsize.inject = data.frame(Scenario = "Inject", Artifact = 2^(0:14), Triples = c("5k", "9.3k", "19.9k", "44.6k", "85.7k", "191.6k", "373.1k", "752.8k", "1.5M", "3M", "5.8M", "11.6M", "23.3M", "46.5M", "93M"))
modelsize.repair = data.frame(Scenario = "Repair", Artifact = 2^(0:14), Triples = c("4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"))
modelsizes = do.call(rbind, list(modelsize.batch, modelsize.inject, modelsize.repair))

# levels for the facets in the plot
levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")

# draw plots
#benchmark.plot.by.case(times.plot, "Batch", modelsizes, levels.cases, "read", "read phase")

scenario = "Batch"

# x axis labels
modelsizes = modelsize.repair

xbreaks = modelsizes$Artifact
xbreaks
xlabels = paste(xbreaks, "\n", modelsizes$Triples, sep = "")
xlabels

# y axis labels
ys = -10:10
ybreaks = 10^ys
ylabels = parse(text = paste("10^", ys, sep = ""))

facet = "Case"
facet = as.formula(paste("~", facet))
title = "MyPlot"

width = 210
height = 297

df = times.plot
df = df[df$Phase == "read", ]
df$Case = factor(df$Case, levels = levels.cases)

df
drops = c("Run","Iteration")
df = df[,!(names(df) %in% drops)]
df
ggplot(df) + 
  #geom_point(aes(x = as.factor(Artifact), y = time, col = Tool, shape = Tool), size = 1.5) +
  geom_line(aes(x = as.factor(Artifact), y = Value, col = Tool, group = Tool), size = 0.15)
  
p = ggplot(df) +
  labs(title = paste(scenario, " scenario, ", title, sep = ""), x = "model size\n#triples", y = "execution time [s]") +
  geom_point(aes(x = as.factor(Artifact), y = Value, col = Tool, shape = Tool), size = 1.5) +
  geom_line(aes(x = as.factor(Artifact), y = Value, col = Tool, group = Tool), size = 0.15) +
  scale_shape_manual(values = seq(0,24)) +
  facet_wrap(facet, ncol = 2, scale = "fixed") +
  theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom")
print(p)
p

plot.filename = gsub(" ", "-", title)
plot.filename = paste("diagrams/", scenario, "-", plot.filename, ".pdf", sep = "")
ggsave(file = plot.filename, width = width, height = height, units = "mm")

