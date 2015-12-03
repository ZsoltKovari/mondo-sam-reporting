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
head(times.plot)
times.plot = rename(times.plot, c("Value"="time"))
head(times.plot)

# plot decoration
scenario = "Batch"
modelsizes = data.frame(Scenario = "Repair", Artifact = 2^(0:14), Triples = c("4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"))
levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")
# x axis labels
xbreaks = modelsizes$Artifact
xlabels = paste(xbreaks, "\n", modelsizes$Triples, sep = "")
# y axis labels
ys = -10:10
ybreaks = 10^ys
ylabels = parse(text = paste("10^", ys, sep = ""))

facet = as.formula(paste("~", "Case"))
title = "MyPlot"

width = 210
height = 297

df = times.plot
df = df[df$Phase == "read", ]
df$Case = factor(df$Case, levels = levels.cases)

head(df)
drops = c("Run","Iteration")
df = df[,!(names(df) %in% drops)]
p = ggplot(df) +
  labs(title = paste(scenario, " scenario, ", title, sep = ""), x = "model size\n#triples", y = "execution time [s]") +
  geom_point(aes(x = as.factor(Artifact), y = time, col = Tool, shape = Tool), size = 1.5) +
  geom_line(aes(x = as.factor(Artifact), y = time, col = Tool, group = Tool), size = 0.15) +
  scale_shape_manual(values = seq(0,24)) +
  scale_x_discrete(breaks = xbreaks, labels = xlabels) +
  scale_y_log10(breaks = ybreaks, labels = ylabels) +
  facet_wrap(facet, ncol = 2, scale = "fixed") +
  theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom")
print(p)

plot.filename = gsub(" ", "-", title)
plot.filename = paste("diagrams/", scenario, "-", plot.filename, ".pdf", sep = "")
ggsave(file = plot.filename, width = width, height = height, units = "mm")

