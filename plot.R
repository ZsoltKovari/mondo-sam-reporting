library("reshape2")
library("ggplot2")
library("plyr")

results.transform = function(results) {
  # filtering for time values (not interested in the number of matches for visualization)
  times = subset(results, MetricName == "Time")

  # convert nanoseconds to seconds
  times$MetricValue = times$MetricValue / 10^9

  # replace hyphen with space in tool names
  times$Tool = gsub('_', ' ', times$Tool)

  # long table to wide table
  times.wide = dcast(times,
                     Tool + Size + MetricName + Scenario + CaseName + Iteration + RunIndex ~ PhaseName,
                     value.var = "MetricValue")

  # calculate aggregated values
  derived.times = times.wide
  derived.times$read.and.check = derived.times$Read + derived.times$Check
  #derived.times$transformation.and.recheck = derived.times$Transformation + derived.times$Recheck

  # summarize along the iterations
  derived.times = ddply(
    .data = derived.times,
    .variables = c("Tool", "Size", "MetricName", "Scenario", "CaseName", "RunIndex"),
    summarize,
    read = sum(Read, na.rm = TRUE),
    check = sum(Check, na.rm = TRUE),
    read.and.check = sum(read.and.check, na.rm = TRUE)
    #transformation = mean(Transformation),
    #recheck = mean(Recheck),
    #transformation.and.recheck = mean(transformation.and.recheck)
  )
  
  print(derived.times)
  
  # take the median values from each measurement
  f = median
  derived.times = ddply(
    .data = derived.times,
    .variables = c("Tool", "Size", "MetricName", "Scenario", "CaseName"),
    summarize,
    read.and.check = median(read.and.check),
    #transformation.and.recheck = f(transformation.and.recheck),
    read = median(read),
    check = median(check)
    #transformation = f(transformation),
    #recheck = f(recheck)
  )

  # melt data to
  plottimes = melt(
    data = derived.times,
    id.vars = c("Tool", "Size", "Scenario", "CaseName"),
    measure.vars = c("read", "check","read.and.check"),
    variable.name = "PhaseName",
    value.name = "time"
  )

  # remove the . characters from the phasename
  print(plottimes)
  plottimes$PhaseName = gsub('\\.', ' ', plottimes$PhaseName)
  #print(plottimes)

  plottimes = plottimes[!is.na(plottimes$time), ]
  plottimes
}

benchmark.plot = function(df, scenario, modelsizes, title, facet, scale, ncol, width = 210, height = 297) {
  # x axis labels
  modelsizes.scenario = modelsizes[modelsizes$Scenario == scenario, "Triples"]

  xbreaks = modelsizes[modelsizes$Scenario == scenario, "Size"]
  xlabels = paste(xbreaks, "\n", modelsizes.scenario, sep="")

  # y axis labels
  ys = -10:10
  ybreaks = 10^ys
  ylabels = parse(text=paste("10^", ys, sep=""))

  plot.filename = gsub(" ", "-", title)

  facet = as.formula(paste("~", facet))

  p = ggplot(df) +
    labs(title = paste(scenario, " scenario, ", title, sep=""), x = "model size\n#triples", y = "execution time [s]") +
    geom_point(aes(x = as.factor(Size), y = time, col = Tool, shape = Tool), size = 1.5) +
    geom_line(aes(x = as.factor(Size), y = time, col = Tool, group = Tool), size = 0.15) +
    scale_shape_manual(values = seq(0,24)) +
    scale_x_discrete(breaks = xbreaks, labels = xlabels) +
    scale_y_log10(breaks = ybreaks, labels = ylabels) +
    facet_wrap(facet, ncol = ncol, scale = scale) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") +
    guides(shape = guide_legend(ncol = 4))
  print(p)
  
  ggsave(file=paste("../diagrams/", scenario, "-", plot.filename, ".pdf", sep=""), width = width, height = height, units = "mm")
}

benchmark.plot.by.phase = function(df, scenario, modelsizes, levels, case, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$CaseName == case, ]
  df$PhaseName = factor(df$PhaseName, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "PhaseName", "free_y", ncol)
}

benchmark.plot.by.case = function(df, scenario, modelsizes, levels, phase, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$PhaseName == phase, ]
  df$CaseName = factor(df$CaseName, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "CaseName", "fixed", ncol)
}


# results.mix = read.csv("../results/results-mix.csv")
results.individual = read.csv("../results/results.csv")

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

# benchmark.plot.by.phase(plottimes.mix, "Batch", modelsizes, levels.phases.batch, query.mix.batch, "query mix", 1)
# benchmark.plot.by.phase(plottimes.mix, "Inject", modelsizes, levels.phases, query.mix, "query mix")
# benchmark.plot.by.phase(plottimes.mix, "Repair", modelsizes, levels.phases, query.mix, "query mix")

benchmark.plot.by.case(plottimes.individual, "Batch", modelsizes, levels.cases, "read", "read phase")
benchmark.plot.by.case(plottimes.individual, "Batch", modelsizes, levels.cases, "check", "check phase")
benchmark.plot.by.case(plottimes.individual, "Batch", modelsizes, levels.cases, "read and check", "read and check phase")


# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "read", "read phase")
# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "check", "check phase")
# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "transformation", "transformation phase")
# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "recheck", "recheck phase")
# 
# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "read and check", "read and check phase")
# benchmark.plot.by.case(plottimes.individual, "Inject", modelsizes, levels.cases, "transformation and recheck", "transformation and recheck phase")
# 
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "read", "read phase")
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "check", "check phase")
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "transformation", "transformation phase")
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "recheck", "recheck phase")
# 
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "read and check", "read and check phase")
# benchmark.plot.by.case(plottimes.individual, "Repair", modelsizes, levels.cases, "transformation and recheck", "transformation and recheck phase")
