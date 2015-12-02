library("ggplot2")

source("process.R")

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
