library("ggplot2")

benchmark.plot = function(df, scenario, modelsizes, title, facet, scale, ncol, width = 210, height = 140) {
  # x axis labels
  modelsizes.scenario = modelsizes[modelsizes$Scenario == scenario, "Triples"]

  xbreaks = modelsizes[modelsizes$Scenario == scenario, "Artifact"]
  xlabels = paste(xbreaks, "\n", modelsizes.scenario, sep = "")
  
  # y axis labels
  ys = -10:10
  ybreaks = 10^ys
  ylabels = parse(text = paste("10^", ys, sep = ""))

  plot.filename = gsub(" ", "-", title)

  facet = as.formula(paste("~", facet))

  p = ggplot(df) +
    labs(title = paste(scenario, " scenario, ", title, sep = ""), x = "model size\n#triples", y = "execution time [s]") +
    geom_point(aes(x = as.factor(Artifact), y = time, col = Tool, shape = Tool), size = 3.0) +
    geom_line(aes(x = as.factor(Artifact), y = time, col = Tool, group = Tool), size = 0.15) +
    scale_shape_manual(values = seq(0,24)) +
    scale_x_discrete(breaks = xbreaks, labels = xlabels) +
    scale_y_log10(breaks = ybreaks, labels = ylabels) +
    facet_wrap(facet, ncol = ncol, scale = scale) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") +
    guides(shape = guide_legend(ncol = 2))
  
  ggsave(file = paste("diagrams/", scenario, "-", plot.filename, ".pdf", sep = ""), width = width, height = height, units = "mm")
}

benchmark.plot.by.phase = function(df, scenario, modelsizes, levels, case, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Case == case, ]
  df$Phase = factor(df$Phase, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "Phase", "free_y", ncol)
}

benchmark.plot.by.case = function(df, scenario, modelsizes, levels, phase, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Phase == phase, ]
  df$Case = factor(df$Case, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "Case", "fixed", ncol)
}
