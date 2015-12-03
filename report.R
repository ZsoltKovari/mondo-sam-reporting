source("process.R")
source("plot.R")

results = read.csv("results.csv")
plot.times = process.results(results)

# modelsizes
modelsize.batch  = data.frame(Scenario = "Batch",  Artifact = 2^(0:14), Triples = c("4.7k", "7.9k", "20.6k", "41k", "89.4k", "191.8k", "374.1k", "716.5k", "1.5M", "2.8M", "5.7M", "11.5M", "23M", "45.9M", "92.3M"))
modelsize.inject = data.frame(Scenario = "Inject", Artifact = 2^(0:14), Triples = c("5k", "9.3k", "19.9k", "44.6k", "85.7k", "191.6k", "373.1k", "752.8k", "1.5M", "3M", "5.8M", "11.6M", "23.3M", "46.5M", "93M"))
modelsize.repair = data.frame(Scenario = "Repair", Artifact = 2^(0:14), Triples = c("4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"))
modelsizes = do.call(rbind, list(modelsize.batch, modelsize.inject, modelsize.repair))

# levels for the facets in the plot
levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")

# draw plots
benchmark.plot.by.case(plot.times, "Batch", modelsizes, levels.cases, "Read", "read phase")
benchmark.plot.by.case(plot.times, "Batch", modelsizes, levels.cases, "Check", "check phase")
benchmark.plot.by.case(plot.times, "Batch", modelsizes, levels.cases, "Read and Check", "read and check phase")
