data <- read.csv("global_data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
