library(reshape2)
election2013 <- read.csv("elections-2013-resultats-detailles.csv")
melted = melt(subset(election2013, Poste == "0"))
casted = dcast(subset(melted, variable=="Votes"), District ~ Candidat, fun.aggregate=sum)
casted$District = as.numeric(lapply(strsplit(as.character(casted$District), "-"), "[", 1))
topThree = casted[c("District", "CODERRE Denis", "BERGERON Richard", "JOLY Mélanie")]
names(topThree) = c("NUM_DISTRICT", "Coderre", "Bergeron", "Joly")
write.csv(topThree, file="data.csv", row.names=FALSE)