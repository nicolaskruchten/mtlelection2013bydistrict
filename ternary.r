
library(vcd)
library(reshape2)
election2013 <- read.csv("elections-2013-resultats-detailles.csv")
melted = melt(subset(election2013, Poste == "0"))
casted = dcast(subset(melted, variable=="Votes"), District ~ Candidat, fun.aggregate=sum)
topFour = casted[c("CODERRE Denis", "BERGERON Richard", "JOLY Mélanie", "CΓTÉ Marcel")]
names(topFour) = c("Coderre", "Bergeron", "Joly", "Côté")


makeTernaryPlot = function (data) {
  matrixData = as.matrix(data)
  ternaryplot(matrixData[which(rowSums(matrixData) > 0),] , 
              main = "Electoral District Results for the 2013 Montreal Mayoral Election", 
              bg = "#eeeeee", border="grey",
              cex=sqrt(rowSums(matrixData))/50, 
              col=rgb(matrixData[,1]/rowSums(matrixData),
                      matrixData[,2]/rowSums(matrixData),
                      matrixData[,3]/rowSums(matrixData)
                      ,0.7), grid=FALSE)
  top <- sqrt(3)/2
  xlim <- c(-0.03, 1.03)
  ylim <- c(-1, top)
  pushViewport(viewport(width = unit(1, "snpc")))
  pushViewport(viewport(width = 0.8, height = 0.8, 
                        xscale = xlim, 
                        yscale = ylim, 
                        name = "plot"))
  grid.lines(c(0.75, 0.00), c(0.5 * top, 0), gp=gpar(lty=3))
  grid.lines(c(0.25, 1.00), c(0.5 * top, 0), gp=gpar(lty=3))
  grid.lines(c(0.50, 0.50), c(1.0 * top, 0), gp=gpar(lty=3))
  
  
  grid.lines(c(0.75, 0.50), c(0.5 * top, 0), gp=gpar(lty=3))
  grid.lines(c(0.25, 0.75), c(0.5 * top, 0.5 * top), gp=gpar(lty=3))
  grid.lines(c(0.25, 0.50), c(0.5 * top, 0), gp=gpar(lty=3))
  
  upViewport(2)
}

pdf(file="ternary.pdf", width=10, height=10)
makeTernaryPlot(topFour[c("Coderre", "Bergeron", "Joly")])
dev.off()
