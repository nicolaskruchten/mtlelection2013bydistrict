library(reshape2)
library(stringr)
library(ggtern)
library(grid)
library(plyr)
library(scales)


election2013 <- read.csv("elections-2013-resultats-detailles.csv")
election2013$early = ifelse(str_detect(election2013$Bureau, "[096]...?"), "early", "electionday")
casted = dcast(subset(election2013, Poste == "0"), 
        District ~ Candidat+early, 
        value.var="Votes", fun.aggregate=sum)

ggtern(data=casted) + 
  geom_segment(aes(
              x=`JOLY Mélanie_early`,
              y=`CODERRE Denis_early`,
              z=`BERGERON Richard_early`,
              xend=`JOLY Mélanie_electionday`,
              yend=`CODERRE Denis_electionday`,
              zend=`BERGERON Richard_electionday`,
              color = ifelse(
                District == '91-Claude-Ryan' | 
                District  == '92-Joseph-Beaubien',
                'Outliers', 'Trend')
                 ),arrow = arrow(length = unit(0.2,"cm"))) +
  labs(x="Joly", y="Coderre", z="Bergeron", 
       title="Montreal 2013 Mayoral Results: Early Voting vs Election Day") +
  scale_color_manual(values=c("Red", "Black")) +
  theme(legend.position="nope", axis.tern.padding    = unit(0.15, 'npc'))



overall = ddply(subset(election2013, Poste == "0"), "early", summarise, Votes=sum(Votes))
#18% early

faceted = ddply(subset(election2013, Poste == "0"), c("early", "Candidat"),
                summarise, Votes=sum(Votes))

faceted = transform(faceted, votePct = ave(Votes, early, FUN=prop.table))

faceted$w = ifelse(faceted$early=="early", 0.182, 0.818)
faceted$c = ifelse(faceted$early=="early", 0.182/2, 0.182+0.818/2)
faceted$l = ifelse(faceted$Candidat!="CODERRE Denis", "", 
                   ifelse(faceted$early=="early", "Early - 18.2%", "Election Day - 81.8%")
                   )
levels(faceted$Candidat) = gsub("Γ","Ô", levels(faceted$Candidat))

ggplot(subset(faceted, Candidat=="CODERRE Denis" | 
                    Candidat == "BERGERON Richard" | 
                    Candidat == "JOLY Mélanie" |
                Candidat == "CÔTÉ Marcel")) +
  geom_bar(aes(x=c, y=votePct, width=w,fill=Candidat),
           col = "Black", stat="identity") +
  geom_text(aes(x=c, y=votePct, label=paste(round(votePct*100,1),"%",sep="")), position = "stack", vjust=5) +
  geom_text(aes(label = l, x = c, y = 1)) + 
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
      title="Montreal 2013 Mayoral Results: Early Voting vs Election Day",
      y='Vote Split By Candidate', 
       x='Vote Split By Early vs Election Day', fill='Candidate' ) + 
  scale_fill_manual(values=c("#4daf4a", "#e41a1c", "#ff7f00", "#377eb8")) +
  theme(legend.position="bottom")




faceted2 = ddply(subset(election2013, Poste == "0"), c("District", "early", "Candidat"),
                 summarise, Votes=sum(Votes))

faceted2 = transform(faceted2, votePct = ave(Votes, District, early, FUN=prop.table))

qplot(data=subset(faceted2, Candidat=="CODERRE Denis" | 
                    Candidat == "BERGERON Richard" | 
                    Candidat == "JOLY Mélanie"),
      x=early, y=votePct, facets=~District, geom="line",
      group=Candidat, color=Candidat) +
  scale_color_manual(values=c("#4daf4a", "#e41a1c", "#377eb8")) +
  theme(legend.position="bottom")

faceted3 = ddply(subset(election2013, Poste == "0"), c("District", "early"),
                 summarise, Votes=sum(Votes))
casted3 = dcast(faceted3, District~early)
casted3$delta = casted3$early / (casted3$early+casted3$electionday)

qplot(data=casted3, x=delta, y=District) + 
  labs(title="Percentage of Early Voting", x='') + 
  scale_x_continuous(labels = percent)