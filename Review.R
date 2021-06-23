library(bibliometrix)
library(plotly)

biblioshiny()

#Import Data-Only CAIS
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/CAIS_Review_0328_merge_remove_review.bib"
#Import Data-Only JAIS
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/JAIS_Review_0328_merge_remove_review.bib"
#Import Data-Only PAJAIS
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/PAJAIS_Review_0328_merge_remove_review.bib"

#Import Data-three journals
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/0406_Reivew.bib"

#Import Data-three journals-分年份
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/2000_2010.bib"

#Import Data-three journals-分年份
rawData <- "C:/Users/kiki3/Documents/Analysis/RawData/2011_2020.bib"

data <- convert2df(file = rawData, format = "bibtex")
Review <- subset(data, select = c(-C1,-AU_UN))
View(Review)

results <- biblioAnalysis(Review, sep = ";")
options(width=100)
S <- summary(object = results, k = 24, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

authorProdOverTime(Review, k = 24, graph = TRUE)

#CAIS
CAIS<- thematicMap(Review,field = "DE",n = 200, minfreq = 50,size = 0.5,n.labels = 1,repel = TRUE)
CAIS$clusters
CAIS$words
CAIS$map
CAIS

#JAIS
JAIS<- thematicMap(Review,field = "DE",n = 50, minfreq = 30,size = 0.5,n.labels = 1,repel = TRUE)
JAIS$clusters
JAIS$words
JAIS$map

#PAJAIS
PAJAIS<- thematicMap(Review,field = "DE",n = 50, minfreq = 20,size = 0.5,n.labels = 1,repel = TRUE)
PAJAIS$clusters
PAJAIS$words
PAJAIS$map

#ALL
res<- thematicMap(Review,field = "DE",n = 300, minfreq = 41,size = 0.5,n.labels = 1,repel = TRUE)
res$clusters
res$words
res$map

#Review CAIS, JAIS, and PAJAIS
x <- c('CAIS', 'JAIS', 'PAJAIS')
y1 <- c(8,4,4)
y2 <- c(6,3,3)
y3 <- c(4,2,2)
X1_text <- c('Meta Analysis;System Development(8)',
             'Research Methodologies(4)',
             'Framework(4)')
X2_text <- c('Research Methodologies(6)',
             'Multilevel Research(3)',
             'Knowledge Management(3)')
X3_text <- c('Enterprise Systems;Qualitative Research(4)',
             'Assumption;Behavioral Research;Case Study;Historical Review;Information Systems Development;Ontology;Realism;TAM(2)',
             'Design Science Research;Digital Transformation;Meta Analysis;Scocial Media Research(2)')
data <- data.frame(x,y1,y2,y3,X1_text,X2_text,X3_text)


fig <- data %>% plot_ly()
fig <- fig %>% add_trace(x = ~y3, y = ~x, type = 'bar',
                         text = X3_text, textposition = 'auto',
                         marker = list(color = '#60ACFC',
                                       width = 1.5))
fig <- fig %>% add_trace(x = ~y2, y = ~x, type = 'bar',
                         text = X2_text, textposition = 'auto',
                         marker = list(color = '#FEB64D',
                         width = 1.5))
fig <- fig %>% add_trace(x = ~y1, y = ~x, type = 'bar',
                         text = X1_text, textposition = 'auto',
                         marker = list(color = '#5BC49F',
                         width = 1.5))

fig <- fig %>% layout(title = "AIS - Review Articles",
                      barmode = 'group',
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))
fig <- fig %>% layout(yaxis = list(title = 'Journals'), barmode = 'group')
fig <- fig %>% layout(xaxis = list(title = 'Times'), barmode = 'group')
fig


threeFieldsPlot(Review,fields = c("AU", "ID", "DE"),
                n = c(20, 20, 20),
                width = 1200,
                height = 600
)

threeFieldsPlot(Review,fields = c("AU", "ID", "DE"),
                n = c(10, 10, 10),
                width = 1200,
                height = 600
)

threeFieldsPlot(Review,fields = c("ID", "AU", "DE"),
                n = c(10, 10, 20),
                width = 1200,
                height = 600
)

threeFieldsPlot(Review,fields = c("ID", "AU", "DE"),
                n = c(20,20,20),
                width = 1200,
                height = 600
)

threeFieldsPlot(Review,fields = c("SO", "ID", "DE"),
                n = c(3, 3, 3),
                width = 1200,
                height = 600
)

?biblioNetwork
?networkPlot

data(Review, package = "bibliometrixData")

NetMatrix <- biblioNetwork(Review, analysis = "co-occurrences", 
network = "author_keywords", sep = ";")

net <- networkPlot(NetMatrix, n = 30, type = "fruchterman", Title = "co-occurrences",labelsize=0.5) 

net2VOSviewer(net)

# #Time Evolution1
# years=c(2010,2015)
# nexus <- thematicEvolution(Review,field = "DE",years = years,n=100,minFreq = 10)
# plotThematicEvolution(nexus$Nodes, nexus$Edges)
# 
# #Time Evolution2
# years=c(2006,2008,2012)
# nexus <- thematicEvolution(Review,field = "DE",years = years,n=100,minFreq = 10)
# plotThematicEvolution(nexus$Nodes, nexus$Edges)

#Time Evolution across 20 years
x <- c('2001-2005','2006-2010','2011-2015','2016-2020')
y1 <- c(4,2,5,5)
y2 <- c(3,2,4,2)
X1_text <- c('Meta Analysis(4)',
             'Delphi Method(2)',
             'Meta Analysis(5)',
             'Framework(5)')
X2_text <- c('Research Methodologies(3)',
             'System Development(2)',
             'Research Methodologies;System Development(4)',
             'Case Study;Design Science Research;Digital Transformation;Information Security;Information System Development;
             Neurois;Qualitative Research;Research Methodologies;Taxonomy(2)')

data <- data.frame(x,y1,y2,X1_text,X2_text)


fig <- data %>% plot_ly()
fig <- fig %>% add_trace(x = ~y2, y = ~x, type = 'bar',
                         name = 'Top2',
                         text = X2_text, textposition = 'auto',
                         marker = list(color = '#FEB64D',
                                       width = 1.5))
fig <- fig %>% add_trace(x = ~y1, y = ~x, type = 'bar',
                         name = 'Top1',
                         text = X1_text, textposition = 'auto',
                         marker = list(color = '#5BC49F',
                                       width = 1.5))

fig <- fig %>% layout(title = "Time Evolution-Keywords",
                      barmode = 'group',
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))
fig <- fig %>% layout(yaxis = list(title = 'Years'), barmode = 'group')
fig <- fig %>% layout(xaxis = list(title = 'Times'), barmode = 'group')
fig
