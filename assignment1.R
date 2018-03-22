rm(list=ls())

web = read.csv("webforum.csv")

######taking a look at the features of the dataset
# calculating the number of Authors involved, threads exist and the number of posts.
length(unique(web$AuthorID))
length(unique(web$ThreadID))
length(unique(web$PostID))

#remove thread with word count 0. Because we cannot compare forum without any contents
webWC = web[web$WC != 0,]

#convert the data type of column "Date" from factor to Date.
webWC$Date = as.Date(as.character(webWC$Date), format = "%Y-%m-%d")

######Multiple Linear Regression(MLR) to model word count 
##code to produce Figure 1.1
fitWC = lm(webWC$WC ~ ., data = webWC[7:10])
summary(fitWC)

##code to produce Figure 1.2
fitWC = lm(webWC$WC ~ ., data = webWC[7:32])
summary(fitWC)


#finding the correlation between the 4 summary language variable with word count 
#Figure 1.3
cor(webWC[,6:10])

#MLR to model the 4 summary language variable (figure 2.2)
fitTone = lm(Tone ~ ., data = webWC[11:32])
summary(fitTone)
swTone = step(fitTone)
summary(swTone)

fitAna = lm(Analytic ~ ., data = webWC[11:32])
summary(fitAna)
swAna = step(fitAna)
summary(swAna)

fitClout = lm(Clout ~ ., data = webWC[11:32])
summary(fitClout)
swClout = step(fitClout)
summary(swClout)

fitAuth = lm(Authentic ~ ., data = webWC[11:32])
summary(fitAuth)
swAuth = step(fitAuth)
summary(swAuth)

##Create a new "Month" column because we would like to aggregate our data at months level
#Basically we would find the mean of the values of the 4 summary language variables of all post within a month
webWC$Month = as.Date(cut(webWC$Date, breaks = "month"))

byThread = aggregate(cbind(webWC$Analytic, webWC$Clout, webWC$Authentic, webWC$Tone), by=list(webWC$ThreadID, webWC$Month), FUN=mean)
colnames(byThread) = c("ThreadID", "Month","Analytic","Clout","Authentic","Tone")

#then we would like to calculate the freq of thread that is active for most number of month.
ThreadSize = as.data.frame(table(byThread$ThreadID))
ThreadSize = ThreadSize[order(-ThreadSize$Freq),]
ThreadSize[which.max(ThreadSize[,2]),]      # this would give us the thread that has been active for the most number of months

### Ploting the time series of the change in language for a particular thread.
## Figure 2.5 is all plotted using this part of R code.
# we just have to manually change the Thread ID and the "start" vector values then we can analyse time series of different threads
plotData = byThread[byThread$ThreadID == 127115,]

full = as.data.frame(seq(from = min(plotData$Month), to = max(plotData$Month), by='1 month'))
colnames(full) = "Months"

full$Analytic = plotData$Analytic[match(full$Months, plotData$Month)]
full$Clout = plotData$Clout[match(full$Months, plotData$Month)]
full$Authentic = plotData$Authentic[match(full$Months, plotData$Month)]
full$Tone = plotData$Tone[match(full$Months, plotData$Month)]

new = ts(full, frequency = 12, start = c(2004,4))
plot(new[,2:5])

#####
##Please remember to detach the 'igraph' library before runnign the decompose function using "detach("package:igraph", unload=TRUE)" command
#Because in the igraph package there is a function with the same name.
#Figure 2.6
# the function used from the zoo package is further explained in the report at page 8

library(zoo)
ana = ts(full$Analytic, frequency = 12, start = c(2004,4))
decomAna = decompose(na.StructTS(ana))
plot(decomAna)
cl = ts(full$Clout, frequency = 12, start = c(2004,4))
decomcl = decompose(na.StructTS(cl))
plot(decomcl)
tn = ts(full$Tone, frequency = 12, start = c(2004,4))
decomTon = decompose(na.StructTS(tn))
plot(decomTon)


####### the thickness of the graph is how frequent the author post on that thread.
#plot network graph of thread with author (Figure 2.3 and 2.4)
library(igraph)
library(igraphdata)

fullVec = webWC[webWC$ThreadID == 127115 & webWC$AuthorID!=-1, 2:3]
thickness = as.data.frame(table(fullVec$AuthorID))

v = rep(127115, length(thickness$Freq))

nodes =cbind.data.frame(thickness,ThreadID=v)
#removing authors that only post one time (figure 2.4)
nodes = nodes[nodes$Freq>=2,]

# to nodes
Tnodes = as.character(nodes[nodes$Freq>=2, 1])
# from nodes
Fnodes = as.character(nodes[nodes$Freq>=2, 3])

graphdata = data.frame(from = Fnodes, to = Tnodes)
g = graph.data.frame(graphdata, directed = TRUE)
E(g)$width = nodes$Freq/5
E(g)$arrow.size = .2
plot(g, vertex.shape="none",layout = layout.circle)

##sort highest to lowest frequency
#to see which author contributed the most in that particular thread.
sorted = nodes[order(-nodes$Freq),]

#then we take the top n authors who contributed the most post in the thread
#figure 2.7, 2.8 ,2.9 and 2.10 is produce using this part of the code
#just change the index value for sortFirst[1:x,] and change the AuthorID
first = webWC[webWC$AuthorID == 8912,]
byfirst = aggregate(cbind(first$Analytic, first$Clout, first$Authentic, first$Tone), by=list(first$ThreadID, first$Month), FUN=mean)
colnames(byfirst) = c("ThreadID", "Date","Analytic","Clout","Authentic","Tone")

frequent = as.data.frame(table(byfirst$ThreadID))
colnames(frequent) = c("ThreadID","freq")

byf = merge(byfirst, frequent, by = "ThreadID")

sortFirst = byf[order(-byf$freq), ]
sortFirst$ThreadID = as.factor(sortFirst$ThreadID)
row.names(sortFirst) = 1:nrow(sortFirst)

#data row 1 to 78 is the top 3 most active author in the thread
#data row 1 to 101 is the top 5........
pp = sortFirst[,]

# "melt" data with the melt() function so that each row is a unique id-variable combination.
#eg:
#mydata
'id	time	x1	x2
  1	 1	  5	  6
  1	 2	  3	  5
  2	 1	  6	  1
  2	 2	  2	  4'

#melt(mydata, id=c("id","time"))

'id	time	variable	value
  1	 1	     x1	      5
  1	 2	     x1	      3
  2	 1	     x1	      6
  2	 2	     x1	      2
  1	 1	     x2	      6
  1	 2	     x2	      5
  2	 1	     x2	      1
  2	 2	     x2	      4'

# this is to transform our data so that we can plot 4 plots together as shown in the figures
pdata = melt(pp, id=c("ThreadID","Date","freq"))

ggplot(pdata,aes(x = Date,y=value,colour=ThreadID,group=ThreadID)) + geom_line(size = 1.5) + geom_point() + ggtitle('AuthorID: 8912') + facet_wrap(~variable, nrow = 4)

ggplot(pdata, aes(factor(ThreadID), value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")
########################################################################
###Ploting one thread one author
##we will be manually changing the AuthorID and rerun the whole chunk of code to get the graph desire
#remember to change the start varibale of the time serires as well
#Figure 2.11 to 2.15 is produced using this part of R code
compa = webWC[webWC$ThreadID == 472752 & webWC$AuthorID == 166362,]
bycompa = aggregate(cbind(compa$Analytic, compa$Clout, compa$Authentic, compa$Tone), by=list(compa$AuthorID, compa$Month), FUN=mean)
colnames(bycompa) = c("AuthorID", "Date","Analytic","Clout","Authentic","Tone")

test = as.data.frame(seq(from = min(bycompa$Date), to = max(bycompa$Date), by='1 month'))
colnames(test) = "Months"

test$Analytic = bycompa$Analytic[match(test$Months, bycompa$Date)]
test$Clout = bycompa$Clout[match(test$Months, bycompa$Date)]
test$Authentic = bycompa$Authentic[match(test$Months, bycompa$Date)]
test$Tone = bycompa$Tone[match(test$Months, bycompa$Date)]

testTS = ts(test, frequency = 12, start = c(2008,11))
plot(testTS[,2:5])

# only plot the time zone whereby the author is active in that thread
timeC = full[full$Months %in% test$Months,]
timeCTS = ts(timeC, frequency = 12, start = c(2008,11))
plot(timeCTS[,2:5])

######################################################



