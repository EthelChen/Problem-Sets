## actors network example

library(igraph)

### GRAPH
## read in a graph in the `graphml' formal: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)
actnet <- read.graph("actors.graphml",format="graphml")


### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.
movies <- read.table("movies.txt", sep="\t", 
	row.names=1, as.is=TRUE, comment.char="", quote="")
## it's a 1 column matrix.  treat it like a vector
movies <- drop(as.matrix(movies))
## each element is a comma-separated set of actor ids.  
## use `strsplit' to break these out
movies <- strsplit(movies,",")
## and finally, match ids to names from actnet
casts <- lapply(movies, 
	function(m) V(actnet)$name[match(m,V(actnet)$id)])
## check it
casts['True Romance']
## format as arules transaction baskets
library(arules)
casttrans <- as(casts, "transactions")


## Set up STM information
castsize <- unlist(lapply(casts, function(m) length(m)))
## see ?rep.int: we're just repeating movie names for each cast member
acti <- factor(rep.int(names(casts),times=castsize))
## actors
actj <- factor(unlist(casts), levels=V(actnet)$name)
## format as STM (if you specify without `x', its binary 0/1)
actmat <- sparseMatrix(i=as.numeric(acti),j=as.numeric(actj),
		dimnames=list(movie=levels(acti),actor=levels(actj)))

## count the number of appearences by actor
nroles <- colSums(actmat)
names(nroles) <- colnames(actmat)


## 1
plot(actnet, vertex.label=NA, vertex.size=3, edge.curved=FALSE)

## 2
which(V(actnet)$name=="Bacon, Kevin")

BaconKevin <- graph.neighborhood(actnet, 1, V(actnet)["Bacon, Kevin"])[[1]]
V(BaconKevin)$color <- "gold"
V(BaconKevin)$vertex.size=10
V(BaconKevin)["Bacon, Kevin"]$color <- "red"
plot(BaconKevin, vertex.label=NA, vertex.frame.color=0, edge.arrow.width=.75,edge.curved=FALSE)

BaconKevin2 <- graph.neighborhood(actnet, 2, V(actnet)["Bacon, Kevin"])[[1]]
V(BaconKevin2)$color <- "green"
V(BaconKevin2)[V(BaconKevin)$name]$color <- "gold"
V(BaconKevin2)["Bacon, Kevin"]$color <- "red"
plot(BaconKevin2, vertex.label=NA, vertex.frame.color=0, edge.arrow.width=.75,edge.curved=FALSE)

BaconKevin3 <- graph.neighborhood(actnet, 3, V(actnet)["Bacon, Kevin"])[[1]]
V(BaconKevin3)$color <- "gold"
V(BaconKevin3)["Bacon, Kevin"]$color <- "red"
plot(BaconKevin3, vertex.label=NA, vertex.frame.color=0, edge.arrow.width=.75,edge.curved=FALSE)

#3
## common actors & connected actors
abetween <- betweenness(actnet)
adegree <- degree(actnet)
which.max(abetween)
which.max(adegree)
plot(adegree,abetween,log="xy")
V(actnet)$name[order(betweenness(actnet), decreasing=TRUE)[1:10]]
V(actnet)$name[order(degree(actnet), decreasing=TRUE)[1:10]]

#4
ARules <- apriori(casttrans, parameter=list(support=.001, confidence=.1))
inspect(ARules)