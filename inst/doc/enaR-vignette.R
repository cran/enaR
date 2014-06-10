### R code from vignette source 'enaR-vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: enaR-vignette.Rnw:120-122 (eval = FALSE)
###################################################
## citation('enaR')
## 


###################################################
### code chunk number 2: z
###################################################
# set plotting parameters
opar <- par(las=1,mar=c(0,0,0,0),xpd=TRUE,bg="white")   

## Taken from https://stat.ethz.ch/pipermail/r-devel/2011-September/062126.html
if (all(ls()!='f.list')){
  require(codetools)
library(enaR)
called.by <- function(tarFunc, tarPack){
  flist <-   sapply(lsf.str(tarPack, all=TRUE), c)
  names(flist) <- NULL
  gotit <- sapply(flist, function(x) tarFunc %in% findGlobals(get(x, tarPack),FALSE)$functions)
  flist[gotit]
}

f.list <- as.character(sapply(lsf.str('package:enaR',all=TRUE),c))
f.array <- array(0,dim=rep(length(f.list),2))
rownames(f.array) <- colnames(f.array) <- f.list
for (i in 1:length(f.list)){
  f.array[match(called.by(f.list[i],'package:enaR'),rownames(f.array)),i] <- 1
}
f.net <- network(t(f.array))
}

plot(f.net,displaylabels=TRUE,label.cex=0.85,arrowhead.cex=0.65,
     edge.lwd=0.75,vertex.col='lightblue',vertex.border='white',edge.col='darkgrey')


###################################################
### code chunk number 3: enaR-vignette.Rnw:156-157
###################################################
getOption("SweaveHooks")[["fig"]]()
# set plotting parameters
opar <- par(las=1,mar=c(0,0,0,0),xpd=TRUE,bg="white")   

## Taken from https://stat.ethz.ch/pipermail/r-devel/2011-September/062126.html
if (all(ls()!='f.list')){
  require(codetools)
library(enaR)
called.by <- function(tarFunc, tarPack){
  flist <-   sapply(lsf.str(tarPack, all=TRUE), c)
  names(flist) <- NULL
  gotit <- sapply(flist, function(x) tarFunc %in% findGlobals(get(x, tarPack),FALSE)$functions)
  flist[gotit]
}

f.list <- as.character(sapply(lsf.str('package:enaR',all=TRUE),c))
f.array <- array(0,dim=rep(length(f.list),2))
rownames(f.array) <- colnames(f.array) <- f.list
for (i in 1:length(f.list)){
  f.array[match(called.by(f.list[i],'package:enaR'),rownames(f.array)),i] <- 1
}
f.net <- network(t(f.array))
}

plot(f.net,displaylabels=TRUE,label.cex=0.85,arrowhead.cex=0.65,
     edge.lwd=0.75,vertex.col='lightblue',vertex.border='white',edge.col='darkgrey')


###################################################
### code chunk number 4: enaR-vignette.Rnw:187-193
###################################################
  rm(list=ls())
par(mfrow=c(1,1))
options(width=72)
figset <- function() par(mar=c(4,4,1,1)+.1)
options(SweaveHooks = list(fig = figset))
options("prompt" = "> ", "continue" = "+  ")


###################################################
### code chunk number 5: enaR-vignette.Rnw:196-197
###################################################
library(enaR)


###################################################
### code chunk number 6: enaR-vignette.Rnw:286-302
###################################################
# generate the flow matrix
flow.mat <- array(abs(rnorm(100,4,2))*sample(c(0,1),100,replace=TRUE),
                  dim=c(4,4))
# name the nodes
rownames(flow.mat) <- colnames(flow.mat) <- paste('node',(1:nrow(flow.mat)),sep='')
# generate the inputs
inputs <- runif(nrow(flow.mat),0,4)
# generate the exports
exports <- inputs
# pack
fake.model <- pack(flow=flow.mat,
                   input=inputs,
                   export=exports,
                   living=TRUE)
# model
fake.model


###################################################
### code chunk number 7: enaR-vignette.Rnw:308-309
###################################################
attributes(fake.model)


###################################################
### code chunk number 8: enaR-vignette.Rnw:316-321
###################################################
fake.model%v%'output'

fake.model%v%'input'

fake.model%v%'living'


###################################################
### code chunk number 9: enaR-vignette.Rnw:326-327
###################################################
fake.model%n%'flow'


###################################################
### code chunk number 10: enaR-vignette.Rnw:335-336
###################################################
unpack(fake.model)  


###################################################
### code chunk number 11: enaR-vignette.Rnw:354-362
###################################################
## --- Check to see if the model is balanced ---#
ssCheck(fake.model)

## --- To BALANCE a model if needed --- #
fake.model <- balance(fake.model,method="AVG2")  

## --- To FORCE BALANCE a model if needed --- #
fake.model <- force.balance(fake.model)


###################################################
### code chunk number 12: enaR-vignette.Rnw:382-384
###################################################
scor.model <- readLines('http://people.uncw.edu/borretts/data/oyster.dat')
m <- read.scor(scor.model,from.file=FALSE)


###################################################
### code chunk number 13: enaR-vignette.Rnw:391-392
###################################################
unpack(m)


###################################################
### code chunk number 14: enaR-vignette.Rnw:397-399
###################################################
data(oyster)
m <- oyster


###################################################
### code chunk number 15: enaR-vignette.Rnw:412-413 (eval = FALSE)
###################################################
##   m <- read.wand('./MDmar02_WAND.xls')


###################################################
### code chunk number 16: enaR-vignette.Rnw:439-440 (eval = FALSE)
###################################################
##   m <- read.enam('./MDMAR02.xlsx')


###################################################
### code chunk number 17: enaR-vignette.Rnw:472-480 (eval = FALSE)
###################################################
## data(oyster)
## # write oyster reef model to a csv file
## write.nea(oyster, file.name="oyster.csv")
## # read in oyster reef model data from NEA.m formatted CSV file
## m <- read.nea("oyster.csv")
## 
## # Again, this model object does NOT contain all 
## # of the information in the "oyster" data object.


###################################################
### code chunk number 18: a
###################################################
data(oyster)  # load data
m <- oyster  
set.seed(2)    # set random seed to control plot
plot(m)       # plot network data object (uses plot.network)


###################################################
### code chunk number 19: b
###################################################
# set colors to use
my.col=c("red","yellow", 
  rgb(204,204,153,maxColorValue=255), 
  "grey22")  
F=m%n%'flow'                   # extract flow information for later use.
f=which(F!=0, arr.ind=T)       # get indices of positive flows
opar <- par(las=1,bg=my.col[4],xpd=TRUE,mai=c(1.02, 0.62, 0.82, 0.42))
set.seed(2)                    # each time the plot is called, the
                               # layout orientation changes.  setting
                               # the seed ensures a consistent
                               # orientation each time the plot
                               # function is called.
plot(m,
     vertex.cex=log(m%v%'storage'), # scale nodes with storage
     label= m%v%'vertex.names',     # add node labels
     boxed.labels=FALSE,
     label.cex=0.65,
     vertex.sides=45,   # to make rounded
     edge.lwd=log10(abs(F[f])),     # scale arrows to flow magnitude
     edge.col=my.col[3],
     vertex.col=my.col[1],
     label.col="white",
     vertex.border = my.col[3],
     vertex.lty = 1,
     xlim=c(-4,1),ylim=c(-2,-2))
rm(opar)             # remove changes to the plotting parameters


###################################################
### code chunk number 20: enaR-vignette.Rnw:538-539
###################################################
getOption("SweaveHooks")[["fig"]]()
data(oyster)  # load data
m <- oyster  
set.seed(2)    # set random seed to control plot
plot(m)       # plot network data object (uses plot.network)


###################################################
### code chunk number 21: enaR-vignette.Rnw:541-542
###################################################
getOption("SweaveHooks")[["fig"]]()
# set colors to use
my.col=c("red","yellow", 
  rgb(204,204,153,maxColorValue=255), 
  "grey22")  
F=m%n%'flow'                   # extract flow information for later use.
f=which(F!=0, arr.ind=T)       # get indices of positive flows
opar <- par(las=1,bg=my.col[4],xpd=TRUE,mai=c(1.02, 0.62, 0.82, 0.42))
set.seed(2)                    # each time the plot is called, the
                               # layout orientation changes.  setting
                               # the seed ensures a consistent
                               # orientation each time the plot
                               # function is called.
plot(m,
     vertex.cex=log(m%v%'storage'), # scale nodes with storage
     label= m%v%'vertex.names',     # add node labels
     boxed.labels=FALSE,
     label.cex=0.65,
     vertex.sides=45,   # to make rounded
     edge.lwd=log10(abs(F[f])),     # scale arrows to flow magnitude
     edge.col=my.col[3],
     vertex.col=my.col[1],
     label.col="white",
     vertex.border = my.col[3],
     vertex.lty = 1,
     xlim=c(-4,1),ylim=c(-2,-2))
rm(opar)             # remove changes to the plotting parameters


###################################################
### code chunk number 22: enaR-vignette.Rnw:604-607
###################################################
St <- enaStructure(m)
attributes(St)
St$ns


###################################################
### code chunk number 23: enaR-vignette.Rnw:670-676
###################################################
  F <- enaFlow(m)
attributes(F)
F$ns
G <- F$G # output-oriented direct flow matrix
rm(G)
F$NP     # input-oriented integral flow matrix


###################################################
### code chunk number 24: enaR-vignette.Rnw:683-686
###################################################
attach(F)
G
detach(F) 


###################################################
### code chunk number 25: enaR-vignette.Rnw:693-694
###################################################
mExp(F$G,2)


###################################################
### code chunk number 26: enaR-vignette.Rnw:706-707
###################################################
  enaAscendency(oyster)


###################################################
### code chunk number 27: enaR-vignette.Rnw:737-740
###################################################
  S <- enaStorage(m)
attributes(S)
S$ns


###################################################
### code chunk number 28: enaR-vignette.Rnw:797-800
###################################################
UF <- enaUtility(m,eigen.check=TRUE,type="flow")  
US <- enaUtility(m,eigen.check=TRUE,type="storage")  
attributes(UF)


###################################################
### code chunk number 29: enaR-vignette.Rnw:847-850
###################################################
E <- enaEnviron(m)
attributes(E)
E$output[1] 


###################################################
### code chunk number 30: enaR-vignette.Rnw:858-860
###################################################
tet <- TET(m)
show(tet)


###################################################
### code chunk number 31: enaR-vignette.Rnw:867-869
###################################################
tes <- TES(m)
show(tes)


###################################################
### code chunk number 32: enaR-vignette.Rnw:877-879
###################################################
C <- enaControl(m)               
attributes(C)


###################################################
### code chunk number 33: enaR-vignette.Rnw:892-897
###################################################
                                        #conduct mixed trophic impacts
mti <- enaMTI(oyster)
attributes(mti)
                                        #shows the total impact matrix
mti$M


###################################################
### code chunk number 34: enaR-vignette.Rnw:906-909
###################################################
  mti <- enaMTI(oyster,eigen.check=FALSE)
attributes(mti)
mti$M  # shows the total impact matrix


###################################################
### code chunk number 35: enaR-vignette.Rnw:938-940
###################################################
ns <- get.ns(m)
str(ns)    # examine the structure of ns


###################################################
### code chunk number 36: enaR-vignette.Rnw:946-948
###################################################
oyster.ena <- enaAll(oyster)
names(oyster.ena)


###################################################
### code chunk number 37: enaR-vignette.Rnw:956-962
###################################################
F <- enaFlow(oyster)

ec <- environCentrality(F$N)
show(ec)

eigenCentrality(F$G)


###################################################
### code chunk number 38: b
###################################################
# set plotting parameters
opar <- par(las=1,mar=c(7,5,1,1),xpd=TRUE,bg="white")   
# find centrality order
o <- order(ec$AEC,decreasing=TRUE)            
bp <- barplot(ec$AEC[o],     # create barplot
              names.arg=NA,
              ylab="Average Environ Centrality",
              col="black",border=NA)
text(bp,-0.008,                # add labels
     labels=names(ec$AEC)[o],
     srt=35,adj=1,cex=1)
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 39: enaR-vignette.Rnw:988-989
###################################################
getOption("SweaveHooks")[["fig"]]()
# set plotting parameters
opar <- par(las=1,mar=c(7,5,1,1),xpd=TRUE,bg="white")   
# find centrality order
o <- order(ec$AEC,decreasing=TRUE)            
bp <- barplot(ec$AEC[o],     # create barplot
              names.arg=NA,
              ylab="Average Environ Centrality",
              col="black",border=NA)
text(bp,-0.008,                # add labels
     labels=names(ec$AEC)[o],
     srt=35,adj=1,cex=1)
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 40: enaR-vignette.Rnw:1010-1026
###################################################
###Check the current orientation
get.orient()
###enaFlow output in row-column
flow.rc <- enaFlow(oyster)$G
###Set the global orientation to school
set.orient('school')
###Check that it worked
get.orient()
###enaFlow output in column-row
flow.cr <- enaFlow(oyster)$G
###Check. Outputs should be transposed from each other.
all(flow.rc == flow.cr)
all(flow.rc == t(flow.cr))
###Now change back to the default orientation ('rc')
set.orient('rc')



###################################################
### code chunk number 41: enaR-vignette.Rnw:1064-1075
###################################################
### Import the model sets
data(bgcModels)
data(troModels)
### Check the first few model names
head(names(bgcModels))
head(names(troModels))
### Isolate a single model
x <- troModels[[1]]
x <- troModels$"Marine Coprophagy (oyster)"
### Check out the model
summary(x)


###################################################
### code chunk number 42: enaR-vignette.Rnw:1249-1250
###################################################
data(troModels)


###################################################
### code chunk number 43: enaR-vignette.Rnw:1259-1275
###################################################
# balance models as necessary
m.list <- lapply(troModels,balance)

# if balancing fails, you can use force.balance
# to repeatedly apply the balancing procedure
unlist(lapply(m.list,ssCheck))
m.list <- lapply(m.list,force.balance)
##Check that all the models are balanced
all(unlist(lapply(m.list,ssCheck)))

# Example Flow Analysis
F.list <- lapply(m.list, enaFlow)

# the full results of the flow analysis is now stored in the elements
# of the F.list.  To get the results for just the first model...
F.list[[1]]


###################################################
### code chunk number 44: enaR-vignette.Rnw:1281-1285
###################################################
# Example of extracting just specific information - Indirect Effects Ratio
IDs <- unlist(lapply(m.list, function(x) enaFlow(x)$ns[8]))
#Look at the first few ID's
head(IDs)


###################################################
### code chunk number 45: enaR-vignette.Rnw:1289-1291
###################################################
# Here is a list containing only the output-oriented integral flow matrices
N.list <- lapply(m.list,function(x) enaFlow(x)$N)


###################################################
### code chunk number 46: enaR-vignette.Rnw:1298-1305
###################################################
# Collecting and combining all network statistics
ns.list <- lapply(m.list,get.ns) # returns as list
ns <- do.call(rbind,ns.list)  # ns as a data.frame
# Let's take a quick look at some of the output
colnames(ns)    # return network statistic names.  
dim(ns)         # show dimensions of ns matrix
ns[1:5,1:5]     # show selected results


###################################################
### code chunk number 47: b
###################################################
opar <- par(las=1,mar=c(9,7,2,1),xpd=TRUE,mfrow=c(1,2),oma=c(1,1,0,0))
x=dim(ns)[1] # number of models
m.select <- 40:45
bp=barplot(ns$ID.F[m.select],ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
        col="darkgreen",border=NA,ylim=c(0,2))
text(bp,-0.05,                # add labels
     labels=rownames(ns)[m.select],
       srt=45,adj=1,cex=0.85)
opar <- par(xpd=FALSE)
abline(h=1,col="orange",lwd=2)
#
plot(ns$FCI,ns$ID.F,pch=20,col="blue",cex=2,
     ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
     xlab="Finn Cycling Index (FCI)",
     xlim=c(0,0.8),ylim=c(0,8))
#
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 48: enaR-vignette.Rnw:1336-1337
###################################################
getOption("SweaveHooks")[["fig"]]()
opar <- par(las=1,mar=c(9,7,2,1),xpd=TRUE,mfrow=c(1,2),oma=c(1,1,0,0))
x=dim(ns)[1] # number of models
m.select <- 40:45
bp=barplot(ns$ID.F[m.select],ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
        col="darkgreen",border=NA,ylim=c(0,2))
text(bp,-0.05,                # add labels
     labels=rownames(ns)[m.select],
       srt=45,adj=1,cex=0.85)
opar <- par(xpd=FALSE)
abline(h=1,col="orange",lwd=2)
#
plot(ns$FCI,ns$ID.F,pch=20,col="blue",cex=2,
     ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
     xlab="Finn Cycling Index (FCI)",
     xlim=c(0,0.8),ylim=c(0,8))
#
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 49: enaR-vignette.Rnw:1363-1366
###################################################
betweenness(oyster)

closeness(oyster)


###################################################
### code chunk number 50: d
###################################################
m <- troModels[[38]]
b <- betweenness(m)         # calculate betweenness centrality
nms <- m%v%'vertex.names'   # get vertex names
show(nms)
nms[b<=(0.1*max(b))] <- NA  # exclude less central nodes

set.seed(3)
opar <- par(xpd=TRUE,mfrow=c(1,1))
# create target plot
gplot.target(m,b,#circ.lab=FALSE,
             edge.col="grey",
             label=nms) # show only labels of most central nodes
             #xlim=c(-1,4))
rm(opar)


###################################################
### code chunk number 51: enaR-vignette.Rnw:1394-1395
###################################################
getOption("SweaveHooks")[["fig"]]()
m <- troModels[[38]]
b <- betweenness(m)         # calculate betweenness centrality
nms <- m%v%'vertex.names'   # get vertex names
show(nms)
nms[b<=(0.1*max(b))] <- NA  # exclude less central nodes

set.seed(3)
opar <- par(xpd=TRUE,mfrow=c(1,1))
# create target plot
gplot.target(m,b,#circ.lab=FALSE,
             edge.col="grey",
             label=nms) # show only labels of most central nodes
             #xlim=c(-1,4))
rm(opar)


###################################################
### code chunk number 52: enaR-vignette.Rnw:1405-1410
###################################################
centralization(oyster, degree)

centralization(oyster,closeness)

centralization(oyster,betweenness)


###################################################
### code chunk number 53: e
###################################################
library(igraph)
### The adjacency matrix
A <- St$A

### creating an iGraph graph
g <- graph.adjacency(A)
plot(g)  # uses iGraph plot tools


###################################################
### code chunk number 54: enaR-vignette.Rnw:1436-1437
###################################################
getOption("SweaveHooks")[["fig"]]()
library(igraph)
### The adjacency matrix
A <- St$A

### creating an iGraph graph
g <- graph.adjacency(A)
plot(g)  # uses iGraph plot tools


###################################################
### code chunk number 55: enaR-vignette.Rnw:1442-1462
###################################################
# betweenness centrality (calculated by iGraph and sna)
betweenness(g) 

# shortest path between any two nodes
shortest.paths(g) 

# average path length in the network (graph theory sense)
average.path.length(g,directed=TRUE)   

diameter(g)  # diameter of the graph

vertex.connectivity(g)  # connectivity of a graph (group cohesion)
subcomponent(g,1,'in')  # subcomponent reachable from 1 along inputs
subcomponent(g,2,'in')  # subcomponent reachable from 2 along inputs
subcomponent(g,1,'out') # subcomponent reachable from 1 along outputs
subcomponent(g,2,'out') # subcomponent reachable from 2 along output

edge.connectivity(g)

detach(package:igraph)  # detach igraph package


