#####################################
#MIT License

#Copyright (c) 2016 Bradley T. Rentz

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#  
#  The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.
#####################################
## Author: Bradley T. Rentz
## This code was last modified on 23 October 2016
## Base R Version: 3.3.1
## OS: Mac OS 10.10.5
## Evtree version 1.0-1
## Patrykit version 1.1-1
## Rpart version 4.1-10
## Code edited in R Studio version 0.99.892
## Description: This code runs 3 tree classification models on topological relations data for the Pohnpeian language 
## based on elicitation sessions from five L1 Pohnpeian speakers using the BowPed Topological Relations Toolkit
## The classification trees include classification tree (rpart), conditional inference classification tree (partykit),
## and evolution tree (evtree)
## Author contact: bradley.rentz@gmail.com
####################################

library(rpart)
library(partykit)
library(tree)
library(rpart.plot)
library(evtree)
library(randomForest)

## load csv file
data <- read.csv("topological_relations_coded_data.csv")

## set variables to factors
data$contact <- as.factor(data$contact)
data$horizontal.ground <- as.factor(data$horizontal.ground)
data$X3d.ground <- as.factor(data$X3d.ground)
data$top.of.ground <- as.factor(data$top.of.ground)
data$containment <- as.factor(data$containment)
data$X3d.fig <- as.factor(data$X3d.fig)
data$go.around.ground <- as.factor(data$go.around.ground)
data$go.around.fig <- as.factor(data$go.around.fig)
data$ground.vertical <- as.factor(data$ground.vertical)
data$fig.attached.to.ground <- as.factor(data$fig.attached.to.ground)
data$ground.larger.2D.fig <- as.factor(data$ground.larger.2D.fig)
data$ground.building <- as.factor(data$ground.building)
data$fig.side.ground <- as.factor(data$fig.side.ground)
data$fig.far.ground <- as.factor(data$fig.far.ground)

### Classification trees
### 1. Rpart
# make basic classification tree, exclude image, speaker, and ground 
topo.rp = as.party(rpart(word ~ ., data=data[ ,-c(1,3, 13)],control=rpart.control(minsplit=20))) ## change minsplit to desired minimum split group size

plot(topo.rp, compress=T, branch=1, margin=0.7)

### 2. ctree
###conditional inference version
topo.party = ctree(word ~ ., data=data[ ,-c(1,3, 13)])
plot(topo.party,drop_terminal=0) # 15in x 50in save as
plot(topo.party,type="simple")


#### 3. Evolution Tree (evtree package)
### evtree ### note: each run of the evtree may produce slightly different result because of stochastic nature of algorithm so set seed
set.seed(146235)
tree.evtree = evtree(word ~  contact + horizontal.ground + X3d.ground+ animacy.fig   + top.of.ground + containment + X3d.fig + go.around.ground + go.around.fig + ground.vertical + fig.attached.to.ground + ground.larger.2D.fig + ground.building + fig.side.ground+fig.far.ground, data=data)

plot(tree.evtree) # plot tree

plot(tree.evtree, gp = gpar(fontsize = 6))   ## change font size


## compare error rates and evaluation function of evtree, CART (rpart), and Condition Inference Tree)
mc <- function(obj) 1 - mean(predict(obj) == data$word)
evalfun <- function(obj) 2 * nrow(data) * mc(obj) +
  width(obj) * log(nrow(data))
trees <- list("evtree" = tree.evtree, "rpart" = topo.rp, "ctree" = topo.party)
round(sapply(trees, function(obj) c("misclassification" = mc(obj),
                                    "evaluation function" = evalfun(obj))), digits = 3)
### to evaluate models, misclassification says which percentage of current data is misclassified by model.
### the evaluation function (2 * number of rows * misclassification rate + the number of terminal nodes * log(number of rows))
### describes the misclassfication rate as a function of tree complexity (the more complex the tree, the higher the evaluation function value)
### The evaluation function aims at describing generalizability vs. overfitting
### A good tree would have low values for both measures. If tree has low misclassification error and high evaluation function it may be overfitted to the data












