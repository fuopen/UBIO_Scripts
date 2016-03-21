# Consensus clustering analysis on the patients
# Distance: Pearson's correlation
# Metrics: PAM and Ward
 
# 1: install R package and load libraries
source("http://bioconductor.org/biocLite.R")
biocLite("ConsensusClusterPlus")
library(ConsensusClusterPlus)
library(mclust)
library(cluster)
library(sparcl)
library(clv)

# Load your dataset, example with blood transcriptomics
# Program expects features as rows and replicates (patients) as columns
load("BloodExprs.Rdata")

# 2: Ward clustering
dt<-as.dist(1-cor(BloodExp, method="pearson"))
hc<-hclust(dt, "ward.D2")
plot(hc)
pdf("ConsensusClusterWard.pdf")
results<-ConsensusClusterPlus(dt, maxK=10, reps=1000, distance="pearson", clusterAlg="hc", innerLinkage="ward.D2", pItem=0.85)
dev.off()
# maxK= the maximum number of clusters to be tried
# reps= the number of bootstraps reps
# pItem= the proportion of patients to be kept between bootstraps

# Pick the most stable group out of the consensus cluster results; ex: 4
groupWARD<-results[[4]]$consensusClass
ColorDendrogram(hc, groupWARD, main="Consensus clustering in full dendrogram", branchlength=0.05)

# 3: PAM clustering

pdf("ConsensusClusterPAM.pdf")
results<-ConsensusClusterPlus(dt, maxK=10, reps=1000, distance="pearson", clusterAlg="pam", pItem=0.85)
dev.off()

# k is the number of stable clusters found in consensus clustering; ex: 5
k<- 5
pam<-pam(dt, k, diss=TRUE)
groupPAM<-pam$clustering

# Comparison of clustering schemes
# Using Adjusted Rand index
Adjrand<-adjustedRandIndex(groupWARD, groupPAM)
# This gives a value between 0 (complete disagreement) and 1 (complete agreement)
# Using Rand index
std<-std.ext(groupWARD, groupPAM)
Rand<-clv.Rand(std)
# Jaccard Index
Jaccard<-clv.Jaccard(std)

# Rand index considers the number of pairs in the same cluster (N11) and the pairs of objects never in the same cluster (N00) divided by all comparisons
# Adjusted Rand index considers the same but takes into account the proportion of correct attribution in clustes by random divided by all comparison
# Jaccard index considers only pairs in the same cluster in both clustering schemes divided by all comparisons 
# See help of clv package for formula: http://artax.karlin.mff.cuni.cz/r-help/library/clv/html/standard_external_measures.html
# Jaccard index is more stringent than Rand, and Adjusted Rand is in the middle