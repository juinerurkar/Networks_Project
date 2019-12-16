library("network")  # tested with version 1.10.2
library("sna")      # tested with version 2.3.2
library("ergm")     # tested with version 3.1-0
library("tergm")    # tested with version 3.1-0
library("texreg")   # tested with version 1.33.2
library("xergm")    # tested with version 1.1.3


# ==============================================================================
# Climate policy domain, time step 1, cross-sectional model
# ==============================================================================

# data preparation
setwd("jpart-infrep-ingold-leifeld/")

clim1.rep <- read.csv2("input/climate9500-rep.csv", row.names = 1, 
    header = TRUE)
clim1.rep <- network(as.matrix(clim1.rep))  # influence reputation network
clim1.collab <- read.csv2("input/climate9500-collab.csv", row.names = 1, 
    header = TRUE)
clim1.collab <- network(as.matrix(clim1.collab))  # collaboration network
set.vertex.attribute(clim1.rep, "bc-perc", 100 * betweenness(clim1.collab, 
    rescale = TRUE))  # betweenness centrality as a vertex attribute
clim1.dm <- read.csv2("input/climate9500-dm.csv", row.names = 1, header = TRUE)
set.vertex.attribute(clim1.rep, "dm", clim1.dm$dm)  # decision-maker?
set.vertex.attribute(clim1.rep, "odeg-rep", degree(clim1.rep, 
    cmode = "outdegree"))  # outdegree centrality as a vertex attribute
com1 <- read.csv2("input/climate9500-committee.csv", row.names = 1, 
    header = TRUE)
set.vertex.attribute(clim1.rep, "committees", rowSums(com1))
    # number of committee memberships as a vertex attribute

# save summary table for the appendix
clim1.names <- get.vertex.attribute(clim1.rep, "vertex.names")
clim1.bc <- 100 * betweenness(clim1.collab, rescale = TRUE)
clim1.committee <- rowSums(com1)
clim1.type <- read.csv2("input/climate9500-type.csv", row.names = 1, 
    header = TRUE)
clim1.appendix <- data.frame(org = clim1.names, type3 = clim1.type[, 1], 
    type5 = clim1.type[, 2], committees = clim1.committee, dm = clim1.dm$dm, 
    betweenness = clim1.bc)
write.csv(clim1.appendix, file = "output/appendix-clim1.csv", row.names = FALSE)

# ERGM
set.seed(12345)
climate1 <- ergm(clim1.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim1.collab) + 
    nodeicov("bc-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 1)
summary(climate1)


# ==============================================================================
# Climate policy domain, time step 2, cross-sectional model
# ==============================================================================

# data preparation
clim2.rep <- read.csv2("input/climate0205-rep.csv", row.names = 1, 
    header = TRUE)
clim2.rep <- network(as.matrix(clim2.rep))  # influence reputation network
clim2.collab <- read.csv2("input/climate0205-collab.csv", row.names = 1, 
    header = TRUE)
clim2.collab <- network(as.matrix(clim2.collab))  # collaboration network
set.vertex.attribute(clim2.rep, "bc-perc-2", 100 * betweenness(clim2.collab, 
    rescale = TRUE))  # betweenness centrality as a vertex attribute
clim2.dm <- read.csv2("input/climate0205-dm.csv", row.names = 1, header = TRUE)
set.vertex.attribute(clim2.rep, "dm", clim2.dm$dm)  # decision-maker?
set.vertex.attribute(clim2.rep, "odeg-rep", degree(clim2.rep, 
    cmode = "outdegree"))  # outdegree centrality as a vertex attribute
com2 <- read.csv2("input/climate0205-committee.csv", row.names = 1, 
    header = TRUE)
set.vertex.attribute(clim2.rep, "committees-2", rowSums(com2))
    # number of committee memberships as a vertex attribute

# save summary table for the appendix
clim2.names <- get.vertex.attribute(clim2.rep, "vertex.names")
clim2.bc <- 100 * betweenness(clim2.collab, rescale = TRUE)
clim2.committee <- rowSums(com2)
clim2.type <- read.csv2("input/climate0205-type.csv", row.names = 1, 
    header = TRUE)
clim2.appendix <- data.frame(org = clim2.names, type3 = clim2.type[, 1], 
    type5 = clim2.type[, 2], committees = clim2.committee, dm = clim2.dm$dm, 
    betweenness = clim2.bc)
write.csv(clim2.appendix, file = "output/appendix-clim2.csv", 
    row.names = FALSE)

# ERGM
set.seed(12345)
climate2 <- ergm(clim2.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim2.collab) + 
    nodeicov("bc-perc-2") + 
    nodeifactor("dm") + 
    nodeicov("committees-2"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 2)
summary(climate2)


# ==============================================================================
# Telecommunications policy domain
# ==============================================================================

# data preparation
telecom.rep <- read.csv2("input/telecom-rep.csv", row.names = 1, header = TRUE)
telecom.rep <- network(as.matrix(telecom.rep))  # influence reputation network
telecom.collab <- read.csv2("input/telecom-collab.csv", row.names = 1, 
    header = TRUE)
for (i in 1:nrow(telecom.collab)) {
  for (j in 1:ncol(telecom.collab)) {
    if (is.na(telecom.collab[i, j])) {
      telecom.collab[i, j] <- telecom.collab[j, i]
    }
    if (is.na(telecom.collab[i, j])) {  # i.e., both ij and ji are missing
      telecom.collab[i, j] <- 0
    }
  }
}
telecom.collab <- network(telecom.collab)  # collaboration network
set.vertex.attribute(telecom.rep, "bc-perc", 100 * betweenness(telecom.collab, 
    rescale = TRUE))  # betweenness centrality as a vertex attribute
telecom.dm <- read.csv2("input/telecom-dm.csv", row.names = 1, header = TRUE)
set.vertex.attribute(telecom.rep, "dm", telecom.dm$dm)  # decision-maker?
set.vertex.attribute(telecom.rep, "odeg-rep", degree(telecom.rep, 
    cmode = "outdegree"))  # outdegree centrality as a vertex attribute
telecom.committee <- rowSums(read.csv2("input/telecom-committee.csv", 
    row.names = 1, header = TRUE))
set.vertex.attribute(telecom.rep, "committees", telecom.committee)
    # number of committee memberships as a vertex attribute

# save summary table for the appendix
telecom.names <- get.vertex.attribute(telecom.rep, "vertex.names")
telecom.bc <- 100 * betweenness(telecom.collab, rescale = TRUE)
telecom.type <- read.csv2("input/telecom-type.csv", row.names = 1, 
    header = TRUE)
telecom.appendix <- data.frame(org = telecom.names, type3 = telecom.type[, 1], 
    type5 = telecom.type[, 2], committees = telecom.committee, 
    dm = telecom.dm$dm, betweenness = telecom.bc)
write.csv(telecom.appendix, file = "output/appendix-telecom.csv", 
    row.names = FALSE)

# ERGM
set.seed(12345)
telecom <- ergm(telecom.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(telecom.collab) + 
    nodeicov("bc-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 3)
summary(telecom)


# ==============================================================================
# PM Visp
# ==============================================================================

# data preparation
pmvisp.rep <- read.csv2("input/pmvisp-rep.csv", row.names = 1, header = TRUE)
pmvisp.rep <- network(as.matrix(pmvisp.rep))  # influence reputation network
pmvisp.collab <- read.csv2("input/pmvisp-collab.csv", row.names = 1, 
    header = TRUE)
for (i in 1:nrow(pmvisp.collab)) {
  for (j in 1:ncol(pmvisp.collab)) {
    if (is.na(pmvisp.collab[i, j])) {
      pmvisp.collab[i, j] <- pmvisp.collab[j, i]
    }
    if (is.na(pmvisp.collab[i, j])) {
      pmvisp.collab[i, j] <- 0
    }
  }
}
pmvisp.collab <- network(pmvisp.collab)  # collaboration network
set.vertex.attribute(pmvisp.rep, "bc-perc", 100 * betweenness(pmvisp.collab, 
    rescale = TRUE))  # betweenness centrality as a vertex attribute
pmvisp.dm <- read.csv2("input/pmvisp-dm.csv", row.names = 1, header = TRUE)
set.vertex.attribute(pmvisp.rep, "dm", pmvisp.dm$dm)  # decision-maker?
set.vertex.attribute(pmvisp.rep, "odeg-rep", degree(pmvisp.rep, 
    cmode = "outdegree"))  # outdegree centrality as a vertex attribute
pmvisp.committee <- rowSums(read.csv2("input/pmvisp-committee.csv", 
    row.names = 1, header = TRUE))
set.vertex.attribute(pmvisp.rep, "committees", pmvisp.committee)
    # number of committee memberships as a vertex attribute

# save summary table for the appendix
pmvisp.names <- get.vertex.attribute(pmvisp.rep, "vertex.names")
pmvisp.bc <- 100 * betweenness(pmvisp.collab, rescale = TRUE)
pmvisp.type <- read.csv2("input/pmvisp-type.csv", row.names = 1, header = TRUE)
pmvisp.appendix <- data.frame(org = pmvisp.names, type3 = pmvisp.type[, 1], 
    type5 = pmvisp.type[, 2], committees = pmvisp.committee, 
    dm = pmvisp.dm$dm, betweenness = pmvisp.bc)
write.csv(pmvisp.appendix, file = "output/appendix-pmvisp.csv", 
    row.names = FALSE)

# ERGM
set.seed(12345)
pmvisp <- ergm(pmvisp.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(pmvisp.collab) + 
    nodeicov("bc-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 4)
summary(pmvisp)


# ==============================================================================
# Toxic chemicals policy domain in Germany
# ==============================================================================

# data preparation
chem.rep <- read.csv2("input/chem-rep.csv", row.names = 1, header = TRUE)
chem.rep <- network(as.matrix(chem.rep))  # influence reputation network
chem.collab <- read.csv2("input/chem-collab.csv", row.names = 1, header = TRUE)
chem.collab[is.na(chem.collab)] <- 0
chem.collab <- as.matrix(symmetrize(chem.collab))
chem.collab <- network(chem.collab)  # collaboration network
set.vertex.attribute(chem.rep, "bc-perc", 100 * betweenness(chem.collab, 
    rescale = TRUE))  # betweenness centrality as a vertex attribute
chem.dm <- read.csv2("input/chem-dm.csv", row.names = 1, header = TRUE)
set.vertex.attribute(chem.rep, "dm", chem.dm$dm)  # decision-maker?
set.vertex.attribute(chem.rep, "odeg-rep", degree(chem.rep, 
    cmode = "outdegree"))  # outdegree centrality as a vertex attribute
chem.committee <- read.csv2("input/chem-committee.csv", row.names = 1, 
    header = TRUE)
set.vertex.attribute(chem.rep, "committees", colSums(chem.committee))
    # number of committee memberships as a vertex attribute

# save summary table for the appendix
chem.names <- get.vertex.attribute(chem.rep, "vertex.names")
chem.bc <- 100 * betweenness(chem.collab, rescale = TRUE)
chem.type <- read.csv2("input/chem-type.csv", row.names = 1, header = TRUE)
chem.appendix <- data.frame(org = chem.names, type = chem.type[, 1], 
    committees = colSums(chem.committee), dm = chem.dm$dm, 
    betweenness = chem.bc)
write.csv(chem.appendix, file = "output/appendix-chem.csv", row.names = FALSE)

# ERGM
set.seed(12345)
chem <- ergm(chem.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(chem.collab) + 
    nodeicov("bc-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 5)
summary(chem)


# ==============================================================================
# Create table with ERGM results
# ==============================================================================

htmlreg(list(climate1, climate2, telecom, pmvisp, chem), 
    file = "output/table-ergm.doc", custom.model.names = c("Climate 95-00", 
    "Climate 02-05", "Telecom", "PM Visp", "Chemicals"), 
    custom.coef.names = c("Edges", "Cyclical ties", "Transitive tries", 
    "Reciprocity", "Outdegree of ego", "Collaboration", "Betweenness of alter", 
    "Alter = decision-maker", "Committee memberships of alter", 
    "Collaboration", "Betweenness of alter", "Committee memberships of alter", 
    "Collaboration", "Collaboration", "Collaboration"), 
    reorder.coef = c(1:5, 8:9, 7, 6), stars = c(0.01, 0.05, 0.1))


# ==============================================================================
# Climate policy domain, TERGM + GOF
# ==============================================================================

set.vertex.attribute(clim2.rep, "bc-perc-1", 100 * betweenness(clim1.collab, 
    rescale = TRUE)) # add betweenness from previous time step as node attribute
bc <- betweenness(clim2.rep, rescale = FALSE) - betweenness(clim1.rep, 
    rescale = FALSE)  # change in betweenness from t1 to t2
bc <- 100 * bc / sum(abs(bc))  # compute percentages
set.vertex.attribute(clim2.rep, "bc-change", bc)
set.vertex.attribute(clim2.rep, "odeg-rep-1", degree(clim1.rep, 
    cmode = "outdegree"))  # add outdegree from previous time step
deg.change <- degree(clim2.rep, cmode = "outdegree") - degree(clim1.rep, 
    cmode = "outdegree")
set.vertex.attribute(clim2.rep, "odeg-change", deg.change)
    # outdegree change between t1 and t2
c1 <- as.matrix(clim1.collab)
c2 <- as.matrix(clim2.collab)
collab.delta <- c2 - c1  # positive or negative change in collaboration t1 to t2
set.vertex.attribute(clim2.rep, "committees-1", rowSums(com1)) # committees t=1
set.vertex.attribute(clim1.rep, "committees-2", rowSums(com2)) # committees t=2

set.seed(12345)
climate.tergm <- ergm(clim2.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep-1") + 
    nodeocov("odeg-change") + 
    edgecov(clim1.collab) + 
    edgecov(collab.delta) + 
    nodeicov("bc-perc-1") + 
    nodeicov("bc-change") + 
    nodeifactor("dm") + 
    edgecov(clim1.rep) + 
    nodeicov("committees-1"), 
    control = control.ergm(seed = 12345)
)
summary(climate.tergm)

# MCMC diagnostics
pdf("output/diagnostics-climate-tergm.pdf")
set.seed(12345)
mcmc.diagnostics(climate.tergm)
dev.off()

# goodness-of-fit boxplots
set.seed(12345)
gof.climate.tergm <- gof(climate.tergm, nsim = 100)

pdf("output/gof-climate-tergm.pdf", width = 8, height = 8)
plot(gof.climate.tergm, boxplot.odegree = FALSE, boxplot.istar = FALSE, 
    boxplot.ostar = FALSE, roc = FALSE, pr = FALSE)
dev.off()

png("output/gof-climate-tergm.png", width = 8, height = 8, units = 'in', 
    res = 300)
plot(gof.climate.tergm, boxplot.odegree = FALSE, boxplot.istar = FALSE, 
    boxplot.ostar = FALSE, roc = FALSE, pr = FALSE)
dev.off()


# ==============================================================================
# Climate policy domain, STERGM
# ==============================================================================

set.vertex.attribute(clim1.rep, "odeg-change", degree(clim2.rep, 
    cmode = "outdegree") - degree(clim1.rep, cmode = "outdegree"))
set.vertex.attribute(clim1.rep, "bc-change", bc)

set.seed(12345)
climate.stergm <- stergm(
    list(clim1.rep, clim2.rep), 
    formation = ~ 
        edges + 
        cyclicalties + 
        transitiveties + 
        mutual + 
        nodeocov("odeg-rep") + 
        nodeocov("odeg-change") + 
        edgecov(clim1.collab) + 
        edgecov(collab.delta) + 
        nodeicov("bc-perc") + 
        nodeicov("bc-change") + 
        nodeifactor("dm") + 
        nodeicov("committees"), 
    dissolution = ~ 
        edges + 
        cyclicalties + 
        transitiveties + 
        mutual + 
        nodeocov("odeg-rep") + 
        nodeocov("odeg-change") + 
        edgecov(clim1.collab) + 
        edgecov(collab.delta) + 
        nodeicov("bc-perc") + 
        nodeicov("bc-change") + 
        nodeifactor("dm") + 
        nodeicov("committees"), 
    estimate = "CMLE",
    eval.loglik = TRUE, 
    control = control.stergm(seed = 12345)
)
summary(climate.stergm)


# ==============================================================================
# Create table with (S)TERGM results
# ==============================================================================

htmlreg(list(climate.tergm, climate.stergm), custom.model.names = c("TERGM", 
    "Tie Formation", "Tie Dissolution"), beside = TRUE, 
    include.nvertices = FALSE, 
    custom.coef.names = c("Edges", "Cyclical ties", "Transitive ties", 
    "Reciprocity", "Outdegree of ego at t=1", "Outdegree of ego (Delta)", 
    "Collaboration at t=1", "Collaboration (Delta)", 
    "Betweenness of alter at t=1", "Betweenness of alter (Delta)", 
    "Alter = decision-maker", "Lag: Influence reputation at t=1", 
    "Committee memberships of alter at t=1", "Outdegree of ego at t=1", 
    "Betweenness of alter at t=1", "Committee memberships of alter at t=1"), 
    reorder.coef = c(1:6, 11, 13, 9:10, 7:8, 12), 
    file = "output/table-tergm.doc", stars = c(0.01, 0.05, 0.1), 
    include.aic = FALSE, include.bic = FALSE, include.loglik = FALSE)


# ==============================================================================
# Summary statistics
# ==============================================================================

# climate 1
n <- dim(as.matrix(clim1.rep))[1]  # num nodes
n * n - n  # num dyads
summary(as.vector(as.matrix(clim1.rep)))  # summary reputation
sd(as.vector(as.matrix(clim1.rep)))  # SD reputation
summary(as.vector(as.matrix(clim1.collab)))  # summary collaboration
sd(as.vector(as.matrix(clim1.collab)))  # SD collaboration
summary(get.vertex.attribute(clim1.rep, "bc-perc"))  # summary betweenness
sd(get.vertex.attribute(clim1.rep, "bc-perc"))  # SD betweenness
cyc <- summary(clim1.rep ~ cyclicalties)[1]
cyc / (n * n - n)  # mean cyclical ties
sd(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # SD cyclical ties
median(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # median cyclical ties
trans <- summary(clim1.rep ~ transitiveties)[1]
trans / (n * n - n)  # mean transitive ties
sd(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # SD transitive ties
median(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # median transitive ties
recip <- summary(clim1.rep ~ mutual)[1]
recip / (n * n - n)  # mean reciprocity
sd(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # SD reciprocity
median(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # median reciprocity
summary(get.vertex.attribute(clim1.rep, "odeg-rep"))  # summary outdegree
sd(get.vertex.attribute(clim1.rep, "odeg-rep"))  # SD outdegree
summary(get.vertex.attribute(clim1.rep, "committees"))  # summary committees
sd(get.vertex.attribute(clim1.rep, "committees"))  # SD committees

# climate 2
n <- dim(as.matrix(clim2.rep))[1]  # num nodes
n * n - n  # num dyads
summary(as.vector(as.matrix(clim2.rep)))  # summary reputation
sd(as.vector(as.matrix(clim2.rep)))  # SD reputation
summary(as.vector(as.matrix(clim2.collab)))  # summary collaboration
sd(as.vector(as.matrix(clim2.collab)))  # SD collaboration
summary(get.vertex.attribute(clim2.rep, "bc-perc-2"))  # summary betweenness
sd(get.vertex.attribute(clim2.rep, "bc-perc-2"))  # SD betweenness
cyc <- summary(clim2.rep ~ cyclicalties)[1]
cyc / (n * n - n)  # mean cyclical ties
sd(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # SD cyclical ties
median(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # median cyclical ties
trans <- summary(clim2.rep ~ transitiveties)[1]
trans / (n * n - n)  # mean transitive ties
sd(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # SD transitive ties
median(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # median transitive ties
recip <- summary(clim2.rep ~ mutual)[1]
recip / (n * n - n)  # mean reciprocity
sd(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # SD reciprocity
median(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # median reciprocity
summary(get.vertex.attribute(clim2.rep, "odeg-rep"))  # summary outdegree
sd(get.vertex.attribute(clim2.rep, "odeg-rep"))  # SD outdegree
summary(get.vertex.attribute(clim2.rep, "committees-2"))  # summary committees
sd(get.vertex.attribute(clim2.rep, "committees-2"))  # SD committees

# climate (both time periods)
summary(get.vertex.attribute(clim1.rep, "dm"))  # summary decision-maker
sd(get.vertex.attribute(clim1.rep, "dm"))  # SD decision-maker
summary(bc)  # summary betweenness delta
sd(bc)  # SD betweenness delta
summary(deg.change)  # summary outdegree delta
sd(deg.change)  # SD outdegree delta
summary(as.vector(collab.delta))  # summary collaboration delta
sd(collab.delta)  # SD collaboration delta

# telecom
n <- dim(as.matrix(telecom.rep))[1]  # num nodes
n * n - n  # num dyads
summary(as.vector(as.matrix(telecom.rep)))  # summary reputation
sd(as.vector(as.matrix(telecom.rep)), na.rm = TRUE)  # SD reputation
summary(as.vector(as.matrix(telecom.collab)))  # summary collaboration
sd(as.vector(as.matrix(telecom.collab)))  # SD collaboration
summary(get.vertex.attribute(telecom.rep, "bc-perc"))  # summary betweenness
sd(get.vertex.attribute(telecom.rep, "bc-perc"))  # SD betweenness
summary(get.vertex.attribute(telecom.rep, "dm"))  # summary decision-maker
sd(get.vertex.attribute(telecom.rep, "dm"))  # SD decision-maker
cyc <- summary(telecom.rep ~ cyclicalties)[1]
cyc / (n * n - n)  # mean cyclical ties
sd(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # SD cyclical ties
median(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # median cyclical ties
trans <- summary(telecom.rep ~ transitiveties)[1]
trans / (n * n - n)  # mean transitive ties
sd(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # SD transitive ties
median(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # median transitive ties
recip <- summary(telecom.rep ~ mutual)[1]
recip / (n * n - n)  # mean reciprocity
sd(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # SD reciprocity
median(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # median reciprocity
summary(get.vertex.attribute(telecom.rep, "odeg-rep"))  # summary outdegree
sd(get.vertex.attribute(telecom.rep, "odeg-rep"))  # SD outdegree
summary(get.vertex.attribute(telecom.rep, "committees"))  # summary committees
sd(get.vertex.attribute(telecom.rep, "committees"))  # SD committees

# PM Visp
n <- dim(as.matrix(pmvisp.rep))[1]  # num nodes
n * n - n  # num dyads
summary(as.vector(as.matrix(pmvisp.rep)))  # summary reputation
sd(as.vector(as.matrix(pmvisp.rep)), na.rm = TRUE)  # SD reputation
summary(as.vector(as.matrix(pmvisp.collab)))  # summary collaboration
sd(as.vector(as.matrix(pmvisp.collab)))  # SD collaboration
summary(get.vertex.attribute(pmvisp.rep, "bc-perc"))  # summary betweenness
sd(get.vertex.attribute(pmvisp.rep, "bc-perc"))  # SD betweenness
summary(get.vertex.attribute(pmvisp.rep, "dm"))  # summary decision-maker
sd(get.vertex.attribute(pmvisp.rep, "dm"))  # SD decision-maker
cyc <- summary(pmvisp.rep ~ cyclicalties)[1]
cyc / (n * n - n)  # mean cyclical ties
sd(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # SD cyclical ties
median(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # median cyclical ties
trans <- summary(pmvisp.rep ~ transitiveties)[1]
trans / (n * n - n)  # mean transitive ties
sd(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # SD transitive ties
median(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # median transitive ties
recip <- summary(pmvisp.rep ~ mutual)[1]
recip / (n * n - n)  # mean reciprocity
sd(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # SD reciprocity
median(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # median reciprocity
summary(get.vertex.attribute(pmvisp.rep, "odeg-rep"))  # summary outdegree
sd(get.vertex.attribute(pmvisp.rep, "odeg-rep"))  # SD outdegree
summary(get.vertex.attribute(pmvisp.rep, "committees"))  # summary committees
sd(get.vertex.attribute(pmvisp.rep, "committees"))  # SD committees

# German toxic chemicals regulation
n <- dim(as.matrix(chem.rep))[1]  # num nodes
n * n - n  # num dyads
summary(as.vector(as.matrix(chem.rep)))  # summary reputation
sd(as.vector(as.matrix(chem.rep)))  # SD reputation
summary(as.vector(as.matrix(chem.collab)))  # summary collaboration
sd(as.vector(as.matrix(chem.collab)))  # SD collaboration
summary(get.vertex.attribute(chem.rep, "bc-perc"))  # summary betweenness
sd(get.vertex.attribute(chem.rep, "bc-perc"))  # SD betweenness
summary(get.vertex.attribute(chem.rep, "dm"))  # summary decision-maker
sd(get.vertex.attribute(chem.rep, "dm"))  # SD decision-maker
cyc <- summary(chem.rep ~ cyclicalties)[1]
cyc / (n * n - n)  # mean cyclical ties
sd(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # SD cyclical ties
median(c(rep(1, cyc), rep(0, (n * n - n) - cyc)))  # median cyclical ties
trans <- summary(chem.rep ~ transitiveties)[1]
trans / (n * n - n)  # mean transitive ties
sd(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # SD transitive ties
median(c(rep(1, trans), rep(0, (n * n - n) - trans)))  # median transitive ties
recip <- summary(chem.rep ~ mutual)[1]
recip / (n * n - n)  # mean reciprocity
sd(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # SD reciprocity
median(c(rep(1, recip), rep(0, (n * n - n) - recip)))  # median reciprocity
summary(get.vertex.attribute(chem.rep, "odeg-rep"))  # summary outdegree
sd(get.vertex.attribute(chem.rep, "odeg-rep"))  # SD outdegree
summary(get.vertex.attribute(chem.rep, "committees"))  # summary committees
sd(get.vertex.attribute(chem.rep, "committees"))  # SD committees


# ==============================================================================
# Appendix: use indegree centrality instead of betweenness centrality
# ==============================================================================

set.vertex.attribute(clim1.rep, "ideg-perc", 100 * degree(clim1.collab, 
    cmode = "indegree", rescale = TRUE))
set.vertex.attribute(clim2.rep, "ideg-perc-2", 100 * degree(clim2.collab, 
    cmode = "indegree", rescale = TRUE))
set.vertex.attribute(telecom.rep, "ideg-perc", 100 * degree(telecom.collab, 
    cmode = "indegree", rescale = TRUE))
set.vertex.attribute(pmvisp.rep, "ideg-perc", 100 * degree(pmvisp.collab, 
    cmode = "indegree", rescale = TRUE))
set.vertex.attribute(chem.rep, "ideg-perc", 100 * degree(chem.collab, 
    cmode = "indegree", rescale = TRUE))

set.seed(12345)
climate1.indegree <- ergm(clim1.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim1.collab) + 
    nodeicov("ideg-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 1)
summary(climate1.indegree)

set.seed(12345)
climate2.indegree <- ergm(clim2.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim2.collab) + 
    nodeicov("ideg-perc-2") + 
    nodeifactor("dm") + 
    nodeicov("committees-2"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 2)
summary(climate2.indegree)

set.seed(12345)
telecom.indegree <- ergm(telecom.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(telecom.collab) + 
    nodeicov("ideg-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 3)
summary(telecom.indegree)

set.seed(12345)
pmvisp.indegree <- ergm(pmvisp.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(pmvisp.collab) + 
    nodeicov("ideg-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 4)
summary(pmvisp.indegree)

set.seed(12345)
chem.indegree <- ergm(chem.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(chem.collab) + 
    nodeicov("ideg-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 5)
summary(chem.indegree)

htmlreg(list(climate1.indegree, climate2.indegree, telecom.indegree, 
    pmvisp.indegree, chem.indegree), file = "output/table-indegree.doc", 
    custom.model.names = c("Climate 95-00", "Climate 02-05", "Telecom", 
    "PM Visp", "Chemicals"), custom.coef.names = c("Edges", "Cyclical ties", 
    "Transitive tries", "Reciprocity", "Outdegree of ego", "Collaboration", 
    "Indegree of alter", "Alter = decision-maker", 
    "Committee memberships of alter", "Collaboration", "Indegree of alter", 
    "Committee memberships of alter", "Collaboration", "Collaboration", 
    "Collaboration"), reorder.coef = c(1:5, 8:9, 7, 6), 
    stars = c(0.01, 0.05, 0.1))


# ==============================================================================
# Appendix: use eigenvector centrality instead of betweenness centrality
# ==============================================================================

set.vertex.attribute(clim1.rep, "evcent-perc", 100 * evcent(clim1.collab, 
    rescale = TRUE))
set.vertex.attribute(clim2.rep, "evcent-perc-2", 100 * evcent(clim2.collab, 
    rescale = TRUE))
set.vertex.attribute(telecom.rep, "evcent-perc", 100 * evcent(telecom.collab, 
    rescale = TRUE))
set.vertex.attribute(pmvisp.rep, "evcent-perc", 100 * evcent(pmvisp.collab, 
    rescale = TRUE))
set.vertex.attribute(chem.rep, "evcent-perc", 100 * evcent(chem.collab, 
    rescale = TRUE))

set.seed(12345)
climate1.eigenvector <- ergm(clim1.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim1.collab) + 
    nodeicov("evcent-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 1)
summary(climate1.eigenvector)

set.seed(12345)
climate2.eigenvector <- ergm(clim2.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(clim2.collab) + 
    nodeicov("evcent-perc-2") + 
    nodeifactor("dm") + 
    nodeicov("committees-2"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 2)
summary(climate2.eigenvector)

set.seed(12345)
telecom.eigenvector <- ergm(telecom.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(telecom.collab) + 
    nodeicov("evcent-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 3)
summary(telecom.eigenvector)

set.seed(12345)
pmvisp.eigenvector <- ergm(pmvisp.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(pmvisp.collab) + 
    nodeicov("evcent-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 4)
summary(pmvisp.eigenvector)

set.seed(12345)
chem.eigenvector <- ergm(chem.rep ~ 
    edges + 
    cyclicalties + 
    transitiveties + 
    mutual + 
    nodeocov("odeg-rep") + 
    edgecov(chem.collab) + 
    nodeicov("evcent-perc") + 
    nodeifactor("dm") + 
    nodeicov("committees"), 
    control = control.ergm(seed = 12345)
)  # ERGM (model 5)
summary(chem.eigenvector)

htmlreg(list(climate1.eigenvector, climate2.eigenvector, telecom.eigenvector, 
    pmvisp.eigenvector, chem.eigenvector), file = 
    "output/table-eigenvector.doc", custom.model.names = c("Climate 95-00", 
    "Climate 02-05", "Telecom", "PM Visp", "Chemicals"), custom.coef.names = 
    c("Edges", "Cyclical ties", "Transitive tries", "Reciprocity", 
    "Outdegree of ego", "Collaboration", "Eigenvector centrality of alter", 
    "Alter = decision-maker", "Committee memberships of alter", 
    "Collaboration", "Eigenvector centrality of alter", 
    "Committee memberships of alter", "Collaboration", "Collaboration", 
    "Collaboration"), reorder.coef = c(1:5, 8:9, 7, 6), 
    stars = c(0.01, 0.05, 0.1))

