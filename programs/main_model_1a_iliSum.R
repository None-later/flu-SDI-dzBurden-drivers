
## Name: Elizabeth Lee
## Date: 9/29/15
## Function: main code for jags model 1a_iliSum
## Filenames: 
## Data Source: 
## Notes: v2: change to prior on phi, see jags_model_1a_ilisum.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(rjags)
require(tidyr)
require(dplyr)
require(readr)
require(ggplot2)
source("source_jags_posterior_plots.R")
set.seed(19)

#### set these ################################
code <- 't2_'
code2 <- '_Oct'
modcode <- '1a_iliSum'
s <- 4
version <- 'v2' 

#### assign plotting params ################################
var.names <- c("lambda", "phi", "theta", "p", "z", "y")
var.coda <- c("lambda", "phi", "theta", "p", "z")

#### assign MCMC params ################################
n.adapt <- 5000 # MH tuning chains to discard
n.update <- 20000 # burn in
n.iter <- 10000 # number of final samples
n.thin <- 1
n.chains <- 2

#### import data ################################
setwd('../R_export/jagsModelData_import')
modeldata <- read_csv(sprintf('modelData_%s.csv', modcode), col_types="icccddd")
# data processing
md <- modeldata %>% filter(season==s) %>% filter(!is.na(covAdjProv))

#### create rjags data ################################
jdata <- list(
  n.loc = nrow(md),
  upsilon = as.double(as.vector(md$covAdjProv)),
  eta = as.double(as.vector(md$pop)),
  y = as.double(as.vector(md$burden))
)

#### assign initial conditions ################################
# # 9/29/15 used automatically assigned initial conditions
# inits <- list(
#     list(lambda=0.05, phi=0.2, theta=1, z=rep(8000, nrow(md))), # chain 1
#     list(lambda=0.01, phi=0.6, theta=20, z=rep(10000, nrow(md))) # chain 2
#   )

#### run jags model ################################
setwd(dirname(sys.frame(1)$ofile))
mobject <- jags.model(sprintf("jags_model_%s_%s.R", modcode, version), data=jdata, n.chains=n.chains, n.adapt=n.adapt) 
update(mobject, n.iter=n.update)
mcoda <- coda.samples(mobject, variable.names=var.coda, n.iter=n.iter, n.thin=n.thin)

#### process data ################################
chain1 <- tbl_df(as.data.frame(mcoda[[1]])) %>% mutate(chain=1) %>% mutate(iter=seq_along(chain))  
chain2 <- tbl_df(as.data.frame(mcoda[[2]])) %>% mutate(chain=2) %>% mutate(iter=seq_along(chain)) 
mout.full <- rbind(chain1, chain2)

# for.plot numbering
zip.forplots <- data.frame(zipname = md$zipname, for.plot = seq_along(md$zipname), y.data = md$burden)

# separate df for parameters
mout.paramsg <- mout.full %>% select(-contains("z["), -contains("p[")) %>% gather(param, sample, lambda:theta) %>% mutate(param = as.character(param))

# separate df for derived quantities (p[i])
der.varnames <- paste0("p", md$zipname)
mout.der <- mout.full %>% select(contains("p["), chain, iter)
names(mout.der) <- c(der.varnames, "chain", "iter")
mout.derg <- gather(mout.der, param, sample, 1:(ncol(mout.der)-2)) %>% mutate(param = as.character(param)) %>% mutate(zipname = substr.Right(param, 3)) %>% mutate(id.combo = paste0(s, zipname))
mout.derg2 <- left_join(mout.derg, zip.forplots %>% select(-y.data), by="zipname")

# separate df for latent burden (z[i])
z.varnames <- paste0("z", md$zipname)
mout.z <- mout.full %>% select(contains("z["), chain, iter)
names(mout.z) <- c(z.varnames, "chain", "iter")
mout.zg <- gather(mout.z, param, sample, 1:(ncol(mout.z)-2)) %>% mutate(param = as.character(param)) %>% mutate(zipname = substr.Right(param, 3)) %>% mutate(id.combo = paste0(s, zipname))
mout.zg2 <- left_join(mout.zg, zip.forplots %>% select(-y.data), by="zipname")

#### write all dfs to file ################################
dir.create(sprintf('../R_export/jagsModelData_export/%s', modcode))
setwd(sprintf('../R_export/jagsModelData_export/%s', modcode))

write.csv(mout.paramsg, sprintf('param_samples_%s_%s_S%s.csv', modcode, version, s), row.names=F)
write.csv(mout.derg2, sprintf('deriv_samples_%s_%s_S%s.csv', modcode, version, s), row.names=F)
write.csv(mout.zg2, sprintf('z_samples_%s_%s_S%s.csv', modcode, version, s), row.names=F)

#### plotting params ################################
w <- 9; h <- 6
w.pix <- 700; h.pix <- 700; sz <- 2
ct <- 12 # plots per figure for posterior
ct2 <- 4 # params per figure for diagnostics
indexes <- seq(1, max(zip.forplots %>% select(for.plot)), by=ct)
plotWrapper <- list(w=w, h=h, ct=ct, indexes=indexes, modcode=modcode, seas=s, ct2=ct2, version=version)

#### plot all posteriors ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s/%s', modcode, version), showWarnings = F)
setwd(sprintf('../graph_outputs/jagsModelDiagnostics/%s/%s', modcode, version))

paramsPlot(mout.paramsg, plotWrapper)
# derPlot(mout.derg2, zip.forplots, plotWrapper)
zPlot(mout.zg2, zip.forplots, plotWrapper)

#### sample diagnostics plots ################################
# plot est. params
png(sprintf('param_diag_%s_%s_S%s.png', modcode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,var.coda[1:3]], cex = sz)
dev.off()
# plot some p_i
png(sprintf('derivSamp_diag_%s_%s_S%s.png', modcode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,5:8], cex = sz)
dev.off()
# plot some z_i
png(sprintf('zSamp_diag_%s_%s_S%s.png', modcode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,360:363], cex = sz)
dev.off()

# #### convergence diagnostics ################################
# gelman.diag(mcoda[,var.coda[1:3]]) 
# raftery.diag(mcoda[,var.coda[1:3]]) 


  