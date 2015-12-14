
## Name: Elizabeth Lee
## Date: 12/12/15
## Function: main code for jags model 2a_seasIntens
## Notes: v1 (12/12/15): b0 + b1*x1popDens[i] + b2*x2unins[i] + b3*x3pov[i] 
## v2 (12/13/15): b0
## v3 (12/13/15): b0 + b1*x1popDens[i]
## v4 (12/13/15): b0 + b2*x2unins[i]
## v5 (12/13/15): b0 + b3*x3pov[i] 
## Data Source: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

main_model_2a <- function(metricname, metriccode, season){
print(sprintf('modeling season %s %s', season, metricname))
  
#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(rjags)
require(tidyr)
require(dplyr)
require(readr)
require(ggplot2)
require(R2jags)
source("source_jags_posterior_plots.R")
# version 1: set.seed(19)

#### set these ################################
version <- 'v5' 
var <- list(span = 0.5, degree = 2)

#### string formatting ################################
code <- ''
code2 <- '_Octfit'
modcode <- '2a'
code.str <- sprintf('_span%s_degree%s', var$span, var$degree)
s <- season
met <- metricname
met.code <- metriccode

#### assign MCMC params ################################
n.adapt <- 8000 # MH tuning chains to discard
n.update <- 30000 # burn in
n.iter <- 15000 # number of final samples
n.thin <- 1
n.chains <- 2

#### import db data ################################
setwd('../R_export')
dbData <- read_csv(sprintf('dbMetrics_periodicReg_%silicnDt%s%s_analyzeDB_st.csv', code, code2, code.str))
bd <- dbData %>% filter(metric==met) %>% select(-metric)

#### import covariate data ################################
setwd('./jagsModelData_import')
cd <- read_csv(sprintf('covData_%s_v1.csv', modcode), col_types="__ddcdddddci")

#### join burden and covariate datasets ################################
md <- full_join(bd, cd, by = c("season", "state"="abbr_st")) %>% 
  filter(season == s) %>%
  filter(!is.na(burden))

#### create rjags data ################################
#### versioning ################################
if (version=='v1'){
  var.names <- c("b0", "b1", "b2", "b3", "sigma_p", "sigma_m", "z", "y")
  var.coda <- c("b0", "b1", "b2", "b3", "sigma_p", "sigma_m", "z")
  jdata <- list(
    n.loc = nrow(md),
    x1popDens = centerStandardize(as.vector(md$popDensity)),
    x2unins = centerStandardize(as.vector(md$uninsured_perc)),
    x3pov = centerStandardize(as.vector(md$inPoverty_perc)),
    y = as.vector(md$burden)
  )
} else if (version=='v2'){
  var.coda <- c("b0", "sigma_p", "sigma_m", "z")
  jdata <- list(
    n.loc = nrow(md),
    y = as.vector(md$burden)
  )
} else if (version=='v3'){
  var.coda <- c("b0", "b1", "sigma_p", "sigma_m", "z")
  jdata <- list(
    n.loc = nrow(md),
    x1popDens = centerStandardize(as.vector(md$popDensity)),
    y = as.vector(md$burden)
  )
} else if (version=='v4'){
  var.coda <- c("b0", "b2", "sigma_p", "sigma_m", "z")
  jdata <- list(
    n.loc = nrow(md),
    x2unins = centerStandardize(as.vector(md$uninsured_perc)),
    y = as.vector(md$burden)
  )
} else if (version=='v5'){
  var.coda <- c("b0", "b3", "sigma_p", "sigma_m", "z")
  jdata <- list(
    n.loc = nrow(md),
    x3pov = centerStandardize(as.vector(md$inPoverty_perc)),
    y = as.vector(md$burden)
  )
}


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
st.forplots <- data.frame(scale = md$state, for.plot = seq_along(md$state), y.data = md$burden, stringsAsFactors = FALSE)

# separate df for parameters
mout.params <- mout.full %>% select(-contains("z["), -contains("y[")) 
mout.paramsg <- gather(mout.params, param, sample, 1:(ncol(mout.params)-2)) %>% mutate(param = as.character(param))

# separate df for latent burden (z[i])
z.varnames <- paste0("z", md$state)
mout.z <- mout.full %>% select(contains("z["), chain, iter)
names(mout.z) <- c(z.varnames, "chain", "iter")
mout.zg <- gather(mout.z, param, sample, 1:(ncol(mout.z)-2)) %>% mutate(param = as.character(param)) %>% mutate(scale = substr.Right(param, 2)) %>% mutate(id.combo = paste0(s, scale))
mout.zg2 <- left_join(mout.zg, st.forplots %>% select(-y.data), by="scale")

#### write all dfs to file ################################
dir.create(sprintf('../R_export/jagsModelData_export/%s_%s', modcode, metriccode), showWarnings = FALSE)
dir.create(sprintf('../R_export/jagsModelData_export/%s_%s/%s', modcode, metriccode, version), showWarnings = FALSE)
setwd(sprintf('../R_export/jagsModelData_export/%s_%s/%s', modcode, metriccode, version))

write.csv(mout.paramsg, sprintf('param_samples_%s_%s_%s_S%s.csv', modcode, metriccode, version, s), row.names=F)
write.csv(mout.zg2, sprintf('z_samples_%s_%s_%s_S%s.csv', modcode, metriccode, version, s), row.names=F)

#### plotting params ################################
w <- 9; h <- 6
w.pix <- 700; h.pix <- 700; sz <- 2
ct <- 8 # plots per figure for posterior
ct2 <- 4 # params per figure for diagnostics
indexes <- seq(1, max(st.forplots %>% select(for.plot)), by=ct)
plotWrapper <- list(w=w, h=h, ct=ct, indexes=indexes, modcode=modcode, seas=s, ct2=ct2, version=version, metriccode=metriccode)

#### plot all posteriors ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s', modcode, metriccode), showWarnings = F)
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s', modcode, metriccode, version), showWarnings = F)
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s/S%s', modcode, metriccode, version, s), showWarnings = F)
setwd(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s/S%s', modcode, metriccode, version, s))

paramsPlot(mout.paramsg, plotWrapper)
zPlot(mout.zg2, st.forplots, plotWrapper)

#### sample diagnostics plots ################################
# plot beta params
betanames <- grep("b+", var.coda, value = T)
png(sprintf('beta_diag_%s_%s_%s_S%s.png', modcode, metriccode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,betanames], cex = sz)
dev.off()
# plot var params
sigmanames <- grep("sigma", var.coda, value = T)
png(sprintf('sigma_diag_%s_%s_%s_S%s.png', modcode, metriccode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,sigmanames], cex = sz)
dev.off()

# plot some z_i
zvarnames <- c("z[1]", "z[2]", "z[3]", "z[4]")
png(sprintf('zSamp_diag_%s_%s_%s_S%s.png', modcode, metriccode, version, s), width = w.pix, height = h.pix)
plot(mcoda[,zvarnames], cex = sz)
dev.off()

# #### convergence diagnostics ################################
# gelman.diag(mcoda[,var.coda[1:4]]) 
# raftery.diag(mcoda[,var.coda[1:4]]) 

}
