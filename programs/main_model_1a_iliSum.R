
## Name: Elizabeth Lee
## Date: 
## Function: 
## Filenames: 
## Data Source: 
## Notes: 
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
set.seed(19)

#### set these ################################
code <- 't2_'
code2 <- '_Oct'
modcode <- '1a_iliSum'
s <- 3

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

#### assign MCMC params ################################
n.adapt <- 5000 # MH tuning chains to discard
n.update <- 20000 # burn in
n.iter <- 10000 # number of final samples
n.thin <- 1

#### assign initial conditions ################################
inits <- list(
    list(theta=1, phi=0.2, lambda=0.05, z=rep(8000, nrow(md))), # chain 1
    list(theta=20, phi=0.63, lambda=0.01, z=rep(10000, nrow(md))) # chain 2
  )

#### assign plotting params ################################
var.names <- c("theta", "phi", "lambda", "p", "z", "y")
var.coda <- c("theta", "phi", "lambda", "z")
var.jags <- c("p") # derived quants

#### run jags model ################################
setwd(dirname(sys.frame(1)$ofile))
mobject <- jags.model(sprintf("jags_model_%s.R", modcode), data=jdata, n.chains=length(inits), n.adapt=n.adapt) 
update(mobject, n.iter=n.update)
mcoda <- coda.samples(mobject, variable.names=var.coda, n.iter=n.iter, n.thin=n.thin)
mjags <- jags.samples(mobject, variable.names=var.jags, n.iter=n.iter, n.thin=n.thin)

#### diagnostics plots ################################
x11()
plot(mcoda[,1:4])
x11()
plot(mcoda[,5:8])

#### convergence diagnostics ################################
gelman.diag(mcoda[,var.coda]) 
raftery.diag(mcoda[,var.coda]) 

#### process data ################################
names.col <- paste0('z_', md$zipname)
chain1 <- tbl_df(as.data.frame(mcoda[[1]])) %>% mutate(chain=1) %>% mutate(iter=seq_along(chain))  
chain2 <- tbl_df(as.data.frame(mcoda[[2]])) %>% mutate(chain=2) %>% mutate(iter=seq_along(chain)) 
mout <- rbind(chain1, chain2)

# CHANGE COLUMN NAMES

mout_gather <- gather(mout, param, sample, 1:353) %>% mutate(param = as.character(param)) 
uq.params <- data.frame(param = as.character(unique(mout_gather$param)), for.plot = seq_along(unique(mout_gather$param)), ys = as.numeric(c(NA, NA, NA, jdata$y)))
mout_gather2 <- left_join(mout_gather, uq.params, by="param")
indexes <- seq(1, max(mout_gather2 %>% select(for.plot)), by=6)


#### plotting params ################################
w <- 9; h <- 6


#### plot data ################################
dir.create('../graph_outputs/jagsModelDiagnostics')
setwd('../graph_outputs/jagsModelDiagnostics')
for (i in indexes){
  dummyplot <- ggplot(mout_gather2 %>% filter(for.plot>= i & for.plot < i+6 & chain==1), aes(x = sample, group = param)) +
    geom_histogram(aes(y = ..density..)) +
    
    # FIX VERTICAL LINE
    geom_vline(aes(x=ys)) +
    facet_wrap(~param, scales="free")
  filelabs <- uq.params %>% select(param) %>% slice(c(i, i+5)) 
  ggsave(sprintf('test_%s_%s-%s.png', modcode, filelabs[1,], filelabs[2,]), dummyplot, width=w, height=h)
}

  