## Name: Elizabeth Lee
## Date: 10/4/17
## Function: main code for jags model 9a_seasIntens single season
## Notes:
## Data Source:
##
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

# main_model_9a <- function(metriccode, season){

#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
herepath <- getwd()
require(rjags)
require(tidyverse)
require(R2jags)
source("source_jags_posterior_plots.R")
set.seed(19)

#### set these ################################
version <- 'v1'
var <- list(span = 0.4, degree = 2)
s <- 5

#### string formatting ################################
code <- ''
code2 <- '_Octfit'
modcode <- '9a'
metriccode <- "iliSum"
code.str <- sprintf('_span%s_degree%s', var$span, var$degree)

#### assign MCMC params ################################
n.adapt <- 8000 # MH tuning chains to discard
n.update <- 30000 # burn in
n.iter <- 15000 # number of final samples
n.thin <- 1
n.chains <- 2

#### import inla model data ################################
cd <- read_csv(paste0(herepath, "/../R_export/inlaModelData_import/inlaImport_model8a_iliSum_v7.csv"))
#### disease datasets ################################
md <- cd %>% filter(season == s & !is.na(O_careseek) & !is.na(graphIdx))

#### create rjags data ################################
var.names <- c(paste0("b", 0:19), "sigma_m", "sigma_st", "sigma_reg", "y")
var.coda <- c(paste0("b", 0:19), "sigma_m", "sigma_st", "sigma_reg")
var.coda2 <- c("y")
jdata <- list(
  n.loc= nrow(md),
  n.st= length(unique(md$fips_st)),
  n.reg= length(unique(md$regionID)),
  O_imscoverage= as.vector(md$O_imscoverage),
  O_careseek= as.vector(md$O_careseek),
  O_insured= as.vector(md$O_insured),
  X_poverty= as.vector(md$X_poverty),
  X_child= as.vector(md$X_child),
  X_adult= as.vector(md$X_adult),
  X_hospaccess= as.vector(md$X_hospaccess),
  X_popdensity= as.vector(md$X_popdensity),
  X_housdensity= as.vector(md$X_housdensity),
  X_vaxcovI= as.vector(md$X_vaxcovI),
  X_vaxcovE= as.vector(md$X_vaxcovE),
  X_H3A= as.vector(md$X_H3A),
  X_B= as.vector(md$X_B),
  X_priorImmunity= as.vector(md$X_priorImmunity),
  X_humidity= as.vector(md$X_humidity),
  X_pollution= as.vector(md$X_pollution),
  X_singlePersonHH= as.vector(md$X_singlePersonHH),
  y= as.vector(md$y1),
  fips_st= factor(md$fips_st),
  regionID= as.vector(md$regionID),
  logE= as.vector(md$logE)
)

print(sprintf('modeling season %s %s', s, metriccode))

#### run jags model ################################
setwd(dirname(sys.frame(1)$ofile))
mobject <- jags.model(sprintf("jags_model_%s_%s.R", modcode, version), data=jdata, n.chains=n.chains, n.adapt=n.adapt)
update(mobject, n.iter=n.update)
mcoda <- coda.samples(mobject, variable.names=var.coda, n.iter=n.iter, n.thin=n.thin)
mcoda2 <- coda.samples(mobject, variable.names = var.coda2, n.iter = n.iter, n.thin = n.thin)

#### process data ################################
# parameters
chain1 <- tbl_df(as.data.frame(mcoda[[1]])) %>% mutate(chain=1) %>% mutate(iter=seq_along(chain))
chain2 <- tbl_df(as.data.frame(mcoda[[2]])) %>% mutate(chain=2) %>% mutate(iter=seq_along(chain))
mout.full <- rbind(chain1, chain2)
# fitted value samples
chain1b <- tbl_df(as.data.frame(mcoda2[[1]])) %>% mutate(chain=1) %>% mutate(iter=seq_along(chain))
chain2b <- tbl_df(as.data.frame(mcoda2[[2]])) %>% mutate(chain=2) %>% mutate(iter=seq_along(chain))
mout.full2 <- rbind(chain1b, chain2b)

# for.plot numbering
cty.forplots <- data.frame(scale = md$fips, for.plot = seq_along(md$fips), y.data = md$y1, stringsAsFactors = FALSE)

# df for parameters
mout.paramsg <- gather(mout.full, param, sample, b0:sigma_st) %>% mutate(param = as.character(param))
mout.summ <- mout.paramsg %>%
  group_by(param) %>%
  summarise(q_025 = quantile(sample, .025), mean = mean(sample), q_975 = quantile(sample, .975)) %>%
  mutate(signif = ifelse(q_025 > 0 | q_975 < 0, TRUE, FALSE))

# df for fitted values
mout.paramsg_fit <- gather(mout.full2, param, sample, -chain, -iter) %>% mutate(param = as.character(param))
mout.summ_fit <- mout.paramsg_fit %>%
  group_by(param) %>%
  summarise(q_025 = quantile(sample, .025), mean = mean(sample), q_975 = quantile(sample, .975))
mout.summ_fit2 <- mout.summ_fit %>%
  mutate(id = as.numeric(gsub("]", "", gsub("y[", "", param, fixed=TRUE), fixed=TRUE))) %>%
  arrange(id)


#### write all dfs to file ################################
dir.create(sprintf('../R_export/jagsModelData_export/%s_%s', modcode, metriccode), showWarnings = FALSE)
dir.create(sprintf('../R_export/jagsModelData_export/%s_%s/%s', modcode, metriccode, version), showWarnings = FALSE)
setwd(sprintf('../R_export/jagsModelData_export/%s_%s/%s', modcode, metriccode, version))

write.csv(mout.paramsg, sprintf('param_samples_%s_%s_%s_S%s.csv', modcode, metriccode, version, s), row.names=F)
write_csv(mout.paramsg_fit, sprintf('y_samples_%s_%s_%s_S%s.csv', modcode, metriccode, version, s))

#### plotting params ################################
w <- 9; h <- 6
w.pix <- 700; h.pix <- 700; sz <- 2
ct <- 8 # plots per figure for posterior
ct2 <- 4 # params per figure for diagnostics
indexes <- seq(1, max(cty.forplots %>% select(for.plot)), by=ct)
plotWrapper <- list(w=w, h=h, ct=ct, indexes=indexes, modcode=modcode, seas=s, ct2=ct2, version=version, metriccode=metriccode)

pltLabs <- data.frame(param = paste0("b", 0:19), label = c("intercept", "imscoverage", "careseek", "insured", "poverty", "child", "adult", "hospaccess", "popdensity", "housdensity", "vaxcovI", "vaxcovE", "H3A", "B", "priorImmunity", "humidity", "pollution", "singlePersonHH", "H3A-adult", "B-child"))
 

#### plot all posteriors ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s', modcode, metriccode), showWarnings = F)
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s', modcode, metriccode, version), showWarnings = F)
dir.create(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s/S%s', modcode, metriccode, version, s), showWarnings = F)
setwd(sprintf('../graph_outputs/jagsModelDiagnostics/%s_%s/%s/S%s', modcode, metriccode, version, s))

paramsPlot(mout.paramsg, plotWrapper)

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

#### forest plots ################################
forestDat <- mout.summ %>%
  left_join(pltLabs, by = c("param"))
forest_params <- ggplot(forestDat %>% filter(param %in% paste0("b", 1:19)), aes(x = label)) +
  geom_pointrange(aes(ymin = q_025, y = mean, ymax = q_975, colour = signif)) +
  # geom_point(aes(y = mean)) +
  geom_hline(yintercept = 0) + 
  scale_y_continuous("mean (95%CI)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45,hjust=1), legend.position = c(0.05, .95), 
        legend.justification = c(0, 1))
print(forest_params)
ggsave(sprintf("forest_%s_%s_%s_S%s.png", modcode, metriccode, version, s), forest_params, width = 6, height = 4)

#### observed vs fitted ################################
plot(x = md$y1, y = mout.summ_fit2$mean, xlab = "Observed", ylab = "Fitted")

#### convergence diagnostics ################################
gelman.diag(mcoda[,var.coda[1:23]])
raftery.diag(mcoda[,var.coda[1:23]])

# }

#### MAIN ################################
