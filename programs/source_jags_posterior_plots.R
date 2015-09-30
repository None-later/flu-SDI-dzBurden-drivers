
## Name: Elizabeth Lee
## Date: 9/29/15
## Function: source file for plotting all posteriors (estimated parameters, derived quantities, and latent dz burden)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### functions #################################
paramsPlot <- function(mout.Params, plotWrapper){
  dummyplot <- ggplot(mout.Params, aes(x = sample, group = param)) +
    geom_histogram(aes(y = ..density..)) +
    facet_grid(~param, scales="free")
  ggsave(sprintf('param_posterior_%s_S%s.png', plotWrapper$modcode, plotWrapper$seas), dummyplot, width=plotWrapper$w, height=plotWrapper$h)
}

derPlot <- function(mout.Der, orig.Ys, plotWrapper){
  for (i in plotWrapper$indexes){
    dummyplot <- ggplot(mout.Der %>% filter(for.plot>= i & for.plot < i+plotWrapper$ct), aes(x = sample, group = zipname)) +
      geom_histogram(aes(y = ..density..)) +
      facet_wrap(~param, scales="free")
    filelabs <- orig.Ys %>% select(zipname) %>% slice(c(i, i+plotWrapper$ct-1)) 
    ggsave(sprintf('deriv_posterior_%s_S%s_%s-%s.png', plotWrapper$modcode, plotWrapper$seas, filelabs[1,], filelabs[2,]), dummyplot, width=plotWrapper$w, height=plotWrapper$h)
  }
}

zPlot <- function(mout.Zs, orig.Ys, plotWrapper){
  for (i in plotWrapper$indexes){
    dummyplot <- ggplot(mout.Zs %>% filter(for.plot>= i & for.plot < i+plotWrapper$ct), aes(x = sample, group = zipname)) +
      geom_histogram(aes(y = ..density..)) +
      # geom_vline(data = orig.Ys %>% filter(for.plot>= i & for.plot < i+plotWrapper$ct), aes(xintercept = y.data, group = zipname)) + # this line adds the original y data
      facet_wrap(~zipname, scales="free")
    filelabs <- orig.Ys %>% select(zipname) %>% slice(c(i, i+plotWrapper$ct-1)) 
    ggsave(sprintf('z_posterior_%s_S%s_%s-%s.png', plotWrapper$modcode, plotWrapper$seas, filelabs[1,], filelabs[2,]), dummyplot, width=plotWrapper$w, height=plotWrapper$h)
  }
}