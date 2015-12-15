
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')

spatial <- list(scale = "state", stringcode = "State", stringabbr = "_st")
span.var <- 0.5 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

data5 <- read_csv(sprintf('fullIndicAll_periodicReg_%silicnDt%s%s_analyzeDB%s.csv', code, code2, code.str, spatial$stringabbr), col_names = T, col_types = list(state = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilicn.dt = col_double(), ILIcn = col_double())) %>%
  rename(scale = state)

data6 <- data5 %>% select(season, scale, in.season) %>% filter(in.season) %>% distinct(season, scale) %>% group_by(season) %>% summarise(counts = length(in.season))

