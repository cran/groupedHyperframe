## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# devtools::install_github('spatstat/spatstat'); packageDate('spatstat')
# devtools::install_github('spatstat/spatstat.data'); packageDate('spatstat.data')
# devtools::install_github('spatstat/spatstat.explore'); packageDate('spatstat.explore')
# devtools::install_github('spatstat/spatstat.geom'); packageDate('spatstat.geom')
# devtools::install_github('spatstat/spatstat.linnet'); packageDate('spatstat.linnet')
# devtools::install_github('spatstat/spatstat.model'); packageDate('spatstat.model')
# devtools::install_github('spatstat/spatstat.random'); packageDate('spatstat.random')
# devtools::install_github('spatstat/spatstat.sparse'); packageDate('spatstat.sparse')
# devtools::install_github('spatstat/spatstat.univar'); packageDate('spatstat.univar')
# devtools::install_github('spatstat/spatstat.utils'); packageDate('spatstat.utils')

## ----setup--------------------------------------------------------------------
library(groupedHyperframe)
library(survival) # to help hyperframe understand Surv object

## ----eval = FALSE-------------------------------------------------------------
# devtools::install_github('tingtingzhan/groupedHyperframe', build_vignettes = TRUE)
# vignette('intro', package = 'groupedHyperframe')

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '`attr`', 'Attributes', '`base::attr`; `base::attributes`',
  '`CRAN`, `R`', 'The Comprehensive R Archive Network', 'https://cran.r-project.org',
  '`data.frame`', 'Data frame', '`base::data.frame`',
  '`formula`', 'Formula', '`stats::formula`',
  '`fv`, `fv.object`', 'Function value table', '`spatstat.explore::fv.object`',
  '`groupedData`', 'Grouped data frame', '`nlme::groupedData`',
  '`hypercolumn`', 'Column of hyper data frame', '`spatstat.geom::hyperframe`',
  '`hyperframe`', 'Hyper data frame', '`spatstat.geom::hyperframe`',
  '`inherits`', 'Class inheritance', '`base::inherits`',
  '`kerndens`', 'Kernel density', '`stats::density.default()$y`',
  '`matrix`', 'Matrix', '`base::matrix`',
  '`mc.cores`', 'Number of CPU cores to use', '`parallel::mclapply`, `parallel::detectCores`',
  '`multitype`', 'Multitype object', '`spatstat.geom::is.multitype`',
  '`ppp`, `ppp.object`', '(Marked) point pattern', '`spatstat.geom::ppp.object`',
  '`~ g1/.../gm`', 'Nested grouping structure', '`nlme::groupedData`; `nlme::lme`',
  '`quantile`', 'Quantile', '`stats::quantile`',
  '`S3`', '`R`\'s simplest object oriented system', 'https://adv-r.hadley.nz/s3.html',
  '`search`', 'Search path', '`base::search`',
  '`Surv`', 'Survival object', '`survival::Surv`',
  '`trapz`, `cumtrapz`', '(Cumulative) trapezoidal integration', '`pracma::trapz`; `pracma::cumtrapz`; https://en.wikipedia.org/wiki/Trapezoidal_rule'
) |>
  matrix(nrow = 3L, dimnames = list(c('Term / Abbreviation', 'Description', 'Reference'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable()

## -----------------------------------------------------------------------------
(s = grouped_ppp(formula = hladr + phenotype ~ OS + gender + age | patient_id/image_id, 
                 data = wrobel_lung, mc.cores = 1L))

## -----------------------------------------------------------------------------
(s_a = grouped_ppp(Ki67 ~ Surv(recfreesurv_mon, recurrence) + race + age | patientID/tissueID, 
                  data = Ki67, coords = FALSE, mc.cores = 1L))

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '`Emark_()`', '`spatstat.explore::Emark`', '`numeric` marks (e.g., *`hladr`*) in `ppp`-hypercolumn',
  '`Vmark_()`', '`spatstat.explore::Vmark`', '`numeric` marks',
  '`markcorr_()`', '`spatstat.explore::markcorr`', '`numeric` marks', 
  '`markvario_()`', '`spatstat.explore::markvario`', '`numeric` marks', 
  '`Gcross_()`', '`spatstat.explore::Gcross`', '`multitype` marks (e.g., *`phenotype`*)', 
  '`Kcross_()`', '`spatstat.explore::Kcross`', '`multitype` marks', 
  '`Jcross_()`', '`spatstat.explore::Jcross`', '`multitype` marks'
) |>
  matrix(nrow = 3L, dimnames = list(c('Function', 'Workhorse', 'Applicable To'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable(caption = 'Batch process that adds an `fv`-hypercolumn')

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '`nncross_()`', '`spatstat.geom::nncross.ppp(., what = \'dist\')`', '`multitype` marks (e.g., *`phenotype`*)'
) |>
  matrix(nrow = 3L, dimnames = list(c('Function', 'Workhorse', 'Applicable To'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable(caption = 'Batch process that adds a `numeric`-hypercolumn')

## -----------------------------------------------------------------------------
r = seq.int(from = 0, to = 250, by = 10)
out = s |>
  Emark_(r = r, correction = 'best', mc.cores = 1L) |> # slow
  # Vmark_(r = r, correction = 'best', mc.cores = 1L) |> # slow
  # markcorr_(r = r, correction = 'best', mc.cores = 1L) |> # slow
  # markvario_(r = r, correction = 'best', mc.cores = 1L) |> # slow
  Gcross_(i = 'CK+.CD8-', j = 'CK-.CD8+', r = r, correction = 'best', mc.cores = 1L) |> # fast
  # Kcross_(i = 'CK+.CD8-', j = 'CK-.CD8+', r = r, correction = 'best', mc.cores = 1L) |> # fast
  nncross_(i = 'CK+.CD8-', j = 'CK-.CD8+', correction = 'best', mc.cores = 1L) # fast

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
afv = out |>
  aggregate_fv(by = ~ patient_id, f_aggr_ = 'mean', mc.cores = 1L)
nrow(afv) # number of patients
names(afv)
dim(afv$hladr.E.cumtrapz) # N(patient) by length(r)

## -----------------------------------------------------------------------------
q = out |>
  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1), mc.cores = 1L)
nrow(q)
names(q)
dim(q$phenotype.nncross.quantile)
dim(q$hladr.quantile)

## -----------------------------------------------------------------------------
(mdist = out$phenotype.nncross |> unlist() |> max())
d = out |> 
  aggregate_kerndens(by = ~ patient_id, from = 0, to = mdist, mc.cores = 1L)
nrow(d)
names(d)
dim(d$phenotype.nncross.kerndens)

