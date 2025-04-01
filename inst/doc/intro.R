## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# devtools::install_github('spatstat/spatstat')
# devtools::install_github('spatstat/spatstat.data')
# devtools::install_github('spatstat/spatstat.explore')
# devtools::install_github('spatstat/spatstat.geom')
# devtools::install_github('spatstat/spatstat.linnet')
# devtools::install_github('spatstat/spatstat.model')
# devtools::install_github('spatstat/spatstat.random')
# devtools::install_github('spatstat/spatstat.sparse')
# devtools::install_github('spatstat/spatstat.univar')
# devtools::install_github('spatstat/spatstat.utils')

## ----setup--------------------------------------------------------------------
library(groupedHyperframe)
library(spatstat.data)
library(survival) # to help hyperframe understand Surv object

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '', 'Forward pipe operator', '`?base::pipeOp` introduced in `R` 4.1.0', 
  '`attr`', 'Attributes', '`base::attr`; `base::attributes`',
  '`CRAN`, `R`', 'The Comprehensive R Archive Network', 'https://cran.r-project.org',
  '`data.frame`', 'Data frame', '`base::data.frame`',
  '`formula`', 'Formula', '`stats::formula`',
  '`fv`, `fv.object`, `fv.plot`', '(Plot of) function value table', '`spatstat.explore::fv.object`, `spatstat.explore::plot.fv`',
  '`groupedData`, `~ g1/.../gm`', 'Grouped data frame; nested grouping structure', '`nlme::groupedData`; `nlme::lme`',
  '`hypercolumns`, `hyperframe`', '(Hyper columns of) hyper data frame', '`spatstat.geom::hyperframe`',
  '`inherits`', 'Class inheritance', '`base::inherits`',
  '`kerndens`', 'Kernel density', '`stats::density.default()$y`',
  # '`matrix`', 'Matrix', '`base::matrix`', # hahaha!!!
  '`mc.cores`', 'Number of CPU cores to use', '`parallel::mclapply`; `parallel::detectCores`',
  '`multitype`', 'Multitype object', '`spatstat.geom::is.multitype`',
  '`object.size`', 'Memory allocation', '`utils::object.size`',
  '`pmean`, `pmedian`', 'Parallel mean and median', '`groupedHyperframe::pmean`; `groupedHyperframe::pmedian`',
  '`pmax`, `pmin`', 'Parallel maxima and minima', '`base::pmax`; `base::pmin`',
  '`ppp`, `ppp.object`', '(Marked) point pattern', '`spatstat.geom::ppp.object`',
  '`quantile`', 'Quantile', '`stats::quantile`',
  '`save`, `xz`', 'Save with `xz` compression', '`base::save(., compress = \'xz\')`; `base::saveRDS(., compress = \'xz\')`; https://en.wikipedia.org/wiki/XZ_Utils', 
  '`S3`, `generic`, `methods`', '`S3` object oriented system',  '`base::UseMethod`; `utils::methods`; `utils::getS3method`; https://adv-r.hadley.nz/s3.html',
  '`search`', 'Search path', '`base::search`',
  '`Surv`', 'Survival object', '`survival::Surv`',
  '`trapz`, `cumtrapz`', '(Cumulative) trapezoidal integration', '`pracma::trapz`; `pracma::cumtrapz`; https://en.wikipedia.org/wiki/Trapezoidal_rule'
) |>
  matrix(nrow = 3L, dimnames = list(c('Term / Abbreviation', 'Description', 'Reference'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable(format = 'html') 
# ?knitr::kable
# default: `|` shown as &...
# format = 'html': `>` shown as &..

## -----------------------------------------------------------------------------
osteo |> as.groupedHyperframe(group = ~ id/brick)

## -----------------------------------------------------------------------------
(Ki67g = Ki67. |> as.groupedHyperframe(group = ~ patientID/tissueID, mc.cores = 1L))

## -----------------------------------------------------------------------------
unclass(object.size(Ki67g)) / unclass(object.size(Ki67.))

## -----------------------------------------------------------------------------
f_g = tempfile(fileext = '.rds')
Ki67g |> saveRDS(file = f_g, compress = 'xz')
f = tempfile(fileext = '.rds')
Ki67. |> saveRDS(file = f, compress = 'xz')
file.size(f_g) / file.size(f) # not much reduction

## -----------------------------------------------------------------------------
(s = grouped_ppp(formula = hladr + phenotype ~ OS + gender + age | patient_id/image_id, 
                 data = wrobel_lung, mc.cores = 1L))

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '`Emark_()`', '`Emark()`', '`numeric` marks', '`.E`',
  '`Vmark_()`', '`Vmark()`', '`numeric` marks', '`.V`',
  '`markcorr_()`', '`markcorr()`', '`numeric` marks', '`.k`',
  '`markvario_()`', '`markvario()`', '`numeric` marks', '`.gamma`',
  '`Gcross_()`', '`Gcross()`', '`multitype` marks', '`.G`',
  '`Kcross_()`', '`Kcross()`', '`multitype` marks', '`.K`',
  '`Jcross_()`', '`Jcross()`', '`multitype` marks', '`.J`'
) |>
  matrix(nrow = 4L, dimnames = list(c('Batch Process', 'Workhorse in **`spatstat.explore`**', 'Applicable To', '`fv`-`hypercolumn` Suffix'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable()

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '`nncross_()`', '`nncross.ppp(., what = \'dist\')`', '`multitype` marks', '`.nncross`'
) |>
  matrix(nrow = 4L, dimnames = list(c('Batch Process', 'Workhorse in **`spatstat.geom`**', 'Applicable To', '`numeric`-`hypercolumn` Suffix'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable()

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
(afv = out |>
  aggregate_fv(by = ~ patient_id, f_aggr_ = pmean, mc.cores = 1L))

## -----------------------------------------------------------------------------
afv$hladr.E.cumtrapz |> .slice(j = '50')

## -----------------------------------------------------------------------------
out |>
  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1), mc.cores = 1L)

## -----------------------------------------------------------------------------
(mdist = out$phenotype.nncross |> unlist() |> max())
out |> 
  aggregate_kerndens(by = ~ patient_id, from = 0, to = mdist, mc.cores = 1L)

