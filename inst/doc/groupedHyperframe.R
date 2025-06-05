## -----------------------------------------------------------------------------
#| warning: false
#| eval: false
# remotes::install_github('tingtingzhan/groupedHyperframe')


## -----------------------------------------------------------------------------
#| warning: false
#| eval: false
# utils::install.packages('groupedHyperframe') # Developers, do NOT use!!


## -----------------------------------------------------------------------------
#| label: prerequisite
#| warning: false
#| eval: false
# remotes::install_github('spatstat/spatstat')
# remotes::install_github('spatstat/spatstat.data')
# remotes::install_github('spatstat/spatstat.explore')
# remotes::install_github('spatstat/spatstat.geom')
# remotes::install_github('spatstat/spatstat.linnet')
# remotes::install_github('spatstat/spatstat.model')
# remotes::install_github('spatstat/spatstat.random')
# remotes::install_github('spatstat/spatstat.sparse')
# remotes::install_github('spatstat/spatstat.univar')
# remotes::install_github('spatstat/spatstat.utils')


## -----------------------------------------------------------------------------
#| message: false
library(groupedHyperframe)
library(survival) # to help hyperframe understand Surv object


## -----------------------------------------------------------------------------
#| echo: false
op = par(no.readonly = TRUE)
options(mc.cores = 1L) # for CRAN submission


## -----------------------------------------------------------------------------
wrobel_lung0 = wrobel_lung |>
  within.data.frame(expr = {
    x = y = NULL
    dapi = phenotype = tissue = NULL
  })


## -----------------------------------------------------------------------------
wrobel_lung0 |> head()


## -----------------------------------------------------------------------------
(wrobel_lung0g = wrobel_lung0 |> as.groupedHyperframe(group = ~ patient_id/image_id))


## -----------------------------------------------------------------------------
unclass(object.size(wrobel_lung0g)) / unclass(object.size(wrobel_lung0))


## -----------------------------------------------------------------------------
f_g = tempfile(fileext = '.rds')
wrobel_lung0g |> saveRDS(file = f_g, compress = 'xz')
f = tempfile(fileext = '.rds')
wrobel_lung0 |> saveRDS(file = f, compress = 'xz')
file.size(f_g) / file.size(f) # not much reduction


## -----------------------------------------------------------------------------
#| message: false
wrobel_lung0g |>
  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = .01, to = .99, by = .01))


## -----------------------------------------------------------------------------
data(Ki67, package = 'groupedHyperframe')
Ki67


## -----------------------------------------------------------------------------
#| message: false
s = Ki67 |>
  aggregate_quantile(by = ~ patientID, probs = seq.int(from = .01, to = .99, by = .01))
s |> head()


## -----------------------------------------------------------------------------
spatstat.data::osteo |> 
  as.groupedHyperframe(group = ~ id/brick)


## -----------------------------------------------------------------------------
(s = wrobel_lung |>
   grouped_ppp(formula = hladr + phenotype ~ OS + gender + age | patient_id/image_id))


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "See for yourself"
0 / c(2.6e-324, 2.5e-324)
c(2.5e-324, 2.6e-324) / 0


## -----------------------------------------------------------------------------
spatstat.data::spruces |> 
  spatstat.explore::markcorr()


## -----------------------------------------------------------------------------
spatstat.data::spruces |> 
  spatstat.explore::markcorr(r = 0:90) |>
  spatstat.explore::as.data.frame.fv() |>
  utils::tail(n = 10L)


## -----------------------------------------------------------------------------
#| results: hide
s |>
  Emark_(correction = 'none')


## -----------------------------------------------------------------------------
r = seq.int(from = 0, to = 250, by = 10)
out = s |>
  Emark_(r = r, correction = 'none') |> # slow
  # Vmark_(r = r, correction = 'none') |> # slow
  # markcorr_(r = r, correction = 'none') |> # slow
  # markvario_(r = r, correction = 'none') |> # slow
  # Kmark_(r = r, correction = 'none') |> # fast
  Gcross_(i = 'CK+.CD8-', j = 'CK-.CD8+', r = r, correction = 'none') |> # fast
  # Kcross_(i = 'CK+.CD8-', j = 'CK-.CD8+', r = r, correction = 'none') |> # fast
  nncross_(i = 'CK+.CD8-', j = 'CK-.CD8+', correction = 'none') # fast


## -----------------------------------------------------------------------------
out


## -----------------------------------------------------------------------------
#| message: false
(afv = out |>
  aggregate_fv(by = ~ patient_id, f_aggr_ = pmean))


## -----------------------------------------------------------------------------
afv$hladr.E.cumtrapz |> .slice(j = '50')


## -----------------------------------------------------------------------------
#| results: hide
r = seq.int(from = 0, to = 1000, by = 50)
s |>
  Emark_(r = r, correction = 'none') |>
  aggregate_fv(by = ~ patient_id, f_aggr_ = pmean)


## -----------------------------------------------------------------------------
#| message: false
out |>
  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1))


## -----------------------------------------------------------------------------
#| message: false
(mdist = out$phenotype.nncross |> unlist() |> max())
out |> 
  aggregate_kerndens(by = ~ patient_id, from = 0, to = mdist)


## -----------------------------------------------------------------------------
data(shapley, package = 'spatstat.data')
shapley


## -----------------------------------------------------------------------------
km = shapley |> .kmeans(formula = ~ x + y + Mag, centers = 3L)
km |> class()


## -----------------------------------------------------------------------------
km1 = shapley |> .kmeans(formula = ~ x + Mag, centers = 3L)
km1 |> class()


## -----------------------------------------------------------------------------
km2 = shapley |> .kmeans(formula = ~ x + y, centers = 3L)
km2 |> class()


## -----------------------------------------------------------------------------
km3 = shapley |> .kmeans(formula = ~ x + y, clusterSize = 1e3L)
km3 |> class()
km3$centers # 5 clusters needed
km3$cluster |> table()


## -----------------------------------------------------------------------------
data(flu, package = 'spatstat.data')
flu$pattern[[1L]] |> 
  spatstat.geom::markformat()


## -----------------------------------------------------------------------------
flu$pattern[] = flu$pattern |> 
  lapply(FUN = `mark_name<-`, value = 'stain') # read ?flu carefully
flu$pattern[[1L]] |> 
  spatstat.geom::markformat()


## -----------------------------------------------------------------------------
flu$pattern[[1L]] |> split_kmeans(formula = ~ x + y, centers = 3L)


## -----------------------------------------------------------------------------
flu$pattern[1:2] |> split_kmeans(formula = ~ x + y, centers = 3L) 


## -----------------------------------------------------------------------------
flu[1:2,] |> split_kmeans(formula = ~ x + y, centers = 3L)


## -----------------------------------------------------------------------------
data(finpines, package = 'spatstat.data')
(r = finpines |> pairwise_cor_spatial())


## -----------------------------------------------------------------------------
r |> as.matrix()

