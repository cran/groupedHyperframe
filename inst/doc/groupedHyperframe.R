## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Environment on author's computer"
#| label: author-env
Sys.info()[c('sysname', 'release', 'machine')]
R.version


## -----------------------------------------------------------------------------
#| eval: false
# utils::install.packages('groupedHyperframe')


## -----------------------------------------------------------------------------
#| message: false
#| label: search
library(groupedHyperframe)
library(survival)


## -----------------------------------------------------------------------------
#| echo: false
#| label: CRAN_cores
options(cores = 2L)


## -----------------------------------------------------------------------------
#| echo: false
Zhan25 = bibentry(
  bibtype = 'article',
  title = 'Quantile Index predictors using R package `hyper.gam`',
  author = c('Tingting Zhan', 'Misung Yi', 'Inna Chervoneva' ),
  journal = 'Bioinformatics',
  volume = {41}, number = {8}, pages = 'btaf430',
  year = '2025', month = '07',
  issn = '1367-4811',
  doi = '10.1093/bioinformatics/btaf430'
)


## -----------------------------------------------------------------------------
#| echo: false
#| comment: ''
c(
  Zhan25,
  citation(package = 'groupedHyperframe')
) |> toBibtex()


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: 'R code in @Zhan25'
Ki67q = groupedHyperframe::Ki67 |>
  within.data.frame(expr = {
    x = y = NULL # remove x- and y-coords for non-spacial application
  }) |>
  as.groupedHyperframe(group = ~ patientID/tissueID) |> 
  quantile(probs = seq.int(from = .01, to = .99, by = .01)) |> 
  aggregate(by = ~ patientID)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: 'A `hyperframe` object *`Ki67q`*: aggregated quantiles'
Ki67q |>
  head()

