## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  error = FALSE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 3,
  fig.dpi = 300,
  eval = requireNamespace("phacking") && requireNamespace("PublicationBias")
)

# round printed numbers to 2 digits without changing options()
inline_hook <- function(x) {
  if (is.numeric(x)) {
    # if integer, print without decimal; otherwise print two places
    res <- ifelse(x == round(x), sprintf("%d", x), sprintf("%.2f", x))
    paste(res, collapse = ", ")
  } else {
    x
  }
}
knitr::knit_hooks$set(inline = inline_hook)

# refs <- list(
#   "SAPB" = "[Mathur & VanderWeele (2020)](https://osf.io/s9dp6/)",   # pubbias
#   "SAPB-E" = "[Mathur & VanderWeele (2019)](https://osf.io/p3xyd/)", # pubbias e
#   "SAPH" = "[Mathur (2022)](https://osf.io/ezjsx/)",                 # phacking
#   "MBMA" = "[Mathur (2022)](https://osf.io/u7vcb/)",                 # multibias
#   "meat" = "[Mathur et al (2021)](https://osf.io/bc2wy/)"
# )

refs <- list(
  "SAPB"   = c("Mathur & VanderWeele", "2020", "https://osf.io/s9dp6/"), # pubbias
  "SAPB-E" = c("Mathur & VanderWeele", "2019", "https://osf.io/p3xyd/"), # pubbias e
  "SAPH"   = c("Mathur",               "2022", "https://osf.io/ezjsx/"), # phacking
  "MBMA"   = c("Mathur",               "2022", "https://osf.io/u7vcb/"), # multibias
  "meat"   = c("Mathur et al",         "2021", "https://osf.io/bc2wy/")
)

# "p" = parenthetical, "t" = textual
ref <- function(key, mode = "p") {
  r <- refs[[key]]
  if (mode == "t") glue::glue("[{r[1]} ({r[2]})]({r[3]})") # [author, year](link)
  else if (mode == "p") glue::glue("[{r[1]}, {r[2]}]({r[3]})") # [author (year)](link)
}

## ----setup--------------------------------------------------------------------
library(PublicationBias)
library(phacking)
library(multibiasmeta)

## ----meta_meat----------------------------------------------------------------
meta_meat

## ----rma----------------------------------------------------------------------
metafor::rma(yi, vi, data = meta_meat, method = "FE")

## ----robu---------------------------------------------------------------------
robumeta::robu(yi ~ 1, data = meta_meat, studynum = cluster, var.eff.size = vi)

## ----pubbias_meat_1-----------------------------------------------------------
pubbias_meat_1 <- pubbias_meta(meta_meat$yi,
                               meta_meat$vi,
                               model_type = "fixed",
                               selection_ratio = 1)
pubbias_meat_1$stats

## ----sr, echo=FALSE-----------------------------------------------------------
sr <- 4

## ----pubbias_meat_4-----------------------------------------------------------
pubbias_meat_4 <- pubbias_meta(meta_meat$yi,
                               meta_meat$vi,
                               model_type = "fixed",
                               selection_ratio = sr)
pubbias_meat_4$stats

## ----pubbias_meat_4_robust----------------------------------------------------
pubbias_meat_4 <- pubbias_meta(meta_meat$yi,
                               meta_meat$vi,
                               cluster = meta_meat$cluster,
                               selection_ratio = 4)
pubbias_meat_4$stats

## ----pubbias_meat_4_worst-----------------------------------------------------
pubbias_meat_4 <- pubbias_meta(meta_meat$yi,
                               meta_meat$vi,
                               cluster = meta_meat$cluster,
                               selection_ratio = 4,
                               return_worst_meta = TRUE)

pubbias_meat_4$stats

## ----svalue_meat_0------------------------------------------------------------
svalue_meat_0 <- pubbias_svalue(meta_meat$yi,
                                meta_meat$vi,
                                cluster = meta_meat$cluster)
svalue_meat_0$stats

## ----qs, echo=FALSE-----------------------------------------------------------
qs <- 0.1

## ----svalue_meat_01-----------------------------------------------------------
svalue_meat_01 <- pubbias_svalue(meta_meat$yi,
                                 meta_meat$vi,
                                 cluster = meta_meat$cluster,
                                 q = 0.1)
svalue_meat_01$stats

## ----significance_funnel, fig.height = 4--------------------------------------
significance_funnel(yi = meta_meat$yi, vi = meta_meat$vi)

## ----phacking_meat------------------------------------------------------------
phacking_meat <- phacking_meta(yi = meta_meat$yi, vi = meta_meat$vi, parallelize = FALSE)

phacking_meat$stats

## ----tcrit, include = FALSE---------------------------------------------------
# temporary workaround for bug in released version of phacking
phacking_meat$values$tcrit <- qnorm(0.975)

## ----rtma_qqplot, fig.width = 3-----------------------------------------------
rtma_qqplot(phacking_meat)

## ----z_density----------------------------------------------------------------
z_density(yi = meta_meat$yi, vi = meta_meat$vi)

## ----multi_meat_0-------------------------------------------------------------
multi_meat_0 <- multibias_meta(yi = meta_meat$yi,
                               vi = meta_meat$vi,
                               selection_ratio = 4,
                               bias_affirmative = 0,
                               bias_nonaffirmative = 0)
multi_meat_0$stats

## ----multi_meat_1-------------------------------------------------------------
multi_meat_1 <- multibias_meta(yi = meta_meat$yi,
                               vi = meta_meat$vi,
                               biased = !meta_meat$randomized,
                               selection_ratio = 4,
                               bias_affirmative = log(1.5),
                               bias_nonaffirmative = log(1.1))
multi_meat_1$stats

## ----multi_meat_1_all---------------------------------------------------------
multi_meat_1 <- multibias_meta(yi = meta_meat$yi,
                               vi = meta_meat$vi,
                               biased = TRUE,
                               selection_ratio = 4,
                               bias_affirmative = log(1.5),
                               bias_nonaffirmative = log(1.1))
multi_meat_1$stats

## ----evalue_meat--------------------------------------------------------------
evalue_meat <- multibias_evalue(yi = meta_meat$yi,
                                vi = meta_meat$vi,
                                biased = !meta_meat$randomized,
                                selection_ratio = 4)
evalue_meat$stats

## ----evalue_meat_01-----------------------------------------------------------
evalue_meat_01 <- multibias_evalue(yi = meta_meat$yi,
                                   vi = meta_meat$vi,
                                   biased = !meta_meat$randomized,
                                   selection_ratio = 4,
                                   q = 0.1)
evalue_meat_01$stats

## ----evalue_meat_conf---------------------------------------------------------
evalue_meat_conf <- multibias_evalue(yi = meta_meat$yi,
                                     vi = meta_meat$vi,
                                     biased = !meta_meat$randomized,
                                     selection_ratio = 4,
                                     assumed_bias_type = list(EValue::confounding()))
evalue_meat_conf$stats

