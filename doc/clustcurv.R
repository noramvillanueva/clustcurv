## ---- include = FALSE-------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
	warning = FALSE,
	tidy.opts = list(
		keep.blank.line = TRUE,
		width.cutoff = 150
		),
	options(width = 150,fig.width=12, fig.height=8),
	eval = TRUE
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
library(clustcurv)
library(condSURV)
data(gbcsCS)
head(gbcsCS[, c(5:10, 13, 14)])

## ---------------------------------------------------------------------------------------------------------------------------------------------------
table(gbcsCS$nodes)
gbcsCS[gbcsCS$nodes > 13,'nodes'] <- 14
gbcsCS$nodes <- factor(gbcsCS$nodes)
levels(gbcsCS$nodes)[14]<- '>13'
table(gbcsCS$nodes)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
fit.kgbcs<- ksurvcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, x = gbcsCS$nodes,
                   algorithm = 'kmedians', k = 4, seed = 300716)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
fit.gbcs <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, x = gbcsCS$nodes,
                     nboot = 100, seed = 300716, algorithm = 'kmedians')

## ---------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit.kgbcs)

summary(fit.gbcs)

## ---- out.width= '100%', fig.align = "center"-------------------------------------------------------------------------------------------------------
autoplot(fit.gbcs , groups_by_colour = FALSE, interactive = TRUE)

## ----out.width= '100%', fig.align = "center"--------------------------------------------------------------------------------------------------------
autoplot(fit.gbcs , groups_by_colour = TRUE, interactive = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
fit.gbcs2 <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, seed = 300716,
                      x = gbcsCS$nodes, algorithm = 'kmeans', nboot = 100)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
data("barnacle5")
head(barnacle5)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
fit.bar <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                          nboot = 100, seed = 300716, algorithm = 'kmeans')

## ---------------------------------------------------------------------------------------------------------------------------------------------------
print(fit.bar)
summary(fit.bar)

## ---- out.width= '100%', fig.align = "center"-------------------------------------------------------------------------------------------------------
autoplot(fit.bar, groups_by_color = TRUE, interactive = TRUE)

