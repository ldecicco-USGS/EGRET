## ----setup, include=FALSE-----------------------------------------------------
library(EGRET)
library(knitr)

knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE,
                      fig.height = 7,
                      fig.width = 7,
                      out.width = "45%",
                      fig.show = 'hold',
                      fig.align = 'center')

## -----------------------------------------------------------------------------
printqUnitCheatSheet()

## ----defaultQ, , echo=TRUE, fig.cap="Plots of discharge with different units."----
eList <- Choptank_eList
plotConcQ(eList)
plotConcQ(eList, qUnit = 1)

## ----customQ------------------------------------------------------------------
qConst_precip <- new("qUnit",
                     qShortName = "   mm  ",
                     qUnitFactor = 1,
                     qUnitName = "Millimeter",
                     qUnitExpress = expression(paste("Precipitation in ",mm)),
                     qUnitTiny = expression(paste("Precipitation ", "(", mm, ")")),
                     shortCode = 1,
                     unitUSGS = "Precipitation, in mm",
                     prefix = "Precipitation")


## ----compareQcustom, fig.cap="Plot with custom axis, using precipitation instead of discharge."----
plotConcQ(eList, qUnit = qConst_precip)

## ----customConc---------------------------------------------------------------
eList$INFO$param.units <- "ng"

deposition <- new("concUnit",
                  longPrefix = "Deposition",
                  shortPrefix = "Dep")

plotConcQ(eList, 
          qUnit = qConst_precip,
          concLab = deposition)


## -----------------------------------------------------------------------------
names(fluxConst)

## -----------------------------------------------------------------------------
printFluxUnitCheatSheet()

## ----defaultFlux, , echo=TRUE, fig.cap="Plots of flux with different units."----
plotFluxHist(eList)
plotFluxHist(eList, fluxUnit = 2)

## -----------------------------------------------------------------------------
gDay <- new("fluxUnit",
             shortName = "    g/day   ",
             unitFactor = 1000,
             unitName = "g/day",
             unitExpress = expression("Flux in g/day"),
             unitExpressTiny = expression("Flux (g/day)"),
             unitEstimate = expression("flux in g/year"),
             unitEstimateTiny = expression("Est. flux in g/day"),
             unitUSGS = "Flux, in grams per day",
             shortCode = 14)

plotFluxHist(eList, fluxUnit = gDay)


## ----spanishmonth, out.width="100%", fig.cap="Concentration by month.", fig.width=8, fig.height=5----
spanish_month <- new("monthLabel",
         monthAbbrev = c("enero",	"feb", 	"marzo", "abr",
                         "mayo",	"jun",	"jul", "agosto", "set",
                         "oct",	"nov", "dic"),
         monthFull = c("enero",	"febrero", 	"marzo", "abril",
                       "mayo",	"junio",	"julio", "agosto", "septiembre",
                       "octubre",	"noviembre", "diciembre"),
         monthSingle = c("E", "F", "M", "A", "M", "J", "J",
                         "A", "S", "O", "N", "D"))

eList$INFO$param.units <- "mg/L"

concentration_esp <- new("concUnit",
                         longPrefix = "Concentración",
                         shortPrefix = "conc.")

boxConcMonth(eList, printTitle = FALSE, showXLabels = FALSE,
             monthLab = spanish_month, concLab = concentration_esp)


