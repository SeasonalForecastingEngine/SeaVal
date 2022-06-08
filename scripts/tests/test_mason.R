### Checking computations with Masons example

rm(list = ls())
library(ggplot2)
devtools::load_all()

dt <- data.table(year=c(2001:2008),tercile_cat=c(rep(-1,4),0,0,1,1),below=c(0.45,0.50,0.35,0.33,0.25,0.2,0.2,0.25),
                 normal=c(0.35,0.30,0.40,0.33,0.35,0.35,0.35,0.4),
                 above=c(0.20,0.20,0.25,0.33,0.40,0.45,0.45,0.35))

dt <- data.table(year=c(2001:2010),tercile_cat=c(rep(-1,4),0,0,1,1,1,0),
                 below=c(0.45,0.50,0.35,0.33,0.25,0.2,0.2,0.25,0.2,0.33),
                 normal=c(0.35,0.30,0.40,0.33,0.35,0.35,0.35,0.4,0.4,0.33),
                 above=c(0.20,0.20,0.25,0.33,0.40,0.45,0.45,0.35,0.4,0.33))

## ROC
# Mason has a ROC score of 0.79 (when event=above-normal)
ro <- ROCS(dt)

# not correct

## Generalized discrimination
# Mason has a gen disc score of 0.875
di <- DISS(dt)

# correct

## Hit scores
# Mason has scores of 0.42 (for highest probability category), 0.54 (for the second highest) and 0.04 (for the lowest)
hs <- HS(dt)

# correct

## Brier Score
# not relevant? since Mason calculates this one for each category separately

## RPS
# Mason has 0.1791
rp <- RPS(dt)

# not correct? There seems to be a mistake in Masons calculations

## Effective interest rate
ei <- EIR(dt)

# correct


