library(tidyverse)
library(dplR)



sj <- read.rwl(fname="data/qpyr/sn_sanjuan.rwl", format="tucson")
ca <- read.rwl(fname="data/qpyr/sn_canar.rwl", format="tucson")

# Replace SNA by SJ and SNB by CA
names(ca) <- stringr::str_replace(names(ca), "SNB", "CA")
names(sj) <- stringr::str_replace(names(sj), "SNA", "SJ")
# Remove g in name of some cores of CA.
names(ca) <- stringr::str_replace(names(ca), "g", "")



# Create subset to compare between sites
caL <- ca[,c("CA0101","CA0102","CA0201","CA0202","CA0301","CA0302","CA0401","CA0402","CA0501","CA0502",
             "CA0601","CA0602","CA0701","CA0702","CA0801","CA0802","CA0901","CA0902","CA1001","CA1002",
             "CA2601","CA2602","CA2701","CA2702","CA2801","CA2802","CA2901","CA2902","CA3001","CA3002")]

caH <- ca[, c("CA1101","CA1102","CA1201","CA1202","CA1301","CA1302","CA1401","CA1402","CA1501","CA1502",
              "CA1601","CA1602","CA1701","CA1702","CA1801","CA1802","CA1901","CA1902","CA2001","CA2002",
              "CA2101","CA2102","CA2201","CA2202","CA2301","CA2302","CA2401","CA2402","CA2501","CA2502")]

# remove the rows with NA across all columns
caL <- caL[rowSums(is.na(caL))!=ncol(caL), ]



computeRWI_splines <- function(rwdf, nsmooth) {

  require(dplyr)
  require(dplR)
  require(purrr)

  # ID cores
  cores <- colnames(rwdf)

  rwi_series <- vector("list", length = length(cores))

  for (i in seq_along(cores)){

    core <- cores[i]

    # select ring width series
    rws <- rwdf[[core]]

    # add row (year) names
    names(rws) <- rownames(rwdf)

    # detrended series
    serie_suavizada <- detrend.series(y = rws, method = "Spline",
                                      y.name = core, nyrs = nsmooth,
                                      make.plot = FALSE) |>
      as.data.frame() |>
      setNames(core) |>
      rownames_to_column("year") |>
      mutate(year = as.numeric(year))

    rwi_series[[i]] <- serie_suavizada

  }

  rwi <- purrr::reduce(rwi_series, full_join, by = "year")
  return(rwi)
  }






