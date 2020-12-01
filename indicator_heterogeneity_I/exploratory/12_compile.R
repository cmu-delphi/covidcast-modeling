#!/usr/bin/Rscript
rmarkdown::render('12_heterogeneity_longer_time_windor.Rmd',
                  params=list(geo_value='county'))
rmarkdown::render('12_heterogeneity_longer_time_windor.Rmd',
                  params=list(geo_value='state'))
