#!/usr/bin/Rscript
for (geo_value_ in c('county', 'state')) {
    rmarkdown::render('12_heterogeneity_longer_time_window.Rmd',
                      params=list(geo_value=geo_value_),
                      output_file=sprintf('12_heterogeneity_longer_time_window_%s.html',
                                          geo_value_))
}
