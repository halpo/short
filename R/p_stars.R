          
#' P-value significance symbols
#' 
#' Create significance symbols for p-values.
#' 
#' @param p.value p-value to generate symbols for.
#' 
#' @examples 
#' p_stars(c(0.5, 0.09, 0.04, 0.009, 0.0009, 0))
#' 
#' @export
p_stars <- 
structure(function(p.value){
    # Convert p-values to significance codes.
    cut(p.value, breaks = c(0.000, 0.001, 0.010, 0.050, 0.100, 1.000)
               , labels = c('***', '** ', '*  ', '.  ', '   ')
               , include.lowest = TRUE
               ) %>%
    add_class('p_stars') %>%
    add_comment("Signif. codes:  0 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019 0.1 \u2018 \u2019 1")
}, interpretation = "Signif. codes:  0 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019 0.1 \u2018 \u2019 1")
