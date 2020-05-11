percent_change <- function(baseline, followup) {
  percent_change <- ((followup - baseline) / baseline) * 100
  return(percent_change)
}
