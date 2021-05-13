# villagers <- sample(dt$name, 9, FALSE)
# dreamies <- sample(dt[!name %in% villagers, name], 5, FALSE)
# 
# dt_dreamies <- dt[name %in% dreamies]
# dt_villagers <- dt[(name %in% villagers)]
# dt_islanders <- dt[!(name %in% villagers)]

calculateProba_NMTs <- function(dt_dreamies, dt_islanders,
                                NMTs = NULL, proba = NULL){
  dt_agg <- dt_islanders[, .N, species]
  dt_agg <- dt_agg[, proba := 1/nrow(dt_agg) * 1/N]
  
  dreamies_sp <- setnames(dt_dreamies[, .N, species], old = "N", new = "num_sp")
  proba_perVisit <- merge(dt_agg, dreamies_sp)[, sum(proba*num_sp)]
  
  if(!is.null(proba)){
    NMTs <- NULL
    out_NMTs <- qnbinom(proba, 1, proba_perVisit)
  }
  if(!is.null(NMTs)){
    proba <- NULL
    out_proba <- pnbinom(NMTs, 1, proba_perVisit)
  }
  
  return(list(proba_perVisit = proba_perVisit,
              out = ifelse(is.null(proba), out_proba, out_NMTs)))
}

#' Title
#'
#' @param p probaPerVisit, the one used by nbinom
#' @param NMTs no NMTs available
#' @param proba wanted proba for success
#'
#' @return
#' @export
#'
#' @examples
plotNbinomDist <- function(p, NMTs = NULL, proba = NULL){
  if(!is.null(proba)){
    x <- seq(0.05, 0.95, .05)
    y <- sapply(x, function(x) qnbinom(x, 1, p))
    dt_plot <- data.table(x, y)
    dt_plot[, x := x*100]
    
    dt_segments <- data.table(
      x = c(proba*100, 0), xend = c(proba*100, proba*100),
      y = c(0, qnbinom(proba, 1, p)),
      yend = c(qnbinom(proba, 1, p), qnbinom(proba, 1, p))
    )
    
    p <- ggplot(data = dt_plot, aes(x = x, y = y)) +
      geom_point() +
      geom_line(linetype = "dashed") +
      geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = dt_segments,
                   col = "red") +
      # geom_vline(xintercept = proba*100, color = "red") +
      # geom_hline(yintercept = qnbinom(proba, 1, p), color = "red") +
      xlab("probability of successful hunt in %") +
      ylab("number of Nook Mile Tickets") +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
  }
  
  if(!is.null(NMTs)){
    x <- seq(0, ceiling(qnbinom(.95, 1, p)/100)*100, length.out = 100)
    y <- sapply(x, function(x) pnbinom(x, 1, p))
    dt_plot <- data.table(x, y)
    dt_plot[, y := y*100]
    
    dt_segments <- data.table(
      x = c(NMTs, 0), xend = c(NMTs, NMTs),
      y = c(0, pnbinom(NMTs, 1, p)*100),
      yend = c(pnbinom(NMTs, 1, p)*100, pnbinom(NMTs, 1, p)*100)
    )
    
    p <- ggplot(data = dt_plot, aes(x = x, y = y)) +
      geom_point() +
      geom_line(linetype = "dashed") +
      geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = dt_segments,
                   col = "red") +
      #geom_vline(xintercept = NMTs, color = "red") +
      #geom_hline(yintercept = pnbinom(NMTs, 1, p)*100, color = "red") +
      xlab("number of Nook Mile Tickets") +
      ylab("probability of successful hunt in %") +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
  }
  
  return(p)
}
