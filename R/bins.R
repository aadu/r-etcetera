#' Assign predicted probabilities to bins
#'
#' @import parallel
#' @param probs Predicted probabilities (0 to 1)
#' @param obs observed outcomes (0 or 1)
#' @param nm Output column name
#' @return dataframe with probs and bins
#' @export
#'
bins = function(probs, obs, nm='predicted'){
  x = factor(binify(probs), levels=1:20)
  tab = data.frame(bin=1:20)
  tab[[nm]] = t(t(table(x)))
  tab[[nm]][tab[[nm]] == 0] = ""
  percs = unlist(mclapply(unique(x),
                 function(i)(round(sum(obs[x==i] == 'yes',
                             na.rm=T) / length(obs[x==i]) * 100))))
  names(percs) <-  unique(x)
  percs <- t(t(percs[order(as.integer(names(percs)))]))
  percs <- data.frame(percs, bin=row.names(percs))
  both = merge(tab, percs, all=T)
  both$percs[is.na(both$percs)] = ""
  both[[nm]] = ifelse(both[[nm]] != "",
                      paste0("(", prettyNum(both[[nm]], big.mark=",",
                      preserve.width = 'individual'), ")"), "")
  both[['percs']] = ifelse(both[['percs']] != "",
                           paste0(both[['percs']], "%"), "")
  out = data.frame(paste(both[['percs']], both[[nm]]))
  names(out) = nm
  out
}


#' @describeIn bins
#' @export
binify = function(x, breaks=20){
  inc = 1 / breaks
  out = as.integer(cut(x, breaks=seq(0, 1, inc),
                   labels=seq(1,breaks)))
  out
}
