#' Display confusion matrix
#'
#' @import caret
#' @param results a results file created using the get_results() function
#' @param positive value of outcome that should be classified as positive;
#' default is 'yes'
#' @return a formatted list of models with accompanying test statistics
#' @export
#'
display_cm = function(results, positive='yes'){
  n = gsub("_prob", "", names(results)[grep("_prob", names(results))])
  cm = .format_cm(n[1], confusionMatrix(
    results[,n[1]], results$obs, positive=positive), results$obs,
    results[[paste0(n[1], '_prob')]])
  if(length(n) > 1){
    for(i in 2:length(n)){
      cm = rbind(cm, .format_cm(n[i], confusionMatrix(
        results[,n[i]], results$obs, positive=positive), results$obs,
        results[[paste0(n[i], '_prob')]]))
    }
  }
  return(cm)
}

#' @describeIn display_cm
#' @description Internal function to format confusion matrix results
#' @param label A label (e.g., 'lda') for the model
#' @param cM A confusion matrix
#' @return A dataframe with one row
#' @export
.format_cm = function(label, cM, obs, probs){
  # Formatting function 1
  .f1 = function(x){round(as.vector(x), 4)}
  .rmse = function(obs, probs){
    obs = ifelse(obs == 'yes', 1, 0)
    sqrt(sum((obs - probs)^2) / length(obs))
  }
  .cor = function(obs, probs){
    obs =ifelse(obs == 'yes', 1, 0)
    cor(obs, probs)
  }
  out = data.frame(
    Model=toupper(label), TP=cM$table['yes', 'yes'], FP=cM$table['yes', 'no'],
    TN=cM$table['no', 'no'], FN=cM$table['no', 'yes'],
    Accuracy=.f1(cM$overall['Accuracy']), Kappa=.f1(cM$overall['Kappa']),
    RMSE=.f1(.rmse(obs, probs)), r=.f1(.cor(obs, probs)),
    Sensitivity=.f1(cM$byClass['Sensitivity']),
    Specificity=.f1(cM$byClass['Specificity']),
    PPV=.f1(cM$byClass['Pos Pred Value']),
    NPV=.f1(cM$byClass['Neg Pred Value']),
    Prevalence=.f1(cM$byClass['Prevalence']),
    Detection=.f1(cM$byClass['Detection Rate']))
  return(out)
}

#' @describeIn display_cm
#' @description Function to bin results into 5% buckets
#' @import caret
#' @import dplyr
#' @param results A results file created using get_results() function
#' @param positive Label for positive class; default = 'yes'
#' @param negative Label for negative class; default = 'no'
#' @return a data frame with percentages and counts for each model for each bin
display_buckets = function(results, positive='yes', negative='no'){
  n = grep("_prob", names(results), value=T)
  fill_missing = function(x, nums=1:20){
    missing_nums = nums[!nums %in% x$bin]
    if(length(missing_nums)){
      newrows = data.frame(
        bin=missing_nums, percent=rep("", length(missing_nums)),
        n=rep("", length(missing_nums)))
      x = rbind(x, newrows)
      x = x[order(x$bin), ]
    }
    x
  }
  add_rownames = function(x){cbind(data.frame(
    bin=as.integer(row.names(x))), x)}
  bins = lapply(
    results[,n], function(x, obs)(x %>% binify %>% as.data.frame %>%
                                    cbind(obs) %>% table %>%
                                    as.data.frame.matrix %>%
                                    add_rownames %>%
                                    mutate(total=(yes + no)) %>%
                                    mutate(percent=(yes / total * 100)) %>%
                                    round %>%
                                    mutate(
                                      percent=ifelse(percent == 0, "",
                                                     paste0(percent, "% "))) %>%
                                    mutate(
                                      n=ifelse(total == 0, "",
                                               paste0("(", total, ")"))) %>%
                                    select(bin, percent, n) %>% fill_missing %>%
                                    mutate(x=paste0(percent, n)) %>%
                                    select(x)), results$obs)
  for(each in names(bins)){
    names(bins[[each]]) <- gsub("_PROB", "", toupper(each))
  }
  bins = as.data.frame(bins)
  bins = cbind(data.frame(bin=paste(
    seq(0,95, 5),"-", seq(5,100, 5), "%", sep="")), bins)
  return(bins)
}


#' @describeIn display_cm
#' @import sjPlot
#' @param cmat Confusion matrix
#' @param buckets
#' @title Title of html file
table_out = function(cmat, buckets, title="Model Performance Comparison"){
  fix_thead = function(page.content, lbl){
    out = strsplit(page.content, "\n")[[1]]
    i = grep("tr",out, perl=TRUE)[1:2]
    out[i] = gsub("tr", "thead", out[i])
    out[1] = paste("<table class = 'display' id='", lbl, "'>", sep="")
    out = paste(out, collapse = "\n")
    return(out)
  }
  out = NA
  out[1] = paste0('
  <!DOCTYPE html>
  <html>
  <head>
  <meta http-equiv="Content-type" content="text/html; charset=utf-8">
  <title>Modeling Results</title>
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  '1.10.1/css/jquery.dataTables.css">
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'colreorder/1.1.2/css/dataTables.colReorder.css">
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'responsive/1.0.0/css/dataTables.responsive.css">
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'colvis/1.1.1/css/dataTables.colVis.css">
  <!--
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'autofill/1.2.1/css/dataTables.autoFill.css">
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'scroller/1.2.2/css/dataTables.scroller.css">
  <link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/',
                  'tabletools/2.2.2/css/dataTables.tableTools.css">
  -->
  <style type="text/css" class="init"></style>
  <script type="text/javascript" language="javascript" src="http://code.jquery',
                  '.com/jquery-1.11.1.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/1.10.1/js/jquery.dataTables.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/colreorder/1.1.2/js/data',
                  'Tables.colReorder.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/responsive/1.0.0/js/data',
                  'Tables.responsive.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/colvis/1.1.1/js/dataTables.colVis.min.js"></script>
  <!--
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/tabletools/2.2.2/js/data',
                  'Tables.tableTools.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/scroller/1.2.2/js/data',
                  'Tables.scroller.min.js"></script>
  <script type="text/javascript" language="javascript" src="http://cdn.data',
                  'tables.net/autofill/1.2.1/js/dataTables.autoFill.js"></script>
  -->

  <script type="text/javascript" class="init">
  $(document).ready(function() {
  $("#buckets").dataTable( {
  "iDisplayLength": 20,
  "dom": "Rlfrtip",
  });
  $("#metrics").dataTable( {
  "iDisplayLength": 20,
  "dom": "Rlfrtip",
  "responsive": true,
  "dom": "C<\'clear\'>lfrtip",
  "order": [[ 8, "desc" ]]
  });
  } );
  </script>
  </head>
  <body>
  <h3>Performance Metrics</h3>\n')
  tab = sjt.df(cmat, describe=F, useViewer=F, no.output=T)
  out[2] = fix_thead(tab$page.content, "metrics")
  tab2 = sjt.df(buckets, describe=F, useViewer=F, no.output=T)
  out[3] = "<h3>Model Calibration</h3>\n"
  out[4] = fix_thead(tab2$page.content, "buckets")
  out[5] = "</body>"
  out = paste(out, collapse="\n")
  return(out)
}
