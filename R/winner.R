#' Pick winner
#'
#' @import stringr
#' @import rmarkdown
#' @param model_name
#' @param config
#' @export
#' @include println.R
winner = function(model_name, config){
  saveRDS(model_name, "./devel/model_name.rds")
  saveRDS(config, "./devel/config.rds")
  rmarkdown::render(
    "./devel/model-log.Rmd", output_format="html_document",
    output_file=paste0(model_name, "-log.html"),
    output_dir=config$logs_dir, clean=TRUE)
}


#' @describeIn winner
#' @param config
#' @export
winners = function(config){
  println("Picking winners.")
  for(model_name in names(config$models)){
    if(config$models[[model_name]] != "" &&
         config$models[[model_name]] != FALSE){
      winner(model_name, config)
    } else {
      println("Winner for", model_name, "not found.")
    }
  }
}
