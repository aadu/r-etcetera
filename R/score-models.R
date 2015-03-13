#' Score models
#'
#' @import data.table
#' @param model_name
#' @param config
#' @export
#' @include println.R
#' @include read-any.R
#' @include bins.R
score_models = function(config){
  dont_correct = c("glmnet", "bayesglm", "lda")
  println("Scoring models.")
  println("Loading the abbreviated modeling table.")
  features = read.all(config$features)
  if(is.data.table(features))
    features = as.data.frame(features)
  out = data.frame(id = features$id)
  for(mn in names(config$models)){
    if(config$models[[mn]] != "" & config$models[[mn]] != FALSE){
      println("Scoring model", mn)
      m = readRDS(paste0(config$winners_dir, mn, "-winner.rds"))
      cat("Model loaded from", paste0(config$winners_dir, mn, "-winner.rds"), "\n")
      out[mn] = try(
        round(predict(m$model, newdata = features, type="prob")[['yes']], 6))
      gc()
      # models_to_correct is a list of models to apply the post-correction to.
      if(!config$models[[mn]] %in% dont_correct){
        out[mn] = try(
          round(predict(m$cal, newdata=data.frame(probs=out[[mn]]),
                        type="response"), 6))
      }
      out[paste0(mn, "_bin")] = binify(out[[mn]], 20)
      gc()
    }
  }

  cat("Saving output to ", paste0(config$out_dir, "model-scores.rds"))
  saveRDS(out, paste0(config$out_dir, "scores.rds"))
  cat("Saving csv file as", paste0(config$out_dir, "scores.csv", "\n"))
  write.csv(out, paste0(config$out_dir, "scores.csv"), row.names = FALSE, na="")
  if(is.null(config$scores))
    return()
  # Combine with scoring table
  scoring_table = read.all(config$scores)
  scoring_table = as.data.table(scoring_table)
  out = as.data.table(out)
  setkey(out, id)
  setkey(scoring_table, id)
  scores = scoring_table[out]
  config$scores = gsub(
    "[.](rds)|(csv)|(sav)", "-new", config$scores, ignore.case=TRUE)
  println("Saving scoring table.")
  saveRDS(scores, paste0(config$scores, ".rds"))
  write.csv(scores, paste0(config$scores, ".csv"), row.names=F, na="")
}


#' @describeIn score_models
#' @param config
#' @param models
#' @export
score_models_split = function(config, models){
  cat("Scoring models -- split version.\n")
  cat("Loading the abbreviated modeling table.\n")
  splits = config$splits
  if(file.exists(paste0(config$dump_dir, "modeling-table-1.rds")) &
       file.exists(paste0(config$dump_dir, "modeling-table-", splits, ".rds"))){
    cat("Split modeling files already exists.\n")
  } else {
    modeling_table = readRDS(paste0(config$dump_dir, "vf.RDS"))
    cat("Modeling table loaded.\n")
    # Create splits
    splits = config$splits
    sp = ceiling(nrow(modeling_table) / splits)
    indx = list()
    for(i in 1:splits){
      if(i != splits)
        indx[[i]] = c(1 + (i - 1) * sp, sp * i)
      else
        indx[[i]] = c(1 + (i - 1) * sp, nrow(modeling_table))
    }
    # Save the various parts
    for(i in 1:splits){
      modeling_table_x = modeling_table[indx[[i]][1]:indx[[i]][2], ]
      cat("Saving part", i, "of", splits, "\n")
      saveRDS(modeling_table_x, paste0(config$dump_dir,
                                   "modeling-table-", i, ".rds"))
      cat("Modeling table part", i, "of", splits, "saved at",
          paste0(config$dump_dir, "modeling-table-", i, ".rds"), "\n")
    }
    cat("Removing original modeling table from memory.\n")
    rm(modeling_table)
  }
  scores = list()
  for(i in 1:splits){
    cat("Loading modeling table", i, "into memory.\n")
    modeling_table = readRDS(
      paste0(config$dump_dir, "modeling-table-", i, ".rds"))
    cat("Loaded.\n")
    out = data.frame(id=modeling_table$id)
    for(m in names(models)){
      cat("Scoring model", m, "for modeling table", i, "\n")
      out[m] = predict(models[[m]], modeling_table, type="prob")[['yes']]
      out[paste0(m, "_bin")] = binify(out[[m]], 20)
    }
    cat("Saving output to ",
        paste0(config$model_dir, "model-scores-", i, ".rds"))
    saveRDS(out, paste0(config$model_dir, "model-scores-", i, ".rds"))
    cat("Saving csv file as",
        paste0(config$model_dir, "model-scores-", i,".csv", "\n"))
    write.csv(out, paste0(config$model_dir, "model-scores-", i, ".csv"),
              row.names=FALSE, na="")
    scores[[i]] = out
    cat("Scoring for modeling table", i, "complete.\n\n")
  }
  # Clear memory
  x = ls()
  x = x[x != "scores"]
  rm(x)
  # Combine splits
  cat("Attempting to combine ", splits, "scored sections.\n")
  combined = scores[[1]]
  for(i in 2:splits){
    combined = rbind(combined, scores[[i]])
  }
  combined = unique(combined)
  cat("Saving combined scores to",
      paste0(config$model_dir, "model-scores.rds"), "\n")
  saveRDS(combined, paste0(config$model_dir, "model-scores.rds"))
  cat("Writing combined scores to",
      paste0(config$model_dir, "model-scores.csv"), "\n")
  write.csv(
    out, paste0(config$model_dir, "model-scores.csv"), row.names=F, na="")
  cat("Scoring complete!\n\n")
}
