#' @title Re(build) the local .rda files
#' @description Querying the online databases can take a long time
#' so local rda files are used to promptly display information in
#' the App's Shiny interface
#' @param dataset The datasets for which we'll (re)create the rda files
#' @return Boolean indicating if the rda files were successfuly created
#' @importFrom nitrcbot read_nitrc_project
#' @export
build_datasets_rdas = function(dataset) {
  if(dataset == "NITRC") {
    nitrc_sets_all <- list_image_sets()
    sets <- list("NITRC" = lapply(nitrc_sets_all[,c("ID","Name","Subjects","Description")], as.character))
    list_of_sets <- lapply(sets$NITRC$ID, FUN = read_nitrc_project)
    merged_sets <- bind_rows(list_of_sets)
    #Reduce(function(x, y) merge(x, y, all=TRUE), lapply(test_set, FUN = read_nitrc_project))
    outdir = file.path("data")
    dir.create(outdir, showWarnings = FALSE)
    destfile = file.path(outdir,paste0(dataset,".rda"))
    save(merged_sets,file=destfile)
  }
  else {
    message("Unknown dataset")
  }
}
