
repdb_add_readme = function(project_dir) {
  restorepoint::restore.point("repdb_add_readme")
  cand_file = file.path(project_dir,"readme","readme_cand.Rds")
  if (!file.exists(cand_file)) return(invisible(NULL))

  df = readRDS(cand_file)
  df$readme_text = lapply(df$file_path, function(fp) {
    file = paste0(project_dir, "/readme/txt/", fp,".txt")
    if (!file.exists(file)) return("")
    txt = paste0(readLines(file, warn=FALSE), collapse="\n")
  })

  repboxDB::repdb_check_data(df, "readme_source")

  parcels = list(readme_source = list(readme_source=df))
  repboxDB::repdb_save_parcels(parcels, file.path(project_dir, "repdb"))

}
