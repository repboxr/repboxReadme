example = function() {
  library(repboxReadme)
  library(EconJournalScrap)
  arts = ejs_load_agg("art") %>%
    filter(!is.na(file_zip)) %>%
    filter(!is.na(readme_file))

  inds = 1:NROW(arts)
  inds = 741:NROW(arts)
  i = 1613
  for (i in inds) {
    art = arts[i,]
    parent_dir = "~/repbox/projects_readme"
    project_dir=file.path(parent_dir, art$artid)
    #if (dir.exists(project_dir)) next
    cat("\n",i,"of", max(inds), " ", project_dir, "\n")
    try(repbox_init_readme(art,project_dir=project_dir, parent_dir = parent_dir))
  }
  rstudioapi::filesPaneNavigate(project_dir)
}

repbox_init_readme = function(art, project_dir=file.path(parent_dir, art$artid), parent_dir, zip_file = art$file_zip) {
  restore.point("repbox_init_readme")
  if (!isTRUE(file.exists(zip_file))) {
    cat("\nrepbox_init_readme: zip file ", zip_file," for ", project_dir, " does not exist.")
    return(FALSE)
  }
  repbox_extract_readme_cand_from_zip(project_dir, zip_file)
  repbox_readme_files_to_txt(project_dir)
  repdb_add_readme(project_dir)
  return(TRUE)

}
