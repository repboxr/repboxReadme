example = function() {
  library(repboxExplore)
  library(repboxEJD)
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")

  pr_df = data.frame(project_dir = project_dirs, artid = tools::file_path_sans_ext(basename(project_dirs)))

  #ejd_make_zip_df()
  zip_df = repboxExplore::read_explore_rds("ejd_zip_df.Rds")

  zip_df = pr_df %>%
    left_join(zip_df, by="artid") %>%
    filter(!is.na(file))

  i = 1
  project_dir = zip_df$project_dir[i]
  zip_file = zip_df$file[i]
  cand_df = repbox_extract_readme_cand_from_zip(project_dir, zip_file,just_return_cand_df = !TRUE, remove_previous_files = TRUE)

  for (i in 1:NROW(zip_df)) {
    project_dir = zip_df$project_dir[i]
    zip_file = zip_df$file[i]
    if (dir.exists(file.path(project_dir,"readme","txt"))) next
    cat("\n",i," ", project_dir, "\n")
    try({
      res = repbox_extract_readme_cand_from_zip(project_dir, zip_file,remove_previous_files = TRUE)
    })
  }

  #rem_files = list.files(paste0(zip_df$project_dir,"/readme/txt"), glob2rx("*.*"), full.names = TRUE, recursive = TRUE,include.dirs = FALSE)
  #file.remove(rem_files)

  for (i in 1:NROW(zip_df)) {
    project_dir = zip_df$project_dir[i]
    repbox_readme_files_to_txt(project_dir)
  }


  for (i in 1:NROW(zip_df)) {
    project_dir = zip_df$project_dir[i]
    cat("\n",i," ", project_dir, "\n")
    repdb_add_readme(project_dir)
  }


  rstudioapi::filesPaneNavigate(project_dir)
}


repbox_extract_readme_cand_from_zip = function(project_dir, zip_file, just_return_cand_df=FALSE, remove_previous_files=FALSE) {
  restore.point("repbox_extract_readme_cand_from_zip")
  project_dir = normalizePath(project_dir)
  dest_dir = file.path(project_dir, "readme/org")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  } else if (remove_previous_files) {
    rem_files = list.files(dest_dir,full.names = TRUE,include.dirs = FALSE,recursive = TRUE)
    suppressWarnings(file.remove(rem_files))
    rem_files = list.files(file.path(project_dir, "readme/txt"),full.names = TRUE, include.dirs = FALSE, recursive = TRUE)
    suppressWarnings(file.remove(rem_files))
  }
  cand_df = find_readme_cand_in_zip(zip_file)

  if (NROW(cand_df)==0) {
    cat("\nNo candidates for README files found for ", project_dir)
    return(NULL)
  }

  cand_df$file_path = cand_df$file
  cand_df$file_name = basename(cand_df$file)

  saveRDS(cand_df, file.path(project_dir, "readme/readme_cand.Rds"))
  if (just_return_cand_df) return(cand_df)

  unzip(zip_file,files = cand_df$file, junkpaths = FALSE,exdir = dest_dir)
  cand_df$dest_file = file.path(project_dir, "readme/org", cand_df$base)
  invisible(cand_df)
}

find_readme_cand_in_zip = function(zip_file, ...) {
  restore.point("extract_readme_candidates_from_zip")
  files = unzip(zip_file, list=TRUE)$Name
  cand_df = find_readme_file_candidates(files)
  cand_df
}

# Heuristically find README files candidates among a list of files
find_readme_file_candidates = function(files, max_cand=Inf, min_score_not_best=60L, ...) {
  restore.point("find_readme_file_candidates")

  files = files[!has.substr(files, "__MACOSX")]
  files = files[!endsWith(files, "/")]
  files=  files[!startsWith(basename(files),"~")]
  files=  files[!startsWith(basename(files),"._")]

  dir = dirname(files)
  base = basename(tolower(files))

  noext = tools::file_path_sans_ext(base)
  ext=tools::file_ext(base)

  score = rep(0L, length(files))

  text_exts = c("pdf","txt","","docx","rtf","md","doc","html","odt")
  ext_ok = ext %in% text_exts
  names(ext_ok) = base

  rows =  which(noext %in% c("readme","read_me","read me") & ext_ok)
  score[rows] = score[rows] + 100L

  rows = which((startsWith(base,"readme") | startsWith(base,"read_me") | startsWith(base,"read me")) & ext_ok)
  score[rows] = score[rows] + 80L

  rows  = which( (has.substr(base,"readme") | has.substr(base,"read_me")) & ext_ok)
  score[rows] = score[rows] + 60L

  rows  = which(has.substr(base,"description") & ext_ok)
  score[rows] = score[rows] + 50L

  rows  = which(has.substr(base,"replicat") & ext_ok)
  score[rows] = score[rows] + 40L

  rows  = which((ext %in% c("pdf","doc","docx","md","odt")))
  score[rows] = score[rows] + 5L

  df = data.frame(file = files, base=base, ext=ext, score=score) %>%
    filter(score > 0) %>%
    arrange(desc(score))

  if (NROW(df)==0) return(df)

  if (NROW(df)>max_cand) {
    df = df[1:max_cand,,drop=FALSE]
  }

  keep_rows = df$score >= min_score_not_best | 1:NROW(df) == 1
  df = df[keep_rows,, drop=FALSE]

  df
}


repbox_readme_txt_files = function(project_dir) {
  list.files(file.path(project_dir, "readme","txt"),glob2rx("*"), full.names = TRUE)
}


repbox_readme_txt_files = function(project_dir, full.names=FALSE) {
  list.files(file.path(project_dir, "readme","txt"),glob2rx("*.*"), full.names = full.names, recursive = TRUE)
}

repbox_readme_txt_to_org_file_name = function(files) {
  mfiles = tools::file_path_sans_ext(files)
  keep_org = !stri_detect_fixed(mfiles,pattern = ".")
  mfiles[keep_org] = files[keep_org]
  mfiles
}
