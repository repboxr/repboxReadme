# Often Readme files are duplicated in reproduction packages
# E.g. a .PDF and a .txt version
# If two files are too similar, we want to keep only
# one readme file.

example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  repbox_load_or_make_readme_ranks(project_dir)

  project_dirs = list.dirs("~/repbox/projects_share", recursive = FALSE)
  for (project_dir in project_dirs) {
    repbox_load_or_make_readme_ranks(project_dir)
  }

  rstudioapi::filesPaneNavigate(project_dir)
}

repbox_load_or_make_readme_ranks = function(project_dir, overwrite=FALSE) {
  restore.point("repbox_load_or_make_readme_ranks")
  file = file.path(project_dir, "readme", "readme_ranks.Rds")
  if (file.exists(file) & !overwrite) {
    df = readRDS(file)
  } else {
    df = repbox_make_readme_ranks(project_dir)
  }
  df
}

repbox_make_readme_ranks = function(project_dir) {
  restore.point("repbox_make_readme_ranks")
  txt_files = repbox_readme_txt_files(project_dir)
  if (length(txt_files)==0) return(NULL)
  long_files = file.path(project_dir, "readme", "txt", txt_files)
  org_files = repbox_readme_txt_to_org_file_name(txt_files)
  org_ext = tools::file_ext(org_files)

  df = tibble(org_file = org_files, ext=org_ext, txt_file = txt_files, score = score_readme_files(org_files)) %>%
    arrange(desc(score), nchar(org_file)) %>%
    mutate(rank = seq_len(n()))

  if (NROW(df)>1) {
    all_sim_df = repbox_readme_sim(long_files, org_files) %>%
      left_join(df %>% select(org_file, rank), by="org_file") %>%
      left_join(df %>% select(other_file = org_file, other_rank=rank), by="other_file")

    sim_df = all_sim_df %>%
      group_by(org_file) %>%
      summarize(
        better_rank_sim = max(c(0,sim[other_rank < rank]))
      )

    df = df %>%
      left_join(sim_df, by="org_file") %>%
      mutate(ignore = better_rank_sim > 0.9)
  } else {
    df$better_rank_sim = 0
    df$ignore = FALSE
  }

  saveRDS(df, file.path(project_dir, "readme", "readme_ranks.Rds"))
  invisible(df)
}

score_readme_files = function(files) {
  score = rep(0, NROW(files))
  noext = tolower(tools::file_path_sans_ext(basename(files)))
  base = tolower(basename(files))
  ext = tools::file_ext(files)

  rows =  which(noext %in% c("readme","read_me","read me"))
  score[rows] = score[rows] + 100L

  rows = which((startsWith(base,"readme") | startsWith(base,"read_me") | startsWith(base,"read me")))
  score[rows] = score[rows] + 80L

  rows  = which( (has.substr(base,"readme") | has.substr(base,"read_me")))
  score[rows] = score[rows] + 60L

  rows  = which(has.substr(base,"description"))
  score[rows] = score[rows] + 50L

  rows  = which(has.substr(base,"replicat"))
  score[rows] = score[rows] + 40L

  rows  = which((ext %in% c("md","txt")))
  score[rows] = score[rows] + 25L
  rows  = which((ext %in% c("doc","docx","md","odt","rtf")))
  score[rows] = score[rows] + 5L
  score
}

repbox_readme_sim = function(long_files, org_files) {
  txt_vec = sapply(long_files, function(file) paste0(readLines(file,warn=FALSE), collapse="\n"))
  comb <- combn(seq_along(txt_vec), 2)

  i = 1
  df <- bind_rows(lapply(1:NCOL(comb), function(i) {
    i1 = comb[1,i];i2 = comb[2,i]
    txt1 <- txt_vec[i1]
    txt2 <- txt_vec[i2]
    tibble(org_file1 = org_files[i1], org_file2=org_files[i2], sim=readme_txt_similarity(txt1, txt2))
  }))

  sim_df = bind_rows(
    df %>% select(org_file = org_file1, other_file = org_file2, sim=sim),
    df %>% select(org_file = org_file2, other_file = org_file1, sim=sim)
  )


  sim_df
}

readme_txt_similarity = function(txt1, txt2) {
  nc1 = nchar(txt1)
  nc2 = nchar(txt2)
  if (nc1 > 1.5*nc2) return(0)
  if (nc2 > 1.5*nc1) return(0)
  if (nc1==nc2) {
    if (txt1==txt2) return(1)
  }
  library(stringdist)

  # don't compare everything since it will become too long
  str1 = stri_sub(txt1,1,10000) %>%
    stri_replace_all_regex("[ \t]+"," ") %>%
    stri_replace_all_regex("[-]+","-") %>%
    stri_replace_all_regex("[=]+","=") %>%
    stri_sub(1,5000)

  str2 = stri_sub(txt2,1, 10000)%>%
    stri_replace_all_regex("[ \t]+"," ") %>%
    stri_replace_all_regex("[-]+","-") %>%
    stri_replace_all_regex("[=]+","=")  %>%
    stri_sub(1,5000)

  #sim = stringdist::stringsim(str1, str2,method = "osa")
  sim = stringdist::stringsim(str1, str2,method = "lcs")
  sim


}
