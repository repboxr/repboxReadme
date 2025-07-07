example = function() {

  library(repboxReadme)
  library(repboxAI)
  #rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key2.txt")
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dirs = list.dirs("~/repbox/projects_readme", recursive=FALSE)
  rgemini::run_gemini
  txt_dirs = file.path(project_dirs, "/readme/txt")
  project_dirs = project_dirs[dir.exists(txt_dirs)]
  length(project_dirs)

  inds = 1:length(project_dirs)
  inds = 1
  inds = 1701:2000
  i = inds[1]
  for (i in inds) {
    project_dir = project_dirs[i]
    fp_dir = file.path(project_dir, "fp")
    #if (dir.exists(fp_dir)) next
    cat("\n", i, "of", max(inds),project_dir,"\n")
    try(repbox_readme_ai_for_project(project_dir))
    Sys.sleep(10)
    cat(paste0("\nrstudioapi::filesPaneNavigate(\"",project_dir,"\")\n"))
  }

  rstudioapi::filesPaneNavigate("/home/rstudio/repbox/projects_readme/aejmac_14_2_4")
  files = list.files(project_dirs, glob2rx("*.txt"), recursive = TRUE)


  library(repboxAI)
  library(repboxReadme)
  steps = repbox_fp_readme_steps(readme_overview=TRUE,readme_data = TRUE, readme_vs_guide=TRUE)
  set_ai_opts(model = "gemini-2.5-flash-lite-preview-06-17")
  project_dirs = list.dirs("~/repbox/projects_readme", recursive=FALSE)
  #rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  repbox_rerun_outages(project_dirs, steps=steps)

}

repbox_readme_ai_for_project = function(project_dir = "~/repbox/projects_readme/aejapp_1_1_4") {
  library(repboxAI)
  library(aikit)


  #rstudioapi::filesPaneNavigate(project_dir)

  steps = repbox_fp_readme_steps(readme_overview=TRUE,readme_data = TRUE, readme_vs_guide=TRUE)

  #set_ai_opts(model = "gemini-2.5-flash")
  set_ai_opts(model = "gemini-2.5-flash-lite-preview-06-17")
  repbox_run_fp(project_dir,steps, overwrite = FALSE)

  #repbox_rerun_outages(project_dir,steps = steps,max_repeat = 1, sleep_sec = 20)
}



rx_agg_readme_ai = function() {
  library(repboxExplore)
  library(repboxAI)
  library(EconJournalScrap)

  parent_dir = "~/repbox/projects_readme"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)

  arts = ejs_load_agg("art") %>% select(artid, journ, year, title)

  df_ov = rai_agg_all_prod_df(project_dirs, "readme_overview") %>% left_join(arts)
  save_explore_rds(df_ov, "fp_readme_overview.Rds")
  rio::export(df_ov, "~/repbox/repbox_reports/fp_readme_overview.csv")

  df_g = rai_agg_all_prod_df(project_dirs, "readme_vs_guide") %>% left_join(arts) %>% select(artid, journ, year, everything())
  save_explore_rds(df_g, "fp_readme_vs_guide.Rds")

  rio::export(df_g, "~/repbox/repbox_reports/fp_readme_vs_guide.csv")


  art_df = df %>%
    group_by(artid) %>%
    summarize(
      num_readme = n(),
      num_is_readme = sum(is_reproduction_package_readme)
    )
  table(art_df$num_readme)

}
