example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  project_dir = "~/repbox/projects_share/restat_2689366"
  repbox_readme_files_to_txt(project_dir)
}

repbox_readme_files_to_txt = function(project_dir) {
  restore.point("repbox_readme_files_to_txt")
  dir = file.path(project_dir, "readme/org")
  files = list.files(dir, full.names = FALSE,include.dirs = FALSE,recursive = TRUE)
  if(length(files)==0) {
    cat("\nNo readme file candidates in ", dir,"\n")
    return(invisible())
  }

  out_dir = file.path(project_dir, "readme/txt")
  if (!dir.exists(out_dir)) dir.create(out_dir)

  file = files[1]
  for (file in files) {
    ext = tolower(tools::file_ext(file))
    if (startsWith(file,"~$")) next
    # just add .txt ending to original file name (including ext)
    in_file = paste0(project_dir, "/readme/org/", file)
    out_file = paste0(out_dir, "/", file)
    file_out_dir = dirname(out_file)
    if (!dir.exists(file_out_dir)) dir.create(file_out_dir,recursive = TRUE)
    if (ext %in% c("","txt","md","html")) {
      file.copy(in_file, out_file)
    } else if (ext == "pdf") {
      out_file = paste0(out_dir, "/", file, ".txt")
      convert_pdf_to_txt(in_file, out_file)
      # correct some common errors
      txt = readLines(out_file,warn = FALSE)
      txt = replace_wrong_pdf_txt_chars(txt)
      #txt <- stri_replace_all_fixed(txt, "\uFB01", "fi")
      writeLines(txt, out_file)
    } else if (ext %in% "doc") {
      restore.point("convert_doc")
      out_file = paste0(out_dir, "/", file, ".md")
      cmd = paste0('antiword "', in_file, '" > "', out_file,'"')
      system(cmd)
    } else if (ext %in% c("docx","rtf","odt")) {
      out_file = paste0(out_dir, "/", file, ".md")
      rmarkdown::pandoc_convert(in_file, output = out_file)
      #text = readtext::readtext(in_file)$text
      #writeLines(text, out_file)
    } else {
      stop(paste0("\nUnknown README file type: ", in_file))
    }
  }
}


convert_pdf_to_txt = function(in_file, out_file) {
  restore.point("convert_pdf_to_txt")
  in_file = normalizePath(in_file)
  out_file = normalizePath(out_file,mustWork = FALSE)
  cmd = paste0('pdftotext -q -layout "',in_file,'" "', out_file,'"')
  res = suppressWarnings(system(cmd,intern = TRUE))
}

replace_wrong_pdf_txt_chars = function(txt, incorrect_char = "") {
  txt = stri_replace_all_fixed(txt, "⫺","-")
  txt = stri_replace_all_fixed(txt,"�","-")
  txt = stri_replace_all_fixed(txt, "â€‹","  ")
  txt = stri_replace_all_fixed(txt, "…", "fi")
  txt = stri_replace_all_fixed(txt, "‡", "fl")
  # Also ensure that incompatible UTF-8 characters are completely removed
  txt = iconv(txt, from = "UTF-8", to = "UTF-8", sub = incorrect_char)
  txt
}

