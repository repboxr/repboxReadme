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

  for (file in files) {
    ext = tolower(tools::file_ext(files))
    if (startsWith(file,"~$")) next
    # just add .txt ending to original file name (including ext)
    out_file = paste0(out_dir, "/", file, ".txt")
    in_file = paste0(project_dir, "/readme/org/", file)
    file_out_dir = dirname(out_file)
    if (!dir.exists(file_out_dir)) dir.create(file_out_dir,recursive = TRUE)
    if (ext %in% c("","txt","md","html")) {
      file.copy(in_file, out_file)
    } else if (ext == "pdf") {
      convert_pdf_to_txt(in_file, out_file)
    } else if (ext %in% c("docx","doc","rtf","odt")) {
      text = readtext::readtext(in_file)$text
      writeLines(text, out_file)
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
