# scripts/update_fig_index.R
update_fig_index <- function(
    repo_root = ".", 
    categories = list(
      list(dir = "figs", pattern = "^float_composites_\\d{8}$",
           title = "Float Composite Archives", latest_name = "latest_float"),
      list(dir = "figs", pattern = "^satellite_composites_\\d{8}$",
           title = "Satellite Composite Archives", latest_name = "latest_sat")
    ),
    add_thumbnails = TRUE,
    max_thumbs_per_cat = 12,
    do_git = FALSE
) {
  op <- options(stringsAsFactors = FALSE); on.exit(options(op), add = TRUE)
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  figs_dir  <- file.path(repo_root, "figs")
  if (!dir.exists(figs_dir)) dir.create(figs_dir, recursive = TRUE, showWarnings = FALSE)
  
  md <- c("# Figure Archives\n")
  
  make_latest_pointer <- function(base_dir, dated_dirs, latest_name) {
    if (!length(dated_dirs)) return(invisible())
    latest_dir  <- dated_dirs[1]
    latest_path <- file.path(base_dir, latest_name)
    if (file.exists(latest_path) || dir.exists(latest_path)) unlink(latest_path, recursive = TRUE, force = TRUE)
    ok <- suppressWarnings(tryCatch(file.symlink(file.path(base_dir, latest_dir), latest_path),
                                    error = function(e) FALSE, warning = function(w) FALSE))
    if (!isTRUE(ok)) {
      dir.create(latest_path, recursive = TRUE, showWarnings = FALSE)
      src <- file.path(base_dir, latest_dir)
      fs <- list.files(src, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
      files_only <- fs[file.info(fs)$isdir == FALSE]
      rel <- substr(files_only, nchar(src)+2, nchar(files_only))
      unique_dirs <- unique(dirname(file.path(latest_path, rel)))
      for (d in unique_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
      file.copy(files_only, file.path(latest_path, rel), overwrite = TRUE)
    }
  }
  
  entries_by_cat <- list()
  
  for (cat in categories) {
    base_dir <- file.path(repo_root, cat$dir)
    if (!dir.exists(base_dir)) next
    entries <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
    entries <- entries[grepl(cat$pattern, entries)]
    if (!length(entries)) next
    
    # sort by YYYYMMDD desc
    keys <- sub("^.*_(\\d{8})$", "\\1", entries)
    entries <- entries[order(keys, decreasing = TRUE)]
    entries_by_cat[[cat$latest_name]] <- entries
    
    md <- c(md, "", paste0("## ", cat$title), "")
    make_latest_pointer(base_dir, entries, latest_name = cat$latest_name)
    md <- c(md, paste0("- 🔹 **Latest:** [", cat$latest_name, "](./", cat$latest_name, "/)"), "")
    
    max_show <- if (is.null(max_thumbs_per_cat)) length(entries) else min(length(entries), max_thumbs_per_cat)
    for (i in seq_along(entries)) {
      d <- entries[i]
      md <- c(md, paste0("- [", d, "](./", d, "/)"))
      if (add_thumbnails && i <= max_show) {
        pngs <- list.files(file.path(base_dir, d), pattern = "\\.png$", full.names = FALSE, recursive = TRUE)
        if (length(pngs)) {
          first_png <- pngs[1]
          md <- c(md, "",
                  "  <details><summary>Preview</summary>",
                  "",
                  paste0("  ![](", "./", d, "/", gsub("^\\./", "", first_png), ")"),
                  "",
                  "  </details>", "")
        }
      }
    }
    md <- c(md, "")
  }
  
  # Write figs/README.md
  out_file <- file.path(figs_dir, "README.md")
  writeLines(md, out_file)
  message("Wrote: ", out_file)
  
  # ---- NEW: build combined figs/latest/ that contains both sources ----------
  combine_latest <- function(figs_dir) {
    L <- file.path(figs_dir, "latest")
    if (file.exists(L) || dir.exists(L)) unlink(L, recursive = TRUE, force = TRUE)
    dir.create(L, showWarnings = FALSE)
    
    # helper to place link or copy as subfolder
    place <- function(src, name) {
      target <- file.path(L, name)
      if (!dir.exists(src)) return()
      ok <- suppressWarnings(tryCatch(file.symlink(src, target),
                                      error = function(e) FALSE, warning = function(w) FALSE))
      if (!isTRUE(ok)) {
        dir.create(target, recursive = TRUE, showWarnings = FALSE)
        fs <- list.files(src, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
        files_only <- fs[file.info(fs)$isdir == FALSE]
        rel <- substr(files_only, nchar(src)+2, nchar(files_only))
        unique_dirs <- unique(dirname(file.path(target, rel)))
        for (d in unique_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
        file.copy(files_only, file.path(target, rel), overwrite = TRUE)
      }
    }
    
    lf <- file.path(figs_dir, "latest_float")
    ls <- file.path(figs_dir, "latest_sat")
    place(lf, "float")
    place(ls, "satellite")
    message("Built combined latest/ with subfolders: float/ and satellite/")
  }
  combine_latest(figs_dir)
  
  if (isTRUE(do_git)) {
    owd <- getwd(); on.exit(setwd(owd), add = TRUE)
    setwd(repo_root)
    system2("git", c("add", "figs/README.md", "figs/latest", "figs/latest_float", "figs/latest_sat"))
    msg <- sprintf("Update figure archive index (%s)", format(Sys.time(), "%Y-%m-%d %H:%M"))
    system2("git", c("commit", "-m", shQuote(msg)))
    system2("git", c("push"))
  }
  
  invisible(out_file)
}

if (identical(environment(), globalenv())) update_fig_index(repo_root = ".")
