# scripts/update_fig_index.R

update_fig_index <- function(
    repo_root = ".",
    add_thumbnails = TRUE,
    thumbs_per_cat = 8,     # how many previews per category
    do_git = FALSE,
    update_root_readme = TRUE,   # also inject a preview block into top-level README
    root_readme_anchor = "<!-- SPARC_CONTEXT_PREVIEWS -->"  # marker in README.md
) {
  op <- options(stringsAsFactors = FALSE); on.exit(options(op), add = TRUE)
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  figs_dir  <- file.path(repo_root, "figs")
  if (!dir.exists(figs_dir)) dir.create(figs_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Helpers --------------------------------------------------------------------
  newest_dir <- function(base_dir, pattern) {
    d <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
    d <- d[grepl(pattern, d)]
    if (!length(d)) return(NULL)
    key <- sub("^.*_(\\d{8})$", "\\1", d)
    d[order(key, decreasing = TRUE)][1]
  }
  
  copy_dir_contents <- function(src, dst, max_files = Inf) {
    if (!dir.exists(src)) return(FALSE)
    if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    all_files <- list.files(src, full.names = TRUE, recursive = TRUE)
    pngs <- all_files[grepl("\\.png$", all_files, ignore.case = TRUE)]
    if (is.finite(max_files)) pngs <- head(pngs, max_files)
    if (!length(pngs)) return(TRUE)
    rel <- substr(pngs, nchar(src) + 2, nchar(pngs))
    subdirs <- unique(dirname(file.path(dst, rel)))
    for (sd in subdirs) if (!dir.exists(sd)) dir.create(sd, recursive = TRUE, showWarnings = FALSE)
    file.copy(pngs, file.path(dst, rel), overwrite = TRUE)
  }
  
  list_pngs <- function(dir_path, max_n = Inf) {
    if (!dir.exists(dir_path)) return(character(0))
    f <- list.files(dir_path, pattern = "\\.png$", full.names = FALSE, recursive = TRUE)
    head(f, max_n)
  }
  
  # 1) Determine newest float/satellite dated folders --------------------------
  newest_float <- newest_dir(figs_dir, "^float_composites_\\d{8}$")
  newest_sat   <- newest_dir(figs_dir, "^satellite_composites_\\d{8}$")
  
  # 2) Build materialized "latest_*" (copy, not symlink) -----------------------
  latest_float_dir <- file.path(figs_dir, "latest_float")
  latest_sat_dir   <- file.path(figs_dir, "latest_sat")
  if (!is.null(newest_float)) copy_dir_contents(file.path(figs_dir, newest_float), latest_float_dir, max_files = 200)
  if (!is.null(newest_sat))   copy_dir_contents(file.path(figs_dir, newest_sat),   latest_sat_dir,   max_files = 200)
  
  # 3) Build combined "latest" with subfolders float/ and satellite/ ----------
  combined_latest <- file.path(figs_dir, "latest")
  if (dir.exists(combined_latest)) unlink(combined_latest, recursive = TRUE, force = TRUE)
  dir.create(combined_latest, showWarnings = FALSE)
  if (dir.exists(latest_float_dir)) copy_dir_contents(latest_float_dir, file.path(combined_latest, "float"), max_files = 200)
  if (dir.exists(latest_sat_dir))   copy_dir_contents(latest_sat_dir,   file.path(combined_latest, "satellite"), max_files = 200)
  
  # 4) Write figs/README.md with archive lists + inline previews ---------------
  md <- c("# Figure Archives",
          "",
          "Quick links:",
          "- 🔹 **Combined latest:** [latest/](./latest/)",
          "- 🔹 **Latest floats:** [latest_float/](./latest_float/)",
          "- 🔹 **Latest satellite:** [latest_sat/](./latest_sat/)",
          "")
  
  add_section <- function(title, pattern) {
    entries <- list.dirs(figs_dir, full.names = FALSE, recursive = FALSE)
    entries <- entries[grepl(pattern, entries)]
    if (!length(entries)) return(character(0))
    keys <- sub("^.*_(\\d{8})$", "\\1", entries)
    entries <- entries[order(keys, decreasing = TRUE)]
    
    sec <- c(paste0("## ", title), "")
    for (d in entries) {
      sec <- c(sec, paste0("- [", d, "](./", d, "/)"))
      if (add_thumbnails) {
        pngs <- list_pngs(file.path(figs_dir, d), thumbs_per_cat)
        if (length(pngs)) {
          grid <- paste0("  ", paste0("![](./", d, "/", pngs, ")", collapse = " "))
          sec <- c(sec, "", grid, "")
        }
      }
    }
    c(sec, "")
  }
  
  md <- c(md,
          add_section("Float Composite Archives", "^float_composites_\\d{8}$"),
          add_section("Satellite Composite Archives", "^satellite_composites_\\d{8}$"))
  
  writeLines(md, file.path(figs_dir, "README.md"))
  message("Wrote ", file.path(figs_dir, "README.md"))
  
  # 5) (Optional) Inject a small preview block into repo root README.md --------
  if (isTRUE(update_root_readme)) {
    top <- file.path(repo_root, "README.md")
    if (file.exists(top)) {
      txt <- readLines(top, warn = FALSE)
      # Build preview block using combined latest
      float_imgs <- list_pngs(file.path(figs_dir, "latest", "float"), 4)
      sat_imgs   <- list_pngs(file.path(figs_dir, "latest", "satellite"), 4)
      block <- c(
        root_readme_anchor,
        "",
        "### Latest Previews",
        "",
        "**Floats**",
        if (length(float_imgs)) paste0("![](figs/latest/float/", float_imgs, ")", collapse = " ") else "_No float images yet._",
        "",
        "**Satellite**",
        if (length(sat_imgs)) paste0("![](figs/latest/satellite/", sat_imgs, ")", collapse = " ") else "_No satellite images yet._",
        "",
        root_readme_anchor
      )
      # Replace or append between anchors
      if (any(grepl(root_readme_anchor, txt, fixed = TRUE))) {
        i1 <- which(grepl(root_readme_anchor, txt, fixed = TRUE))[1]
        i2 <- which(grepl(root_readme_anchor, txt, fixed = TRUE))[length(which(grepl(root_readme_anchor, txt, fixed = TRUE)))]
        new_txt <- c(txt[1:(i1-1)], block, txt[(i2+1):length(txt)])
      } else {
        new_txt <- c(txt, "", block)
      }
      writeLines(new_txt, top)
      message("Updated ", top, " with inline previews.")
    }
  }
  
  if (isTRUE(do_git)) {
    owd <- getwd(); on.exit(setwd(owd), add = TRUE)
    setwd(repo_root)
    system2("git", c("add", "figs/README.md", "figs/latest", "figs/latest_float", "figs/latest_sat", "README.md"))
    msg <- sprintf("Update figure archive and previews (%s)", format(Sys.time(), "%Y-%m-%d %H:%M"))
    system2("git", c("commit", "-m", shQuote(msg)))
    system2("git", c("push"))
  }
  
  invisible(TRUE)
}

# Run if executed directly
if (identical(environment(), globalenv())) update_fig_index(repo_root = ".")
