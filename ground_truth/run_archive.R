# coppock_etal_2023/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (fetched by download_original.R)
# Description: Run every deposited script in a scratch copy of the archive and
#   record where each one stopped. The deposit is run twice: once exactly as
#   shipped, and once stripped to data plus code, so that a script which passes
#   only because the deposit ships the object it reads is visible as a failure.
#   The scratch copy also answers a question the checksum gate cannot: which
#   deposited files the deposit's own code overwrites when it is run in place.
#
#   Both tables are long, one row per script per pass and one row per file per
#   pass. The stripped pass runs fit_all_models.R first, because every analysis
#   script reads a model object only that script builds, so a stripped pass in
#   any other order records only that the objects are missing.

library(tidyverse)
library(here)

here::i_am("ground_truth/run_archive.R")

# Where the deposit is copied to. An environment variable so the script is
# usable by anyone with the repo; nothing is ever run inside original/.
archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "coppock_etal_2023_archive"))
stopifnot(nzchar(archive_run_dir))

original_dir <- here::here("original")
stopifnot(dir.exists(original_dir))

# The deposit's fourteen documented scripts, in the order a reader meets them in
# README.txt. fit_all_models.R runs last in the as-shipped pass so that the
# analysis scripts see the deposited fits rather than regenerated ones.
analysis_scripts <- c(
  "figure_1.R", "figure_2.R", "figure_3.R", "figure_4.R", "figure_5.R", "figure_6.R",
  "appendix_figures_1_and_2.R", "appendix_figure_3.R", "appendix_figure_4_5_6_and_7.R",
  "appendix_figure_8.R", "appendix_figure_9.R",
  "appendix_table_1.R", "appendix_tables_2_3_4_5_6_and_7.R"
)
fit_script <- "fit_all_models.R"

# A committed file records properties of the deposit, never of the machine that
# ran it, so any absolute path in a captured error message is removed. A regex
# rather than a literal: R resolves tempdir() through /private and can hand back
# a doubled slash, which literal replacement misses.
scrub <- function(x) str_replace_all(x, "(/private)?/+[^ '\"]*?/(shipped|stripped)/+", "<scratch>/")

make_copy <- function(dest, strip) {
  unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(original_dir, full.names = TRUE), dest, recursive = TRUE)
  if (strip) {
    # Data plus code. The deposited model fits are derived objects, so they go;
    # the directory stays, because fit_all_models.R writes into it.
    unlink(list.files(file.path(dest, "fitted_models"), full.names = TRUE))
  }
  invisible(dest)
}

# Run one script in its own R session, from the copy's root, and record where it
# stopped. The timeout is tested before the error text: a killed script's dying
# message is not the reason it stopped.
run_script <- function(dir, script, pass, timeout = 3600) {
  path <- file.path("code", script)
  started <- Sys.time()
  result <- system2("Rscript", c("--vanilla", shQuote(path)),
                    stdout = TRUE, stderr = TRUE, timeout = timeout)
  status <- attr(result, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  # The timeout is tested before the error text. A killed script's dying message
  # is not the reason it stopped, and filing it as one turns a resource limit
  # into a code defect.
  timed_out <- status == 124L || elapsed >= timeout
  error_line <- result[str_detect(result, "^Error")] |> head(1)
  tibble(
    pass = pass,
    script = script,
    exit_status = status,
    outcome = case_when(
      timed_out ~ "timeout",
      status == 0 ~ "clean",
      TRUE ~ "error"
    ),
    stopped_at = if (length(error_line) == 0) NA_character_ else scrub(str_squish(error_line)),
    log = paste(result, collapse = "\n")
  )
}

run_pass <- function(dir, scripts, pass) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  map(scripts, function(s) run_script(dir, s, pass)) |> list_rbind()
}

# As shipped ----
shipped_dir <- file.path(archive_run_dir, "shipped")
make_copy(shipped_dir, strip = FALSE)

deposited_fits <- list.files(file.path(shipped_dir, "fitted_models"))
mtime_before <- file.mtime(file.path(shipped_dir, "fitted_models", deposited_fits))

# Whether a file a pass wrote holds the bytes the deposit ships under that name.
matches_deposit <- function(dir, files) {
  unname(tools::md5sum(file.path(dir, files)) == tools::md5sum(file.path(original_dir, files)))
}

shipped <- run_pass(shipped_dir, analysis_scripts, "as_shipped")
shipped_fit <- run_pass(shipped_dir, fit_script, "as_shipped")

# What running the deposit in place would have cost. Only the names are kept:
# a modification time is a property of the run, not of the deposit.
mtime_after <- file.mtime(file.path(shipped_dir, "fitted_models", deposited_fits))
overwritten <- file.path("fitted_models",
                         deposited_fits[!is.na(mtime_after) & mtime_after > mtime_before])
added <- setdiff(
  list.files(shipped_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  list.files(original_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
)

shipped_overwrites <- bind_rows(
  tibble(file = overwritten, effect = "overwritten",
         matches_deposit = matches_deposit(shipped_dir, overwritten)),
  tibble(file = added, effect = "added", matches_deposit = NA)
)

# Stripped to data plus code ----
stripped_dir <- file.path(archive_run_dir, "stripped")
make_copy(stripped_dir, strip = TRUE)

stripped_fit <- run_pass(stripped_dir, fit_script, "stripped")
stripped <- run_pass(stripped_dir, analysis_scripts, "stripped")

# Every model object the deposit ships is withheld from this pass, so each one has
# a row: rebuilt where the deposit's own code wrote it again, withheld where it
# did not.
rebuilt <- file.exists(file.path(stripped_dir, "fitted_models", deposited_fits))
stripped_added <- setdiff(
  list.files(stripped_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  list.files(original_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
)

stripped_overwrites <- bind_rows(
  tibble(file = file.path("fitted_models", deposited_fits[rebuilt]), effect = "rebuilt") |>
    mutate(matches_deposit = matches_deposit(stripped_dir, file)),
  tibble(file = file.path("fitted_models", deposited_fits[!rebuilt]), effect = "withheld",
         matches_deposit = NA),
  tibble(file = stripped_added, effect = "added", matches_deposit = NA)
)

overwrites <- bind_rows(as_shipped = shipped_overwrites, stripped = stripped_overwrites,
                        .id = "pass") |>
  arrange(pass, effect, file, .locale = "en")

# Every script run here is an analysis script: the deposit ships no cleaning code,
# and helpers.R, which the others source, is never run on its own and has no row.
status <- bind_rows(shipped, shipped_fit, stripped_fit, stripped) |>
  mutate(role = "analysis")

# The log is a property of the run and carries scratch paths, so it is printed
# and not committed.
walk(which(status$outcome != "clean"), function(i) {
  print(str_glue("[{status$pass[i]}] {status$script[i]}: {status$outcome[i]}"))
  print(status$stopped_at[i])
})

stopifnot(nrow(status) == 2 * (length(analysis_scripts) + 1),
          !anyDuplicated(status[c("pass", "script")]))

status |>
  select(pass, script, role, exit_status, outcome, stopped_at) |>
  write_csv(here::here("ground_truth", "archive_run_status.csv"))

write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(status |> count(pass, outcome))
print(str_glue("Scratch copy: {archive_run_dir}"))
