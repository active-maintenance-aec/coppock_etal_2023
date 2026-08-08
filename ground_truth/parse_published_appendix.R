# coppock_etal_2023/ground_truth/parse_published_appendix.R
# Output: ground_truth/published_appendix_values.csv
# Depends on: the published Online Appendix PDF, and pdftotext on the PATH
# Description: Transcribe every cell of the published appendix's seven regression
#   tables. Each cell prints an estimate, a standard error, a significance marker
#   and a 95 per cent interval, so a table of 48 rows carries several hundred
#   published numbers and hand transcription is not an option.
#
#   The appendix is not redistributed with this repository. Point
#   PUBLISHED_MATERIALS_DIR at a directory holding coppock_etal_2023_appendix.pdf
#   and re-run; the committed CSV is the output of that run and is what the
#   ground truth reads.
#
#   Cells are read positionally within each table's own block of the text layer,
#   never in reading order across the document, and the table a cell belongs to
#   is taken from the caption above it rather than from a line number.

library(tidyverse)
library(here)

here::i_am("ground_truth/parse_published_appendix.R")

materials_dir <- Sys.getenv("PUBLISHED_MATERIALS_DIR",
                            unset = here::here("published_materials"))
appendix_pdf <- file.path(materials_dir, "coppock_etal_2023_appendix.pdf")

if (!file.exists(appendix_pdf)) {
  stop("Set PUBLISHED_MATERIALS_DIR to a directory holding coppock_etal_2023_appendix.pdf. ",
       "Looked in: ", materials_dir)
}

text_file <- tempfile(fileext = ".txt")
status <- system2("pdftotext", c("-layout", shQuote(appendix_pdf), shQuote(text_file)))
stopifnot(status == 0, file.exists(text_file))

lines <- read_lines(text_file)

# The regression tables live in appendix section 6 and the two three-wave tables
# that follow it. Section 8 opens the questionnaire, whose numbered response
# scales would otherwise be read as table cells, so the scan stops there.
outcomes_start <- which(str_detect(lines, "^\\s*8\\s+Outcomes\\s*$"))[1]
stopifnot(!is.na(outcomes_start))

captions <- tibble(
  line = which(str_detect(lines, "^\\s*Table \\d+: ")),
  caption = str_squish(lines[which(str_detect(lines, "^\\s*Table \\d+: "))])
) |>
  filter(line < outcomes_start) |>
  mutate(number = as.integer(str_extract(caption, "(?<=Table )\\d+")))

# Two tables in this range print no estimates: the sample composition table that
# opens the appendix, and the two hand-entered tables of section 7. They carry a
# different shape and are transcribed as claims rather than as cells.
regression_captions <- captions |>
  filter(str_detect(caption, "Average treatment effects|Conditional average|Persistence"))

stopifnot(nrow(regression_captions) == 7)

block_end <- c(regression_captions$line[-1], outcomes_start) - 1

cell_pattern <- "(-?\\d+\\.\\d+) \\((\\d+\\.\\d+)\\)(\\*?) \\[(-?\\d+\\.\\d+), (-?\\d+\\.\\d+)\\]"

# The row label is everything to the left of the first cell on the line, squished.
parse_block <- function(start, end, table_number, caption) {
  block <- lines[start:end]
  keep <- str_detect(block, cell_pattern)
  map(which(keep), function(i) {
    line <- block[i]
    matches <- str_match_all(line, cell_pattern)[[1]]
    label <- str_squish(str_sub(line, 1, str_locate(line, cell_pattern)[1, "start"] - 1))
    tibble(
      appendix_table = table_number,
      caption = caption,
      row_label = label,
      column_position = seq_len(nrow(matches)),
      estimate = as.numeric(matches[, 2]),
      std_error = as.numeric(matches[, 3]),
      significance_marker = matches[, 4],
      interval_first = as.numeric(matches[, 5]),
      interval_second = as.numeric(matches[, 6])
    )
  }) |>
    list_rbind()
}

published <- pmap(
  list(regression_captions$line, block_end, regression_captions$number,
       regression_captions$caption),
  parse_block
) |>
  list_rbind()

# The interval is printed low-then-high in some columns and high-then-low in
# others: the deposit negates the misinformation contrast without reversing its
# bounds, so those cells print reversed. Both orders are recorded as printed and
# the sorted pair is what any comparison uses.
published <- published |>
  mutate(
    interval_low = pmin(interval_first, interval_second),
    interval_high = pmax(interval_first, interval_second),
    interval_printed_descending = interval_first > interval_second
  )

write_csv(published, here::here("ground_truth", "published_appendix_values.csv"))

print(published |> count(appendix_table, caption))
print(tibble(
  cells = nrow(published),
  published_numbers = nrow(published) * 4L,
  starred = sum(published$significance_marker == "*"),
  descending_intervals = sum(published$interval_printed_descending)
))
