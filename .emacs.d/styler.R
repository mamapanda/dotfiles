#!/usr/bin/env Rscript
library(styler)

stdin <- file("stdin")
contents <- readLines(stdin)
style_text(contents, transformers = tidyverse_style(indent_by = 4))
