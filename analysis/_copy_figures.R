# Pre-render step: copy the pipeline's figures (../figures) into the Quarto
# project so they are published with the site. Base R only (runs --vanilla).
dir.create("figures", showWarnings = FALSE)
f <- list.files("../figures", "\\.png$", full.names = TRUE)
invisible(file.copy(f, "figures", overwrite = TRUE))
cat("copied", length(f), "figures\n")
