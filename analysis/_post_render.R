# Post-render step: GitHub Pages needs .nojekyll in the published folder so it
# serves Quarto's site_libs/ as-is. Base R only (runs --vanilla).
file.create("../docs/.nojekyll")
