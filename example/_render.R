render_report = function(dataset, ncomp = 15, scale = FALSE) {
  rmarkdown::render(
    "Report.Rmd", params = list(
      dataset = dataset,
      ncomp = ncomp,
      scale = scale
    ),
    output_file = paste0("Report-", dataset, "-", ncomp, ".pdf")
  )
}

frontmatter <- rmarkdown::yaml_front_matter("Report.Rmd")

for(dataset in frontmatter$params$dataset$choices) {
  render_report(dataset)
}
