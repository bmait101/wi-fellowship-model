
colz <- tibble::tibble(
  group = c("UW-SG", "UW-WRI", "DOA-CMP", "DNR-DG", "DNR-FM", "DNR-WQ", "DHS", "EPA"), 
  cols = c("#e06666", "#ea9999", "#ffff75", "#3c78d8", "#6d9eeb", "#a4c2f4", "#d393ff", "#93c47d")
)

colz <- dplyr::arrange(colz, group)
