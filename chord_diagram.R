# Mobile App Data Analysis and Visualization
# Author: Owais Raza (took help with Git's copilot)
# Date: June 01 2025
# Listening to: Coke Studio Season 7| Hans Dhuni| Ustad Raees Khan
# Description: Creating chord_diagram

required_packages <- c("dplyr", "tidyr", "stringr", "showtext", "circlize", "reshape2")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
lapply(required_packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

df <- readRDS("cleaned_appdata_with_haddon.rds")

library(showtext)
font_add_google("Crimson Pro", "crimson")
showtext_auto()

factors <- c("Human", "Vehicle", "Physical Environment", "Social Environment")
chord_data <- df %>%
  filter(stringr::str_detect(Haddon_top_factor, "&")) %>%
  mutate(Factor_List = strsplit(Haddon_top_factor, " & ")) %>%
  select(`App name`, Factor_List) %>%
  tidyr::unnest_longer(Factor_List)
pairs <- chord_data %>%
  group_by(`App name`) %>%
  summarise(pairs = list(t(combn(Factor_List, 2))), .groups = "drop") %>%
  tidyr::unnest(cols = c(pairs)) %>%
  mutate(Factor1 = pairs[,1], Factor2 = pairs[,2]) %>%
  count(Factor1, Factor2)
mat <- reshape2::acast(pairs, Factor1 ~ Factor2, value.var = "n", fill = 0)
mat[is.na(mat)] <- 0
mat <- mat + t(mat)
create_chord_plot <- function() {
  circos.clear()
  circos.par(start.degree = 80, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  par(mar = rep(2, 4), family = "crimson")
  chordDiagram(
    mat, 
    grid.col = c(
      "Human"="#f37021",
      "Vehicle"="#0db14b",
      "Physical Environment"="#cc004c",
      "Social Environment"="#135FEB"
    ), 
    scale = F,
    transparency = .7,
    directional = 1,
    direction.type = c("diffHeight"), 
    diffHeight  = 0,
    annotationTrack = "grid", 
    annotationTrackHeight = c(0.09, 0.09),
    link.sort = F, 
    link.largest.ontop = F
  )
  circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      if(sector.index == "Social Environment") {
        circos.text(x = mean(xlim), y = 2.5, labels = "Social", facing = "bending", cex = 1.5)
        circos.text(x = mean(xlim), y = 2.0, labels = "Environment", facing = "bending", cex = 1.5)
      } else {
        circos.text(x = mean(xlim), y = 2.5, labels = sector.index, facing = "bending", cex = 1.5)
      }
      circos.axis(h = "top", labels.niceFacing = T)
    }
  )
  title("Interrelationship of Haddon's Matrix Factors Across Apps", family = "crimson", cex.main = 1.5)
}
png(filename = "chord_diagram.png", width = 10, height = 10, units = "in", res = 900)
create_chord_plot()
dev.off()
jpeg(filename = "chord_diagram.jpg", width = 10, height = 10, units = "in", res = 900, quality = 100)
create_chord_plot()
dev.off()