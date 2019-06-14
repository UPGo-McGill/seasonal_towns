figure <- list()

base_map <- tm_shape(DA, bbox = bb(st_bbox(DA), xlim=c(-0.02, 1.02),
                                        ylim=c(0.01, 1.05), relative = TRUE),
                     unit = "km") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(DA) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_layout(frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("right", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")


  
  base_map +
  tm_shape(DA) +
  tm_polygons(col = "v_CA16_2540: Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)", 
              palette = "Greens",
              border.col = "#f0f0f0",
              title = "") +
  tm_layout(title = "Figure 1. Title")

names(DA[,47])
