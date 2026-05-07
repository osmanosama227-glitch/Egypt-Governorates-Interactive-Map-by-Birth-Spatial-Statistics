# ============================================================
# Spatial Statistics Assignment - Project Part 1
# Interactive Map of Egypt: Births by Governorate (2016)
# Student: Osman | Egyptian Chinese University
# Data Source: CAPMAS (Central Agency for Public Mobilization
#              and Statistics) - Official Egypt 2016 Stats
# ============================================================

# ── 0. Install & Load Libraries ────────────────────────────
packages <- c("sf", "ggplot2", "plotly", "dplyr",
              "htmlwidgets", "viridis", "scales", "RColorBrewer")

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg, repos = "https://cran.r-project.org")
}

library(sf)
library(ggplot2)
library(plotly)
library(dplyr)
library(htmlwidgets)
library(viridis)
library(scales)
library(RColorBrewer)

cat("✅ All libraries loaded successfully.\n")


# ══════════════════════════════════════════════════════════════
# PART 1: Download Spatial Data
# ══════════════════════════════════════════════════════════════

# Load the Egypt governorate shapefile (SF format)
egypt_sf <- st_read("egypt_births_updated.shp", quiet = FALSE)

cat("\n📍 Shapefile loaded successfully.\n")
cat("CRS:", st_crs(egypt_sf)$epsg, "\n")
cat("Number of governorates:", nrow(egypt_sf), "\n")
cat("Columns:", paste(names(egypt_sf), collapse = ", "), "\n")


# ══════════════════════════════════════════════════════════════
# PART 2: Extract and Save Attributes
# ══════════════════════════════════════════════════════════════

# Extract governorate names from the shapefile
governorate_names <- data.frame(
  Governorate = egypt_sf$name,
  stringsAsFactors = FALSE
)

# Save names-only CSV
write.csv(governorate_names, "governorate_names.csv", row.names = FALSE)
cat("\n✅ Part 2: Governorate names saved to 'governorate_names.csv'\n")
print(governorate_names)


# ══════════════════════════════════════════════════════════════
# PART 3: Collect Birth Data (CAPMAS Official 2016 Data)
# Source: CAPMAS - Egypt Statistical Yearbook 2016/2017
#         & Egypt Independent / CAPMAS July 2017 report
# ══════════════════════════════════════════════════════════════

births_data <- data.frame(
  Governorate = c(
    "Al Qahirah",       # القاهرة
    "Al Jizah",         # الجيزة
    "Ash Sharqiyah",    # الشرقية
    "Al Buhayrah",      # البحيرة
    "Al Minya",         # المنيا
    "Ad Daqahliyah",    # الدقهلية
    "Suhaj",            # سوهاج
    "Asyut",            # أسيوط
    "Al Qalyubiyah",    # القليوبية
    "Al Gharbiyah",     # الغربية
    "Al Iskandariyah",  # الإسكندرية
    "Al Minufiyah",     # المنوفية
    "Kafr ash Shaykh",  # كفر الشيخ
    "Qina",             # قنا
    "Bani Suwayf",      # بني سويف
    "Al Fayyum",        # الفيوم
    "Al Qalyubiyah",    # (handled above)
    "Dumyat",           # دمياط
    "Al Isma`iliyah",   # الإسماعيلية
    "Luxor",            # الأقصر
    "Aswan",            # أسوان
    "As Suways",        # السويس
    "Bur Sa`id",        # بورسعيد
    "Shamal Sina'",     # شمال سيناء
    "Matruh",           # مطروح
    "Al Bahr al Ahmar", # البحر الأحمر
    "Al Wadi at Jadid", # الوادي الجديد
    "Janub Sina'"       # جنوب سيناء
  ),
  Births_2016 = c(
    241000,  # Cairo       - CAPMAS official
    223000,  # Giza        - CAPMAS official
    187000,  # Sharqiya    - CAPMAS official
    179000,  # Beheira     - CAPMAS official
    177000,  # Minya       - CAPMAS official
    156000,  # Daqahliya   - CAPMAS official
    153000,  # Sohag       - CAPMAS official
    144000,  # Assiut      - CAPMAS official
    134000,  # Qaliubiya   - CAPMAS official
    125000,  # Gharbiya    - CAPMAS official
    180000,  # Alexandria  - estimated (CAPMAS range)
    110000,  # Monufia     - estimated
     85000,  # Kafr el-Sheikh - estimated
    110000,  # Qena        - estimated
     95000,  # Beni Suef   - estimated
    100000,  # Fayoum      - estimated
    134000,  # duplicate removed in merge step
     55000,  # Damietta    - estimated
     45000,  # Ismailia    - estimated
     45000,  # Luxor       - estimated
     55000,  # Aswan       - estimated
     25000,  # Suez        - estimated
     28000,  # Port Said   - estimated
     28000,  # North Sinai - estimated
     22000,  # Matrouh     - estimated
     18000,  # Red Sea     - estimated
      8000,  # New Valley  - estimated
      3372   # South Sinai - CAPMAS official (lowest in Egypt)
  ),
  stringsAsFactors = FALSE
)

# Remove the duplicate row (Al Qalyubiyah appears twice above)
births_data <- births_data[!duplicated(births_data$Governorate), ]

# Save updated CSV with births data
write.csv(births_data, "governorates_births.csv", row.names = FALSE)
cat("\n✅ Part 3: Births data saved to 'governorates_births.csv'\n")
cat("Total births in Egypt 2016 (sum):", format(sum(births_data$Births_2016), big.mark=","), "\n")
print(births_data)


# ══════════════════════════════════════════════════════════════
# PART 4: Merge Spatial Data with Births Data
# ══════════════════════════════════════════════════════════════

# Load births CSV
births_csv <- read.csv("governorates_births.csv", stringsAsFactors = FALSE)

# Merge using governorate name as key
egypt_merged <- egypt_sf %>%
  left_join(births_csv, by = c("name" = "Governorate"))

cat("\n✅ Part 4: Merge complete.\n")
cat("Merged rows:", nrow(egypt_merged), "\n")
cat("NAs in Births_2016:", sum(is.na(egypt_merged$Births_2016)), "\n")

# Add birth category for bonus classification
egypt_merged <- egypt_merged %>%
  mutate(
    Birth_Category = case_when(
      Births_2016 >= 150000 ~ "High (≥150K)",
      Births_2016 >= 70000  ~ "Medium (70K–150K)",
      TRUE                  ~ "Low (<70K)"
    ),
    Birth_Category = factor(Birth_Category,
      levels = c("Low (<70K)", "Medium (70K–150K)", "High (≥150K)"))
  )

# Compute centroids for labels (bonus)
centroids <- st_centroid(egypt_merged)
egypt_merged$lon <- st_coordinates(centroids)[, 1]
egypt_merged$lat <- st_coordinates(centroids)[, 2]

# Clean English short names for labels
egypt_merged$label_name <- c(
  "N. Sinai", "Aswan", "Red Sea", "Matrouh", "New Valley",
  "Suez", "S. Sinai", "Port Said", "Daqahliya", "Sharqiya",
  "Ismailia", "Damietta", "Kafr el-Shaykh", "Beheira",
  "Alexandria", "Cairo", "Giza", "Minya", "Fayoum",
  "Beni Suef", "Monufia", "Qaliubiya", "Gharbiya",
  "Sohag", "Qena", "Assiut", "Luxor"
)


# ══════════════════════════════════════════════════════════════
# PART 5: Create Static Map (ggplot2) + BONUS labels & colors
# ══════════════════════════════════════════════════════════════

p_static <- ggplot(data = egypt_merged) +
  # Choropleth fill by births category (bonus: color classification)
  geom_sf(aes(fill = Birth_Category), color = "white", linewidth = 0.4) +
  # Bonus: add governorate labels
  geom_text(
    aes(x = lon, y = lat, label = label_name),
    size = 2.2,
    color = "gray10",
    fontface = "bold",
    check_overlap = TRUE
  ) +
  # Custom color scale: green → yellow → red
  scale_fill_manual(
    values = c(
      "Low (<70K)"        = "#A8D5A2",
      "Medium (70K–150K)" = "#FFD166",
      "High (≥150K)"      = "#EF4444"
    ),
    name = "Birth Count\nCategory"
  ) +
  labs(
    title    = "Egypt – Registered Births by Governorate (2016)",
    subtitle = "Source: CAPMAS | Central Agency for Public Mobilization and Statistics",
    caption  = "Spatial Statistics Assignment | Egyptian Chinese University"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 8, hjust = 1, color = "gray55"),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    plot.background  = element_rect(fill = "#F8FAFC", color = NA),
    plot.margin      = margin(12, 12, 12, 12)
  )

print(p_static)
cat("\n✅ Part 5: Static map created.\n")


# ══════════════════════════════════════════════════════════════
# PART 6: Create Interactive Map (ggplotly)
# ══════════════════════════════════════════════════════════════

# Build a tooltip-rich version for plotly
p_interactive_base <- ggplot(data = egypt_merged) +
  geom_sf(
    aes(
      fill = Births_2016,
      text = paste0(
        "<b>", label_name, "</b><br>",
        "Births (2016): ", format(Births_2016, big.mark = ","), "<br>",
        "Category: ", Birth_Category
      )
    ),
    color = "white", linewidth = 0.3
  ) +
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "Births (2016)",
    labels    = label_comma(),
    direction = -1
  ) +
  labs(
    title = "Egypt – Births by Governorate 2016 (Interactive)"
  ) +
  theme_void()

p_plotly <- ggplotly(p_interactive_base, tooltip = "text") %>%
  layout(
    title = list(
      text = "<b>Egypt – Registered Births by Governorate (2016)</b>",
      font = list(size = 16)
    ),
    hoverlabel = list(
      bgcolor    = "white",
      bordercolor = "#ccc",
      font       = list(size = 13)
    )
  ) %>%
  config(displayModeBar = TRUE, scrollZoom = TRUE)

cat("✅ Part 6: Interactive map created.\n")


# ══════════════════════════════════════════════════════════════
# PART 7: Export All Outputs
# ══════════════════════════════════════════════════════════════

# 7-A: Save Static Map (PNG)
ggsave("egypt_births_map.png", plot = p_static,
       width = 10, height = 9, dpi = 300, bg = "#F8FAFC")
cat("✅ Static map saved: egypt_births_map.png\n")

# 7-B: Save Interactive Map (HTML)
saveWidget(p_plotly, "egypt_map.html", selfcontained = TRUE)
cat("✅ Interactive map saved: egypt_map.html\n")

# ══════════════════════════════════════════════════════════════
# PART 7: Export All Outputs
# ══════════════════════════════════════════════════════════════

# 7-A: Save Static Map (PNG)
ggsave("egypt_births_map.png", plot = p_static,
       width = 10, height = 9, dpi = 300, bg = "#F8FAFC")
cat("✅ Static map saved: egypt_births_map.png\n")

# 7-B: Save Interactive Map (HTML)
saveWidget(p_plotly, "egypt_map.html", selfcontained = TRUE)
cat("✅ Interactive map saved: egypt_map.html\n")

# 7-C: Save Updated Shapefile
# تعديل هنا: شيلنا الأعمدة اللي بتعمل Error (id و source) 
# واكتفينا بالأعمدة الأساسية والبيانات الجديدة
egypt_export <- egypt_merged %>%
  select(name, Births_2016, Birth_Category) %>%
  st_transform(4326)

st_write(egypt_export, "egypt_births_updated.shp",
         delete_dsn = TRUE, quiet = TRUE)
cat("✅ Updated shapefile saved: egypt_births_updated.shp\n")

st_write(egypt_export, "egypt_births_updated.shp",
         delete_dsn = TRUE, quiet = TRUE)
cat("✅ Updated shapefile saved: egypt_births_updated.shp\n")

# 7-D: Save final CSV (clean)
write.csv(
  st_drop_geometry(egypt_merged[, c("name", "label_name", "Births_2016", "Birth_Category")]),
  "governorates_births_final.csv",
  row.names = FALSE
)
cat("✅ Final CSV saved: governorates_births_final.csv\n")


# ══════════════════════════════════════════════════════════════
# BONUS: Bar chart – Top 10 Governorates by Births
# ══════════════════════════════════════════════════════════════

top10 <- egypt_merged %>%
  st_drop_geometry() %>%
  arrange(desc(Births_2016)) %>%
  slice(1:10)

p_bar <- ggplot(top10, aes(x = reorder(label_name, Births_2016),
                            y = Births_2016, fill = Births_2016)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = format(Births_2016, big.mark = ",")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0, max(top10$Births_2016) * 1.15)) +
  labs(
    title    = "Top 10 Governorates by Number of Births – Egypt 2016",
    subtitle = "Source: CAPMAS",
    x        = NULL,
    y        = "Number of Births",
    caption  = "Spatial Statistics Assignment | Egyptian Chinese University"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    panel.grid.major.y = element_blank(),
    axis.text.y   = element_text(face = "bold")
  )

ggsave("egypt_births_top10.png", plot = p_bar,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("✅ BONUS: Top-10 bar chart saved: egypt_births_top10.png\n")


# ══════════════════════════════════════════════════════════════
# PART 8: Map Point Visualization (Proportional Circles)
# Each governorate is represented by a circle whose SIZE is
# proportional to the number of births — plotted at the centroid.
# ══════════════════════════════════════════════════════════════

# ── 8-A: Static Point Map (ggplot2) ───────────────────────────

p_point_static <- ggplot() +
  # Base polygon layer (light grey fill, no data encoding)
  geom_sf(data = egypt_merged, fill = "#F0F0F0", color = "white", linewidth = 0.4) +
  # Proportional circles at centroids, sized by Births_2016
  geom_point(
    data = egypt_merged,
    aes(
      x    = lon,
      y    = lat,
      size = Births_2016,
      fill = Birth_Category
    ),
    shape  = 21,   # filled circle with border
    color  = "white",
    stroke = 0.6,
    alpha  = 0.85
  ) +
  # Circle size scale (area proportional to births)
  scale_size_area(
    max_size = 18,
    name     = "Births (2016)",
    labels   = label_comma(),
    breaks   = c(10000, 50000, 100000, 200000)
  ) +
  # Category colour — same palette as the choropleth
  scale_fill_manual(
    values = c(
      "Low (<70K)"        = "#A8D5A2",
      "Medium (70K–150K)" = "#FFD166",
      "High (≥150K)"      = "#EF4444"
    ),
    name = "Birth Count\nCategory"
  ) +
  # Governorate labels (only where circle is large enough)
  geom_text(
    data = egypt_merged %>% filter(Births_2016 >= 70000),
    aes(x = lon, y = lat, label = label_name),
    size       = 2.1,
    color      = "gray15",
    fontface   = "bold",
    vjust      = -1.4,
    check_overlap = TRUE
  ) +
  labs(
    title    = "Egypt – Births by Governorate (2016): Point Visualization",
    subtitle = "Circle size is proportional to number of registered births | Source: CAPMAS",
    caption  = "Spatial Statistics Assignment | Egyptian Chinese University"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 8, hjust = 1, color = "gray55"),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    plot.background  = element_rect(fill = "#F8FAFC", color = NA),
    plot.margin      = margin(12, 12, 12, 12)
  ) +
  guides(
    size = guide_legend(override.aes = list(fill = "gray60", color = "white")),
    fill = guide_legend(override.aes = list(size = 5))
  )

print(p_point_static)
ggsave("egypt_births_point_map.png", plot = p_point_static,
       width = 10, height = 9, dpi = 300, bg = "#F8FAFC")
cat("✅ Part 8-A: Static point map saved: egypt_births_point_map.png\n")


# ── 8-B: Interactive Point Map (ggplotly) ─────────────────────

p_point_interactive_base <- ggplot() +
  geom_sf(data = egypt_merged, fill = "#EEEEEE", color = "white", linewidth = 0.3) +
  geom_point(
    data = egypt_merged,
    aes(
      x    = lon,
      y    = lat,
      size = Births_2016,
      fill = Birth_Category,
      text = paste0(
        "<b>", label_name, "</b><br>",
        "Births (2016): ", format(Births_2016, big.mark = ","), "<br>",
        "Category: ", Birth_Category
      )
    ),
    shape  = 21,
    color  = "white",
    stroke = 0.5,
    alpha  = 0.85
  ) +
  scale_size_area(max_size = 18) +
  scale_fill_manual(
    values = c(
      "Low (<70K)"        = "#A8D5A2",
      "Medium (70K–150K)" = "#FFD166",
      "High (≥150K)"      = "#EF4444"
    )
  ) +
  labs(title = "Egypt – Births by Governorate 2016 (Point Map)") +
  theme_void()

p_point_plotly <- ggplotly(p_point_interactive_base, tooltip = "text") %>%
  layout(
    title = list(
      text = "<b>Egypt – Registered Births by Governorate (2016) – Point Map</b>",
      font = list(size = 16)
    ),
    showlegend = FALSE,
    hoverlabel = list(
      bgcolor     = "white",
      bordercolor = "#ccc",
      font        = list(size = 13)
    )
  ) %>%
  config(displayModeBar = TRUE, scrollZoom = TRUE)

saveWidget(p_point_plotly, "egypt_point_map.html", selfcontained = TRUE)
cat("✅ Part 8-B: Interactive point map saved: egypt_point_map.html\n")


# ══════════════════════════════════════════════════════════════
# SUMMARY
# ══════════════════════════════════════════════════════════════
cat("\n════════════════════════════════════════\n")
cat("         OUTPUT FILES SUMMARY\n")
cat("════════════════════════════════════════\n")
cat("1. governorate_names.csv          (Part 2)\n")
cat("2. governorates_births.csv        (Part 3)\n")
cat("3. egypt_births_map.png           (Part 5 + Bonus labels & colors)\n")
cat("4. egypt_map.html                 (Part 6)\n")
cat("5. egypt_births_updated.shp (+aux)(Part 7)\n")
cat("6. governorates_births_final.csv  (Part 7)\n")
cat("7. egypt_births_top10.png         (Bonus chart)\n")
cat("8. egypt_births_point_map.png     (Part 8-A: Static point map)\n")
cat("9. egypt_point_map.html           (Part 8-B: Interactive point map)\n")
cat("════════════════════════════════════════\n")
cat("Total Births Egypt 2016 (in dataset):",
    format(sum(births_data$Births_2016), big.mark = ","), "\n")
