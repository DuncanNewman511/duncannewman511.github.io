# --- Begin script ------------------------------------------------------------
# Phone Image EXIF analysis using magick (no ExifTool)
# Windows R 4.5.1, JPEG only

# --- 1. Install / load packages ---------------------------------------------
packages <- c(
  "magick", "tidyverse", "lubridate",
  "sf", "rnaturalearth", "rnaturalearthdata", "ggspatial"
)

installed <- installed.packages()[, "Package"]
for(p in packages) if(!p %in% installed) install.packages(p, dependencies=TRUE)

library(magick)
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# --- 2. Configure directories ----------------------------------------------
img_dir <- "C:/Users/dunca/OneDrive/Desktop/ENMA 754/Exif_Assignment/Photos"  # <<-- EDIT this
out_dir <- file.path(getwd(), "results_exif_magick")
dir.create(out_dir, showWarnings = FALSE)

# --- 3. Find JPEG images ---------------------------------------------------
img_files <- list.files(img_dir, pattern="\\.(jpg|jpeg|JPG|JPEG)$",
                        recursive=TRUE, full.names=TRUE)
cat("Found", length(img_files), "JPEG images.\n")
if(length(img_files)==0) stop("No JPEG images found. Check img_dir.")

# --- 4. Extract EXIF from each image --------------------------------------
extract_exif <- function(file){
  img <- image_read(file)
  info <- image_attributes(img)
  exif <- info$exif
  tibble(
    SourceFile = file,
    DateTimeOriginal = exif[["DateTimeOriginal"]] %||% NA,
    GPSLatitude = exif[["GPSLatitude"]] %||% NA,
    GPSLongitude = exif[["GPSLongitude"]] %||% NA,
    Make = exif[["Make"]] %||% NA,
    Model = exif[["Model"]] %||% NA
  )
}

exif_list <- lapply(img_files, extract_exif)
exif <- bind_rows(exif_list)

# --- 5. Parse dates and coordinates ---------------------------------------
exif <- exif %>%
  mutate(
    DateTimeParsed = parse_date_time(DateTimeOriginal,
                                     orders=c("Y:m:d H:M:S","Y-m-d H:M:S"),
                                     tz="UTC"),
    GPSLatitude_num = as.numeric(GPSLatitude),
    GPSLongitude_num = as.numeric(GPSLongitude),
    has_datetime = !is.na(DateTimeParsed),
    has_gps = !is.na(GPSLatitude_num) & !is.na(GPSLongitude_num),
    complete_exif = has_datetime & has_gps
  )

# --- 6. Report counts ------------------------------------------------------
total_images <- nrow(exif)
n_complete <- sum(exif$complete_exif, na.rm=TRUE)
n_missing_any <- total_images - n_complete
cat("Total images:", total_images, "\n")
cat("Complete EXIF (DateTime + GPS):", n_complete, "\n")
cat("Incomplete EXIF:", n_missing_any, "\n")

# --- 7. Missing fields summary ---------------------------------------------
fields_to_check <- c("DateTimeParsed","GPSLatitude_num","GPSLongitude_num","Make","Model")
missing_summary <- map_dfr(fields_to_check, function(fld){
  tibble(
    field=fld,
    missing_count=sum(is.na(exif[[fld]])),
    present_count=sum(!is.na(exif[[fld]])),
    pct_present=round(100*sum(!is.na(exif[[fld]]))/total_images,1)
  )
})
print(missing_summary)

# --- 8. Per-image missing report -------------------------------------------
exif_missing_report <- exif %>%
  mutate(
    missing_fields = pmap_chr(select(., all_of(fields_to_check)), function(...){
      vals <- list(...); names(vals) <- fields_to_check
      m <- names(vals)[map_lgl(vals, ~ is.na(.x))]
      if(length(m)==0) return("none")
      paste(m, collapse=";")
    })
  ) %>%
  select(SourceFile, missing_fields)

print(exif_missing_report)

# --- 9. Map of GPS points --------------------------------------------------
gps_df <- exif %>% filter(!is.na(GPSLatitude_num) & !is.na(GPSLongitude_num))
if(nrow(gps_df)>0){
  pts_sf <- st_as_sf(gps_df, coords=c("GPSLongitude_num","GPSLatitude_num"), crs=4326, remove=FALSE)
  bbox <- st_bbox(pts_sf)
  lon_buf <- max(0.1*(bbox$xmax-bbox$xmin),0.01)
  lat_buf <- max(0.1*(bbox$ymax-bbox$ymin),0.01)
  map_bbox <- c(xmin=bbox$xmin-lon_buf, ymin=bbox$ymin-lat_buf,
                xmax=bbox$xmax+lon_buf, ymax=bbox$ymax+lat_buf)
  
  world <- ne_countries(scale="medium", returnclass="sf")
  
  map_plot <- ggplot() +
    geom_sf(data=world, fill="gray95", color="gray70") +
    geom_point(data=gps_df, aes(x=GPSLongitude_num, y=GPSLatitude_num),
               size=1.6, alpha=0.8) +
    coord_sf(xlim=c(map_bbox["xmin"],map_bbox["xmax"]),
             ylim=c(map_bbox["ymin"],map_bbox["ymax"]), expand=FALSE) +
    annotation_scale(location="bl", width_hint=0.2) +
    annotation_north_arrow(location="bl", which_north="true",
                           pad_x=unit(0.02,"npc"), pad_y=unit(0.06,"npc"),
                           style=north_arrow_fancy_orienteering) +
    labs(title="Photo locations (GPS)", x="Longitude", y="Latitude") +
    theme_minimal()
  
  ggsave(file.path(out_dir,"photo_locations_map.png"), map_plot, width=8, height=6, dpi=300)
}

# --- 10. Histograms by month & hour ---------------------------------------
time_df <- exif %>% filter(!is.na(DateTimeParsed)) %>%
  mutate(year_month=floor_date(DateTimeParsed,"month"),
         hour=hour(DateTimeParsed))

if(nrow(time_df)>0){
  month_plot <- time_df %>%
    count(year_month) %>%
    ggplot(aes(x=year_month,y=n)) +
    geom_col() + geom_line(aes(y=n), group=1) +
    labs(title="Images by month", x="Month", y="Count") + theme_minimal()
  month_plot()
  
  hour_plot <- time_df %>%
    ggplot(aes(x=hour)) +
    geom_histogram(binwidth=1, boundary=-0.5, color="black", fill=NA) +
    scale_x_continuous(breaks=0:23) +
    labs(title="Images by hour of day", x="Hour (0-23)", y="Count") + theme_minimal()
  hour_lot()
}

# --- 11. Modality detection (simple) --------------------------------------
peak_detector <- function(x){
  if(length(unique(x))<2) return(0)
  dens <- density(x, na.rm=TRUE)
  y <- dens$y
  maxima <- which(diff(sign(diff(y)))==-2)+1
  length(maxima)
}
modality_summary <- tibble(
  series=c("month","hour"),
  n_peaks=c(
    ifelse(nrow(time_df)>0, peak_detector(time_df %>% count(year_month) %>% pull(n)), NA),
    ifelse(nrow(time_df)>0, peak_detector(time_df$hour), NA)
  ),
  interpretation=c(
    ifelse(!is.na(n_peaks[1]), c("no clear peaks","unimodal","bimodal","multimodal")[pmin(n_peaks[1]+1,4)], NA),
    ifelse(!is.na(n_peaks[2]), c("no clear peaks","unimodal","bimodal","multimodal")[pmin(n_peaks[2]+1,4)], NA)
  )
)
print(modality_summary)
write_csv(modality_summary, file.path(out_dir,"modality_summary.csv"))

# --- 12. Overall summary ---------------------------------------------------
summary_tbl <- tibble(
  total_images = total_images,
  complete_exif = n_complete,
  incomplete_exif = n_missing_any,
  pct_complete = round(100*n_complete/total_images,1)
)
write_csv(summary_tbl, file.path(out_dir,"overall_summary.csv"))

cat("\nAll CSVs and plots saved in:", out_dir,"\n")
# --- End script -------------------------------------------------------------