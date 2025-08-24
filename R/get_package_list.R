# get list of packages used in the R project

used_pkgs <- unique(renv::dependencies()$Package)

# Get installed packages info
installed_pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])

# Merge to get versions
used_with_versions <- merge(
  data.frame(Package = used_pkgs),
  installed_pkgs,
  by = "Package",
  all.x = TRUE
)

print(used_with_versions, row.names = FALSE)

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Packages")
writeData(wb, "Packages", used_with_versions)

saveWorkbook(wb, "../pkg_used.xlsx", overwrite = TRUE)