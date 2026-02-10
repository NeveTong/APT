check_create_folder <- function(folders) {
  for (f in folders) {
    if (!dir.exists(f)) {
      dir.create(f)
    }
  }
}