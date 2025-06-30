# shiny-nametagger
# functions

# create temporary directory
fn_dir <- function(session) {
  wd <- file.path(tempdir(check = TRUE), session$token)
  if (!dir.exists(wd)) dir.create(wd)
  cat(paste0("Temp directory: ", wd, "\n"))
  return(wd)
}

# return version
fn_version <- function() {
  return("v1.2.3")
}

# validation
fn_validate <- function(input, message1, message2, message3) {
  if (missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing", message1))
  if (gcheck == 1) {
    m1 <- sub("Argument ", "", message1)
    m1 <- sub(" missing.", "", m1)
  }

  if (all(is.null(input))) {
    if (missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if (all(is.na(input))) {
      if (missing(message2)) {
        if (gcheck == 1) message2 <- paste0("Argument ", m1, " is NA.", sep = "")
        if (gcheck != 1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if (all(nchar(input) == 0)) {
      if (missing(message3)) {
        if (gcheck == 1) message3 <- paste0("Argument ", m1, " is empty.", sep = "")
        if (gcheck != 1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# validate numeric
fn_validate_numeric <- function(input) {
  if (is.na(input) || !is.numeric(input)) print("Input is not a numeric.")
}

# validate image input
fn_validate_im <- function(x) {
  if (!is.null(x)) {
    y <- tolower(sub("^.+[.]", "", basename(x$datapath)))
    if (!y %in% c("jpg", "png", "jpeg", "gif")) {
      return("Image must be one of JPG/JPEG, PNG or GIF formats.")
    }
    if ((x$size / 1024 / 1024) > 1) {
      return("Image must be less than 1MB in size.")
    }
  }
}

# copy directories
copy_dirs <- function(path) {
  dirs_to_copy <- c("_extensions", "fonts", "www", "assets")

  # ensure the directory exists and copy the contents
  copy_directory <- function(dir_name) {
    dir_to_create <- file.path(path, dir_name)
    if (!dir.exists(dir_to_create)) {
      dir.create(dir_to_create, recursive = TRUE)
    }
    file.copy(
      from = list.files(dir_name, full.names = TRUE),
      to = dir_to_create,
      recursive = TRUE
    )
  }

  for (dir_name in dirs_to_copy) {
    copy_directory(dir_name)
  }

  # copy files with extensions *.r and *.qmd to output directory
  files_to_copy_r <- list.files(pattern = "\\.r$", full.names = TRUE)
  files_to_copy_qmd <- list.files(pattern = "\\.qmd$", full.names = TRUE)

  if (length(files_to_copy_r) > 0) {
    file.copy(from = files_to_copy_r, to = path)
  }

  if (length(files_to_copy_qmd) > 0) {
    file.copy(from = files_to_copy_qmd, to = path)
  }
}

# country data, 248 countries
# data from https://github.com/datasets/country-list/blob/main/data.csv
# flags from https://github.com/HatScripts/circle-flags
cou <- readr::read_csv("country.csv", locale = locale(encoding = "UTF-8"))
cou$path <- paste0("www/flags/", cou$code, ".svg")

# set defaults -----------------------------------------------------------------

# get vector of logos
get_logos <- function() {
  logos <- c("", list.files("www/logos", full.names = TRUE))
  names(logos) <- gsub(".png$|.jpg$|.svg$", "", basename(logos))
  names(logos)[1] <- "none"
  return(logos)
}

# create dataframe for logo images
logos <- data.frame(path = sub("www/", "", get_logos()))
logos$label <- names(get_logos())
logos$img <- sprintf(paste0("<img class='logo-picker-outer' src='", logos$path, "'><div class='logo-picker-inner'>%s</div></img>"), logos$label)
