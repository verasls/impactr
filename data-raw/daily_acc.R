zip::unzip("data-raw/acc_data.zip")

daily_acc <- read_acc("data-raw/acc_data.csv")  |>
  define_region(
    start_time = "2017-01-03 00:01:00",
    end_time = "2017-01-05 23:59:59"
  )

usethis::use_data(daily_acc, compress = "xz", overwrite = TRUE)
