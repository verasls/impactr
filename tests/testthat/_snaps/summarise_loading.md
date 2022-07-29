# summarising works

    Code
      summarise_loading(data, "acc", "all", ranges_acc = 1:5)
    Output
      $`Summary per day`
      $`Summary per day`$`Vertical peak ACC`
      # A tibble: 1 x 15
        filename    date       weekday measu~1 varia~2 n_peaks min_p~3 max_p~4 mean_~5
        <chr>       <date>     <chr>     <int> <chr>     <dbl>   <dbl>   <dbl>   <dbl>
      1 daily_acc_~ 2016-01-20 Wednes~       1 vertic~    2879     1.3    5.25    1.66
      # ... with 6 more variables: sd_peaks <dbl>, n_peaks_1_to_2_g <dbl>,
      #   n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>, n_peaks_4_to_5_g <dbl>,
      #   n_peaks_above_5_g <dbl>, and abbreviated variable names 1: measurement_day,
      #   2: variable, 3: min_peaks, 4: max_peaks, 5: mean_peaks
      # i Use `colnames()` to see all variable names
      
      $`Summary per day`$`Resultant peak ACC`
      # A tibble: 1 x 15
        filename    date       weekday measu~1 varia~2 n_peaks min_p~3 max_p~4 mean_~5
        <chr>       <date>     <chr>     <int> <chr>     <dbl>   <dbl>   <dbl>   <dbl>
      1 daily_acc_~ 2016-01-20 Wednes~       1 result~    3805     1.3    5.89    1.73
      # ... with 6 more variables: sd_peaks <dbl>, n_peaks_1_to_2_g <dbl>,
      #   n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>, n_peaks_4_to_5_g <dbl>,
      #   n_peaks_above_5_g <dbl>, and abbreviated variable names 1: measurement_day,
      #   2: variable, 3: min_peaks, 4: max_peaks, 5: mean_peaks
      # i Use `colnames()` to see all variable names
      
      
      $`Daily average`
      $`Daily average`$`Vertical peak ACC`
      # A tibble: 1 x 12
        filename       varia~1 n_peaks min_p~2 max_p~3 mean_~4 sd_pe~5 n_pea~6 n_pea~7
        <chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 daily_acc_3d.~ vertic~ 2879    1.3     5.25    1.66    0.37    2523    319    
      # ... with 3 more variables: n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>,
      #   n_peaks_above_5_g <chr>, and abbreviated variable names 1: variable,
      #   2: min_peaks, 3: max_peaks, 4: mean_peaks, 5: sd_peaks,
      #   6: n_peaks_1_to_2_g, 7: n_peaks_2_to_3_g
      # i Use `colnames()` to see all variable names
      
      $`Daily average`$`Resultant peak ACC`
      # A tibble: 1 x 12
        filename       varia~1 n_peaks min_p~2 max_p~3 mean_~4 sd_pe~5 n_pea~6 n_pea~7
        <chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 daily_acc_3d.~ result~ 3805    1.3     5.89    1.73    0.42    3125    620    
      # ... with 3 more variables: n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>,
      #   n_peaks_above_5_g <chr>, and abbreviated variable names 1: variable,
      #   2: min_peaks, 3: max_peaks, 4: mean_peaks, 5: sd_peaks,
      #   6: n_peaks_1_to_2_g, 7: n_peaks_2_to_3_g
      # i Use `colnames()` to see all variable names
      
      

