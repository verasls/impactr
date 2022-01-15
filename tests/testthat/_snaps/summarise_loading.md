# summarising works

    Code
      summarise_loading(data, "acc", "all", ranges_acc = 1:5)
    Output
      $`Summary per day`
      $`Summary per day`$`Vertical peak ACC`
      # A tibble: 1 x 15
        filename         date       weekday measurement_day variable n_peaks min_peaks
        <chr>            <date>     <chr>             <int> <chr>      <dbl>     <dbl>
      1 daily_acc_3d.csv 2016-01-20 Wednes~               1 vertica~    2879       1.3
      # ... with 8 more variables: max_peaks <dbl>, mean_peaks <dbl>, sd_peaks <dbl>,
      #   n_peaks_1_to_2_g <dbl>, n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>,
      #   n_peaks_4_to_5_g <dbl>, n_peaks_above_5_g <dbl>
      
      $`Summary per day`$`Resultant peak ACC`
      # A tibble: 1 x 15
        filename         date       weekday measurement_day variable n_peaks min_peaks
        <chr>            <date>     <chr>             <int> <chr>      <dbl>     <dbl>
      1 daily_acc_3d.csv 2016-01-20 Wednes~               1 resulta~    3805       1.3
      # ... with 8 more variables: max_peaks <dbl>, mean_peaks <dbl>, sd_peaks <dbl>,
      #   n_peaks_1_to_2_g <dbl>, n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>,
      #   n_peaks_4_to_5_g <dbl>, n_peaks_above_5_g <dbl>
      
      
      $`Daily average`
      $`Daily average`$`Vertical peak ACC`
      # A tibble: 1 x 12
        filename         variable      n_peaks min_peaks max_peaks mean_peaks sd_peaks
        <chr>            <chr>         <chr>   <chr>     <chr>     <chr>      <chr>   
      1 daily_acc_3d.csv vertical_pea~ 2879    1.3       5.25      1.66       0.37    
      # ... with 5 more variables: n_peaks_1_to_2_g <chr>, n_peaks_2_to_3_g <chr>,
      #   n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>, n_peaks_above_5_g <chr>
      
      $`Daily average`$`Resultant peak ACC`
      # A tibble: 1 x 12
        filename         variable      n_peaks min_peaks max_peaks mean_peaks sd_peaks
        <chr>            <chr>         <chr>   <chr>     <chr>     <chr>      <chr>   
      1 daily_acc_3d.csv resultant_pe~ 3805    1.3       5.89      1.73       0.42    
      # ... with 5 more variables: n_peaks_1_to_2_g <chr>, n_peaks_2_to_3_g <chr>,
      #   n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>, n_peaks_above_5_g <chr>
      
      

