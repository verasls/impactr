# summarising works

    Code
      summarise_loading(data, "acc", "all", ranges_acc = 1:5)
    Output
      $`Summary per day`
      $`Summary per day`$`Vertical peak ACC`
      # A tibble: 1 x 15
        filename     date       weekday measurement_day variable     n_peaks min_peaks
        <chr>        <date>     <chr>             <int> <chr>          <dbl>     <dbl>
      1 acc_data.csv 2017-01-03 Tuesday               1 vertical_pe~     937       1.3
      # ... with 8 more variables: max_peaks <dbl>, mean_peaks <dbl>, sd_peaks <dbl>,
      #   n_peaks_1_to_2_g <dbl>, n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>,
      #   n_peaks_4_to_5_g <dbl>, n_peaks_above_5_g <dbl>
      
      $`Summary per day`$`Resultant peak ACC`
      # A tibble: 1 x 15
        filename     date       weekday measurement_day variable     n_peaks min_peaks
        <chr>        <date>     <chr>             <int> <chr>          <dbl>     <dbl>
      1 acc_data.csv 2017-01-03 Tuesday               1 resultant_p~    1055       1.3
      # ... with 8 more variables: max_peaks <dbl>, mean_peaks <dbl>, sd_peaks <dbl>,
      #   n_peaks_1_to_2_g <dbl>, n_peaks_2_to_3_g <dbl>, n_peaks_3_to_4_g <dbl>,
      #   n_peaks_4_to_5_g <dbl>, n_peaks_above_5_g <dbl>
      
      
      $`Daily average`
      $`Daily average`$`Vertical peak ACC`
      # A tibble: 1 x 12
        filename     variable          n_peaks min_peaks max_peaks mean_peaks sd_peaks
        <chr>        <chr>             <chr>   <chr>     <chr>     <chr>      <chr>   
      1 acc_data.csv vertical_peak_acc 937     1.3       2.9       1.42       0.09    
      # ... with 5 more variables: n_peaks_1_to_2_g <chr>, n_peaks_2_to_3_g <chr>,
      #   n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>, n_peaks_above_5_g <chr>
      
      $`Daily average`$`Resultant peak ACC`
      # A tibble: 1 x 12
        filename     variable          n_peaks min_peaks max_peaks mean_peaks sd_peaks
        <chr>        <chr>             <chr>   <chr>     <chr>     <chr>      <chr>   
      1 acc_data.csv resultant_peak_a~ 1055    1.3       2.93      1.49       0.12    
      # ... with 5 more variables: n_peaks_1_to_2_g <chr>, n_peaks_2_to_3_g <chr>,
      #   n_peaks_3_to_4_g <chr>, n_peaks_4_to_5_g <chr>, n_peaks_above_5_g <chr>
      
      

