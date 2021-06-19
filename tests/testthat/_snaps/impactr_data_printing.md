# printing from read_acc() works

    Code
      read_acc(test_path("test-data-hip-imu.csv"))
    Output
      # Start time:              2017-12-09 15:00:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         100 x 4
         timestamp             acc_X   acc_Y acc_Z
         <dttm>                <dbl>   <dbl> <dbl>
       1 2017-12-09 15:00:00 -0.0122 0.00830 0.967
       2 2017-12-09 15:00:00 -0.0151 0.00732 0.967
       3 2017-12-09 15:00:00 -0.0137 0.00635 0.964
       4 2017-12-09 15:00:00 -0.0132 0.00684 0.969
       5 2017-12-09 15:00:00 -0.0137 0.00586 0.965
       6 2017-12-09 15:00:00 -0.0137 0.00586 0.965
       7 2017-12-09 15:00:00 -0.0151 0.00586 0.967
       8 2017-12-09 15:00:00 -0.0146 0.00635 0.964
       9 2017-12-09 15:00:00 -0.0156 0.00586 0.966
      10 2017-12-09 15:00:00 -0.0151 0.00635 0.963
      # ... with 90 more rows

# printing from specify_parameter() works

    Code
      specify_parameters(data, "hip", 80)
    Output
      # Start time:              2017-12-09 15:00:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Hip
      # Subject body mass:       80kg
      # Filter:                  No filter applied
      # Data dimensions:         100 x 4
         timestamp             acc_X   acc_Y acc_Z
         <dttm>                <dbl>   <dbl> <dbl>
       1 2017-12-09 15:00:00 -0.0122 0.00830 0.967
       2 2017-12-09 15:00:00 -0.0151 0.00732 0.967
       3 2017-12-09 15:00:00 -0.0137 0.00635 0.964
       4 2017-12-09 15:00:00 -0.0132 0.00684 0.969
       5 2017-12-09 15:00:00 -0.0137 0.00586 0.965
       6 2017-12-09 15:00:00 -0.0137 0.00586 0.965
       7 2017-12-09 15:00:00 -0.0151 0.00586 0.967
       8 2017-12-09 15:00:00 -0.0146 0.00635 0.964
       9 2017-12-09 15:00:00 -0.0156 0.00586 0.966
      10 2017-12-09 15:00:00 -0.0151 0.00635 0.963
      # ... with 90 more rows

# printing from filter_acc() works

    Code
      filter_acc(data)
    Output
      # Start time:              2017-12-09 15:00:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  Butterworth (4th-ord, low-pass, 20Hz)
      # Data dimensions:         100 x 4
         timestamp             acc_X   acc_Y acc_Z
       * <dttm>                <dbl>   <dbl> <dbl>
       1 2017-12-09 15:00:00 -0.0122 0.00830 0.967
       2 2017-12-09 15:00:00 -0.0137 0.00735 0.966
       3 2017-12-09 15:00:00 -0.0142 0.00666 0.966
       4 2017-12-09 15:00:00 -0.0138 0.00630 0.966
       5 2017-12-09 15:00:00 -0.0134 0.00613 0.966
       6 2017-12-09 15:00:00 -0.0138 0.00600 0.966
       7 2017-12-09 15:00:00 -0.0146 0.00584 0.966
       8 2017-12-09 15:00:00 -0.0154 0.00581 0.965
       9 2017-12-09 15:00:00 -0.0154 0.00618 0.965
      10 2017-12-09 15:00:00 -0.0146 0.00694 0.965
      # ... with 90 more rows

# printing from find_peaks() works

    Code
      find_peaks(data, "vertical")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         263 x 2
         timestamp           vertical_peak_acc
         <dttm>                          <dbl>
       1 2021-04-06 15:43:00              2.70
       2 2021-04-06 15:43:00              1.78
       3 2021-04-06 15:43:03              1.64
       4 2021-04-06 15:43:04              1.61
       5 2021-04-06 15:43:06              1.34
       6 2021-04-06 15:43:09              2.59
       7 2021-04-06 15:43:10              1.30
       8 2021-04-06 15:43:11              1.40
       9 2021-04-06 15:43:14              1.42
      10 2021-04-06 15:43:16              1.66
      # ... with 253 more rows

---

    Code
      find_peaks(data, "resultant")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         312 x 2
         timestamp           resultant_peak_acc
         <dttm>                           <dbl>
       1 2021-04-06 15:43:00               3.77
       2 2021-04-06 15:43:00               6.30
       3 2021-04-06 15:43:02               1.52
       4 2021-04-06 15:43:03               1.79
       5 2021-04-06 15:43:04               1.52
       6 2021-04-06 15:43:04               1.32
       7 2021-04-06 15:43:05               2.13
       8 2021-04-06 15:43:05               1.47
       9 2021-04-06 15:43:06               1.38
      10 2021-04-06 15:43:07               1.45
      # ... with 302 more rows

---

    Code
      find_peaks(data, "all")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         357 x 3
         timestamp           vertical_peak_acc resultant_peak_acc
         <dttm>                          <dbl>              <dbl>
       1 2021-04-06 15:43:00              2.70               3.77
       2 2021-04-06 15:43:00              1.78               6.30
       3 2021-04-06 15:43:02             NA                  1.52
       4 2021-04-06 15:43:03              1.64              NA   
       5 2021-04-06 15:43:03             NA                  1.79
       6 2021-04-06 15:43:04             NA                  1.52
       7 2021-04-06 15:43:04             NA                  1.32
       8 2021-04-06 15:43:04              1.61              NA   
       9 2021-04-06 15:43:05             NA                  2.13
      10 2021-04-06 15:43:05             NA                  1.47
      # ... with 347 more rows

