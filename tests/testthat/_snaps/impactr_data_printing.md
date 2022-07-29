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
      # i Use `print(n = ...)` to see more rows

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
      # i Use `print(n = ...)` to see more rows

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
         timestamp              acc_X   acc_Y acc_Z
         <dttm>                 <dbl>   <dbl> <dbl>
       1 2017-12-09 15:00:00 -0.00948 0.00552 0.678
       2 2017-12-09 15:00:00 -0.0138  0.00738 0.964
       3 2017-12-09 15:00:00 -0.0150  0.00744 1.05 
       4 2017-12-09 15:00:00 -0.0140  0.00656 0.995
       5 2017-12-09 15:00:00 -0.0131  0.00590 0.943
       6 2017-12-09 15:00:00 -0.0135  0.00578 0.943
       7 2017-12-09 15:00:00 -0.0147  0.00585 0.966
       8 2017-12-09 15:00:00 -0.0155  0.00592 0.976
       9 2017-12-09 15:00:00 -0.0154  0.00623 0.970
      10 2017-12-09 15:00:00 -0.0146  0.00691 0.962
      # ... with 90 more rows
      # i Use `print(n = ...)` to see more rows

# printing from find_peaks() works

    Code
      find_peaks(data, "vertical")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         251 x 2
         timestamp           vertical_peak_acc
         <dttm>                          <dbl>
       1 2021-04-06 15:43:00              1.83
       2 2021-04-06 15:43:03              1.41
       3 2021-04-06 15:43:04              1.59
       4 2021-04-06 15:43:06              1.35
       5 2021-04-06 15:43:09              2.61
       6 2021-04-06 15:43:11              1.38
       7 2021-04-06 15:43:14              1.42
       8 2021-04-06 15:43:16              1.36
       9 2021-04-06 15:43:16              1.46
      10 2021-04-06 15:43:17              1.32
      # ... with 241 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      find_peaks(data, "resultant")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         303 x 2
         timestamp           resultant_peak_acc
         <dttm>                           <dbl>
       1 2021-04-06 15:43:00               2.24
       2 2021-04-06 15:43:00               1.43
       3 2021-04-06 15:43:02               1.49
       4 2021-04-06 15:43:03               1.68
       5 2021-04-06 15:43:04               1.49
       6 2021-04-06 15:43:04               1.30
       7 2021-04-06 15:43:05               2.13
       8 2021-04-06 15:43:05               1.34
       9 2021-04-06 15:43:06               1.39
      10 2021-04-06 15:43:07               1.46
      # ... with 293 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      find_peaks(data, "all")
    Output
      # Start time:              2021-04-06 15:43:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Data dimensions:         331 x 3
         timestamp           vertical_peak_acc resultant_peak_acc
         <dttm>                          <dbl>              <dbl>
       1 2021-04-06 15:43:00              1.83               2.24
       2 2021-04-06 15:43:00             NA                  1.43
       3 2021-04-06 15:43:02             NA                  1.49
       4 2021-04-06 15:43:03              1.41              NA   
       5 2021-04-06 15:43:03             NA                  1.68
       6 2021-04-06 15:43:04             NA                  1.49
       7 2021-04-06 15:43:04             NA                  1.30
       8 2021-04-06 15:43:04              1.59              NA   
       9 2021-04-06 15:43:05             NA                  2.13
      10 2021-04-06 15:43:05             NA                  1.34
      # ... with 321 more rows
      # i Use `print(n = ...)` to see more rows

