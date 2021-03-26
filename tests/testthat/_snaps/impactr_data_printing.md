# printing from read_acc() works

    Code
      read_acc(test_path("test-data-hip-imu.csv"))
    Output
      # Start time:              2017-12-09 15:00:00
      # Sampling frequency:      100Hz
      # Accelerometer placement: Non-specified
      # Subject body mass:       Non-specified
      # Filter:                  No filter applied
      # Peaks:                   Not detected
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
      # Peaks:                   Not detected
      # Data dimensions:         100 x 4
         timestamp             acc_X   acc_Y acc_Z
       * <dttm>                <dbl>   <dbl> <dbl>
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
      # Peaks:                   Not detected
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

