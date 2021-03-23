# `impactr_data` objects print correctly

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
