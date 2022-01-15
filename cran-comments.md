## Release summary

This is a resubmission. In this version I have fixed the test failures when running on CRAN machines.

## Test environments

* local macOS install, R 4.1.2
* GitHub Actions (ubuntu-20.04): devel, release
* GitHub Actions (Microsft Windows Server 2019 10.0.17763): release
* GitHub Actions (macOS 10.15.7): release
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                      user system elapsed
  summarise_loading 40.928  7.510  48.452
  remove_nonwear     8.490  0.724   9.215
  import_dataset     4.573  0.708   5.283
