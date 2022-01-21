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
  summarise_loading 31.949 10.345  43.089
  remove_nonwear     8.999  5.343  14.725
