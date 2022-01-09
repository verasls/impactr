## Test environments

* local macOS install, R 4.1.2
* GitHub Actions (ubuntu-20.04): devel, release
* GitHub Actions (Microsft Windows Server 2019 10.0.17763): release
* GitHub Actions (macOS 10.15.7): release
* win-builder: devel

## R CMD check results

No errors or warnings. One or two notes depending on the test environment.

The notes are due to a large data file that was bundled in the package.
These data is needed for the users to explore all features of the package without having to collect data of their own.
This data file will change very infrequently.

### local macOS

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is 15.1Mb
  sub-directories of 1Mb or more:
    data  14.1Mb

### GitHub Actions (ubuntu-20.4): devel

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Lucas Veras <lucasdsveras@gmail.com>’
  Size of tarball: 14383331 bytes

* checking installed package size ... NOTE
    installed size is 16.4Mb
    sub-directories of 1Mb or more:
      data  13.9Mb
      libs   1.6Mb

### GitHub Actions (ubuntu-20.4): release

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Lucas Veras <lucasdsveras@gmail.com>’
  Size of tarball: 14383446 bytes

* checking installed package size ... NOTE
    installed size is 16.4Mb
    sub-directories of 1Mb or more:
      data  13.9Mb
      libs   1.6Mb

### GitHub Actions (Microsft Windows Server 2019 10.0.17763): release

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Lucas Veras <lucasdsveras@gmail.com>'
  Size of tarball: 14383155 bytes

* checking installed package size ... NOTE
    installed size is 15.7Mb
    sub-directories of 1Mb or more:
      data  13.9Mb

### GitHub Actions (macOS 10.15.7): release

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Lucas Veras <lucasdsveras@gmail.com>’
  Size of tarball: 14376246 bytes

* checking installed package size ... NOTE
    installed size is 15.1Mb
    sub-directories of 1Mb or more:
      data  14.1Mb

### win-builder: devel

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is 15.7Mb
  sub-directories of 1Mb or more:
    data  13.9Mb
    libs   1.0Mb
