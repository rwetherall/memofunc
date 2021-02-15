## Test environments

* local OS X install, R 3.6.2
* ubuntu 14.04 (on travis-ci), (oldrel) R 3.5.3, (release) R 3.6.2, devel
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

Changes made based on submission feedback:

* There are no additional licence restricitions, so removed LICENCE file and adjusted DESRIPTION file accordingly.
* Examples added for exported functions in cache.R
* Added documentation for hash.list and other functions to indicate what they are responsible for, thus hash.list.RD and hash.RD should show differences
