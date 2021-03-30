#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector get_curve_start(NumericVector acc, NumericVector peaks_idx) {

    // Correct indices from R to C++
    peaks_idx = peaks_idx - 1;

    int n = peaks_idx.size();
    double dFdT = 0;
    NumericVector curve_start(n);

    for (int i = 0; i < n; ++i) {
        for (int j = peaks_idx[i] - 1; j > 0; --j) {
            dFdT = acc[j + 1] - acc[j - 1];
            if (dFdT < 0) {
                curve_start[i] = j;
                break;
            }
        }
    }

    // Correct indices from C++ to R
    curve_start = curve_start + 1;
    return curve_start;

}
