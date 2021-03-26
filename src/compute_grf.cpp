#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector compute_grf(List coeff, NumericVector peaks, double body_mass) {

    double b0 = as<double>(coeff["b0"]);
    double b1 = as<double>(coeff["b1"]);
    double b2 = as<double>(coeff["b2"]);
    double b3 = as<double>(coeff["b3"]);

    int n = peaks.size();
    NumericVector grf(n);

    for (int i = 0; i < n; ++i) {
        grf[i] = b0 + b1 * peaks[i] + b2 * body_mass + b3 * peaks[i] * body_mass;
    }

    return grf;

}
