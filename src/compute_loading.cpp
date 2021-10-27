#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector compute_loading(List coeff, NumericVector peaks,
                              double body_mass) {

    double b0 = as<double>(coeff["b0"]);
    double b1 = as<double>(coeff["b1"]);
    double b2 = as<double>(coeff["b2"]);
    double b3 = as<double>(coeff["b3"]);
    double b4 = as<double>(coeff["b4"]);

    int n = peaks.size();
    NumericVector loading(n);

    for (int i = 0; i < n; ++i) {
        if (b4 == 0) {
            loading[i] = b0 + b1 * peaks[i] + b2 * body_mass +
                b3 * peaks[i] * body_mass;
        } else {
            loading[i] = b0 + (b1 * peaks[i]) + (b2 * pow(peaks[i], 2)) +
                (b3 * body_mass) + (b4 * peaks[i] * body_mass);
        }
    }

    return loading;

}
