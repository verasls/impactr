#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector compute_peak_acc_rate(NumericVector acc, NumericVector start_idx,
                                    NumericVector peaks_idx, double samp_freq) {

    int n = start_idx.size();
    NumericVector peak_acc_rate(n);
    double dF = 0;
    double dT = 0;
    std::vector<double> dFdT;

    for (int i = 0; i < n; ++i) {
        if (peaks_idx[i] - start_idx[i] <= 1) {
            peak_acc_rate[i] = NA_REAL;
        } else {
            for (int j = start_idx[i] + 1; j < peaks_idx[i]; ++j) {
                dF = acc[j + 1] - acc[j - 1];
                dT = 2 / samp_freq;
                dFdT.push_back(dF / dT);
            }
            std::vector<double>::iterator max_rate;
            max_rate = std::max_element(dFdT.begin(), dFdT.end());
            peak_acc_rate[i] = *max_rate;
            std::fill(dFdT.begin(), dFdT.end(), 0);
        }
    }

    return peak_acc_rate;

}
