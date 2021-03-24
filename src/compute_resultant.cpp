#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector compute_resultant(NumericVector acc_X,
                                NumericVector acc_Y,
                                NumericVector acc_Z) {

    int n = acc_X.size();
    NumericVector acc_R(n);
    NumericVector acc_X2 = pow(acc_X, 2);
    NumericVector acc_Y2 = pow(acc_Y, 2);
    NumericVector acc_Z2 = pow(acc_Z, 2);

    for (int i = 0; i < n; ++i) {
        acc_R[i] = sqrt(acc_X2[i] + acc_Y2[i] + acc_Z2[i]);
    }

    return acc_R;

}
