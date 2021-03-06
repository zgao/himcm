#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <vector>

using namespace std;

FILE *fout;

double pi = 3.1415;
int iterations = 40000;
int possibilities = 13;
double real_probs[] = {0.0999, 0.1499, 0.0999, 0.3499, 0.2499, 0.0499, 0.0006};
vector<double> lambdas;
vector<double> periods;
vector<double> powers;

inline double r2() {
    return (double)rand() / (double)RAND_MAX;
}

inline double get_f(double period, double power, double x) {
    double adjusted = x / period;
    return exp((adjusted - floor(adjusted) - 1) * power);
}

inline double next_end(double period, double x) {
    return period * (floor(x / period) + 2);
}

double eval_divergence(double lambda, double period, double power) {

    double sum = 0.0;
    double sq = 0.0;

    double total_kl = 0.0;
    vector<double> *sofar = new vector<double>();
    sofar->push_back(0.0);
    for (int i = 1; i <= iterations; i++) {
        double poisson, last, next;
        do {
            poisson = -log(r2()) / lambda;
            last = *(sofar->end() - 1);
            next = last + poisson;
        } while (next >= next_end(period, last));
        sofar->push_back(next);
    }
    vector<double> *filtered_sofar = new vector<double>();
    for (vector<double>::iterator i = sofar->begin() + 1;
            i != sofar->end(); i++) {
        if (get_f(period, power, *i) > r2() ||
                (i != sofar->end() - 1 && *(i + 1) >= next_end(period, *i) - period)) {
            double d = *i;
            filtered_sofar->push_back(d);
        }
    }
    double diff_buckets[7];
    for (int i = 0; i < 7; i++) diff_buckets[i] = 0.0;
    for (vector<double>::iterator i = filtered_sofar->begin(); i != filtered_sofar->end() - 1; i++) {
        double t = *i, t2 = *(i + 1);
        double d = t2 - t;
        sum += d;
        sq += d * d;
        int diff_bucket = (int) floor(t2 - t + 0.5);
        diff_bucket = min(diff_bucket, 6);
        diff_buckets[diff_bucket] += 1.0;
    }
    for (int i = 0; i < 7; i++) {
        diff_buckets[i] /= (double) (filtered_sofar->size() - 1);
        printf("%.6lf ", diff_buckets[i]);
    }
    printf("\n");

    sum /= (filtered_sofar->size() - 1);
    sq /= (filtered_sofar->size() - 1);
    printf("MEAN %.6lf\n", sum);
    printf("VARIANCE %.6lf\n", sq - sum * sum);

    for (int i = 0; i < 7; i++) {
        total_kl += log(real_probs[i] / diff_buckets[i]) * real_probs[i];
    }
    //printf("%.6lf\n", total_kl);
    return total_kl;
}

int main() {
    srand(33333);
    fout = fopen("results.out", "w");

    eval_divergence(1.304, 3.422, 5.980);

    /*
    for (int i = 0; i <= possibilities; i++) {
        lambdas.push_back((0.5* i + 3.5 * (possibilities - i)) / possibilities);
        periods.push_back((3.0 * i + 5.0 * (possibilities - i)) / possibilities);
        powers.push_back((10.0 * i + 25.0 * (possibilities - i)) / possibilities);
    }

    double best_lambda = -1, best_period = -1, best_power = -1, best_kl = 1e9;
    for (int i = 0; i <= possibilities; i++)
        for (int j = 0; j <= possibilities; j++)
            for (int k = 0; k <= possibilities; k++) {
                printf("%.6lf %.6lf %.6lf\n", lambdas[i], periods[j], powers[k]);
                double this_kl = eval_divergence(lambdas[i], periods[j], powers[k]);
                if (this_kl < best_kl) {
                    best_kl = this_kl;
                    best_lambda = lambdas[i];
                    best_period = periods[j];
                    best_power = powers[k];
                    printf("BETTER %.6lf %.6lf %.6lf %.6lf\n", best_kl,
                            best_lambda, best_period, best_power);
                }
            }

    fprintf(fout, "%.6lf %.6lf %.6lf %.6lf\n", best_kl, best_lambda, best_period, best_power);*/
    return 0;
}
