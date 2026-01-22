#include <vector>
#include <string>
#include <numeric>
#include <algorithm>
#include <deque>
#include <limits>

using namespace std;

/**
 * This problem reduces to a dynamic programming problem.
 * 1.  Calculate the travel time for a Guardian from node 1 to any node `k`.
 * 2.  For each thief `i` targeting node `C[i][0]` at time `C[i][1]`, calculate the
 * latest departure time (`LDT`) a Guardian can leave node 1 to intercept them.
 * LDT[i] = C[i][1] - (travel time to C[i][0]).
 * 3.  If any `LDT[i] < 0`, the problem is impossible, return "-1".
 * 4.  The "thief waiting time" is `C[i][1] - (Guardian arrival time)`.
 * Let T_depart be the departure time of the guardian assigned to thief `i`.
 * Guardian arrival = T_depart + (travel time)
 * Waiting time = C[i][1] - (T_depart + travel time)
 * = (C[i][1] - travel time) - T_depart
 * = LDT[i] - T_depart
 * 5.  We want to minimize `sum(LDT[i] - T_depart_g(i))` over all thieves `i`, where
 * `T_depart_g(i)` is the departure time of the guardian assigned to thief `i`.
 * This is equivalent to: `sum(LDT) - max(sum(T_depart_g(i)))`.
 * 6.  A guardian `g` assigned a set of thieves `S_g` must depart at
 * `T_depart_g <= min(LDT[i])` for all `i` in `S_g`. To maximize the sum,
 * they will depart at exactly `T_depart_g = min(LDT[i] for i in S_g)`.
 * 7.  The score we want to maximize is `sum( |S_g| * T_depart_g )` over all `A` guardians.
 * 8.  This is a DP problem: `dp[i][j]` = max score using `j` guardians for
 * thieves `i...M-1` (after sorting LDTs).
 * `dp[i][j] = max_{k=i...M-1} ( (k-i+1) * LDT[i] + dp[k+1][j-1] )`
 * 9.  This O(M^2 * A) DP can be optimized to O(M * A) using Convex Hull Trick (CHT)
 * because the transition is of the form `max(y_k + m*x_k)`.
 */
string solve(int A, vector<int> &B, vector<vector<int>> &C) {
    int N = B.size() + 1;
    vector<long long> dist(N, 0);
    for (int i = 0; i < N - 1; ++i) {
        dist[i + 1] = dist[i] + B[i];
    }

    int M = C.size();
    if (M == 0) {
        return "0";
    }

    vector<long long> LDT(M);
    long long sumLDT = 0;

    for (int i = 0; i < M; ++i) {
        long long travelTime = dist[C[i][0] - 1];
        LDT[i] = (long long)C[i][1] - travelTime;
        if (LDT[i] < 0) {
            return "-1";
        }
        sumLDT += LDT[i];
    }

    sort(LDT.begin(), LDT.end());

    // dp[i] = max score for thieves i...M-1 using (j-1) guardians
    // We only need to store the DP state for the previous 'j'
    vector<long long> dp(M + 1, -2e18); // Use a very small number (not LLONG_MIN to avoid overflow)
    dp[M] = 0; // Base case: 0 thieves, 0 score

    for (int j = 1; j <= A; ++j) {
        vector<long long> new_dp(M + 1, -2e18);
        deque<pair<long long, long long>> hull;

        // Build hull from points (k, dp[k+1]) for k=0..M-1
        // The points are (x_k, y_k) = (k, dp[k+1]) from the (j-1) step
        for (int k = 0; k < M; ++k) {
            long long x = k;
            long long y = dp[k + 1]; 
            
            if (y <= -1e18) continue; // Skip unreachable states

            pair<long long, long long> p3 = {x, y};
            while (hull.size() >= 2) {
                auto p1 = hull[hull.size() - 2];
                auto p2 = hull[hull.size() - 1];
                
                // Check for convexity: slope(p1,p2) >= slope(p2,p3)
                // (y2-y1)*(x3-x2) >= (y3-y2)*(x2-x1)
                // Use __int128 for safety, but long long products (10^18 * 10^4) are ok.
                if ((__int128)(p2.second - p1.second) * (p3.first - p2.first) >= 
                    (__int128)(p3.second - p2.second) * (p2.first - p1.first)) {
                    hull.pop_back();
                } else {
                    break;
                }
            }
            hull.push_back(p3);
        }

        if (hull.empty()) {
            // No solution was possible with j-1 guardians, so none with j
            dp = new_dp; // new_dp is all -2e18
            continue;
        }

        // Query the hull
        // We iterate i from M-1 down to 0
        // The query slope m = LDT[i] is non-increasing
        int hull_ptr = 0;
        for (int i = M - 1; i >= 0; --i) {
            long long m = LDT[i];
            
            // Find the best point on the hull for slope m
            // We are maximizing y_k + m * x_k
            while (hull_ptr < hull.size() - 1) {
                long long val1 = hull[hull_ptr].second + m * hull[hull_ptr].first;
                long long val2 = hull[hull_ptr + 1].second + m * hull[hull_ptr + 1].first;
                if (val1 < val2) {
                    hull_ptr++;
                } else {
                    break;
                }
            }
            
            long long max_val = hull[hull_ptr].second + m * hull[hull_ptr].first;
            
            // Transition: dp[i][j] = LDT[i]*(1-i) + max_{k=i...M-1} (k*LDT[i] + dp[k+1][j-1])
            new_dp[i] = LDT[i] * (1LL - i) + max_val;
        }
        dp = new_dp; // Update dp for the next iteration of j
    }

    // After A iterations, dp[0] holds dp[0][A], the max score
    if (dp[0] <= -1e18) {
        // This case should be covered by the LDT[i] < 0 check,
        // but as a safeguard (e.g., M > 0 but A = 0, though A >= 1)
        return "-1"; 
    }

    long long max_score = dp[0];
    long long min_wait_time = sumLDT - max_score;

    return to_string(min_wait_time);
}