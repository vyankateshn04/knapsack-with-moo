/*

knapsack algorithm
Dp Time complexity = total_transactions*mx_exec_time

solving cses - book shop problem, 
where price is eq to `execution time` and pages is eq to `gas fees` 


*/

#include <iostream>
#include <vector>
#include <fstream>
using namespace std;

#define int long long

int total_transactions, mx_exec_time;
vector<int> exec_time, gas_fees;

// Recursion explanation:
// solve(i, remaining_time) = maximum gas fees we can get considering transactions from index i to end
//                            with remaining_time execution time left
// Base case: if i == total_transactions, return 0 (no more transactions)
//           if remaining_time < 0, return -infinity (invalid state)
// Recurrence: solve(i, remaining_time) = max(
//                 solve(i+1, remaining_time),  // don't take transaction i
//                 gas_fees[i] + solve(i+1, remaining_time - exec_time[i])  // take transaction i
//             )

int solve_using_dp() {
    // dp[i][j] = maximum gas fees using transactions from index i to end with j execution time remaining
    vector<vector<int>> dp(total_transactions + 1, vector<int>(mx_exec_time + 1, 0));
    
    // Base case: dp[total_transactions][j] = 0 for all j (already initialized)
    
    // Fill the dp table from bottom to top
    for(int i = total_transactions - 1; i >= 0; i--) {
        for(int j = 0; j <= mx_exec_time; j++) {
            // Option 1: Don't take transaction i
            dp[i][j] = dp[i+1][j];
            
            // Option 2: Take transaction i (if we have enough time)
            if(j >= exec_time[i]) {
                dp[i][j] = max(dp[i][j], gas_fees[i] + dp[i+1][j - exec_time[i]]);
            }
        }
    }
    
    return dp[0][mx_exec_time];
}

int32_t main() {
    ifstream inputFile("ip4.txt");
    if (!inputFile.is_open()) {
        cerr << "Could not open the file!" << endl;
        return 1;
    }
    inputFile >> total_transactions >> mx_exec_time;
    
    exec_time.resize(total_transactions);
    gas_fees.resize(total_transactions);
    
    for(auto&e: exec_time) inputFile >> e;
    for(auto&e: gas_fees) inputFile >> e;
    
    int dp_ans = solve_using_dp();

    // ofstream outputFile("output_7.txt");
    cout << dp_ans << endl;
    // outputFile.close();
    return 0;
}