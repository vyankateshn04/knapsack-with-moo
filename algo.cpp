/*

knapsack algorithm
Dp Time complexity = total_transactions*mx_exec_time

solving cses - book shop problem, 
where price is eq to `execution time` and pages is eq to `gas fees` 


*/

#include <iostream>
#include <vector>
using namespace std;

#define int long long

int total_transactions, mx_exec_time;
vector<int> exec_time, gas_fees;

int solve_using_dp() {
    vector<int> dp(mx_exec_time+1);
    
    for(int i = total_transactions-1; i >= 0; i--) {
        for(int j = mx_exec_time; j >= exec_time[i]; j--) {
            dp[j] = max(dp[j], gas_fees[i] + dp[j-exec_time[i]]);
        }
    }
 
    cout << dp[mx_exec_time] << endl;
}

int solve_using_moo() {

}

int32_t main() {
    cin >> total_transactions >> mx_exec_time;
    
    exec_time.resize(total_transactions);
    gas_fees.resize(total_transactions);
    
    for(auto&e: exec_time) cin >> e;
    for(auto&e: gas_fees) cin >> e;
    
    int dp_ans = solve_using_dp();
}