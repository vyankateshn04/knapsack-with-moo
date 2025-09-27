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

int solve_using_dp() {
    vector<int> dp(mx_exec_time+1);
    
    for(int i = total_transactions-1; i >= 0; i--) {
        for(int j = mx_exec_time; j >= exec_time[i]; j--) {
            dp[j] = max(dp[j], gas_fees[i] + dp[j-exec_time[i]]);
        }
    }
 
    return dp[mx_exec_time];
}

int solve_using_moo() {
    // step 1: Initialize the population
    // 10 for ex
    int initial_population;
    cout << "enter the intial population:\n";
    cin >> initial_population;

    // we will use string of size total trasaction for each individual
    // if the bit is set for that particular entry then it is present in the answer set
    // ex: string "10010" represent we will take transaction 0 and 3.

    // generate initial_population no. of string

}

int32_t main() {
    ifstream inputFile("input_7.txt");
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

    ofstream outputFile("output_7.txt");
    outputFile << dp_ans << endl;
    outputFile.close();
    return 0;
}