#include <iostream>
#include <algorithm>
#include <vector>
#include <fstream>
using namespace std;

#define int long long

int total_transactions, mx_exec_time;
vector<int> exec_time, gas_fees;


int greedy_soln() {
    // order[i] = index of the i-th transaction
    vector<int> order(total_transactions);
    for (int i = 0; i < total_transactions; ++i) {
        order[i] = i;
    }

    // Sort by decreasing gas_fees / exec_time ratio
    sort(order.begin(), order.end(), [&](int a, int b) {
        // Compare gas_fees[a]/exec_time[a] > gas_fees[b]/exec_time[b]
        // without floating point: cross multiply
        long long lhs = gas_fees[a] * exec_time[b];
        long long rhs = gas_fees[b] * exec_time[a];
        if (lhs != rhs) return lhs > rhs;
        // tie-breaker: smaller execution time first
        return exec_time[a] < exec_time[b];
    });

    int used_time = 0;
    int total_fee = 0;

    for (int idx : order) {
        if (used_time + exec_time[idx] <= mx_exec_time) {
            used_time += exec_time[idx];
            total_fee += gas_fees[idx];
        }
    }

    return total_fee;
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
    
    int dp_ans = greedy_soln();

    // ofstream outputFile("output_7.txt");
    cout << dp_ans << endl;
    // outputFile.close();
    return 0;
}