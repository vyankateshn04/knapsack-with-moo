#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <random>
#include <fstream>
#include <sstream>     
#include <chrono>

using namespace std;

struct Transaction {
    int id;
    double exec_time;
    double gas_fee;
};

struct Individual {
    std::vector<char> included_transactions;

    double total_exec_time;
    double negative_total_gas_fee; // We minimize this to maximize the fee

    // Stores how much the exec_time exceeds the maximum allowed time.
    // This will be 0 if the solution is feasible.
    double constraint_violation = 0.0; 

    // NSGA-II specific fields
    int rank = 0; // The non-domination front number
    double crowding_distance = 0.0;
};

void calculate_objectives(Individual& individual, const std::vector<Transaction>& all_transactions, double mx_exec_time) {
    individual.total_exec_time = 0.0;
    double total_gas_fee = 0.0;
    individual.constraint_violation = 0.0;

    for (size_t i = 0; i < all_transactions.size(); ++i) {
        if (individual.included_transactions[i]) {
            individual.total_exec_time += all_transactions[i].exec_time;
            total_gas_fee += all_transactions[i].gas_fee;
        }
    }
    // Store the negative gas fee for minimization
    individual.negative_total_gas_fee = -total_gas_fee;

    // Check if the constraint is violated and calculate by how much
    if (individual.total_exec_time > mx_exec_time) {
        individual.constraint_violation = individual.total_exec_time - mx_exec_time;
    }
}

// Check for dominance using the constrained dominance principle
bool dominates(const Individual& ind1, const Individual& ind2) {
    bool ind1_is_feasible = (ind1.constraint_violation == 0);
    bool ind2_is_feasible = (ind2.constraint_violation == 0);

    // Case 1: One is feasible, the other is not
    if (ind1_is_feasible && !ind2_is_feasible) {
        return true;
    }
    if (!ind1_is_feasible && ind2_is_feasible) {
        return false;
    }

    // Case 2: Both are infeasible
    if (!ind1_is_feasible && !ind2_is_feasible) {
        // The one with the smaller violation wins
        return ind1.constraint_violation < ind2.constraint_violation;
    }

    // Case 3: Both are feasible -> use original dominance logic
    bool better_in_one = (ind1.total_exec_time < ind2.total_exec_time) || (ind1.negative_total_gas_fee < ind2.negative_total_gas_fee);
    bool not_worse_in_any = (ind1.total_exec_time <= ind2.total_exec_time) && (ind1.negative_total_gas_fee <= ind2.negative_total_gas_fee);
    return better_in_one && not_worse_in_any;
}

// Performs the non-dominated sort on the population
// Returns a vector of fronts, where each front is a vector of indices
std::vector<std::vector<int>> non_dominated_sort(const std::vector<Individual>& population) {
    int n = population.size();
    std::vector<std::vector<int>> fronts(1);
    std::vector<int> domination_count(n, 0);
    std::vector<std::vector<int>> dominated_solutions(n);

    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (dominates(population[i], population[j])) {
                dominated_solutions[i].push_back(j);
                domination_count[j]++;
            } else if (dominates(population[j], population[i])) {
                dominated_solutions[j].push_back(i);
                domination_count[i]++;
            }
        }
        if (domination_count[i] == 0) {
            fronts[0].push_back(i);
        }
    }

    int current_front = 0;
    while (current_front < fronts.size()) {
        std::vector<int> next_front;
        for (int i : fronts[current_front]) {
            for (int j : dominated_solutions[i]) {
                domination_count[j]--;
                if (domination_count[j] == 0) {
                    next_front.push_back(j);
                }
            }
        }
        current_front++;
        if (!next_front.empty()) {
            fronts.push_back(next_front);
        }
    }
    return fronts;
}


// Calculates crowding distance for individuals within a single front
void calculate_crowding_distance(std::vector<Individual>& front) {
    if (front.empty()) return;
    int size = front.size();
    for (auto& ind : front) {
        ind.crowding_distance = 0.0;
    }

    // Sort by first objective (exec_time)
    std::sort(front.begin(), front.end(), [](const Individual& a, const Individual& b) {
        return a.total_exec_time < b.total_exec_time;
    });

    front[0].crowding_distance = front[size - 1].crowding_distance = 1; // Infinity for boundaries
    double f_max = front[size - 1].total_exec_time;
    double f_min = front[0].total_exec_time;
    if (f_max - f_min > 0) {
        for (int i = 1; i < size - 1; ++i) {
            front[i].crowding_distance += (front[i + 1].total_exec_time - front[i].total_exec_time) / (f_max - f_min);
        }
    }

    // Sort by second objective (gas_fee)
    std::sort(front.begin(), front.end(), [](const Individual& a, const Individual& b) {
        return a.negative_total_gas_fee < b.negative_total_gas_fee;
    });

    front[0].crowding_distance = front[size - 1].crowding_distance = 1; // Infinity for boundaries
    f_max = front[size - 1].negative_total_gas_fee;
    f_min = front[0].negative_total_gas_fee;
     if (f_max - f_min > 0) {
        for (int i = 1; i < size - 1; ++i) {
            front[i].crowding_distance += (front[i + 1].negative_total_gas_fee - front[i].negative_total_gas_fee) / (f_max - f_min);
        }
    }
}

// Tournament Selection based on rank and crowding distance
Individual selection(const std::vector<Individual>& population) {
    int i = rand() % population.size();
    int j = rand() % population.size();
    const Individual& ind1 = population[i];
    const Individual& ind2 = population[j];

    if (ind1.rank < ind2.rank) return ind1;
    if (ind2.rank < ind1.rank) return ind2;
    if (ind1.crowding_distance > ind2.crowding_distance) return ind1;
    return ind2;
}

// Single-point crossover
std::pair<Individual, Individual> crossover(const Individual& p1, const Individual& p2, double crossover_rate) {
    Individual c1 = p1, c2 = p2;
    if((double)rand() / RAND_MAX > crossover_rate) {
        int crossover_point = rand() % p1.included_transactions.size();
        for (int i = crossover_point; i < p1.included_transactions.size(); ++i) {
            std::swap(c1.included_transactions[i], c2.included_transactions[i]);
        }
    }
    return {c1, c2};
}

// Bit-flip mutation
void mutate(Individual& individual, double mutation_rate) {
    for (size_t i = 0; i < individual.included_transactions.size(); ++i) {
        if ((double)rand() < mutation_rate * RAND_MAX) {
            individual.included_transactions[i] = !individual.included_transactions[i];
        }
    }
}









int main(int argc, char* argv[]) {

    if (argc != 3) {
        cerr << "Usage: ./nsga2 mempool.csv block_gas_limit\n";
        return 1;
    }

    srand(42);  // fixed seed for reproducibility

    string filename = argv[1];
    double mx_exec_time = stod(argv[2]);

    int population_size = 100;
    int generations = 200;
    double mutation_rate = 0.001;
    double crossover_rate = 0.95;

    // ============================================================
    // READ CSV
    // Extract:
    //   gas_used (column 12)
    //   tx_fee_paid_wei (column 17)
    // ============================================================

    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error: Could not open mempool file\n";
        return 1;
    }

    string line;
    getline(file, line);  // skip header

    vector<Transaction> all_transactions;
    int idx = 0;

    while (getline(file, line)) {

        stringstream ss(line);
        string token;
        vector<string> columns;

        while (getline(ss, token, ',')) {
            columns.push_back(token);
        }

        if (columns.size() < 18)
            continue;  // safety check

        double gas_used = stod(columns[12]);      // gas_used
        long long fee_paid = stoll(columns[17]); // tx_fee_paid_wei

        // cout << gas_used << " , fee: " << fee_paid << endl; 
        // Store fee as double (NSGA uses double internally)
        all_transactions.push_back({idx, gas_used, (double)fee_paid});
        idx++;
    }

    file.close();


    auto start = chrono::high_resolution_clock::now();

    int total_transactions = all_transactions.size();
    if (total_transactions == 0) return 0;
    cout << total_transactions << endl;
    // ============================================================
    // INITIALIZE POPULATION
    // ============================================================

    vector<Individual> population(population_size);

    for (auto& ind : population) {
        ind.included_transactions.resize(total_transactions);

        for (int i = 0; i < total_transactions; ++i) {
            ind.included_transactions[i] = (rand() % 1000 < 15);
        }

        calculate_objectives(ind, all_transactions, mx_exec_time);
    }

    cout << population[0].negative_total_gas_fee << endl;

    // ============================================================
    // EVOLUTION LOOP
    // ============================================================

    for (int gen = 0; gen < generations; ++gen) {

        vector<Individual> offspring;

        while (offspring.size() < population_size) {

            Individual p1 = selection(population);
            Individual p2 = selection(population);

            auto children = crossover(p1, p2, crossover_rate);

            mutate(children.first, mutation_rate);
            mutate(children.second, mutation_rate);

            calculate_objectives(children.first, all_transactions, mx_exec_time);
            calculate_objectives(children.second, all_transactions, mx_exec_time);

            offspring.push_back(children.first);
            offspring.push_back(children.second);
        }

        vector<Individual> combined_pop = population;
        combined_pop.insert(combined_pop.end(), offspring.begin(), offspring.end());

        auto fronts = non_dominated_sort(combined_pop);

        vector<Individual> next_population;

        for (const auto& front_indices : fronts) {

            vector<Individual> current_front;

            for (int index : front_indices)
                current_front.push_back(combined_pop[index]);

            if (next_population.size() + current_front.size() <= population_size) {

                next_population.insert(next_population.end(),
                                       current_front.begin(),
                                       current_front.end());

            } else {

                calculate_crowding_distance(current_front);

                sort(current_front.begin(), current_front.end(),
                     [](const Individual& a, const Individual& b) {
                         return a.crowding_distance > b.crowding_distance;
                     });

                int remaining = population_size - next_population.size();

                next_population.insert(next_population.end(),
                                       current_front.begin(),
                                       current_front.begin() + remaining);
                break;
            }
        }

        population = next_population;
    }

    // ============================================================
    // SELECT BEST FEASIBLE SOLUTION (MAX FEE)
    // ============================================================

    auto final_fronts = non_dominated_sort(population);

    double best_fee = -1;
    int best_index = -1;

    if (final_fronts.empty() || final_fronts[0].empty()) return 0;

    int counting = 0, total = 0;

    for (int index : final_fronts[0]) {

        const auto& sol = population[index];

        ++total;

        if (sol.constraint_violation == 0) {

            ++counting;

            double fee = -sol.negative_total_gas_fee;

            if (fee > best_fee) {
                best_fee = fee;
                best_index = index;
            }
        }
    }

    // cout << "number of valid solutions: " << counting << "/" << total << endl;

    // cout << "best index: " << best_index << endl;

    if (best_index == -1) return 0;

    const auto& best_solution = population[best_index];

    // ============================================================
    // OUTPUT ONLY SELECTED TRANSACTION INDICES
    // ============================================================

    // for (int i = 0; i < best_solution.included_transactions.size(); ++i) {
    //     if (best_solution.included_transactions[i]) {
    //         cout << i << "\n";
    //     }
    // }

    auto end = chrono::high_resolution_clock::now();

    cout << "time taken: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << endl;

    cout << "fee: " << -best_solution.negative_total_gas_fee <<"\ngas used:" << best_solution.total_exec_time << endl;
    return 0;
}
