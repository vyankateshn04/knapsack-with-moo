#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <random>
#include <fstream>
#include <sstream>     
#include <chrono>
#include <limits>

using namespace std;

struct Transaction {
    int id;
    double gas_units;
    double tx_fee;
};

struct Individual {
    vector<int> selected_idx;

    double total_gas_units = 0.0;
    double negative_total_tx_fee = 0.0;

    double constraint_violation = 0.0;

    // NSGA-II specific fields
    int rank = 0;
    double crowding_distance = 0.0;
};

void calculate_objectives(Individual& individual, const vector<Transaction>& all_transactions, double mx_gas_units) {
    individual.total_gas_units = 0.0;
    double total_tx_fee = 0.0;
    individual.constraint_violation = 0.0;

    for (int tx_id : individual.selected_idx) {
        individual.total_gas_units += all_transactions[tx_id].gas_units;
        total_tx_fee += all_transactions[tx_id].tx_fee;
    }
    // Store the negative tx fee for minimization
    individual.negative_total_tx_fee = -total_tx_fee;

    // Check if the constraint is violated and calculate by how much
    if (individual.total_gas_units > mx_gas_units) {
        individual.constraint_violation = individual.total_gas_units - mx_gas_units;
    }
}

void toggle_selected_sorted(vector<int>& selected_idx, int tx_id) {
    auto it = lower_bound(selected_idx.begin(), selected_idx.end(), tx_id);
    if (it != selected_idx.end() && *it == tx_id) {
        selected_idx.erase(it);
    } else {
        selected_idx.insert(it, tx_id);
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
    bool better_in_one = (ind1.total_gas_units < ind2.total_gas_units) || (ind1.negative_total_tx_fee < ind2.negative_total_tx_fee);
    bool not_worse_in_any = (ind1.total_gas_units <= ind2.total_gas_units) && (ind1.negative_total_tx_fee <= ind2.negative_total_tx_fee);
    return better_in_one && not_worse_in_any;
}

namespace {
struct FenwickMax {
    explicit FenwickMax(int n) : bit(static_cast<size_t>(n) + 1, -1) {}

    void update(int idx, int value) {
        for (int i = idx + 1; i < static_cast<int>(bit.size()); i += i & -i) {
            bit[i] = max(bit[i], value);
        }
    }

    int query(int idx) const {
        int best = -1;
        for (int i = idx + 1; i > 0; i -= i & -i) {
            best = max(best, bit[i]);
        }
        return best;
    }

    vector<int> bit;
};

struct FeasiblePoint {
    int idx;
    double total_gas_units;
    double negative_total_tx_fee;
    int y_rank = 0;
};
} // namespace

// Performs the non-dominated sort on the population
// Returns a vector of fronts, where each front is a vector of indices
vector<vector<int>> non_dominated_sort(const vector<Individual>& population) {
    vector<vector<int>> fronts;
    if (population.empty()) {
        return fronts;
    }

    vector<int> feasible_idx;
    vector<int> infeasible_idx;
    feasible_idx.reserve(population.size());
    infeasible_idx.reserve(population.size());

    for (int i = 0; i < static_cast<int>(population.size()); ++i) {
        if (population[i].constraint_violation == 0.0) {
            feasible_idx.push_back(i);
        } else {
            infeasible_idx.push_back(i);
        }
    }

    if (!feasible_idx.empty()) {
        vector<FeasiblePoint> feasible_points;
        feasible_points.reserve(feasible_idx.size());

        vector<double> y_values;
        y_values.reserve(feasible_idx.size());

        for (int idx : feasible_idx) {
            feasible_points.push_back({
                idx,
                population[idx].total_gas_units,
                population[idx].negative_total_tx_fee,
                0
            });
            y_values.push_back(population[idx].negative_total_tx_fee);
        }

        sort(y_values.begin(), y_values.end());
        y_values.erase(unique(y_values.begin(), y_values.end()), y_values.end());

        for (auto& point : feasible_points) {
            point.y_rank = static_cast<int>(lower_bound(
                y_values.begin(),
                y_values.end(),
                point.negative_total_tx_fee
            ) - y_values.begin());
        }

        sort(feasible_points.begin(), feasible_points.end(),
            [](const FeasiblePoint& a, const FeasiblePoint& b) {
                if (a.total_gas_units != b.total_gas_units) {
                    return a.total_gas_units < b.total_gas_units;
                }
                if (a.negative_total_tx_fee != b.negative_total_tx_fee) {
                    return a.negative_total_tx_fee < b.negative_total_tx_fee;
                }
                return a.idx < b.idx;
            });

        FenwickMax fenwick(static_cast<int>(y_values.size()));
        int max_rank = -1;

        size_t x_begin = 0;
        while (x_begin < feasible_points.size()) {
            size_t x_end = x_begin + 1;
            while (x_end < feasible_points.size() &&
                   feasible_points[x_end].total_gas_units == feasible_points[x_begin].total_gas_units) {
                ++x_end;
            }

            size_t y_begin = x_begin;
            while (y_begin < x_end) {
                size_t y_end = y_begin + 1;
                while (y_end < x_end &&
                       feasible_points[y_end].negative_total_tx_fee == feasible_points[y_begin].negative_total_tx_fee) {
                    ++y_end;
                }

                int y_rank = feasible_points[y_begin].y_rank;
                int front_rank = fenwick.query(y_rank) + 1;
                if (front_rank > max_rank) {
                    fronts.resize(static_cast<size_t>(front_rank) + 1);
                    max_rank = front_rank;
                }

                for (size_t i = y_begin; i < y_end; ++i) {
                    fronts[front_rank].push_back(feasible_points[i].idx);
                }

                fenwick.update(y_rank, front_rank);
                y_begin = y_end;
            }

            x_begin = x_end;
        }
    }

    if (!infeasible_idx.empty()) {
        sort(infeasible_idx.begin(), infeasible_idx.end(),
            [&](int a, int b) {
                if (population[a].constraint_violation != population[b].constraint_violation) {
                    return population[a].constraint_violation < population[b].constraint_violation;
                }
                return a < b;
            });

        size_t begin = 0;
        while (begin < infeasible_idx.size()) {
            size_t end = begin + 1;
            const double violation = population[infeasible_idx[begin]].constraint_violation;
            while (end < infeasible_idx.size() &&
                   population[infeasible_idx[end]].constraint_violation == violation) {
                ++end;
            }

            fronts.emplace_back();
            auto& front = fronts.back();
            front.reserve(end - begin);
            for (size_t i = begin; i < end; ++i) {
                front.push_back(infeasible_idx[i]);
            }
            begin = end;
        }
    }

    return fronts;
}


// Calculates crowding distance for individuals within a single front
void calculate_crowding_distance(vector<Individual>& front) {
    if (front.empty()) return;
    int size = front.size();
    for (auto& ind : front) {
        ind.crowding_distance = 0.0;
    }

    // Sort by first objective (gas_units)
    sort(front.begin(), front.end(), [](const Individual& a, const Individual& b) {
        return a.total_gas_units < b.total_gas_units;
    });

    front[0].crowding_distance = front[size - 1].crowding_distance = 1e9; // Infinity for boundaries
    double f_max = front[size - 1].total_gas_units;
    double f_min = front[0].total_gas_units;
    if (f_max - f_min > 0) {
        for (int i = 1; i < size - 1; ++i) {
            front[i].crowding_distance += (front[i + 1].total_gas_units - front[i].total_gas_units) / (f_max - f_min);
        }
    }

    // Sort by second objective (tx_fee)
    sort(front.begin(), front.end(), [](const Individual& a, const Individual& b) {
        return a.negative_total_tx_fee < b.negative_total_tx_fee;
    });

    front[0].crowding_distance = front[size - 1].crowding_distance = 1e9; // Infinity for boundaries
    f_max = front[size - 1].negative_total_tx_fee;
    f_min = front[0].negative_total_tx_fee;
     if (f_max - f_min > 0) {
        for (int i = 1; i < size - 1; ++i) {
            front[i].crowding_distance += (front[i + 1].negative_total_tx_fee - front[i].negative_total_tx_fee) / (f_max - f_min);
        }
    }
}

void assign_rank_and_crowding(vector<Individual>& population) {
    if (population.empty()) return;

    auto fronts = non_dominated_sort(population);
    const double kBoundaryDistance = 1e9;

    for (auto& ind : population) {
        ind.rank = numeric_limits<int>::max();
        ind.crowding_distance = 0.0;
    }

    for (size_t front_idx = 0; front_idx < fronts.size(); ++front_idx) {
        const auto& front = fronts[front_idx];
        if (front.empty()) continue;

        for (int idx : front) {
            population[idx].rank = static_cast<int>(front_idx);
            population[idx].crowding_distance = 0.0;
        }

        if (front.size() <= 2) {
            for (int idx : front) {
                population[idx].crowding_distance = kBoundaryDistance;
            }
            continue;
        }

        vector<int> order = front;
        sort(order.begin(), order.end(), [&](int a, int b) {
            return population[a].total_gas_units < population[b].total_gas_units;
        });

        population[order.front()].crowding_distance = kBoundaryDistance;
        population[order.back()].crowding_distance = kBoundaryDistance;

        double f_min = population[order.front()].total_gas_units;
        double f_max = population[order.back()].total_gas_units;
        if (f_max - f_min > 0.0) {
            for (size_t i = 1; i + 1 < order.size(); ++i) {
                int idx = order[i];
                if (population[idx].crowding_distance < kBoundaryDistance) {
                    population[idx].crowding_distance +=
                        (population[order[i + 1]].total_gas_units - population[order[i - 1]].total_gas_units) /
                        (f_max - f_min);
                }
            }
        }

        order = front;
        sort(order.begin(), order.end(), [&](int a, int b) {
            return population[a].negative_total_tx_fee < population[b].negative_total_tx_fee;
        });

        population[order.front()].crowding_distance = kBoundaryDistance;
        population[order.back()].crowding_distance = kBoundaryDistance;

        f_min = population[order.front()].negative_total_tx_fee;
        f_max = population[order.back()].negative_total_tx_fee;
        if (f_max - f_min > 0.0) {
            for (size_t i = 1; i + 1 < order.size(); ++i) {
                int idx = order[i];
                if (population[idx].crowding_distance < kBoundaryDistance) {
                    population[idx].crowding_distance +=
                        (population[order[i + 1]].negative_total_tx_fee - population[order[i - 1]].negative_total_tx_fee) /
                        (f_max - f_min);
                }
            }
        }
    }
}

// Tournament Selection based on rank and crowding distance
int selection(const vector<Individual>& population, mt19937_64& rng) {
    uniform_int_distribution<int> pick_idx(0, static_cast<int>(population.size()) - 1);
    int i = pick_idx(rng);
    int j = pick_idx(rng);
    const Individual& ind1 = population[i];
    const Individual& ind2 = population[j];

    if (ind1.rank < ind2.rank) return i;
    if (ind2.rank < ind1.rank) return j;
    if (ind1.crowding_distance > ind2.crowding_distance) return i;
    return j;
}

// Single-point crossover
pair<Individual, Individual> crossover(
    const Individual& p1,
    const Individual& p2,
    double crossover_rate,
    mt19937_64& rng,
    int total_transactions
) {
    Individual c1 = p1, c2 = p2;
    bernoulli_distribution do_crossover(crossover_rate);
    if (do_crossover(rng)) {
        uniform_int_distribution<int> point_dist(0, total_transactions - 1);
        int crossover_point = point_dist(rng);

        auto p1_split = lower_bound(p1.selected_idx.begin(), p1.selected_idx.end(), crossover_point);
        auto p2_split = lower_bound(p2.selected_idx.begin(), p2.selected_idx.end(), crossover_point);

        c1.selected_idx.clear();
        c1.selected_idx.reserve(distance(p1.selected_idx.begin(), p1_split) +
                                distance(p2_split, p2.selected_idx.end()));
        c1.selected_idx.insert(c1.selected_idx.end(), p1.selected_idx.begin(), p1_split);
        c1.selected_idx.insert(c1.selected_idx.end(), p2_split, p2.selected_idx.end());

        c2.selected_idx.clear();
        c2.selected_idx.reserve(distance(p2.selected_idx.begin(), p2_split) +
                                distance(p1_split, p1.selected_idx.end()));
        c2.selected_idx.insert(c2.selected_idx.end(), p2.selected_idx.begin(), p2_split);
        c2.selected_idx.insert(c2.selected_idx.end(), p1_split, p1.selected_idx.end());
    }
    return {c1, c2};
}

// Bit-flip mutation
void mutate(Individual& individual, int total_transactions, double mutation_rate, mt19937_64& rng) {
    const size_t n = static_cast<size_t>(total_transactions);
    if (n == 0 || mutation_rate <= 0.0) return;

    if (mutation_rate >= 1.0) {
        for (size_t i = 0; i < n; ++i) {
            toggle_selected_sorted(individual.selected_idx, static_cast<int>(i));
        }
        return;
    }

    static thread_local geometric_distribution<size_t> gap_dist;
    static thread_local double cached_rate = -1.0;
    if (cached_rate != mutation_rate) {
        gap_dist.param(geometric_distribution<size_t>::param_type(mutation_rate));
        cached_rate = mutation_rate;
    }

    size_t idx = gap_dist(rng);

    while (idx < n) {
        toggle_selected_sorted(individual.selected_idx, static_cast<int>(idx));
        size_t gap = gap_dist(rng);
        if (idx > numeric_limits<size_t>::max() - 1 - gap) {
            break;
        }
        idx += 1 + gap;
    }
}









int main(int argc, char* argv[]) {

    if (argc != 3) {
        cerr << "Usage: ./nsga2 mempool.csv block_gas_limit\n";
        return 1;
    }

    mt19937_64 rng(42); // fixed seed for reproducibility

    string filename = argv[1];
    double mx_gas_units = stod(argv[2]);

    int population_size = 50;
    int generations = 100;
    double mutation_rate = 0.0015;
    double crossover_rate = 0.95;

    cout << "population: " << population_size << ", gen: " << generations << endl;

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


    auto start = chrono::steady_clock::now();

    int total_transactions = all_transactions.size();
    if (total_transactions == 0) return 0;
    cout << "total transactions: " << total_transactions << endl;
    // ============================================================
    // INITIALIZE POPULATION
    // ============================================================

    vector<Individual> population(population_size);

    for (auto& ind : population) {
        bernoulli_distribution init_pick(15.0 / 1000.0);
        ind.selected_idx.clear();
        ind.selected_idx.reserve(static_cast<size_t>(total_transactions * 0.05));

        for (int i = 0; i < total_transactions; ++i) {
            if (init_pick(rng)) {
                ind.selected_idx.push_back(i);
            }
        }

        calculate_objectives(ind, all_transactions, mx_gas_units);
    }

    // cout << population[0].negative_total_tx_fee << endl;

    // ============================================================
    // EVOLUTION LOOP
    // ============================================================

    long long total_p0_to_p1_us = 0;
    long long total_p1_to_p2_us = 0;
    long long total_p2_to_p3_us = 0;
    long long total_p3_to_p4_ns = 0;
    long long total_generation_us = 0;
    // long long total_children_evaluated = 0;

    for (int gen = 0; gen < generations; ++gen) {
        // auto t_start = chrono::steady_clock::now();
        
        assign_rank_and_crowding(population);
    
        // auto t_after_p1 = chrono::steady_clock::now();    
        
        vector<Individual> offspring;
        
        while (offspring.size() < population_size) {
            // auto t_gen_start = chrono::steady_clock::now();
            
            Individual p1 = population[selection(population, rng)];
            Individual p2 = population[selection(population, rng)];
            
            // auto t_after_p1 = chrono::steady_clock::now();
            
            auto children = crossover(p1, p2, crossover_rate, rng, total_transactions);
            
            // auto t_after_p2 = chrono::steady_clock::now();

            mutate(children.first, total_transactions, mutation_rate, rng);
            mutate(children.second, total_transactions, mutation_rate, rng);

            // auto t_after_p3 = chrono::steady_clock::now();

            calculate_objectives(children.first, all_transactions, mx_gas_units);
            calculate_objectives(children.second, all_transactions, mx_gas_units);

            // auto t_after_p4 = chrono::steady_clock::now();

            offspring.push_back(children.first);
            offspring.push_back(children.second);

            // total_p0_to_p1_us += chrono::duration_cast<chrono::microseconds>(t_after_p1 - t_gen_start).count();
            // total_p1_to_p2_us += chrono::duration_cast<chrono::microseconds>(t_after_p2 - t_after_p1).count();
            // total_p2_to_p3_us += chrono::duration_cast<chrono::microseconds>(t_after_p3 - t_after_p2).count();
            // total_p3_to_p4_ns += chrono::duration_cast<chrono::microseconds>(t_after_p4 - t_after_p3).count();
            // total_generation_us += chrono::duration_cast<chrono::microseconds>(t_after_p4 - t_gen_start).count();
        }

        vector<Individual> combined_pop = population;
        combined_pop.insert(combined_pop.end(), offspring.begin(), offspring.end());

        // auto t_after_p2 = chrono::steady_clock::now();

        auto fronts = non_dominated_sort(combined_pop);

        // auto t_after_p3 = chrono::steady_clock::now();

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
        auto t_after_p4 = chrono::steady_clock::now();

        // total_p0_to_p1_us += chrono::duration_cast<chrono::microseconds>(t_after_p1 - t_start).count();
        // total_p1_to_p2_us += chrono::duration_cast<chrono::microseconds>(t_after_p2 - t_after_p1).count();
        // total_p2_to_p3_us += chrono::duration_cast<chrono::microseconds>(t_after_p3 - t_after_p2).count();
        // total_p3_to_p4_ns += chrono::duration_cast<chrono::microseconds>(t_after_p4 - t_after_p3).count();
        // total_generation_us += chrono::duration_cast<chrono::microseconds>(t_after_p4 - t_start).count();
        
    }

    // cout << "time taken: " << total_p1_to_p2_us << " " 
    //     << total_p2_to_p3_us << " "
    //     << total_p3_to_p4_us << endl;

    // ============================================================
    // SELECT BEST FEASIBLE SOLUTION (MAX FEE)
    // ============================================================

    auto final_fronts = non_dominated_sort(population);

    double best_fee = -1;
    int best_index = -1;

    if (final_fronts.empty() || final_fronts[0].empty()) return 0;

    // int counting = 0, total = 0;

    for (int index : final_fronts[0]) {

        const auto& sol = population[index];

        // ++total;

        if (sol.constraint_violation == 0) {

            // ++counting;

            double fee = -sol.negative_total_tx_fee;

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

    // for (int tx_id : best_solution.selected_idx) {
    //     cout << tx_id << "\n";
    // }

    auto end = chrono::steady_clock::now();

    cout << "time taken: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << endl;
    // cout << "aggeregate p0->p1 time (ms): " << (total_p0_to_p1_us / 1000.0) << endl;
    // cout << "aggregate p1->p2 time (ms): " << (total_p1_to_p2_us / 1000.0) << endl;
    // cout << "aggregate p2->p3 time (ms): " << (total_p2_to_p3_us / 1000.0) << endl;
    // cout << "aggregate p3->p4 time (ms): " << (total_p3_to_p4_ns / 1000.0) << endl;
    // cout << "aggregate generation time (ms): " << (total_generation_us / 1000.0) << endl;

    cout << "fee: " << -best_solution.negative_total_tx_fee <<"\ngas used:" << best_solution.total_gas_units << endl;
    return 0;
}
