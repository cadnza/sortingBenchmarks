# Sorting Benchmarks

![](https://img.shields.io/github/v/release/cadnza/sortingBenchmarks)

This is a simple script to examine how different sorting algorithms behave at different volumes of elements to sort. It's really just a boredom buster, but it runs nicely on a server, and it's good at what it does.

## Sorts

The thing benchmarks the following sorting algorithms:

-   Insertion sort
-   Selection sort
-   Bubble sort
-   Merge sort
-   Heap sort
-   Quick sort

## Owner's manual

Here's how it works:

1. You give the script a value each for `nMax`, `nTrialsPer`, and `fData`.
2. For each sorting algorithm, the script starts with a vector of length `1` and times how long it takes the algorithm to sort it. Then it gets a new vector of the same length and repeats the process until it's sorted the same lengthed vector `nTrialsPer` times (so you can run stats on how long it takes for the algorithm to sort a vector of length `n`).
3. Then it increments the vector's length and goes again, repeating the process until it's benchmarked a vector of `nMax` length `nTrialsPer` times for each algorithm.

The thing loops in order of

1. trial number,
2. vector length, and
3. algorithm,

and it saves the data to `fName` after each trial number, so you can expect it to save every `nTrialsPer`th part through the process. So if your server goes down partway through the benchmarking process, go ahead and run the script again; it'll pick up where it left off.

Note that `nMax`, `nTrialsPer`, and `fData` are _unnamed_ arguments, so _e.g._ if `nMax = 1000`, `nTrialsPer = 30`, and `fData = "sorting.rds"`, then the call looks like this:

```
./benchmark.R 1000 30 sorting.rds
```

## Output

The data is saved as an R data frame object in [RDS format](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readRDS) for compatibility and granularity. It comes with the following fields:

| Variable | Description                               |
| -------- | ----------------------------------------- |
| `id`     | Observation ID                            |
| `label`  | Sorting algorithm used                    |
| `trial`  | Trial batch number from 1 to `nTrialsPer` |
| `n`      | Vector length from 1 to `nMax`            |
| `v`      | Actual vector sorted                      |
| `start`  | Starting time                             |
| `end`    | Ending time                               |
| `t`      | Total time to sort                        |

Happy benchmarking!
