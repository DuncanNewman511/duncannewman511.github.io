/******************************************************************************
* FILE: mpi_prime.c
* DESCRIPTION: OpenMP parallel prime number generator
*              Replaces original MPI code with OpenMP threads.
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define LIMIT 2500000

int isprime(int n) {
    if (n > 10) {
        int sqrtn = (int)sqrt(n);
        for (int i = 3; i <= sqrtn; i += 2)
            if (n % i == 0)
                return 0;
        return 1;
    } else {
        return 0; // primes below 10 counted separately
    }
}

int main() {
    int total_primes = 4; // primes 2,3,5,7
    int max_prime = 7;

    double start_time = omp_get_wtime();

    #pragma omp parallel
    {
        int pc_local = 0;
        int max_local = 0;

        // Distribute odd numbers among threads
        #pragma omp for schedule(dynamic)
        for (int n = 11; n <= LIMIT; n += 2) {
            if (isprime(n)) {
                pc_local++;
                if (n > max_local) max_local = n;
            }
        }

        // Thread-safe accumulation
        #pragma omp critical
        {
            total_primes += pc_local;
            if (max_local > max_prime) max_prime = max_local;
        }
    }

    double end_time = omp_get_wtime();

    printf("Using OpenMP parallelism\n");
    printf("Done. Largest prime is %d Total primes %d\n", max_prime, total_primes);
    printf("Wallclock time elapsed: %.2lf seconds\n", end_time - start_time);

    return 0;
}
