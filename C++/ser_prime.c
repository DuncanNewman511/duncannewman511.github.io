/******************************************************************************
* FILE: ser_prime.c
* DESCRIPTION: Sequential prime number generator
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define LIMIT 2500000     /* Numbers to scan */
#define PRINT 100000      /* Progress print interval */

int isprime(int n) {
    if (n > 10) {
        int sqrtn = (int)sqrt(n);
        for (int i = 3; i <= sqrtn; i += 2)
            if (n % i == 0)
                return 0;
        return 1;
    } else {
        return 0;
    }
}

int main() {
    int pc = 4;      /* Count of primes below 10 (2,3,5,7) */
    int foundone = 0;

    printf("Starting. Numbers to be scanned= %d\n", LIMIT);

    for (int n = 11; n <= LIMIT; n += 2) {
        if (isprime(n)) {
            pc++;
            foundone = n;
        }
        if ((n - 1) % PRINT == 0)
            printf("Numbers scanned= %d   Primes found= %d\n", n - 1, pc);
    }

    printf("Done. Largest prime is %d Total primes %d\n", foundone, pc);
    return 0;
}
