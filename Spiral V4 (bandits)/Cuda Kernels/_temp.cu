
    #include "thrust/tuple.h"

    extern "C" {
	    __global__ void CPP11Test(int *c, const int *a, const int *b)
	    {
		    int i = threadIdx.x; asd
		    auto lamb = [](int x) {return x + 1; }; // Works.
		    auto t = thrust::make_tuple(1, 2, 3);
		    c[i] = a[i] + b[i];
	    }
    }
    