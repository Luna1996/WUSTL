#ifndef H3_C
#define H3_C
#include <stdlib.h>

int* contour2D_(float** img, int W, int H, int iso) {
	int    nv = 0, ne = 0;
	float* v = malloc(W * H * 4 * sizeof(float));
	int*   e = malloc(W * H * 4 * sizeof(int));
}

#endif