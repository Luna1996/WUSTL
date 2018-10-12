#include "WolframLibrary.h"
#include "mathlink.h"
#include <stdlib.h>
DLLEXPORT mint WolframLibrary_getVersion() { return WolframLibraryVersion; }
DLLEXPORT int  WolframLibrary_initialize(WolframLibraryData libData) { return 0; }
DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {}

DLLEXPORT int contour2DM(WolframLibraryData libData, MLINK mlp) {
	int *a = malloc(10 * sizeof(int));
	for (int i = 0; i < 10; i++) a[i] = i * i;
	while (MLReady(mlp)) MLGetNext(mlp);
	MLPutInteger32List(mlp, a, 10);
	free(a);
	MLFlush(mlp);

	return LIBRARY_NO_ERROR;
}