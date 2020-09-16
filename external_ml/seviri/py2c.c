/* 
 * C module which takes input data from a Fortran script and
 * and calls the Python Neural network for CPH and COT. Results
 * are stored in a 3d array in linear representation.
 *
 * 2020/09/15, DP: Initial version
 * 2020/09/16, DP: Code cleanup and tabs to spaces conversion
 *
 * Bugs:
 * None known.
 *
 * */

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <Python.h>
#include <stdlib.h>
#include <arrayobject.h>

float *py_neural_net(void *vis006, void *vis008, void *nir016, void *ir039, 
				  void *ir062, void *ir073, void *ir087, void *ir108, 
				  void *ir120, void *ir134, void *lsm, void *skt, 
				  int *nx, int *ny)
{
	
    // initialize Python interpreter
    Py_Initialize();
    import_array();
     
    npy_intp dims[2];
    dims[0] = *nx;
    dims[1] = *ny;
	
    // declare PyObjects
    PyObject *mName, *pModule, *pFunc, *args_var;
    PyObject *vis006py, *vis008py, *nir016py, *ir039py, *ir062py, *ir073py; 
    PyObject *ir087py, *ir108py, *ir120py, *ir134py, *lsmpy, *sktpy;
    PyObject *res, *py_nx, *py_ny;
	
    int  i, j, k, idx, res_length;
    const int nvars = 2;           // COT and CPH
    const int nparams = 3;         // regression, binary, uncertainty
    int ntot = nvars * nparams; 
    int nxny = *nx * *ny;
     
    // allocate output 1d array
    float *results = (float *) malloc(nxny * ntot * sizeof(float));

	// define and import Python module 
	pModule = PyImport_ImportModule("predictCPHCOT");
	if (pModule != NULL){
	    // define function name
		pFunc = PyObject_GetAttrString(pModule, (char*)"predict_CPH_COT");
		
		if (PyCallable_Check(pFunc)){
			// create numpy arrays from C array			
			vis006py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, vis006);
			vis008py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, vis008);
			nir016py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, nir016);
			ir039py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir039);
			ir062py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir062);
			ir073py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir073);
			ir087py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir087);
			ir108py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir108);
			ir120py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir120);
			ir134py = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, ir134);
			lsmpy = PyArray_SimpleNewFromData(2, dims, NPY_BYTE, lsm);
            sktpy = PyArray_SimpleNewFromData(2, dims, NPY_FLOAT32, skt);

			// generate args tuple for function call
			args_var = PyTuple_Pack(12, vis006py, vis008py, nir016py, ir039py, 
                                    ir062py, ir073py, ir087py, ir108py, ir120py,
					                ir134py, lsmpy, sktpy);
               
			// call python function for COT              
			res = PyObject_CallObject(pFunc, args_var);
                             
            /* Function call returns list in  the form of:
             *  [COT_regression, COT_binary, COT_uncertainty,
             *   CPH_regression, CPH_binary, CPH_uncertainty] */
                         			               
            /* assign 2D Python pointers for each of the 6 result arrays to 1D flattened 
             *array using 3D linear representation indexing*/               
			if (res != NULL){
                   res_length = PyList_Size(res);
			    for (i=0; i<res_length; i++){ 
                       PyArrayObject *tmp_var = PyList_GetItem(res, i);
			        for (j=0; j < *nx; j++){
                           for (k=0; k < *ny; k++){
                               // 3d linear representaion index
                               idx = k + *ny * j + *ny * *nx * i;
			                results[idx] = *(float *) PyArray_GETPTR2(tmp_var, (npy_int)j, (npy_int)k);
                           }
			        }
                }

                // decrement reference counter of this object
                Py_DECREF(res);
                //Py_DECREF(tmp_var);
                Py_DECREF(args_var);
                    
			} else {
				// decrement reference counter of this objects
				Py_DECREF(pFunc);
				Py_DECREF(pModule);
				Py_DECREF(vis006py);
				Py_DECREF(vis008py);
				Py_DECREF(nir016py);
				Py_DECREF(ir039py);
				Py_DECREF(ir062py);
				Py_DECREF(ir073py);
				Py_DECREF(ir087py);
				Py_DECREF(ir108py);
				Py_DECREF(ir120py);
				Py_DECREF(ir134py);
				Py_DECREF(lsmpy);
				Py_DECREF(sktpy);
				PyErr_Print();
				fprintf(stderr, "Call failed\n");
			}
		} else{
			if (PyErr_Occurred())
				PyErr_Print();
			fprintf(stderr, "Cannot find function\n");
		}
		// decrement reference counter of this objects
		Py_DECREF(pModule);
		Py_DECREF(pFunc);
	} else {
		PyErr_Print();
		fprintf(stderr, "Failed to load module\n");
		// return 1;
	}
	Py_FinalizeEx();
	// finalize Python interpreter
	return results;
}
