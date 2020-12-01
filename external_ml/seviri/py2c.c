/* 
 * C module which takes input data from a Fortran script and
 * and calls the Python Neural network for CPH and COT. Results
 * are stored in a 3d array in linear representation.
 *
 * 2020/09/15, DP: Initial version
 * 2020/09/24, DP: Code cleanup and tabs to spaces conversion
 * 2020/10/01, DP: Fixed memory leak + simplification + improvements
 *
 * Bugs:
 * None known.
 *
 * */

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <Python.h>
#include <stdlib.h>
#include <arrayobject.h>


void py_neural_net(void *vis006, void *vis008, void *nir016, void *ir039, 
				   void *ir062, void *ir073, void *ir087, void *ir108, 
				   void *ir120, void *ir134, void *lsm, void *skt, 
				   int *nx, int *ny, float *reg_cot, char *bin_cot, 
                   float *unc_cot, float *reg_cph, char *bin_cph, 
                   float *unc_cph)
{
	
    // initialize Python interpreter
    if (!Py_IsInitialized()){
        Py_Initialize();
    }

    if(PyArray_API == NULL)
    {
            import_array(); 
    }
    
    //import_array();
    
    // declare Python Objects 
    npy_intp dims[2];
    dims[0] = *nx;
    dims[1] = *ny;
	
    PyObject *mName, *pModule, *pFunc, *args_var;
    PyObject *vis006py, *vis008py, *nir016py, *ir039py, *ir062py, *ir073py; 
    PyObject *ir087py, *ir108py, *ir120py, *ir134py, *lsmpy, *sktpy;
    PyObject *res;
	       
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

            PyArray_ENABLEFLAGS((PyArrayObject*) vis006py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) vis008py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) nir016py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir039py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir062py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir073py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir087py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir108py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir120py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) ir134py, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) lsmpy, NPY_ARRAY_OWNDATA);
            PyArray_ENABLEFLAGS((PyArrayObject*) sktpy, NPY_ARRAY_OWNDATA);

            // generate args tuple for function call
            args_var = PyTuple_Pack(12, vis006py, vis008py, nir016py, ir039py, 
                                    ir062py, ir073py, ir087py, ir108py, ir120py,
                                    ir134py, lsmpy, sktpy);
               
            // call python function for COT              
            res = PyObject_CallObject(pFunc, args_var);
                             
            /* Function call returns list in  the form of:
             *  [COT_regression, COT_binary, COT_uncertainty,
             *   CPH_regression, CPH_binary, CPH_uncertainty] */

            int idx;
            // assign numpy result arrays to pre-allocated Fortran arrays     
            if (res != NULL){
                for (int i=0; i < *nx; i++){
                    for (int j=0; j < *ny; j++){
                            
                        idx = i * *nx + j;

                        reg_cot[idx] = *(float *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 0),
                                                                  (npy_int)i, (npy_int)j);

                        bin_cot[idx] = *(char *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 1),
                                                                 (npy_int)i, (npy_int)j);

                        unc_cot[idx] = *(float *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 2), 
                                                                  (npy_int)i, (npy_int)j);

                        reg_cph[idx] = *(float *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 3), 
                                                                  (npy_int)i, (npy_int)j);

                        bin_cph[idx] = *(char *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 4),
                                                                 (npy_int)i, (npy_int)j);

                        unc_cph[idx] = *(float *) PyArray_GETPTR2((PyArrayObject *)PyList_GetItem(res, 5), 
                                                                  (npy_int)i, (npy_int)j);
                    }
			    }
               
                // decrement reference counter of this objects
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
    }
    // finalize Python interpreter
    Py_FinalizeEx(); 
}
