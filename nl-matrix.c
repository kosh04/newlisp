/* nl-matrix.c --- matrix functions for newLISP

    Copyright (C) 2016 Lutz Mueller

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

/* Since version 8.9.9 all matrix operations work on lists and 
   arrays. Previously only lists where supported.
*/

#include "newlisp.h"
#include "protos.h"
#include "float.h"

double * * multiply(double ** A, double ** B, int m, int n, int k, int l);
double * * invert(double * * A, int n, int * err, double tiny);
int ludcmp(double * * a, int n, int * indx, double * d, double tiny);
void lubksb(double * * a, int n, int * indx, double * b);
double * * getMatrix(CELL * params, int * type, int * n, int * m, int * err);
double * * makeMatrix(CELL * number, int n, int m);
int getDimensions(CELL * mat, int * n, int * m);
double * * data2matrix(CELL * list, int n, int m);
CELL * matrix2data(double * * matrix, int type, int n, int m);
double * * allocateMatrix(int rows, int cols);
void freeMatrix(double * * m, int rows);

/* since version 7.4.8 'transpose' tramspose any matrix not only
   matrices containing numbers.

   some of the algorithms used come from C LAPACK and index
   starting 1 not 0 as usual in C
*/ 
CELL * p_matTranspose(CELL * params)
{
CELL * A;
CELL * B;
CELL * rowA = NULL;
CELL * cell = NULL;
CELL * conCell;
CELL * new;
int n, m, i, j;
CELL * * Brows;

A = evaluateExpression(params);

if(A->type == CELL_ARRAY)
    return(arrayTranspose(A));

if(A->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_NOT_MATRIX, params));

if(getDimensions(A, &n, &m) == FALSE)
    return(errorProcExt(ERR_NOT_MATRIX, params));
    
if(n == 0 || m == 0)
    return(errorProcExt(ERR_WRONG_DIMENSIONS, params));

Brows = allocMemory(sizeof(CELL *) * m);
for(j = 0; j < m; j++)
    {
    Brows[j] = getCell(CELL_EXPRESSION);
    if(j > 0) Brows[j-1]->next = Brows[j];
    }

B = makeCell(CELL_EXPRESSION, (UINT)Brows[0]);

for(i = 0; i < n; i++)
    {
    if(i == 0)
        rowA = (CELL*)A->contents;
    else
        rowA = rowA->next;

    if(rowA->type != CELL_EXPRESSION)
        conCell = rowA;
    else
        conCell = NULL;

    for(j = 0; j < m; j++)
        {
        if(conCell == NULL)
            {
            if(j == 0)
                cell = (CELL*)rowA->contents;
            else
                cell = cell->next;
            new = copyCell(cell);
            }
        else
            new = copyCell(conCell);

        if( i == 0)
            Brows[j]->contents = (UINT)new;
        else
            Brows[j]->next = new;

        Brows[j] = new;
        }
    }

free(Brows);

return(B);
}


CELL * p_matMultiply(CELL * params)
{
CELL * C;
double * * X = NULL;
double * * Y = NULL;
int typeX, typeY, typeM;
double * * M = NULL;
int n, m, k, l;
int err = 0;

if((X = getMatrix(params, &typeX, &n, &m, &err)) == NULL)
    return(errorProcExt(err, params));

if((Y = getMatrix(params->next, &typeY, &k, &l, &err)) == NULL)
    {
    freeMatrix(X, n);
    return(errorProcExt(err, params->next));
    }

typeM = (typeX == CELL_ARRAY || typeY == CELL_ARRAY) ? CELL_ARRAY : CELL_EXPRESSION;

if(m != k)
    err = ERR_WRONG_DIMENSIONS;
else if((M = multiply(X, Y, n, m, k, l)) == NULL)
    err = ERR_NOT_ENOUGH_MEMORY;

freeMatrix(X, n);
freeMatrix(Y, k);

if(err) return(errorProc(err));

C = matrix2data(M, typeM, n, l);
freeMatrix(M, n);

return(C);
}

CELL * p_matInvert(CELL * params)
{
CELL * dataY;
int n, m, err = 0;
int typeA;
double * * A;
double * * Y;
#ifdef FP_NAN
double tiny = FP_NAN;
#else
double tiny = sqrt(-1);
#endif

if((A = getMatrix(params, &typeA, &n, &m, &err)) == NULL)
    return(errorProcExt(err, params));

if(params->next != nilCell)
    getFloat(params->next, &tiny);

if(n != m) 
    {
    freeMatrix(A, n);
    return(errorProcExt(ERR_WRONG_DIMENSIONS, params));
    }

if( (Y = invert(A, n, &err, tiny)) == NULL)
    {
    freeMatrix(A, n);
    if(err) return(errorProc(err));
    return(nilCell);
    }

dataY = matrix2data(Y, typeA, n, n);

freeMatrix(A, n);
freeMatrix(Y, n);

return(dataY);
}


CELL * p_determinant(CELL * params)
{
double * * M;
double d;
int typeM, n, m, i, err;
int * indx;
#ifdef FP_NAN
double tiny = FP_NAN;
#else
double tiny = sqrt(-1);
#endif

if( (M = getMatrix(params, &typeM, &n, &m, &err)) == NULL)
    return(errorProcExt(err, params));

if(params->next != nilCell)
    getFloat(params->next, &tiny);

if(n != m) 
    {
    freeMatrix(M, n);
    return(errorProcExt(ERR_WRONG_DIMENSIONS, params));
    }

indx = (int *)calloc((n + 1), sizeof(int));

if(ludcmp(M, n, indx, &d, tiny) == FALSE)
    {
    free(indx);
    freeMatrix(M, n);
    return(nilCell);
    }
    
for(i = 1; i <= n; i++)
    d *= M[i][i];

free(indx);
freeMatrix(M, n);

return(stuffFloat(d));
}


CELL * p_matScalar(CELL * params)
{
CELL * op;
double * * A = NULL;
double * * B = NULL;
double * * M = NULL;
int typeA, typeB, typeC = 0;
int type;
int n, m, k, l, err = 0;

CELL * result;
op = params;
op = evaluateExpression(op);

if(op->type == CELL_SYMBOL)
    type = *((SYMBOL *)op->contents)->name;
else if(op->type == CELL_PRIMITIVE)
    type = *(char *)op->aux;
else
    return(errorProcExt(ERR_ILLEGAL_TYPE, params));

params = params->next;

if( (A = getMatrix(params, &typeA, &n, &m, &err)) == NULL)
    return(errorProcExt(err, params));

getEvalDefault(params->next, &result);
if(isNumber(result->type))
    {
    k = n, l = m;
    if((B = makeMatrix(result, k, l)) == NULL)
        {
        err = ERR_NOT_ENOUGH_MEMORY;
        goto MAT_SCALAR_FIN;
        }
    typeB = CELL_EXPRESSION;
    }
else
    { 
    params = makeCell(CELL_QUOTE, (UINT)result);
    if( (B = getMatrix(params, &typeB, &k, &l, &err)) == NULL)
        {
        freeMatrix(A, n);
        return(errorProc(err));
        }
    else
        {
        params->contents = (UINT)nilCell;
        deleteList(params);
        }
    }

typeC = (typeA == CELL_ARRAY || typeB == CELL_ARRAY) ? CELL_ARRAY : CELL_EXPRESSION;

if(n != k || m != l)
    {
    freeMatrix(A, n);
    freeMatrix(B, k);
    return(errorProc(ERR_WRONG_DIMENSIONS));
    }
else if((M = allocateMatrix(n, m)) == NULL)
    err = ERR_NOT_ENOUGH_MEMORY;
else 
    {
    for(k = 1; k <= n; k++)
     for(l = 1; l <= m; l++)
        {
        switch(type)
            {
            case '+': M[k][l] = A[k][l] + B[k][l]; break;
            case '-': M[k][l] = A[k][l] - B[k][l]; break;
            case '*': M[k][l] = A[k][l] * B[k][l]; break;
            case '/': M[k][l] = A[k][l] / B[k][l]; break;
            default:
            return(errorProcExt(ERR_ILLEGAL_TYPE, op));
            }
        }
}

MAT_SCALAR_FIN:
freeMatrix(A, n);
freeMatrix(B, n);
if(err) return (errorProc(err));

result = matrix2data(M, typeC, n, m);
freeMatrix(M, n);

return(result);
}


/* ------------------- C = A*B, A and B unchanged --------------------- */

double * * multiply(double ** A, double ** B, int n, int m, int k, int l)
{
double * * C;
int i, j, s;
double sum;

if(m != k)
    {
    errorProc(ERR_WRONG_DIMENSIONS);
    return(NULL);
    }

if((C = allocateMatrix(n, l)) == NULL)
    return(NULL);

for(i = 1; i <= n; i++)
    {
    for(j = 1; j <= l; j++)
        {
        sum = 0.0;
        for(s = 1; s <= m; s++)
            sum += A[i][s] * B[s][j];
        C[i][j] = sum;
        }
    }

return(C);
}


/* ----- return inverse of A in Y, A will contain LU decomposition --- */

double * * invert(double * * A, int n, int * err, double tiny)
{
double * * Y = NULL;
double * col;
double d;
int i, j;
int * indx;


col = (double *)calloc(n + 1, sizeof(double));
indx = (int *)calloc((n + 1), sizeof(int));

if(ludcmp(A, n, indx, &d, tiny) == FALSE)
    goto INVERT_FIN;

if((Y = allocateMatrix(n, n)) == NULL)
    {
    *err = ERR_NOT_ENOUGH_MEMORY;
    goto INVERT_FIN;
    }

for(j = 1; j <= n; j++)
    {
    for(i = 1; i <= n; i++) col[i] = 0.0;
    col[j] = 1.0;
    lubksb(A, n, indx, col);
    for(i = 1; i <= n; i++) Y[i][j] = col[i];
    }

INVERT_FIN:
free(col);
free(indx);

return(Y);
}


/* ------------------------- LU decomposition --------------------------- 
// algorithms ludcmp() and lubskb() adapted from:
// Numerical Recipes in 'C', 2nd Edition
// W.H. Press, S.A. Teukolsky
// W.T. Vettering, B.P. Flannery
*/

int ludcmp(double * * a, int n, int * indx, double * d, double tiny)
{
int i, imax = 0, j, k;
double big, dum, sum, temp;
double * vv;

vv = (double *)calloc(n + 4, sizeof(double));
*d = 1.0;

/* find abs biggest number in each row and put 1/big in vector */
for (i = 1; i <= n; i++)
    {
    big = 0.0;
    for(j = 1;j <= n; j++)
        if ((temp = fabs(a[i][j])) > big) big = temp;

    if(big == 0.0)
        {
        free(vv);
        return(FALSE);
        }

    vv[i] = 1.0 / big;
    }

for (j = 1; j <= n; j++)
    {
    for (i = 1; i < j; i++)
        {
        sum = a[i][j];
        for(k = 1; k < i; k++) sum -= a[i][k] * a[k][j];
        a[i][j] = sum;
        }
    big = 0.0;

    for (i = j; i <= n; i++)
        {
        sum = a[i][j];
        for(k = 1; k < j; k++)
            sum -= a[i][k] * a[k][j];
        a[i][j] = sum;

        if ( (dum = vv[i] * fabs(sum)) >= big)
            {
            big = dum;
            imax = i;
            }
        }

    if (j != imax)
        {
        for (k = 1; k <= n; k++)
            {
            dum = a[imax][k];
            a[imax][k] = a[j][k];
            a[j][k] = dum;
            }

        *d = -(*d);
        vv[imax] = vv[j];
        }


    indx[j] = imax;

    if (a[j][j] == 0.0) 
        {
#ifdef FP_NAN
        if(tiny != FP_NAN)
#else
        if(!isnan(tiny)) 
#endif
            a[j][j] = tiny;
        else
            {
            free(vv);
            return(FALSE);
            }
        }

    if (j != n)
        {
        dum = 1.0 / (a[j][j]);
        for(i = j+1; i <= n; i++) a[i][j] *= dum;
        }
    }

free(vv);
return(TRUE);
}


void lubksb(double * * a, int n, int * indx, double * b)
{
int i, ii = 0, ip, j;
double sum;

for (i = 1; i <= n; i++)
    {
    ip = indx[i];
    sum = b[ip];
    b[ip] = b[i];
    if (ii)
        for(j = ii; j <= i-1; j++) sum -= a[i][j] * b[j];
    else
        if(sum) ii = i;
    b[i] = sum;
    }

for (i = n; i >= 1; i--)
    {
    sum = b[i];
    for(j = i+1; j <= n; j++) sum -= a[i][j] * b[j];
    b[i] = sum / a[i][i];
    }
}

/* ------------ KMEANS clustering ------------------------------- */

#ifdef KMEANS
#define DOUBLE_MAX 1.0e308 /* approximate for IEEE 754 */

/* (kmeans-train data k context [centroidsi [criterium]])
   data = n * m matrix (list or array)
   k = number of clusters
   centroids = k * m matrix of start centroids (optional) (list or array)
   when centroids then optional convergence criterium, 1e-10 by default
   the m of optional centroids may be smaller than m in data to force
   reduction of dimension, e.g. when the last column of date is pre-labeled

   data and optional centroids as either array or list

   if no centroids are given, they are calculated from
   inital random membership

   returns a list of idecreasing inner SSQs from start to 
   end of iterations

   result context ctx
   ctx:centroids (m * k list)
   ctx:clusters list with k sublists of indices into data (k sublists)
   cts:labels list with n cluster numbers for each data records  (n list)

   labels go from 1 to k

   NOTE, indices in C-source are not 0-based as usually in C but 
   go from 1 to N
*/
CELL * p_kmeansTrain(CELL * params)
{
double * * X; /* data */
double * * C; /* centroids `*/
int * labels; /* membership */
int * counts; /* cluster sizes */
double * ssqs; /* inner SSQs of clusters */
double ssqTotal; /* total squared sum inner deviations */
double ssqRecord; /* inner ssq of a record in a cluster */
double ssqMin;     /* minimum ssq for one record */
double ssqPrev = DOUBLE_MAX;
double dist;
double crit = 1.0e-10;
CELL * centroids;
CELL * clusters;
CELL * deviations;
CELL * SSQlist = NULL;
CELL * membership;
CELL * * cellArray;
SYMBOL * ctx;
SYMBOL * sPtr;
int n, m, k; /* rows, columns, groups */
int i, j, l; /* loop imdices */
int err = 0, type;
int kc, mc;
UINT p;

if((X = getMatrix(params, &type, &n, &m, &err)) == NULL)
    return(errorProcExt(err, params));

params = getInteger(params->next, &p);
k = p;

if((ctx = getCreateContext(params, TRUE)) == NULL)
  return(errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, params));

membership = getCell(CELL_EXPRESSION);
sPtr = translateCreateSymbol("labels", CELL_EXPRESSION, ctx, TRUE); 
assignSymbol(sPtr, membership);

/* make centroid matrix */
params = params->next;
if(params == nilCell)
    C = allocateMatrix(k, m);
/* or get predefined centroids from user */
else 
    {
    if((C = getMatrix(params, &type, &kc, &mc, &err)) == NULL)  
        return(errorProcExt(err, params));
    /* allow forcing less columns via optional centroids mc <= m */
    if((kc != k) || (mc >  m)) 
        return(errorProcExt(ERR_WRONG_DIMENSIONS, params));
    m = mc;
    /* optionally custom criterium */
    if(params->next != nilCell)
        getFloat(params->next, &crit);
    }

counts = allocMemory((k + 1) * sizeof(int));
labels = allocMemory((n + 1) * sizeof(int));
ssqs = callocMemory((k + 1) * sizeof(double));

if(params != nilCell)
    goto ASSIGN_MEMBERSHIP;

/* asssign random membership */
for(i = 1; i <= n; i++)
    labels[i] = (i % k) + 1;

ITERATE_KMEANS:
/* calc KMEANS centroids from membership */
for(l = 1; l <= k; l++)     
    {
    counts[l] = 0;
    ssqs[l] = 0.0;
    for(j = 1; j <= m; j++)
        C[l][j] = 0;
    }

for(i = 1; i <= n; i++)
    {
    l = labels[i];
    counts[l] += 1;
    for(j = 1; j <= m; j++)
        C[l][j] += X[i][j];
    }

for(l = 1; l <= k; l++) 
    for(j = 1; j <= m; j++)
        C[l][j] = C[l][j] / counts[l];

/* assign each record in X to closest centroid */
ASSIGN_MEMBERSHIP:
ssqTotal = 0.0;
for(i = 1; i <= n; i++)
    {
    ssqMin = DOUBLE_MAX;
    for(l = 1; l <= k; l++)
        {
        ssqRecord = 0.0;
        for(j = 1; j <= m; j++)
            {
            dist = X[i][j] - C[l][j];
            ssqRecord += dist * dist;
            }
        if(ssqRecord < ssqMin)
            {
            ssqMin = ssqRecord;
            labels[i] = l;
            }
        }
    ssqTotal += ssqMin;
    ssqs[labels[i]] += ssqMin;
    }

if(fabs(ssqPrev - ssqTotal) > crit)
    {
    ssqPrev = ssqTotal;
    if(SSQlist == NULL) SSQlist = getCell(CELL_EXPRESSION);
    addList(SSQlist, stuffFloat(ssqTotal));
    goto ITERATE_KMEANS;
    }
            
/* entroid matrix as nested list */
centroids = matrix2data(C, CELL_EXPRESSION, k, m);
sPtr = translateCreateSymbol("centroids", CELL_EXPRESSION, ctx, TRUE); 
assignSymbol(sPtr, centroids);
/* average intra cluster deviation */
deviations = getCell(CELL_EXPRESSION);
sPtr = translateCreateSymbol("deviations", CELL_EXPRESSION, ctx, TRUE); 
assignSymbol(sPtr, deviations);

/* cluster memberships, for each cluster a list of X data record indices */
clusters = getCell(CELL_EXPRESSION);
cellArray = (CELL * *)allocMemory(k * sizeof(CELL));
for(l = 1; l <= k; l++)
    {
    cellArray[l] = getCell(CELL_EXPRESSION);
    addList(clusters, cellArray[l]);
    dist = sqrt(ssqs[l] / counts[l]);
    addList(deviations, stuffFloat(dist)); 
    }
for(i = 1; i <= n; i++)
    {
    l = labels[i];
    addList(cellArray[l], stuffInteger(i - 1));
    }

sPtr = translateCreateSymbol("clusters", CELL_EXPRESSION, ctx, TRUE); 
assignSymbol(sPtr, clusters);

for(i = 1; i <= n; i++) 
    addList(membership, stuffInteger(labels[i]));

freeMatrix(C, k);
freeMatrix(X, n);
free(cellArray);
free(ssqs);
free(labels);
free(counts);

return(SSQlist);
}

/* (kmeans-query data-record-vector centroids)

   calculates eucledian distances to all centroids
   from a previous (kmeans-train data k context [centroids])

   can also be used on the original data to get
   eucledian distance from one data record to all
   other records (a KNN fucntion could be built this way)

   (kmeans-query data-record-vector data)

*/

CELL * p_kmeansQuery(CELL * params)
{
CELL * data;
CELL * centroids;
CELL * dat;
CELL * result;
CELL * row;
CELL * cent;
double dist;
double ssq;

params = getEvalDefault(params, &data);
if(data->type != CELL_EXPRESSION)
    errorProcExt(ERR_LIST_EXPECTED, data);

getEvalDefault(params, &centroids);
if(centroids->type == CELL_ARRAY)
    {
    centroids = arrayList(centroids, TRUE); 
    pushResult(centroids);
    }
   
else if(centroids->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_NOT_MATRIX, centroids));

row = (CELL *)centroids->contents;
if(row->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_LIST_EXPECTED, row));

result = getCell(CELL_EXPRESSION);
while(row != nilCell) /* for each centroid */
    {
    cent = (CELL *)row->contents;
    dat = (CELL *)data->contents;
    ssq = 0.0;
    while(cent != nilCell && dat != nilCell) 
        {
        dist = fabs(getDirectFloat(cent) - getDirectFloat(dat));
        ssq += dist * dist; 
        cent = cent->next;
        dat = dat->next;
        }
    dist = sqrt(ssq);
    addList(result, stuffFloat(dist));
    row = row->next;
    }

return(result);
}

#endif

/* ------------ make a matrix from list or array expr ----------- */

double * * getMatrix(CELL * params, int * type, int * n, int * m, int *err)
{
CELL * data;
double * * M;

getEvalDefault(params, &data);

if(data->type != CELL_EXPRESSION && data->type != CELL_ARRAY)
    {
    *err = ERR_NOT_MATRIX;
    return(NULL);
    }

*type = data->type;

if(getDimensions(data, n, m) == FALSE)
    {
    *err = ERR_NOT_MATRIX;
    return(NULL);
    }

if( (M = data2matrix(data, *n, *m)) == NULL)
    {
    *err = ERR_NOT_ENOUGH_MEMORY;
    return(NULL);
    }

return(M);
}

double * * makeMatrix(CELL * number, int n, int m)
{
double s;
double * * matrix;
int i, j;

#ifndef NEWLISP64
if(number->type == CELL_FLOAT)
    s = *(double *)&number->aux;
else if(number->type == CELL_INT64)
    s = *(INT64 *)&number->aux;
#else
if(number->type == CELL_FLOAT)
    s = *(double *)&number->contents;
#endif
else
    s = *(INT64 *)&number->contents;

if((matrix = allocateMatrix(n, m)) == NULL)
    return(NULL);

for(i = 1; i <= n; i++)
    for(j = 1; j <= m; j++)
        matrix[i][j] = s;

return(matrix);
}

/* --------------------------- get dimensons on a matrix --------------- */

int getDimensions(CELL * mat, int * n, int * m)
{
CELL * row;
CELL * cell;
int rows, cols;
CELL * * addr;

if(mat->type == CELL_ARRAY)
    {
    addr = (CELL * *)mat->contents;
    *n = (mat->aux - 1) / sizeof(CELL *);
    cell = *addr;
    if(cell->type != CELL_ARRAY)
        {
        errorProcExt(ERR_WRONG_DIMENSIONS, mat);
        return(FALSE);
        }
    *m = (cell->aux - 1) / sizeof(CELL *);
    return(TRUE);
    }

/* mat->type is CELL_EXPRESSSION */

row = (CELL *)mat->contents;
if(row->type != CELL_EXPRESSION)
    {
    errorProcExt(ERR_NOT_MATRIX, mat);
    return(FALSE);
    }

cell = (CELL *)row->contents;

rows = cols = 0;
while(row != nilCell)
    {
    ++rows;
    row = row->next;
    }

while(cell != nilCell)
    {
    ++cols;
    cell = cell->next;
    }

if(rows == 0 || cols == 0)
    {
    errorProcExt(ERR_WRONG_DIMENSIONS, mat);
    return(FALSE);
    }

*n = rows;
*m = cols;

return(TRUE);
}


/* ------------ copy array/list into a matrix of doubles -------- */


/* xlate lists or arrays into C arrays with 1-offest for 1st element
wrong row type result and missing row elelements result on 0.0 */
double * * data2matrix(CELL * list, int n, int m)
{
double ** matrix;
CELL * row;
CELL * cell;
int i, j, size;
CELL * * addr;
CELL * * rowAddr;

if((matrix = allocateMatrix(n, m)) == NULL)
    return(NULL);

if(list->type == CELL_ARRAY)
    {
    addr = (CELL * *)list->contents;    
    for(i = 1; i <= n; i++)
        {
        row = *(addr + i - 1);
        if(row->type != CELL_ARRAY)
            {
            for(j = 1; j <= m; j++) matrix[i][j] = 0.0;
            continue;
            }
        rowAddr = (CELL * *)row->contents;
        size = (row->aux - 1)/sizeof(CELL *);
        for(j = 1; j <= m; j++)
            {
            if(j <= size)
                matrix[i][j] = getDirectFloat((CELL *)*(rowAddr + j - 1));
            else matrix[i][j] = 0.0;
            }
        }
    return(matrix);
    }

/* list->type == CELL_EXPRESSION */

row = (CELL *)list->contents;
for(i = 1; i <= n; i++)
    {
    if(row->type != CELL_EXPRESSION)
        for(j = 1; j <= m; j++) matrix[i][j] = 0.0;
    else
        {
        cell = (CELL *)row->contents;
        for(j = 1; j <= m; j++)
            {
            matrix[i][j] = getDirectFloat(cell);
            cell = cell->next;
            }
        }
    row = row->next;
    }

return(matrix);
}


/* ------- allocate an array/list and copy matrix into it ---- */

CELL * matrix2data(double ** matrix, int type, int n, int m)
{
CELL * list;
CELL * row;
CELL * cell;
int i, j;
double fnum;
CELL * * addr;
CELL * * rowAddr;

list = getCell(type);

if(type == CELL_EXPRESSION)
    {
    row = getCell(CELL_EXPRESSION);
    list->contents = (UINT)row;
    for(i = 1; i <= n; i++)
        {
        cell = getCell(CELL_FLOAT);
        row->contents = (UINT)cell;
        for(j = 1; j <= m; j++)
            {
            fnum = matrix[i][j];
#ifndef NEWLISP64
            *(double *)&cell->aux = fnum;
#else
            *(double *)&cell->contents = fnum;
#endif
            if(j == m) break;
            cell->next = getCell(CELL_FLOAT);
            cell = cell->next;
            }
        if(i == n) break;
        row->next = getCell(CELL_EXPRESSION);
        row = row->next;
        }
    }

else /* type is CELL_ARRAY */
    {
    list->aux = n * sizeof(CELL *) + 1;
    addr = (CELL * *)allocMemory(list->aux);
    list->contents = (UINT)addr;

    for(i = 1; i <= n; i++)
        {
        cell = getCell(CELL_ARRAY);
        cell->aux = m * sizeof(CELL *) + 1;
        rowAddr = (CELL * *)allocMemory(cell->aux);
        cell->contents = (UINT)rowAddr;
        *(addr + i -1) = cell;
        for(j = 1; j <= m; j++)
            {
            fnum = matrix[i][j];
            *(rowAddr + j - 1) = stuffFloat(fnum);
            }
        }
    }

return(list);
}


/* ------------------------ allocate/free a matrix ----------------- */

double * * allocateMatrix(int rows, int cols)
{
int i;
double * * m;

m = (double **) calloc(rows + 1, sizeof(double *));
if (!m)
    return(NULL);

for(i = 1; i <= rows; i++)
    {
    m[i] = (double *)calloc(cols + 1, sizeof(double));
    if (!m[i])
        return(NULL);
    }

return(m);
}


void freeMatrix(double * * m, int rows)
{
int i;

if(m == NULL) return;

for(i = 1; i <= rows; i++)
    free(m[i]);

free(m);
}

/* end of file */
