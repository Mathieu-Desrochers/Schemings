(declare (unit hungarian))

(foreign-declare "

#include <hungarian.h>

// allocates a hungarian_problem_t
hungarian_problem_t* malloc_hungarian_problem_t()
{
  hungarian_problem_t* hungarian_problem = malloc(sizeof(hungarian_problem_t));
  return hungarian_problem;
}

// frees a hungarian_problem_t
void free_hungarian_problem_t(hungarian_problem_t* hungarian_problem)
{
  free(hungarian_problem);
}

// allocates a hungarian cost matrix
int** malloc_hungarian_cost_matrix(int rows, int columns)
{
  int** hungarian_cost_matrix = calloc(rows, sizeof(int*));

  int i;

  for (i = 0; i < rows; i++)
  {
    hungarian_cost_matrix[i] = calloc(columns, sizeof(int));
  }

  return hungarian_cost_matrix;
}

// sets a hungarian cost matrix value
void hungarian_cost_matrix_set(int** hungarian_cost_matrix, int row, int column, int value)
{
  (hungarian_cost_matrix[row])[column] = value;
}

// frees a hungarian cost matrix
void free_hungarian_cost_matrix(int** hungarian_cost_matrix, int rows, int columns)
{
  int i;

  for (i = 0; i < rows; i++)
  {
    free(hungarian_cost_matrix[i]);
  }

  free(hungarian_cost_matrix);
}

// gets the column assigned to a row
int hungarian_get_row_assignment(hungarian_problem_t* hungarian_problem, int row, int columns)
{
  int i;

  for (i = 0; i < columns; i++)
  {
    if ((hungarian_problem->assignment[row])[i] == 1)
    {
      break;
    }
  }

  return i;
}

")

;; hungarian-problem-t pointers definitions
(define-foreign-type hungarian-problem-t "hungarian_problem_t")
(define-foreign-type hungarian-problem-t* (c-pointer hungarian-problem-t))

;; hungarian-problem-t pointers memory management
(define malloc-hungarian-problem-t (foreign-lambda hungarian-problem-t* "malloc_hungarian_problem_t"))
(define free-hungarian-problem-t (foreign-lambda void "free_hungarian_problem_t" hungarian-problem-t*))

;; hungarian cost matrix memory management
(define malloc-hungarian-cost-matrix
  (foreign-lambda (c-pointer (c-pointer int))" malloc_hungarian_cost_matrix" int int))
(define hungarian-cost-matrix-set!
  (foreign-lambda void "hungarian_cost_matrix_set" (c-pointer (c-pointer int)) int int int))
(define free-hungarian-cost-matrix
  (foreign-lambda void "free_hungarian_cost_matrix" (c-pointer (c-pointer int)) int int))

;; initializes a hungarian problem
(define hungarian-init
  (foreign-lambda int "hungarian_init" hungarian-problem-t* (c-pointer (c-pointer int)) int int int))

;; solves a hungarian problem
(define hungarian-solve (foreign-lambda void "hungarian_solve" hungarian-problem-t*))

;; gets the column assigned to a row
(define hungarian-get-row-assignment
  (foreign-lambda int "hungarian_get_row_assignment" hungarian-problem-t* int int))

;; frees a hungarian problem
(define hungarian-free (foreign-lambda void "hungarian_free" hungarian-problem-t*))
