#include "dbg.h"
#include <stdint.h>
#include "dlx.h"

/*
 * Creates empty DLX matrix.
 */
struct cell *create_empty_matrix() {
  struct cell *h = malloc(sizeof(struct cell));
  if (h != NULL) {
    h->L = h;
    h->R = h;
    h->U = NULL;
    h->D = NULL;
    h->C = NULL;
  }
  return h;
}

/*
 * Frees all memory associated with given matrix.
 */
void free_matrix(struct cell *h) {
  if (h == NULL) return;

  if (h->C != NULL) {  /* Not a header cell, so make h point to the header
                        * cell. */
    h = h->C;
    while (h->C != NULL) h = h->R;
  }

  /* Cells are freed from left-most column to right-most column, and within each
   * column, top to bottom. */
  struct cell *c = h->R;
  while (c != h) {
    struct cell *r = c->D;
    while (r != c) {
      r = r->D;
      free(r->U);
    }
    c = c->R;
    free(c->L);
  }
  free(h);
}

/*
 * Inserts column. No-op if column exists. Search always move rightwards from
 * starting cell (this is intentional).
 *
 * Input: xp: a pointer to a pointer to any valid cell.
 *
 * Return code: 1 if column already exists, 0 if insertion is successful, -1 for
 * any other error. Input pointer is modified to point to inserted cell.
 *
 * Warning: matrix is in an inconsistent state if function fails.
 */
int add_column(struct cell **xp, column_name new_N) {
  struct cell *c = *xp;
  if (c->C != NULL) c = c->C;  /* c to point to column header if it doesn't
                                * already do so */

  /* Step 1: check that c is to the left of the target location */
  column_name current_N = (c->C == NULL) ? COLUMN_NAME_MIN : c->N;
  if (current_N > new_N) {  /* too far to the right */
    while (c->C != NULL) c = c->R;  /* Move back to the header */
    current_N = COLUMN_NAME_MIN;
  }
  check_debug(current_N <= new_N, "assertion failed: current_N <= new_N");

  /* Step 2: move c such that it just crosses or touches the target location */
  while (current_N < new_N) {
    c = c->R;
    current_N = (c->C == NULL) ? COLUMN_NAME_MAX : c->N;
  }

  /* Step 3: check case where column already exists */
  if (current_N == new_N) {
    *xp = c;
    return 1;
  }
  check_debug(((c->L->C == NULL) ? COLUMN_NAME_MIN : c->L->N) < new_N && new_N < current_N,
              "assertion failed: previous_N < new_N && new_N < current_N");

  /* Step 4: Insert col to the left of c */
  struct cell *new_c = malloc(sizeof(struct cell));
  check_mem(new_c);
  new_c->L = c->L;
  new_c->R = c;
  new_c->U = new_c;
  new_c->D = new_c;
  new_c->C = new_c;
  new_c->S = 0;
  new_c->N = new_N;
  c->L->R = new_c;
  c->L = new_c;
  *xp = new_c;
  return 0;

error:
  return -1;
}

/*
 * Inserts a set.
 *
 * Input: xp: a pointer to a pointer to any valid cell. set: array should (but
 * not necessarily) be in circularly increasing order for efficiency. e.g. "1 2
 * 3" or "3 1 2" is okay, but "3 2 1" or "1 3 2" is not.
 *
 * Output: 0 for success, -1 for error. Input pointer to the cell pointer is
 * modified to point to the last added cell.
 *
 * Warning: matrix is in an inconsistent state if function fails.
 */
int add_set(struct cell **xp, column_name *set, size_t n_set_elem) {
  struct cell *c = *xp;

  struct cell *y_prev, *y;
  for (size_t i = 0; i < n_set_elem; ++i) {
    check(add_column(&c, set[i]) >= 0, "Failed to add column header.");
    /* note: c points to the column header after add_column */

    y = malloc(sizeof(struct cell));
    check_mem(y);
    if (i == 0) {
      y->L = y;
      y->R = y;
    } else {
      y->L = y_prev;
      y->R = y_prev->R;
      y_prev->R->L = y;
      y_prev->R = y;
    }
    y->U = c->U;
    y->D = c;
    c->U->D = y;
    c->U = y;
    y->C = c;

    ++(c->S);
    y_prev = y;
  }

  *xp = y;
  return 0;

error:
  return -1;
}

/*
 * Find column with the smallest number of elements.
 *
 * Input: header cell.
 */
struct cell *find_smallest_col(const struct cell *h) {
  check(h->C == NULL, "Not header cell.");
  struct cell *c = h->R;
  struct cell *min_c;
  size_t min_S = SIZE_MAX;
  while (c != h) {
    if (c->S < min_S) {
      min_S = c->S;
      min_c = c;
    }
    c = c->R;
  }
  return min_c;

error:
  return NULL;
}

/*
 * Covers the column that a given cell is in. Does not free memory.
 */
void cover_col(struct cell *x) {
  struct cell *c = x->C;
  c->R->L = c->L;
  c->L->R = c->R;

  struct cell *i = c->D;
  while (i != c) {
    /* For each cell i in the column, cover the row. */
    struct cell *j = i->R;
    while (j != i) {
      j->D->U = j->U;
      j->U->D = j->D;
      --j->C->S;
      j = j->R;
    }

    i = i->D;
  }
}

/*
 * Uncovers the column that a given cell is in.
 */
void uncover_col(struct cell *x) {
  struct cell *c = x->C;

  /* Programming note: the uncovering is done in exactly the reverse order by
   * which the column was previously covered! */
  struct cell *i = c->U;
  while (i != c) {
    /* For each cell i in the column, uncover the row. */
    struct cell *j = i->L;
    while (j != i) {
      ++j->C->S;
      j->D->U = j;
      j->U->D = j;
      j = j->L;
    }

    i = i->U;
  }

  c->R->L = c;
  c->L->R = c;
}

/*
 * The Dancing Links X algorithm. Note: allocates memory for output array.
 *
 * Input: h: header cell. k: always start at 0.
 *
 * Output: pointers to cells whose rows are part of the solution.
 *
 * Return code: 0 = good. 1 = no solution. -1 = error.
 */
int dlx_solve(const struct cell *h, depth_t k, struct cell ***output_arr, size_t *output_arr_size) {
  check(h->C == NULL, "Not header cell.");

  if (h->R == h) {
    *output_arr = malloc(sizeof(struct cell *) * k);
    check_mem(*output_arr);
    *output_arr_size = k;
    return 0;
  }

  int ret = 1;
  struct cell *c = find_smallest_col(h);
  cover_col(c);

  struct cell *candidate_row = c->D;
  struct cell *j;  /* variable to traverse candidate_row */
  while (candidate_row != c) {
    j = candidate_row->R;
    while (j != candidate_row) {
      cover_col(j);
      j = j->R;
    }

    ret = dlx_solve(h, k+1, output_arr, output_arr_size);

    j = candidate_row->L;
    while (j != candidate_row) {
      uncover_col(j);
      j = j->L;
    }

    if (ret == 0) {
      (*output_arr)[k] = candidate_row;
      break;
    }

    candidate_row = candidate_row->D;
  }

  uncover_col(c);
  return ret;

error:
  return -1;
}

/*
 * Helper function that wraps dlx_solve(). Note: allocates memory for output
 * array.
 *
 * Input: header cell and the most probable number of elements each solution set
 * will have. The latter will be used to allocate memory (but will be expanded
 * if there are more elements).
 */
int solve(const struct cell *h, size_t suggested_set_size,
          int ***set_covers, size_t **set_cover_sizes, size_t *n_sets) {
  struct cell **ans = NULL;
  size_t ans_len = 0;
  int ret_code = dlx_solve(h, 0, &ans, &ans_len);

  if (ret_code == 0) {
    *n_sets = ans_len;
    *set_covers = malloc(sizeof(int *) * ans_len);
    check_mem(*set_covers);
    *set_cover_sizes = malloc(sizeof(size_t) * ans_len);
    check_mem(*set_cover_sizes);

    struct cell *x;
    size_t j, set_max_size;
    for (size_t i = 0; i < ans_len; ++i) {
      x = ans[i];
      set_max_size = suggested_set_size;
      (*set_covers)[i] = malloc(sizeof(int) * set_max_size);

      j = 0;
      do {
        if (j >= set_max_size) {
          set_max_size *= 2;
          int *tmp = realloc((*set_covers)[i], sizeof(int) * set_max_size);
          check_mem(tmp);
          (*set_covers)[i] = tmp;
        }

        (*set_covers)[i][j] = x->C->N;
        x = x->R;
        ++j;
      } while (x != ans[i]);

      (*set_cover_sizes)[i] = j;
    }
  }

  return ret_code;

error:
  for (size_t i = 0; i < ans_len; ++i) {
    free((*set_covers)[i]);
  }
  free(*set_cover_sizes);
  free(*set_covers);
  return -1;
}
