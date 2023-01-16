#define _POSIX_C_SOURCE 200112L

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>

#include <wlr/util/log.h>

/* REFACTOR WITH realloc INSTEAD OF malloc+free LATER */

struct id_node {
  int id;
  struct id_node* next;
};

/* we will have a vector holding the allocated ids, a list that holds
   the holes in the allocated vector, and a max used id.  */

#define VEC_MIN_LENGTH 16

/* unused slots are NULL */
void** alloc_vec = NULL;
struct id_node* free_list = NULL;
int max_id = -1;
int vec_len = 0;
int vec_size = VEC_MIN_LENGTH; 

bool free_list_empty(void) {
  return free_list == NULL;
}

int free_list_pop(void) {
  struct id_node* hold = free_list;
  free_list  = hold->next;
  return hold->id;
}

int free_list_push(int id) {
  struct id_node* new = malloc(sizeof(struct id_node));
  if (!new) {
    wlr_log(WLR_ERROR, "Could not allocate an id struct");
    return -1;
  }
  new->id = id;
  new->next = free_list;
  free_list = new;
  return 0;
}

void free_list_prune (int max) {
  /* remove all items greater than max */
  struct id_node* tmp = free_list;
  struct id_node* hold;
  if (!free_list_empty()) {
    if (free_list->id > max) {
      free_list = free_list->next;
      tmp = free_list;
    }
    if (tmp) {
      while (tmp->next) {
	if (tmp->next->id > max) {
	  hold = tmp->next->next;
	  free(tmp->next);
	  tmp->next = hold;
	} else {
	  tmp = tmp->next;
	}
      }
    }
  }
}

int vec_push(void* data) {
  int ret = -1;
  if (free_list_empty()) {
    if (max_id == vec_size - 1) {
      /* no holes and full, resize */
      void** hold = calloc(sizeof(void*), vec_size * 2);
      if (!hold) {
	wlr_log(WLR_ERROR, "Could not allocate id vector");
	return -1;
      }
      memcpy(hold, alloc_vec, sizeof(void*) * vec_size);
      free(alloc_vec);
      alloc_vec = hold;
      alloc_vec[++max_id] = data;
    } else {
      /* push at the end */
      alloc_vec[++max_id] = data;      
    }
    ret = max_id;
  } else {
    /* fill hole */
    ret = free_list_pop();
    alloc_vec[ret] = data;
  }
  vec_len++;
  return ret;
}

int vec_remove(int id) {
  if (id == max_id) {
    alloc_vec[max_id--] = NULL;
  } else {
    if (free_list_push(id)) {
      return -1;
    }
    alloc_vec[id] = NULL;
  }
  if ((--vec_len < vec_size / 4) && vec_size > VEC_MIN_LENGTH) {
    /* shrink vec */
    void** hold = calloc(sizeof(void*), vec_size / 2);
    if (!hold) {
      wlr_log(WLR_ERROR, "Could not allocate id vector");
      vec_len++; 		/* restore state */
      return -1;
    }
    int in_idx = 0;
    int out_idx = 0;
    void* tmp;
    while (in_idx < vec_size && out_idx < vec_size / 2) {
      /* copy the non-NULL ones */
      tmp = alloc_vec[in_idx++];
      if (tmp) {
	hold[out_idx++] = tmp;
      }
    }
    free(alloc_vec);
    alloc_vec = hold;
    vec_size = vec_size / 2;
    free_list_prune(vec_size - 1);
  }
  return 0;
}

void* vec_get(int id) {
  if (id < 0 || id >= vec_size) {
    wlr_log(WLR_ERROR, "ID out of vector bounds");
    return NULL;
  }
  return alloc_vec[id];
}

int id_init(void) {
  vec_size = VEC_MIN_LENGTH;
  vec_len = 0;
  max_id = -1;
  alloc_vec = calloc(vec_size, sizeof(void*));
  if (!alloc_vec) {
    wlr_log(WLR_ERROR, "Could not allocate id vec");
    return -1;
  }
  for (int i = 0; i < vec_size; i++) {
    alloc_vec[i] = NULL;
  }
  free_list = NULL;
  return 0;
}

int id_allocate (void* data) {
  /* get a new id that is not in use and associate it with the given
     id. Values less than zero indicate and error */
  return vec_push(data);
}

int id_free (int id) {
  /* release an id. Cleanup of the data must be done manually. returns
     < 0 on error */
  return vec_remove(id);
}

void* id_get (int id) {
  return vec_get(id);
}
