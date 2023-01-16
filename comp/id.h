/* id management */

int id_init(void);

int id_allocate(void* data);

int id_free(int id);

void* id_get(int id);
