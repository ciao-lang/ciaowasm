/*
 *  main-ciaowasm.c
 *
 *  Main file for ciaoengwasm.pl (called from ciao-eng.js)
 *
 *  Copyright (C) 2016-2022 Jose F. Morales
 */

#include <emscripten.h>
#include <ciao_prolog.h>

ciao_ctx ctx;
ciao_query *query; // query global variable

int EMSCRIPTEN_KEEPALIVE ciaowasm_init(char *bootfile) {
  const char *boot_path = NULL;

  /* Set engine options and initialize */
  ciao_opts(NULL, 0, NULL, 0, NULL, &boot_path);
  ciao_init(NULL);

  ctx = ciao_ctx_new();

  ciao_load_qfile_s(ctx, bootfile);

  return 0; // OK
}

int EMSCRIPTEN_KEEPALIVE ciaowasm_boot(void) {
  ciao_frame_begin_s(ctx); // TODO: it should not be needed
  ciao_boot(ctx); //firstgoal(ctx, GET_ATOM("internals:boot"));
  ciao_frame_end_s(ctx);
  return 0;
}

/* Run a 0-ary predicate */
int EMSCRIPTEN_KEEPALIVE ciaowasm_run(char *atom) {
  /* wam->next_insn set to boot code in local_init_each_time */
  /*w->choice->heap_top = w->heap_top;*/     /* Isn't this unnecessary? */
  /*  Fills in worker_entry */
  ciao_bool r;
  ciao_frame_begin_s(ctx);
  r = ciao_commit_call_term_s(ctx, ciao_structure_s(ctx, atom, 0));
  ciao_frame_end_s(ctx);
  return r;
}

/* Begin a new query */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_begin(char *atom) {
  ciao_frame_begin_s(ctx);
  query = ciao_query_begin_s(ctx, atom, 0); // a 0-ary predicate
  return TRUE;
}

/* Check if current query is ok */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_ok(void) {
  return ciao_query_ok(query);
}

/* Ask for the next solution */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_next(void) {
  return ciao_query_next(query);
}

/* End current query */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_end(void) {
  ciao_query_end(query);
  ciao_frame_end_s(ctx);
  return TRUE;
}
