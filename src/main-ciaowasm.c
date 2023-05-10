/*
 *  main-ciaowasm.c
 *
 *  Main file for ciaoengwasm.pl (called from ciao-prolog.js)
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

/* Check if current query is suspended */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_suspended(void) {
  return ciao_query_suspended(query);
}

/* Resume a suspended query */
int EMSCRIPTEN_KEEPALIVE ciaowasm_query_resume(void) {
  ciao_query_resume(query);
  return TRUE;
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
