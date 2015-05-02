#include <unistd.h>

#include "erl_nif.h"

static ERL_NIF_TERM exec_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned path_length, args_count, arg_length;
  ERL_NIF_TERM head, tail;
  int i = 0;

  enif_get_list_length(env, argv[0], &path_length);
  enif_get_list_length(env, argv[1], &args_count);

  char* exec_argv[args_count + 2];
  char path[path_length + 1];

  if (!enif_get_string(env, argv[0], path, path_length + 1, ERL_NIF_LATIN1) || !enif_is_list(env, argv[1])) {
    return enif_make_badarg(env);
  }

  tail = argv[1];
  while(enif_get_list_cell(env, tail, &head, &tail) != 0) {
    enif_get_list_length(env, head, &arg_length);

    char* arg = (char*) malloc(sizeof(char) * (arg_length + 1));

    if (!enif_get_string(env, head, arg, arg_length + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
    }

    exec_argv[i + 1] = arg;

    i++;
  }

  exec_argv[0] = path;
  exec_argv[args_count + 1] = NULL;

  execv(path, exec_argv);

  return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
  {"exec", 2, exec_nif}
};

ERL_NIF_INIT(quissh_nif, nif_funcs, NULL, NULL, NULL, NULL);
