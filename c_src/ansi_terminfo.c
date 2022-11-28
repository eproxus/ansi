/*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020 VMware, Inc. or its affiliates.  All rights reserved.
 */

#include <string.h>
#include <unistd.h>

#include <curses.h>
#include <term.h>
#include <tic.h>

#include "erl_nif.h"

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_invalid_capability;
static ERL_NIF_TERM am_cancelled;
static ERL_NIF_TERM am_absent;

#define RAISE_TERM(type, term)                                                 \
  enif_raise_exception(env, enif_make_tuple3(env, am_error, type, term))
#define RAISE_STRERROR(msg)                                                    \
  RAISE_TERM(msg, enif_make_string(env, strerror(errno), ERL_NIF_LATIN1))

static int terminfo_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  am_error = enif_make_atom(env, "error");
  am_invalid_capability = enif_make_atom(env, "invalid_capability");
  am_cancelled = enif_make_atom(env, "cancelled");
  am_absent = enif_make_atom(env, "absent");

  return 0;
}

static int fdname_to_fd(ErlNifEnv* env, const ERL_NIF_TERM arg);

static ERL_NIF_TERM
isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret, fd;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    /* argv[1] is the name of the file descriptor: stdin, stdout or
     * stderr. */
    ret = fdname_to_fd(env, argv[0]);
    if (ret == -1) {
        return enif_make_badarg(env);
    }
    fd = ret;

    ret = isatty(fd);
    return enif_make_atom(env, ret ? "true" : "false");
}

static ERL_NIF_TERM
setupterm_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret, fd, use_TERM_envvar, errret;
    char term_name[32];

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    /* argv[0] is the terminal name as a string. The string is empty is
     * setupterm(3) should use $TERM. */
    if (!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    ret = enif_get_string(env, argv[0],
        term_name, (unsigned int)sizeof(term_name),
        ERL_NIF_LATIN1);
    if (ret <= 0) {
        return enif_make_badarg(env);
    }
    use_TERM_envvar = ret == 1;

    /* argv[1] is the name of the file descriptor: stdout or stderr. */
    ret = fdname_to_fd(env, argv[1]);
    if (ret == -1 || ret == STDIN_FILENO) {
        return enif_make_badarg(env);
    }
    fd = ret;

    // ret = setupterm(use_TERM_envvar ? NULL : term_name, fd, &errret);
    ret = setupterm(use_TERM_envvar ? NULL : term_name, fd, NULL);
    if (ret == ERR) {
        return enif_make_badarg(env);
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
tigetnum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret, cap_value;
    char cap_name[32];

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    /* argv[0] is the capability name as a string. */
    if (!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    ret = enif_get_string(env, argv[0],
        cap_name, (unsigned int)sizeof(cap_name),
        ERL_NIF_LATIN1);
    if (ret <= 0) {
        return enif_make_badarg(env);
    }

    cap_value = tigetnum(cap_name);

    return enif_make_int(env, cap_value);
}

static ERL_NIF_TERM
tigetflag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret, cap_value;
    char cap_name[32];

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    /* argv[0] is the capability name as a string. */
    if (!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    ret = enif_get_string(env, argv[0],
        cap_name, (unsigned int)sizeof(cap_name),
        ERL_NIF_LATIN1);
    if (ret <= 0) {
        return enif_make_badarg(env);
    }

    cap_value = tigetflag(cap_name);

    return enif_make_int(env, cap_value);
}

static ERL_NIF_TERM
tigetstr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;
    ErlNifBinary name;
    char *value, *bin;
    ERL_NIF_TERM result;

    if (!enif_inspect_binary(env, argv[0], &name) || name.size <= 0) {
        return RAISE_TERM(am_invalid_capability, argv[0]);
    }

    value = tigetstr((char *)name.data);

    if (value == CANCELLED_STRING) {
        return RAISE_TERM(am_cancelled, argv[0]);
    } else if (value == NULL) {
        return RAISE_TERM(am_absent, argv[0]);
    } else {
        // printf("%s\n", strlen(value));
        bin = enif_make_new_binary(env, strlen(value), &result);
        strcpy(value, bin);
        return result;
    }
}

static ERL_NIF_TERM
putp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;
    ErlNifBinary capname;
    char *cap_value;

    if (!enif_inspect_binary(env, argv[0], &capname) || capname.size <= 0) {
        return RAISE_TERM(am_invalid_capability, argv[0]);
    }

    cap_value = tigetstr((char *)capname.data);

    if (cap_value == CANCELLED_STRING) {
        return RAISE_TERM(am_cancelled, argv[0]);
    } else if (cap_value == NULL) {
        return RAISE_TERM(am_absent, argv[0]);
    }

    putp(cap_value);
    return enif_make_atom(env, "ok");
}

static int
fdname_to_fd(ErlNifEnv* env, const ERL_NIF_TERM arg)
{
    int ret;
    char atom_arg[10];

    if (!enif_is_atom(env, arg)) {
        return -1;
    }

    ret = enif_get_atom(env, arg,
        atom_arg, (unsigned int)sizeof(atom_arg),
        ERL_NIF_LATIN1);
    if (ret == 0) {
        return -1;
    }

    if (strcmp(atom_arg, "stdin") == 0) {
        return STDIN_FILENO;
    } else if (strcmp(atom_arg, "stdout") == 0) {
        return STDOUT_FILENO;
    } else if (strcmp(atom_arg, "stderr") == 0) {
        return STDERR_FILENO;
    }

    return -1;
}

static ErlNifFunc nif_funcs[] = {
    {"isatty", 1, isatty_nif},
    {"setupterm", 2, setupterm_nif},
    {"tigetnum", 1, tigetnum_nif},
    {"tigetflag", 1, tigetflag_nif},
    {"tigetstr", 1, tigetstr_nif},
    {"putp", 1, putp_nif},
};

ERL_NIF_INIT(ansi_terminfo, nif_funcs, &terminfo_load, NULL, NULL, NULL);

