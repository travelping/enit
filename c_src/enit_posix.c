#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>

#include <erl_nif.h>
#include <erl_driver.h>

static inline ERL_NIF_TERM
error2tuple(ErlNifEnv *env, int error)
{
	return enif_make_tuple(env, 2,
			       enif_make_atom(env, "error"),
			       enif_make_atom(env, erl_errno_id(error)));
}

static inline void
free_argv(char **argv)
{
	for (char **p = argv; *p; p++)
		enif_free(*p);
	enif_free(argv);
}

static ERL_NIF_TERM
exec_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
	int _errno;

	unsigned int len;
	ERL_NIF_TERM head, tail;

	char **exec_argv;
	int exec_argc = 0;

	assert(argc == 1);

	if (!enif_get_list_length(env, argv[0], &len) || len < 1)
		return enif_make_badarg(env);

	exec_argv = enif_alloc(sizeof(char *) * (len + 1));
	if (exec_argv == NULL)
		return error2tuple(env, errno);
	memset(exec_argv, 0, sizeof(char *) * (len + 1));

	for (const ERL_NIF_TERM *cur = argv;
	     enif_get_list_cell(env, *cur, &head, &tail);
	     cur = &tail) {
		if (!enif_get_list_length(env, head, &len) || len < 1) {
			free_argv(exec_argv);
			return enif_make_badarg(env);
		}

		exec_argv[exec_argc] = enif_alloc(len + 1);
		if (exec_argv[exec_argc] == NULL) {
			_errno = errno;
			free_argv(exec_argv);
			return error2tuple(env, _errno);
		}

		if (enif_get_string(env, head, exec_argv[exec_argc], len + 1,
				    ERL_NIF_LATIN1) < 1) {
			free_argv(exec_argv);
			return enif_make_badarg(env);
		}

		exec_argc++;
	}
	exec_argv[exec_argc] = NULL;

	execv(exec_argv[0], exec_argv);

	_errno = errno;
	free_argv(exec_argv);

	return error2tuple(env, _errno);
}

static ERL_NIF_TERM
setuid_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
	char name_buf[sysconf(_SC_LOGIN_NAME_MAX) + 1]; /* FIXME: error handling */
	char pwd_buf[sysconf(_SC_GETPW_R_SIZE_MAX)];

	struct passwd passwd, *pwd_p;

	assert(argc == 1);

	if (enif_get_string(env, argv[0], name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getpwnam_r(name_buf, &passwd, pwd_buf, sizeof(pwd_buf), &pwd_p);
	if (pwd_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

	if (setuid(pwd_p->pw_uid))
		return error2tuple(env, errno);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
setgid_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
	char name_buf[sysconf(_SC_LOGIN_NAME_MAX) + 1]; /* FIXME: error handling */
	char gr_buf[sysconf(_SC_GETGR_R_SIZE_MAX)];

	struct group group, *gr_p;

	assert(argc == 1);

	if (enif_get_string(env, argv[0], name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getgrnam_r(name_buf, &group, gr_buf, sizeof(gr_buf), &gr_p);
	if (gr_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

	if (setgid(gr_p->gr_gid))
		return error2tuple(env, errno);

	return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"exec", 1, exec_nif},
    {"setuid", 1, setuid_nif},
    {"setgid", 1, setgid_nif}
};
ERL_NIF_INIT(enit_vm, nif_funcs, NULL, NULL, NULL, NULL)
