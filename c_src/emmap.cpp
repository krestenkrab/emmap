

#include "erl_nif_compat.h"
#include <sys/mman.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

static ErlNifResourceType* MMAP_RESOURCE;

typedef struct
{
  int direct;
  int prot;
  bool closed;
  ErlNifRWLock* rwlock;
  void* mem;
  size_t len;
} mhandle;

static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);

static int emmap_unmap(mhandle *handle, bool from_dtor)
{
  if (handle->mem != 0) {
    if (from_dtor || !handle->direct) {
      int result = munmap(handle->mem, handle->len);
      handle->mem = 0;
      return result;
    } else {
      handle->closed = true;
    }
  }
  return 0;
}

void emmap_dtor(ErlNifEnv* env, void* arg)
{
  mhandle* handle = (mhandle*)arg;
  emmap_unmap(handle, true);

  // only the destructor destroys the rwlock
  enif_rwlock_destroy(handle->rwlock);
}

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_DIRECT;
static ERL_NIF_TERM ATOM_READ;
static ERL_NIF_TERM ATOM_WRITE;
static ERL_NIF_TERM ATOM_NONE;
static ERL_NIF_TERM ATOM_PRIVATE;
static ERL_NIF_TERM ATOM_SHARED;
static ERL_NIF_TERM ATOM_ANON;
static ERL_NIF_TERM ATOM_FILE;
static ERL_NIF_TERM ATOM_FIXED;
static ERL_NIF_TERM ATOM_NOCACHE;

static ERL_NIF_TERM emmap_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

extern "C" {

    static ErlNifFunc nif_funcs[] =
    {
        {"open",              4, emmap_open},
        {"close",             1, emmap_close},
        {"pread",             3, emmap_pread},
        {"pwrite",            3, emmap_pwrite},
    };

    ERL_NIF_INIT(emmap, nif_funcs, &on_load, NULL, NULL, NULL);
};

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    MMAP_RESOURCE = enif_open_resource_type_compat(env, "mmap_resource", &emmap_dtor, flags, 0);

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_ERROR = enif_make_atom(env, "error");

    ATOM_DIRECT = enif_make_atom(env, "direct");
    ATOM_READ = enif_make_atom(env, "read");
    ATOM_WRITE = enif_make_atom(env, "write");
    ATOM_NONE = enif_make_atom(env, "none");
    ATOM_PRIVATE = enif_make_atom(env, "private");
    ATOM_SHARED = enif_make_atom(env, "shared");
    ATOM_ANON = enif_make_atom(env, "anon");
    ATOM_FILE = enif_make_atom(env, "file");
    ATOM_FIXED = enif_make_atom(env, "fixed");
    ATOM_NOCACHE = enif_make_atom(env, "nocache");

    return 0;
}

static ERL_NIF_TERM describe_error(ErlNifEnv* env, int err) {
  switch (err) {
  case EAGAIN:
    return enif_make_atom(env, "eagain");
  case EINVAL:
    return enif_make_atom(env, "einval");
  case ENOSPC:
    return enif_make_atom(env, "enospc");
  case ENOENT:
    return enif_make_atom(env, "enoent");
  case ENOMEM:
    return enif_make_atom(env, "enomem");
  case EACCES:
    return enif_make_atom(env, "eacces");
  case EBADF:
    return enif_make_atom(env, "ebadf");
  case ENODEV:
    return enif_make_atom(env, "enodev");
  case ENXIO:
    return enif_make_atom(env, "enxio");
  case EOVERFLOW:
    return enif_make_atom(env, "eoverflow");
  }
  return enif_make_tuple2(env,
                          enif_make_atom(env, "errno"),
                          enif_make_int(env, err));
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, int err) {
  return enif_make_tuple2(env, ATOM_ERROR, describe_error(env, err));
}


int decode_flags(ErlNifEnv* env, ERL_NIF_TERM list, int *prot, int *flags, bool *direct)
{
  bool d = false;
  int f = MAP_FILE;
  int p = 0;
  ERL_NIF_TERM head;
  while (enif_get_list_cell(env, list, &head, &list)) {

    if (enif_is_identical(head, ATOM_READ)) {
      p |= PROT_READ;
    } else if (enif_is_identical(head, ATOM_DIRECT)) {
      d = true;
    } else if (enif_is_identical(head, ATOM_WRITE)) {
      p |= PROT_WRITE;
//    } else if (enif_is_identical(head, ATOM_NONE)) {
//    p |= PROT_NONE;

    } else if (enif_is_identical(head, ATOM_PRIVATE)) {
      f |= MAP_PRIVATE;
    } else if (enif_is_identical(head, ATOM_SHARED)) {
      f |= MAP_SHARED;
//  } else if (enif_is_identical(head, ATOM_ANON)) {
//    f |= MAP_ANON;
//  } else if (enif_is_identical(head, ATOM_FILE)) {
//    f |= MAP_FILE;
//  } else if (enif_is_identical(head, ATOM_FIXED)) {
//    f |= MAP_FIXED;
    } else if (enif_is_identical(head, ATOM_NOCACHE)) {
      f |= MAP_NOCACHE;

    } else {
      return 0;
    }
  }

  // direct cannot be write
  if (d & ((p & PROT_WRITE) != 0))
    return 0;

  // default to private
  if ((f & (MAP_SHARED|MAP_PRIVATE)) == 0)
    f |= MAP_PRIVATE;

  // default to read-only
  if ((p & (PROT_READ|PROT_WRITE)) == 0)
    p |= PROT_READ;

  *flags = f;
  *prot = p;
  *direct = d;

  return 1;
}

static ERL_NIF_TERM emmap_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int flags;
  int prot;
  bool direct;
  unsigned long int len;
  unsigned long int offset;
  char buf[1024];

#ifndef NDEBUG
  if ( sizeof(long int) != sizeof(size_t) ) {
    abort();
  }
#endif

  if (argc == 4
      && enif_get_string(env, argv[0], buf, 1024, ERL_NIF_LATIN1)
      && enif_get_ulong(env, argv[1], &offset)
      && enif_get_ulong(env, argv[2], &len)
      && decode_flags(env, argv[3], &prot, &flags, &direct)) {

    int mode = (((prot & PROT_WRITE)==PROT_WRITE) ? O_RDWR : O_RDONLY);

    int fd = open(buf, mode);
    if (fd < 0) {
      return make_error_tuple(env, errno);
    }

    void * res = mmap(0, (size_t) len, prot, flags, fd, (size_t) offset);
    if (res == MAP_FAILED) {
      return make_error_tuple(env, errno);
    }

    close(fd);

    mhandle* handle = (mhandle*)enif_alloc_resource_compat(env, MMAP_RESOURCE,
                                                           sizeof(mhandle));

    handle->rwlock = enif_rwlock_create((char*)"mmap");
    handle->prot = prot;
    handle->mem = res;
    handle->len = len;
    handle->closed = false;
    handle->direct = direct;

    ERL_NIF_TERM resource = enif_make_resource(env, handle);
    enif_release_resource_compat(env, handle);

    return enif_make_tuple2(env,
                            enif_make_atom(env, "ok"),
                            resource);

  } else {
      return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM emmap_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle *handle;
  if (argc==1 && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle))
    {
      int res;
      enif_rwlock_rwlock(handle->rwlock);
      res = emmap_unmap(handle, false);
      enif_rwlock_rwunlock(handle->rwlock);

      if (res == 0) {
        return ATOM_OK;
      }

      return make_error_tuple(env, errno);
    }
  else
    {
      return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM emmap_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long pos, bytes;
  mhandle *handle;
  if (argc==3
      && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_ulong(env, argv[1], &pos)
      && enif_get_ulong(env, argv[2], &bytes)
      && pos >= 0
      && bytes >= 0
      && (pos + bytes) <= handle->len
      )
    {
      ErlNifBinary bin;

      if ((handle->prot & PROT_READ) == 0) {
        return make_error_tuple(env, EACCES);
      }

      // if this mmap is direct, use a resource binary
      if (handle->direct) {

        ERL_NIF_TERM res = enif_make_resource_binary
          (env, handle, (void*) (((char*)handle->mem) + pos), bytes);

        return enif_make_tuple2(env, ATOM_OK, res);

      } else {

        // When it is non-direct, we have to allocate the binary
        if (!enif_alloc_binary((size_t) bytes, &bin)) {
          return make_error_tuple(env, ENOMEM);
        }

        enif_rwlock_rlock(handle->rwlock);
        if (handle->closed) {
          enif_rwlock_runlock(handle->rwlock);
          return enif_make_badarg(env);
        }
        memcpy(bin.data, (void*) (((char*)handle->mem) + pos), bytes);
        enif_rwlock_runlock(handle->rwlock);

        ERL_NIF_TERM res = enif_make_binary(env, &bin);
        return enif_make_tuple2(env, ATOM_OK, res);
      }
    }
  else
    {
      return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM emmap_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  unsigned long pos;
  mhandle *handle;
  if (argc==3
      && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_ulong(env, argv[1], &pos)
      && enif_inspect_binary(env, argv[2], &bin)
      && pos >= 0
      && (pos + bin.size) <= handle->len
      )
    {

      if ((handle->prot & PROT_WRITE) == 0) {
        return make_error_tuple(env, EACCES);
      }

      enif_rwlock_rwlock(handle->rwlock);
      if (handle->closed) {
        enif_rwlock_rwunlock(handle->rwlock);
        return enif_make_badarg(env);
      } else {
        memcpy((void*) (((char*)handle->mem) + pos), bin.data, bin.size);
        enif_rwlock_rwunlock(handle->rwlock);
      }

      return ATOM_OK;
    }
  else
    {
      return enif_make_badarg(env);
    }
}
