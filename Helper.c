#include <dirent.h>
#include <unistd.h>
#include <limits.h>

unsigned int __hscore_d_type( struct dirent* d )
{
  return d->d_type;
}

unsigned int __hscore_d_namlen( struct dirent* d )
{
  return d->d_namlen;
}

int __hscore_readdir_r(DIR * dir, struct dirent * d, struct dirent ** dp)
{
#if HAVE_READDIR_R
  if (d == NULL) {
    *dp = readdir(dir);
    return 0;
  } else {
    return readdir_r(dir, d, dp);
  }
#else
  *dp = readdir(dir);
  return 0;
#endif
}

unsigned int __hscore_sizeof_dirent()
{
  static unsigned int nm_max = (unsigned int)-1;

  if (nm_max == (unsigned int)-1) {
#ifdef NAME_MAX
    nm_max = NAME_MAX + 1;
#else
    nm_max = pathconf(".", _PC_NAME_MAX);
    if (nm_max == -1) { nm_max = 255; }
    nm_max++;
#endif
  }
  return nm_max + sizeof(struct dirent);
}
