#include <sys/dirent.h>

unsigned int
__hscore_d_type( struct dirent* d )
{
  return (d->d_type);
}

unsigned int
__hscore_d_namlen( struct dirent* d )
{
  return (d->d_namlen);
}
