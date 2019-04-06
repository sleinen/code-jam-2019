/*
 qr2-maze.c

 Date Created: Sat Apr  6 11:00:16 2019
 Author:       Simon Leinen  <simon.leinen@switch.ch>
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define MAXSIZE 1000

int
main ()
{
  int c, ncases;

  scanf ("%d\n", &ncases);
  for (c = 1; c <= ncases; ++c)
    {
      unsigned N, k, l;
      char hers[2 * (MAXSIZE - 1) + 1];
      char mine[2 * (MAXSIZE - 1) + 1];

      scanf ("%u\n", &N);
      scanf ("%s\n", hers);
      l = strlen (hers);
      for (k = 0; k < l; ++k)
	{
	  mine[k] = (hers[k] == 'S') ? 'E' : 'S';
	}
      mine[l] = 0;
      printf ("Case #%d: %s\n", c, mine);
    }
}
