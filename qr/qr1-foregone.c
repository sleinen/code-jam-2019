/*
 qr1-foregone.c

 Date Created: Sat Apr  6 10:44:40 2019
 Author:       Simon Leinen  <simon.leinen@switch.ch>
 */

#include <string.h>
#include <stdio.h>

#define MAXDIGITS 100

int
main ()
{
  int c, ncases, i, l, start;

  char N[MAXDIGITS+1];
  char A[MAXDIGITS+1];
  char B[MAXDIGITS+1];
  scanf ("%d\n", &ncases);
  for (c = 1; c <= ncases; ++c)
    {
      scanf ("%s\n", N);
      l = strlen (N);
      start = -1;
      strncpy (A, N, MAXDIGITS);
      memset (B, '0', MAXDIGITS);
      A[l] = B[l] = 0;
      for (i = 0; i < l; ++i)
	{
	  if (N[i] == '4')
	    {
	      A[i] = '3';
	      B[i] = '1';
	      if (start == -1)
		{
		  start = i;
		}
	    }
	}
      printf ("Case #%d: %s %s\n", c, A, B+start);
    }
}
