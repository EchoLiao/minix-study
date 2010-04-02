/*
 * (c) copyright 1987 by the Vrije Universiteit, Amsterdam, The Netherlands.
 * See the copyright notice in the ACK home directory, in the file "Copyright".
 */
/* $Header: /cvsup/minix/src/lib/ansi/strchr.c,v 1.1.1.1 2005/04/21 14:56:06 beng Exp $ */

#include	<string.h>

/* ***************************************************************************
* @Description  在字符串s中查找字符c, 找到则返回该位置的地址; 否则返回NULL
* 
* @Param s
* @Param c
* 
* @Returns   
** **************************************************************************/
char *
strchr(register const char *s, register int c)
{
	c = (char) c;

	while (c != *s) {
		if (*s++ == '\0') return NULL;
	}
	return (char *)s;
}
