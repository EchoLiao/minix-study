/*	execve() - basic program execution call		Author: Kees J. Bot
 *								21 Jan 1994
 */

#define _MINIX_SOURCE

#define nil 0
#define execve _execve
#define sbrk _sbrk
#include <lib.h>
#include <unistd.h>
#include <string.h>
#include <stddef.h>

/* execve("/bin/ls", "ls -l -h", "HOME=/home/scr"); */
int execve(const char *path, char * const *argv, char * const *envp)
{
    char * const *ap;
    char * const *ep;
    char *frame;
    char **vp;
    char *sp;
    size_t argc;
    size_t frame_size;
    size_t string_off;
    size_t n;
    int ov;
    message m;

    /* Assumptions: size_t and char *, it's all the same thing. */

    /* Create a stack image that only needs to be patched up slightly
     * by the kernel to be used for the process to be executed.
     */

    ov= 0;			/* No overflow yet. */
    frame_size= 0;		/* Size of the new initial stack. */
    string_off= 0;		/* Offset to start of the strings. */
    argc= 0;		/* Argument count. */

    /* execve("/bin/ls", "ls -l -h", "HOME=/home/scr");                     */
    /* execve(const char *path, char * const *argv, char * const *envp) */
    for (ap= argv; *ap != nil; ap++) {
	n = sizeof(*ap) + strlen(*ap) + 1;
	frame_size+= n;
	if (frame_size < n) ov= 1;
	string_off+= sizeof(*ap);
	argc++;
    }

    for (ep= envp; *ep != nil; ep++) {
	n = sizeof(*ep) + strlen(*ep) + 1;
	frame_size+= n;
	if (frame_size < n) ov= 1;
	string_off+= sizeof(*ap);
    }

    /* Add an argument count and two terminating nulls. */
    frame_size+= sizeof(argc) + sizeof(*ap) + sizeof(*ep);
    string_off+= sizeof(argc) + sizeof(*ap) + sizeof(*ep);

    /* Align. */
    frame_size= (frame_size + sizeof(char *) - 1) & ~(sizeof(char *) - 1);

    /* The party is off if there is an overflow. */
    /* 最少为4(argc占1,apP最少占2,epP最少占1)字长,
     * 即: execve("/bin/ls", "/bin/ls", NULL); */
    if (ov || frame_size < 3 * sizeof(char *)) {
	errno= E2BIG;
	return -1;
    }

    /* Allocate space for the stack frame. */
    if ((frame = (char *) sbrk(frame_size)) == (char *) -1) {
	errno = E2BIG;
	return -1;
    }

    /* Set arg count, init pointers to vector and string tables. */
    * (size_t *) frame = argc;
    
    /* 注意这里进行了(char**)类型的强制转换!!! 使得 ../../servers/pm/exec.c中,
     * insert_arg函数得以正常运行!!!!! */ 
    vp = (char **) (frame + sizeof(argc)); 
    sp = frame + string_off; /* 这里并没有进行强制转换 */ 

    /* Load the argument vector and strings. */
    for (ap= argv; *ap != nil; ap++) {
	*vp++= (char *) (sp - frame);
	n= strlen(*ap) + 1;
	memcpy(sp, *ap, n);
	sp+= n;
    }
    *vp++= nil; /* 注意: 这里表示指针指向NULL, 而不是表示**vp='\0' */ 

    /* Load the environment vector and strings. */
    for (ep= envp; *ep != nil; ep++) {
	*vp++= (char *) (sp - frame);
	n= strlen(*ep) + 1;
	memcpy(sp, *ep, n);
	sp+= n;
    }
    *vp++= nil;

    /* Padding. */
    while (sp < frame + frame_size)
	*sp++= 0; /* *sp='\0' ..., (sp是被声明为char*类型的) */

    /* We can finally make the system call. */
    m.m1_i1 = strlen(path) + 1;
    m.m1_i2 = frame_size;
    m.m1_p1 = (char *) path;
    m.m1_p2 = frame;

    /* Clear unused fields */
    m.m1_i3 = 0;
    m.m1_p3 = NULL;

    (void) _syscall(MM, EXEC, &m);

    /* Failure, return the memory used for the frame and exit. */
    (void) sbrk(-frame_size);
    return -1;
}
