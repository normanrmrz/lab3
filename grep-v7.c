/* UNIX V7 source code: see /COPYRIGHT or www.tuhs.org for details. */

/*
 * grep -- print lines matching (or not matching) a pattern
 *
 *	status returns:
 *		0 - ok, and some matches
 *		1 - ok, but no matches
 *		2 - some error
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
// Add include for stdlib.
#include <stdlib.h>

//add prototypes of subroutines and BSIZE
//also added the appropriate return types for subroutines. 
void errexit(char * s, char * f);
void compile(char * astr);
void execute(char * file);
int advance(char * lp, char * ep);
void succeed(char * f);
int ecmp(char * a, char * b, int count);
#define BSIZE 512 

#define	CBRA	1
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11
#define	CKET	12
#define	CBACK	18

#define	STAR	01

#define	LBSIZE	512
#define	ESIZE	8192
#define	NBRA	9


/* expbuff is a variable (char array) that contains the regular expression being passed to grep */
//char	expbuf[ESIZE];
char *expbuf;
long	lnum;
char	linebuf[LBSIZE+1];
//char	ybuf[ESIZE];
char *ybuf;
int	bflag;
int	lflag;
int	nflag;
int	cflag;
int	vflag;
int	nfile;
int	hflag	= 1;
int	sflag;
int	yflag;
int	circf;
long	tln;
int	nsucc;
char	*braslist[NBRA];
char	*braelist[NBRA];
char	bittab[] = {
	1,
	2,
	4,
	8,
	16,
	32,
	64,
	128
};


/* Main - The main function first checks for flags and sets them if necessary.
If the yflag is set, the program goes through a small process to
build a new expression where characters are replaced with a
bracketed expression with a lower and an uppercase version of
the letter. This new expression matches without regardless of case. */ 

int main(argc, argv)
char **argv;
{
	expbuf = malloc(ESIZE);
	ybuf = malloc(ESIZE);
	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'y':
			yflag++;
			continue;

		case 'h':
			hflag = 0;
			continue;

		case 's':
			sflag++;
			continue;

		case 'v':
			vflag++;
			continue;

		case 'b':
			bflag++;
			continue;

		case 'l':
			lflag++;
			continue;

		case 'c':
			cflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'e':
			--argc;
			++argv;
			goto out;

		default:
			errexit("grep: unknown flag\n", (char *)NULL);
			continue;
		}
out:
	if (argc<=0)
		exit(2);
	if (yflag) {
		register char *p, *s;
		for (s = ybuf, p = *argv; *p; ) {
			if (*p == '\\') {
				*s++ = *p++;
				if (*p)
					*s++ = *p++;
			} else if (*p == '[') {
				while (*p != '\0' && *p != ']')
					*s++ = *p++;
			} else if (islower(*p)) {
				*s++ = '[';
				*s++ = toupper(*p);
				*s++ = *p++;
				*s++ = ']';
			} else
				*s++ = *p++;
			if (s >= ybuf+ESIZE-5)
				errexit("grep: argument too long\n", (char *)NULL);
		}
		*s = '\0';
		*argv = ybuf;
	}
	compile(*argv);
	nfile = --argc;
	if (argc<=0) {
		if (lflag)
			exit(1);
		execute((char *)NULL);
	} else while (--argc >= 0) {
		argv++;
		execute(*argv);
	}

	exit(nsucc == 0);
}

/* Compile - The input to the compile funtion is a pointer to the expression.
In compile, the program takes the expression in its raw string
form and converts it into a form useable by the subroutine. 
This is done using the ep variable which points to the the expbuf variable. 
expbuf is a buffer for the regular expression and stores it in the following format:
first, add a tag that indicates the type of the element, and then depending 
on what type it is, follow it with the contents for the tag. 
For example, a '.' or an EOF character will be one element in the buffer, 
replaced by one of the constants defined at the top. 
For elements, like parens or characters, they will first have an element with the appropriate
constant added, then for parens, the number for its count is added.
If it is a character, the actual character is added to the buffer. This
function returns nothing when finished, but the result of the
algorithm is stored in expbuf. */
void compile(astr)
char *astr;
{

	register int c;
	register char *ep, *sp;
	char *cstart;
	char *lastep;
	int cclcnt;
	char bracket[NBRA], *bracketp;
	int closed;
	char numbra;
	char neg;
	char b1, b2;
	int i;

	ep = expbuf;
	sp = astr;
	lastep = 0;
	bracketp = bracket;
	closed = numbra = 0;
	if (*sp == '^') {
		circf++;
		sp++;
	}
	for (;;) {
		if (ep >= &expbuf[ESIZE])
			goto cerror;
		if ((c = *sp++) != '*')
			lastep = ep;
		switch (c) {

		case '\0':
			*ep++ = CEOF;
			return;

		case '.':
			*ep++ = CDOT;
			continue;



		case '*':
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if (*sp != '\0')
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if(&ep[17] >= &expbuf[ESIZE])
				goto cerror;
			*ep++ = CCL;
			neg = 0;
			if((c = *sp++) == '^') {
				neg = 1;
				c = *sp++;
			}
			cstart = sp;
			do {
				if (c=='\0')
					goto cerror;
				if (c=='-' && sp>cstart && *sp!=']') {
					for (c = sp[-2]; c<*sp; c++)
						ep[c>>3] |= bittab[c&07];
					sp++;
				}
				ep[c>>3] |= bittab[c&07];
			} while((c = *sp++) != ']');
			if(neg) {
				for(cclcnt = 0; cclcnt < 16; cclcnt++)
					ep[cclcnt] ^= -1;
				ep[0] &= 0376;
			}

			ep += 16;

			continue;

		case '\\':
			if((c = *sp++) == '(') {
				if(numbra >= NBRA) {
					goto cerror;
				}
				*bracketp++ = numbra;
				*ep++ = CBRA;
				*ep++ = numbra++;
				continue;
			}
			if(c == ')') {
				if(bracketp <= bracket) {
					goto cerror;
				}
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;
			}

			if(c >= '1' && c <= '9') {
				if((c -= '1') >= closed)
					goto cerror;
				*ep++ = CBACK;
				*ep++ = c;
				continue;
			}

		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
    cerror:
	errexit("grep: RE error\n", (char *)NULL);
}

/* Execute - This function manages the process of reading in the 
lines to be matched. Once the linebuf variable is filled,
the subroutine advance is called with the line buffer and the expression
buffer (linebuf and expbuf) as parameters. */
void execute(file)
char *file;
{
	register char *p1, *p2;
	register int c;

	if (file) {
		if (freopen(file, "r", stdin) == NULL)
			errexit("grep: can't open %s\n", file);
	}
	lnum = 0;
	tln = 0;
	for (;;) {
		lnum++;
		p1 = linebuf;
		while ((c = getchar()) != '\n') {
			if (c == EOF) {
				if (cflag) {
					if (nfile>1)
						printf("%s:", file);
					printf("%lD\n", tln);
				}
				return;
			}
			*p1++ = c;
			if (p1 >= &linebuf[LBSIZE-1])
				break;
		}
		*p1++ = '\0';
		p1 = linebuf;
		p2 = expbuf;
		if (circf) {
			if (advance(p1, p2)){
				goto found;
			}
			goto nfound;
		}
		/* fast check for first character */
		if (*p2==CCHR) {
			c = p2[1];
			do {
				if (*p1!=c)
					continue;
				if (advance(p1, p2))
					goto found;
			} while (*p1++);
			goto nfound;
		}
		/* regular algorithm */
		do {
			if (advance(p1, p2))
				goto found;
		} while (*p1++);
	nfound:
		if (vflag)
			succeed(file);
		continue;
	found:
		if (vflag==0)
			succeed(file);
	}

}

/* Advance - The goal of the advance function is to step through the
expression buffer to see if the contents of it are able to be matched
with the start of the line buffer. If a CCHR is next, then it will
directly match the next character in the expression buffer to the
one in the line buffer. For CDOT, it will just continue because
'.' will match to any character. The advance function will keep processing
until a case fails, in which case it willreturn 0, or until the CEOF case is
reached. When the CEOF case is reached, that is a marker for the end
of the expression, meaning that the entire expression buffer has been processed,
so in this case it will return true. 
The advance function will call itself recursively when either a bracketed or
a parenthesized expression is used with the '*' character. It makes
the recursive call because the bracket or parenthesis can be treated
as a smaller expression, where the recursive call will do the matching.
When control returns to the execute function, if a match was found to
the inital position in the line buffer, it will jump to the found tag.
If not, the line buffer will be incremented and it will attempt to match
again and will continue to do this until it is matched or the end
of the line buffer is reached.
 */ 
int advance(lp, ep)
register char *lp, *ep;
{
	register char *curlp;
	char c;
	char *bbeg;
	int ct;

	for (;;) switch (*ep++) {

	case CCHR:
		if (*ep++ == *lp++)
			continue;
		return(0);

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp==0)
			continue;
		return(0);

	case CEOF:
		return(1);

	case CCL:
		c = *lp++ & 0177;
		if(ep[c>>3] & bittab[c & 07]) {
			ep += 16;
			continue;
		}
		return(0);
	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CBACK:
		bbeg = braslist[*ep];
		if (braelist[*ep]==0)
			return(0);
		ct = braelist[*ep++] - bbeg;
		if(ecmp(bbeg, lp, ct)) {
			lp += ct;
			continue;
		}
		return(0);

	case CBACK|STAR:
		bbeg = braslist[*ep];
		if (braelist[*ep]==0)
			return(0);
		ct = braelist[*ep++] - bbeg;
		curlp = lp;
		while(ecmp(bbeg, lp, ct))
			lp += ct;
		while(lp >= curlp) {
			if(advance(lp, ep))	return(1);
			lp -= ct;
		}
		return(0);


	case CDOT|STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (*lp++ == *ep);
		ep++;
		goto star;

	case CCL|STAR:
		curlp = lp;
		do {
			c = *lp++ & 0177;
		} while(ep[c>>3] & bittab[c & 07]);
		ep += 16;
		goto star;

	star:
		if(--lp == curlp) {
			continue;
		}

		if(*ep == CCHR) {
			c = ep[1];
			do {
				if(*lp != c)
					continue;
				if(advance(lp, ep))
					return(1);
			} while(lp-- > curlp);
			return(0);
		}

		do {
			if (advance(lp, ep))
				return(1);
		} while (lp-- > curlp);
		return(0);

	default:
		errexit("grep RE botch\n", (char *)NULL);
	}

}

/* Succeed - When a match is found in execute, it goes to the found block, where
the succeed function is called. The purpose of the succeed function
is to print to standard out. If no flags are set, then it will only 
print the contents of linebuf to stdout. 
Additional output may be printed depending on what flags have been set. 
For example, if the b flag is set, the byte
position of the file will be added to line, and if the n flag is set,
it will add the number of the line. 
When the succeed function is finished printing, control is passed
back to the execute function. From there, lines will be read and
matched from the input file until it reaches the end of the file.
From there control returns to main function, where the program
finishes executing by calling exit. When exit is called, it
checks if nsucc is equal to 0. Since nsucc is set to 1 if a match
is found in succeed, if there was a match, it will set the exit
status to 0. 
If it failed to match, the exit status will be 1. 
*/
void succeed(f)
char *f;
{
	long ftell();
	nsucc = 1;
	if (sflag)
		return;
	if (cflag) {
		tln++;
		return;
	}
	if (lflag) {
		printf("%s\n", f);
		fseek(stdin, 0l, 2);
		return;
	}
	if (nfile > 1 && hflag)
		printf("%s:", f);
	if (bflag)
		printf("%ld:", (ftell(stdin)-1)/BSIZE);
	if (nflag)
		printf("%ld:", lnum);
	printf("%s\n", linebuf);


}

int ecmp(a, b, count)
char	*a, *b;
{
	register int cc = count;
	while(cc--)
		if(*a++ != *b++)	return(0);
	return(1);
}

void errexit(s, f)
char *s, *f;
{
	fprintf(stderr, s, f);
	exit(2);
}