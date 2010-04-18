/* Copyright (c) 2008, 2009
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 *      Micah Cowan (micah@cowan.name)
 *      Sadrul Habib Chowdhury (sadrul@users.sourceforge.net)
 * Copyright (c) 1993-2002, 2003, 2005, 2006, 2007
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, see
 * http://www.gnu.org/licenses/, or contact Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02111-1301  USA
 *
 ****************************************************************
 */

#include <sys/types.h>
#include "config.h"
#include "screen.h"
#include "extern.h"

#define INPUTLINE (flayer->l_height - 1)

static void InpProcess __P((char **, int *));
static void InpProcessForAllocatedData __P((char **, int *));
static void InpAbort __P((void));
static void InpRedisplayLine __P((int, int, int, int));
static void InpFree __P((void *));

extern struct layer *flayer;
extern struct display *display;
extern struct mchar mchar_blank, mchar_so;

struct inpline
{
  char  buf[101];	/* text buffer */
  int  len;		/* length of the editible string */
  int  pos;		/* cursor position in editable string */
  struct inpline *next, *prev;
};

/* 'inphist' is used to store the current input when scrolling through history.
 * inpline->prev == history-prev
 * inpline->next == history-next
 */
static struct inpline inphist;

struct inpdata
{
  struct inpline inp;
  int  inpmaxlen;	/* 100, or less, if caller has shorter buffer */
  char inpstring[101];	/* the prompt */
  int  inpstringlen;	/* length of the prompt */
  int  inpmode;		/* INP_NOECHO, INP_RAW, INP_EVERY */
  void (*inpfinfunc) __P((char *buf, int len, char *priv));
  char  *priv;		/* private data for finfunc */
  int  privdata;	/* private data space */
  char *search; 	/* the search string */
};

struct inpdata_priv
{
  char *data;
  void (*free) __P((char *data));
};

static struct LayFuncs InpLf =
{
  InpProcess,
  InpAbort,
  InpRedisplayLine,
  DefClearLine,
  DefRewrite,
  DefResize,
  DefRestore,
  0
};

static struct LayFuncs InpForAllocatedDataLf =
{
  InpProcessForAllocatedData,
  InpAbort,
  InpRedisplayLine,
  DefClearLine,
  DefRewrite,
  DefResize,
  DefRestore,
  InpFree
};

/*
**   Here is the input routine
*/

/* called once, after InitOverlayPage in Input() or Isearch() */
void
inp_setprompt(p, s)
char *p, *s;
{
  struct inpdata *inpdata;
  
  inpdata = (struct inpdata *)flayer->l_data;
  if (p)
    {
      if (p != inpdata->inpstring)
        strncpy(inpdata->inpstring, p, sizeof(inpdata->inpstring) - 1);
      inpdata->inpstring[sizeof(inpdata->inpstring) - 1] = 0;
      inpdata->inpstringlen = strlen(inpdata->inpstring);
    }
  if (s)
    {
      if (s != inpdata->inp.buf)
	strncpy(inpdata->inp.buf, s, sizeof(inpdata->inp.buf) - 1);
      inpdata->inp.buf[sizeof(inpdata->inp.buf) - 1] = 0;
      inpdata->inp.pos = inpdata->inp.len = strlen(inpdata->inp.buf);
    }
  InpRedisplayLine(INPUTLINE, 0, flayer->l_width - 1, 0);
  flayer->l_x = inpdata->inpstringlen + (inpdata->inpmode & INP_NOECHO ? 0 : inpdata->inp.pos);
  flayer->l_y = INPUTLINE;
}

static int
InputImpl(istr, len, mode, finfunc, priv, data, lf)
char *istr;
int len;
int mode;
void (*finfunc) __P((char *buf, int len, char *priv));
char *priv;
int data;
struct LayFuncs *lf;
{
  int maxlen;
  struct inpdata *inpdata;
  
  if (!flayer)
    return 0;

  if (strlen(istr) > 100)
    {
      LMsg(0, "prompt message too long");
      return 0;
    }
  if (len > 100)
    len = 100;
  if (!(mode & INP_NOECHO))
    {
      maxlen = flayer->l_width - 1 - strlen(istr);
      if (len > maxlen)
	len = maxlen;
    }
  if (len < 0)
    {
      LMsg(0, "Width %d chars too small", -len);
      return 0;
    }
  if (InitOverlayPage(sizeof(*inpdata), lf, 1))
    return 0;
  flayer->l_mode = 1;
  inpdata = (struct inpdata *)flayer->l_data;
  inpdata->inpmaxlen = len;
  inpdata->inpfinfunc = finfunc;
  inpdata->inp.pos = inpdata->inp.len = 0;
  inpdata->inp.prev = inphist.prev;
  inpdata->inpmode = mode;
  inpdata->privdata = data;
  if (!priv)
    priv = (char*)&inpdata->privdata;
  inpdata->priv = priv;
  inpdata->inpstringlen = 0;
  inpdata->inpstring[0] = '0';
  inpdata->search = NULL;
  if (istr)
    inp_setprompt(istr, (char *)NULL);
  return 1;
}

/*
 * We dont use HS status line with Input().
 * If we would use it, then we should check e_tgetflag("es") if
 * we are allowed to use esc sequences there.
 *
 * mode is an OR of
 * INP_NOECHO == suppress echoing of characters.
 * INP_RAW    == raw mode. call finfunc after each character typed.
 * INP_EVERY  == digraph mode.
 */
int
Input(istr, len, mode, finfunc, priv, data)
char *istr;
int len;
int mode;
void (*finfunc) __P((char *buf, int len, char *priv));
char *priv;
int data;
{
  return InputImpl(istr, len, mode, finfunc, priv, data, &InpLf);
}

int
InputForAllocatedData(istr, len, mode, finfunc, priv, free_priv)
char *istr;
int len;
int mode;
void (*finfunc) __P((char *buf, int len, char *priv));
char *priv;
void (*free_priv) __P((char *priv));
{
  struct inpdata_priv *privdata = malloc(sizeof(struct inpdata_priv));
  if (privdata == NULL)
    return 0;
  privdata->data = priv;
  privdata->free = free_priv;
  return InputImpl(istr, len, mode, finfunc, (char *)privdata, 0, &InpForAllocatedDataLf);
}

static void
erase_chars(inpdata, from, to, x, mv)
struct inpdata *inpdata;
char *from;
char *to;
int x;
int mv;
{
  int chng;
  ASSERT(from < to);
  if (inpdata->inp.len > to - inpdata->inp.buf)
    bcopy(to, from, inpdata->inp.len - (to - inpdata->inp.buf));
  chng = to - from;
  if (mv)
    {
      x -= chng;
      inpdata->inp.pos -= chng;
    }
  inpdata->inp.len -= chng;
  if (!(inpdata->inpmode & INP_NOECHO))
    {
      struct mchar mc;
      char *s = from < to ? from : to;
      mc = mchar_so;
      while (s < inpdata->inp.buf+inpdata->inp.len)
	{
	  mc.image = *s++;
	  LPutChar(flayer, &mc, x++, INPUTLINE);
	}
      while (chng--)
	LPutChar(flayer, &mchar_blank, x++, INPUTLINE);
      x = inpdata->inpstringlen + inpdata->inp.pos;
      LGotoPos(flayer, x, INPUTLINE);
    }
}

static void
InpProcessImpl(ppbuf, plen, priv)
char **ppbuf;
int *plen;
char *priv;
{
  int len, x;
  char *pbuf;
  char ch;
  struct inpdata *inpdata;
  struct display *inpdisplay;
  int prev, next, search = 0;
  void (*layFree)(void *) = NULL;

  inpdata = (struct inpdata *)flayer->l_data;
  inpdisplay = display;

#define RESET_SEARCH do { if (inpdata->search) Free(inpdata->search); } while (0)

  LGotoPos(flayer, inpdata->inpstringlen + (inpdata->inpmode & INP_NOECHO ? 0 : inpdata->inp.pos), INPUTLINE);
  if (ppbuf == 0)
    {
      InpAbort();
      return;
    }
  x = inpdata->inpstringlen + inpdata->inp.pos;
  len = *plen;
  pbuf = *ppbuf;
  while (len)
    {
      char *p = inpdata->inp.buf + inpdata->inp.pos;

      ch = *pbuf++;
      len--;
      if (inpdata->inpmode & INP_EVERY)
	{
	  inpdata->inp.buf[inpdata->inp.len] = ch;
	  if (ch)
	    {
	      display = inpdisplay;
	      (*inpdata->inpfinfunc)(inpdata->inp.buf, inpdata->inp.len, priv);
	      ch = inpdata->inp.buf[inpdata->inp.len];
	    }
	}
      else if (inpdata->inpmode & INP_RAW)
	{
	  display = inpdisplay;
          (*inpdata->inpfinfunc)(&ch, 1, priv);	/* raw */
	  if (ch)
	    continue;
	}
      if (((unsigned char)ch & 0177) >= ' ' && ch != 0177 && inpdata->inp.len < inpdata->inpmaxlen)
	{
	  if (inpdata->inp.len > inpdata->inp.pos)
	    bcopy(p, p+1, inpdata->inp.len - inpdata->inp.pos);
	  inpdata->inp.buf[inpdata->inp.pos++] = ch;
	  inpdata->inp.len++;

	  if (!(inpdata->inpmode & INP_NOECHO))
	    {
	      struct mchar mc;
	      mc = mchar_so;
	      mc.image = *p++;
	      LPutChar(flayer, &mc, x, INPUTLINE);
	      x++;
	      if (p < inpdata->inp.buf+inpdata->inp.len)
		{
		  while (p < inpdata->inp.buf+inpdata->inp.len)
		    {
		      mc.image = *p++;
		      LPutChar(flayer, &mc, x++, INPUTLINE);
		    }
		  x = inpdata->inpstringlen + inpdata->inp.pos;
		  LGotoPos(flayer, x, INPUTLINE);
		}
	    }
	  RESET_SEARCH;
	}
      else if ((ch == '\b' || ch == 0177) && inpdata->inp.pos > 0)
	{
	  erase_chars(inpdata, p-1, p, x, 1);
	  RESET_SEARCH;
	}
      else if (ch == '\025')			/* CTRL-U */
	{
	  x = inpdata->inpstringlen;
	  if (inpdata->inp.len && !(inpdata->inpmode & INP_NOECHO))
	    {
	      LClearArea(flayer, x, INPUTLINE, x + inpdata->inp.len - 1, INPUTLINE, 0, 0);
	      LGotoPos(flayer, x, INPUTLINE);
	    }
	  inpdata->inp.len = inpdata->inp.pos = 0;
	}
      else if (ch == '\013')			/* CTRL-K */
	{
	  x = inpdata->inpstringlen + inpdata->inp.pos;
	  if (inpdata->inp.len > inpdata->inp.pos && !(inpdata->inpmode & INP_NOECHO))
	    {
	      LClearArea(flayer, x, INPUTLINE, x + inpdata->inp.len - inpdata->inp.pos - 1, INPUTLINE, 0, 0);
	      LGotoPos(flayer, x, INPUTLINE);
	    }
	  inpdata->inp.len = inpdata->inp.pos;
	}
      else if (ch == '\027' && inpdata->inp.pos > 0)		/* CTRL-W */
	{
	  char *oldp = p--;
	  while (p > inpdata->inp.buf && *p == ' ')
	    p--;
	  while (p > inpdata->inp.buf && *(p - 1) != ' ')
	    p--;
	  erase_chars(inpdata, p, oldp, x, 1);
	  RESET_SEARCH;
	}
      else if (ch == '\004' && inpdata->inp.pos < inpdata->inp.len)	/* CTRL-D */
	{
	  erase_chars(inpdata, p, p+1, x, 0);
	  RESET_SEARCH;
	}
      else if (ch == '\001' || (unsigned char)ch == 0201)	/* CTRL-A */
	{
	  LGotoPos(flayer, x -= inpdata->inp.pos, INPUTLINE);
	  inpdata->inp.pos = 0;
	}
      else if ((ch == '\002' || (unsigned char)ch == 0202) && inpdata->inp.pos > 0)	/* CTRL-B */
	{
	  LGotoPos(flayer, --x, INPUTLINE);
	  inpdata->inp.pos--;
	}
      else if (ch == '\005' || (unsigned char)ch == 0205)	/* CTRL-E */
	{
	  LGotoPos(flayer, x += inpdata->inp.len - inpdata->inp.pos, INPUTLINE);
	  inpdata->inp.pos = inpdata->inp.len;
	}
      else if ((ch == '\006' || (unsigned char)ch == 0206) && inpdata->inp.pos < inpdata->inp.len)	/* CTRL-F */
	{
	  LGotoPos(flayer, ++x, INPUTLINE);
	  inpdata->inp.pos++;
	}
      else if ((prev = ((ch == '\020' || (unsigned char)ch == 0220) &&	/* CTRL-P */
	      inpdata->inp.prev)) ||
	  (next = ((ch == '\016' || (unsigned char)ch == 0216) &&  /* CTRL-N */
		   inpdata->inp.next)) ||
	  (search = ((ch == '\022' || (unsigned char)ch == 0222) && inpdata->inp.prev)))
	{
	  struct mchar mc;
	  struct inpline *sel;
	  int pos = -1;

	  mc = mchar_so;

	  if (prev)
	    sel = inpdata->inp.prev;
	  else if (next)
	    sel = inpdata->inp.next;
	  else
	    {
	      /* search */
	      inpdata->inp.buf[inpdata->inp.len] = 0;	/* Remove the ctrl-r from the end */
	      if (!inpdata->search)
		inpdata->search = SaveStr(inpdata->inp.buf);
	      for (sel = inpdata->inp.prev; sel; sel = sel->prev)
		{
		  char *f;
		  if ((f = strstr(sel->buf, inpdata->search)))
		    {
		      pos = f - sel->buf;
		      break;
		    }
		}
	      if (!sel)
		continue;	/* Did not find a match. Process the next input. */
	    }

	  if (inpdata->inp.len && !(inpdata->inpmode & INP_NOECHO))
	    LClearArea(flayer, inpdata->inpstringlen, INPUTLINE, inpdata->inpstringlen + inpdata->inp.len - 1, INPUTLINE, 0, 0);

	  if ((prev || search) && !inpdata->inp.next)
	    inphist = inpdata->inp;
	  memcpy(&inpdata->inp, sel, sizeof(struct inpline));
	  if (pos != -1)
	    inpdata->inp.pos = pos;
	  if (inpdata->inp.len > inpdata->inpmaxlen)
	    inpdata->inp.len = inpdata->inpmaxlen;
	  if (inpdata->inp.pos > inpdata->inp.len)
	    inpdata->inp.pos = inpdata->inp.len;

	  x = inpdata->inpstringlen;
	  p = inpdata->inp.buf;

	  if (!(inpdata->inpmode & INP_NOECHO))
	    {
	      while (p < inpdata->inp.buf+inpdata->inp.len)
		{
		  mc.image = *p++;
		  LPutChar(flayer, &mc, x++, INPUTLINE);
		}
	    }
	  x = inpdata->inpstringlen + inpdata->inp.pos;
	  LGotoPos(flayer, x, INPUTLINE);
	}

      else if (ch == '\003' || ch == '\007' || ch == '\033' ||
	       ch == '\000' || ch == '\n' || ch == '\r')
	{
          if (ch != '\n' && ch != '\r')
	    inpdata->inp.len = 0;
	  inpdata->inp.buf[inpdata->inp.len] = 0;

	  if (inpdata->inp.len && !(inpdata->inpmode & (INP_NOECHO | INP_RAW)))
	    {
	      struct inpline *store;

	      /* Look for a duplicate first */
	      for (store = inphist.prev; store; store = store->prev)
		{
		  if (strcmp(store->buf, inpdata->inp.buf) == 0)
		    {
		      if (store->next)
			store->next->prev = store->prev;
		      if (store->prev)
			store->prev->next = store->next;
		      store->pos = inpdata->inp.pos;
		      break;
		    }
		}

	      if (!store)
		{
		  store = malloc(sizeof(struct inpline));
		  memcpy(store, &inpdata->inp, sizeof(struct inpline));
		}
	      store->next = &inphist;
	      store->prev = inphist.prev;
	      if (inphist.prev)
		inphist.prev->next = store;
	      inphist.prev = store;
	    }

          layFree = flayer->l_layfn->lf_LayFree;
	  flayer->l_data = 0;	/* so inpdata does not get freed */
          InpAbort();		/* redisplays... */
	  *ppbuf = pbuf;
	  *plen = len;
	  display = inpdisplay;
	  if ((inpdata->inpmode & INP_RAW) == 0)
            (*inpdata->inpfinfunc)(inpdata->inp.buf, inpdata->inp.len, priv);
	  else
            (*inpdata->inpfinfunc)(pbuf - 1, 0, priv);
	  if (inpdata->search)
	    free(inpdata->search);
          if (layFree)
            (*layFree)(inpdata);
	  free(inpdata);
	  return;
	}
      else
	{
	  /* The user was searching, and then pressed some non-control input. So reset
	   * the search string. */
	  RESET_SEARCH;
	}
    }
  if (!(inpdata->inpmode & INP_RAW))
    {
      flayer->l_x = inpdata->inpstringlen + (inpdata->inpmode & INP_NOECHO ? 0 : inpdata->inp.pos);
      flayer->l_y = INPUTLINE;
    }
  *ppbuf = pbuf;
  *plen = len;
}

static void
InpProcess(ppbuf, plen)
char **ppbuf;
int *plen;
{
  struct inpdata *inpdata = (struct inpdata *)flayer->l_data;
  InpProcessImpl(ppbuf, plen, inpdata->priv);
}

static void
InpProcessForAllocatedData(ppbuf, plen)
char **ppbuf;
int *plen;
{
  struct inpdata *inpdata = (struct inpdata *)flayer->l_data;
  struct inpdata_priv *priv = (struct inpdata_priv *)inpdata->priv;
  InpProcessImpl(ppbuf, plen, priv->data);
}


static void
InpAbort()
{
  LAY_CALL_UP(LayRedisplayLine(INPUTLINE, 0, flayer->l_width - 1, 0));
  ExitOverlayPage();
}

static void
InpRedisplayLine(y, xs, xe, isblank)
int y, xs, xe, isblank;
{
  int q, r, s, l, v;
  struct inpdata *inpdata;

  inpdata = (struct inpdata *)flayer->l_data;
  if (y != INPUTLINE)
    {
      LAY_CALL_UP(LayRedisplayLine(y, xs, xe, isblank));
      return;
    }
  inpdata->inp.buf[inpdata->inp.len] = 0;
  q = xs;
  v = xe - xs + 1;
  s = 0;
  r = inpdata->inpstringlen;
  if (v > 0 && q < r)
    {
      l = v;
      if (l > r - q)
	l = r - q;
      LPutStr(flayer, inpdata->inpstring + q - s, l, &mchar_so, q, y);
      q += l;
      v -= l;
    }
  s = r;
  r += inpdata->inp.len;
  if (!(inpdata->inpmode & INP_NOECHO) && v > 0 && q < r)
    {
      l = v;
      if (l > r - q)
	l = r - q;
      LPutStr(flayer, inpdata->inp.buf + q - s, l, &mchar_so, q, y);
      q += l;
      v -= l;
    }
  s = r;
  r = flayer->l_width;
  if (!isblank && v > 0 && q < r)
    {
      l = v;
      if (l > r - q)
	l = r - q;
      LClearArea(flayer, q, y, q + l - 1, y, 0, 0);
      q += l;
    }
}

static void
InpFree(l_data)
void *l_data;
{
  struct inpdata *inpdata = (struct inpdata *)l_data;
  struct inpdata_priv *priv = (struct inpdata_priv *)inpdata->priv;
  if (priv->free)
    (*priv->free)(priv->data);
  free(priv);
}

