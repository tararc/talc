/* Elf file, section, and symbol manipulation routines.
   Copyright 1996, 1997 Linux International.

   Contributed by Richard Henderson <rth@tamu.edu>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ident "$Id: coff_obj_common.c,v 1.1 1999/10/19 18:25:00 mwh Exp $"

#include <string.h>
#include <assert.h>

#include "obj.h"
#include "util.h"

/*======================================================================*/

/* Standard ELF hash function.  */
unsigned long
obj_elf_hash_n(const char *name, unsigned long n)
{
  unsigned long h = 0;
  unsigned long g;
  unsigned char ch;

  while (n > 0)
    {
      ch = *name++;
      h = (h << 4) + ch;
      if ((g = (h & 0xf0000000)) != 0)
	{
	  h ^= g >> 24;
	  h &= ~g;
	}
      n--;
    }
  return h;
}

unsigned long
obj_elf_hash (const char *name)
{
  return obj_elf_hash_n(name, strlen(name));
}

void
obj_set_symbol_compare (struct obj_file *f,
		        int (*cmp)(const char *, const char *),
			unsigned long (*hash)(const char *))
{
  if (cmp)
    f->symbol_cmp = cmp;
  if (hash)
    {
      struct obj_symbol *tmptab[HASH_BUCKETS], *sym, *next;
      int i;

      f->symbol_hash = hash;

      memcpy(tmptab, f->symtab, sizeof(tmptab));
      memset(f->symtab, 0, sizeof(f->symtab));

      for (i = 0; i < HASH_BUCKETS; ++i)
	for (sym = tmptab[i]; sym ; sym = next)
	  {
	    unsigned long h = hash(sym->name) % HASH_BUCKETS;
	    next = sym->next;
	    sym->next = f->symtab[h];
	    f->symtab[h] = sym;
	  }
    }
}

struct obj_symbol *
obj_add_symbol (struct obj_file *f, const char *name, unsigned long symidx,
		unsigned short type, unsigned char class, int secidx, 
		unsigned int value)
{
  struct obj_symbol *sym;
  unsigned long hash = f->symbol_hash(name) % HASH_BUCKETS;

#ifdef VERBOSE
  printf("Adding symbol |%s|(#%d), type/class=%d/%d, sec=%d, val=%#x\n",
	 name, symidx, type, class, secidx, value);
  fflush(stdout);
#endif

  for (sym = f->symtab[hash]; sym; sym = sym->next)
    if (f->symbol_cmp(sym->name, name) == 0)
      {
	int o_secidx = sym->secidx;
	int o_class = sym->class;

	/* A redefinition!  Is it legal?  */

	if (secidx == SECTION_UNDEF)
	  return sym;
	else if (o_secidx == SECTION_UNDEF)
	  goto found;
	else if (class == CLASS_EXTERNAL && o_class == CLASS_STATIC)
	  {
	    /* Cope with local and global symbols of the same name
	       in the same object file, as might have been created
	       by ld -r.  The only reason locals are now seen at this
	       level at all is so that we can do semi-sensible things
	       with parameters.  */
	    
	    struct obj_symbol *nsym, **p;

	    nsym = arch_new_symbol();
  	    nsym->next = sym->next;

	    /* Excise the old (local) symbol from the hash chain.  */
	    for (p = &f->symtab[hash]; *p != sym; p = &(*p)->next)
	      continue;
	    *p = sym = nsym;
	    goto found;
	  }
	else if (class == CLASS_STATIC)
	  {
	    /* Another symbol of the same name has already been defined.
	       Just add this to the local table.  */
	    sym = arch_new_symbol();
	    sym->next = NULL;
	    f->local_symtab[symidx] = sym;
	    goto found;
	  }
	else if (class == CLASS_WEAKEXT)
	  return sym;
	else if (o_class == CLASS_WEAKEXT)
	  goto found;
	else
	  {
	    /* Don't report an error if the symbol is coming from
	       the kernel or some external module.  */
	    if (secidx <= SHN_HIRESERVE)
	      error("%s multiply defined", name);
	    return sym;
	  }
      }

  /* Completely new symbol.  */
  sym = arch_new_symbol();
  sym->next = f->symtab[hash];
  f->symtab[hash] = sym;

  if (class == CLASS_STATIC)
    f->local_symtab[symidx] = sym;

found:
  sym->name = name;
  sym->value = value;
  sym->secidx = secidx;
  sym->class = class;
  sym->type = type;

  return sym;
}

struct obj_symbol *
obj_find_symbol (struct obj_file *f, const char *name)
{
  struct obj_symbol *sym;
  unsigned long hash = f->symbol_hash(name) % HASH_BUCKETS;

  for (sym = f->symtab[hash]; sym; sym = sym->next)
    if (f->symbol_cmp(sym->name, name) == 0)
      return sym;

  return NULL;
}

unsigned int
obj_symbol_final_value (struct obj_file *f, struct obj_symbol *sym)
{
  if (sym)
    {
      int idx;
      if (sym->secidx >= SHN_LORESERVE)
	return sym->value;

      idx = sym->secidx-1;
      assert(idx >= 0 && idx < f->header->f_nscns);
      return sym->value + f->sections[idx]->header->s_vaddr;
    }
  else
    {
      /* As a special case, a NULL sym has value zero.  */
      return 0;
    }
}

static int
obj_load_order_prio(struct obj_section *a)
{
  unsigned int af, ac;

  af = a->header->s_flags;

  ac = 0;
  if (a->name[0] != '.' || strlen(a->name) != 10 ||
      strcmp(a->name + 5, ".init")) ac |= 32;
  if (af & SFLAG_READ) ac |= 16;
  if (!(af & SFLAG_WRITE)) ac |= 8;
  if (af & SFLAG_EXECUTE) ac |= 4;
  /*
  if (a->header.sh_type != SHT_NOBITS) ac |= 2;
  */
  ac |= 2;
  return ac;
}

void
obj_insert_section_load_order (struct obj_file *f, struct obj_section *sec)
{
  struct obj_section **p;
  int prio = obj_load_order_prio(sec);
  for (p = f->load_order_search_start; *p ; p = &(*p)->load_next)
    if (obj_load_order_prio(*p) < prio)
      break;
  sec->load_next = *p;
  *p = sec;
}

