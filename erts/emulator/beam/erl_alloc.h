/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2002-2025. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifndef ERL_ALLOC_H__
#define ERL_ALLOC_H__

#include "erl_alloc_types.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_threads.h"
#include "erl_mmap.h"

typedef enum {
    ERTS_ALC_S_INVALID = 0,

    ERTS_ALC_S_GOODFIT,
    ERTS_ALC_S_BESTFIT,
    ERTS_ALC_S_AFIT,
    ERTS_ALC_S_FIRSTFIT,

    ERTS_ALC_S_MIN = ERTS_ALC_S_GOODFIT,
    ERTS_ALC_S_MAX = ERTS_ALC_S_FIRSTFIT
} ErtsAlcStrat_t;

#include "erl_alloc_util.h"

#ifdef DEBUG
#  undef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 0
#endif

#ifndef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 1
#endif

#if ERTS_CAN_INLINE && ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_DO_INLINE 1
#  define ERTS_ALC_INLINE static ERTS_INLINE
#  define ERTS_ALC_FORCE_INLINE static ERTS_FORCE_INLINE
#else
#  define ERTS_ALC_DO_INLINE 0
#  define ERTS_ALC_INLINE
#  define ERTS_ALC_FORCE_INLINE
#endif

#define ERTS_ALC_NO_FIXED_SIZES \
  (ERTS_ALC_N_MAX_A_FIXED_SIZE - ERTS_ALC_N_MIN_A_FIXED_SIZE + 1)

#define ERTS_ALC_IS_FIX_TYPE(T) \
    (ERTS_ALC_T2N(T) >= ERTS_ALC_N_MIN_A_FIXED_SIZE && \
     ERTS_ALC_T2N(T) <= ERTS_ALC_N_MAX_A_FIXED_SIZE)

#define ERTS_ALC_FIX_TYPE_IX(T) \
  (ASSERT(ERTS_ALC_IS_FIX_TYPE(T)), \
   ERTS_ALC_T2N((T)) - ERTS_ALC_N_MIN_A_FIXED_SIZE)

void erts_sys_alloc_init(void);
void *erts_sys_alloc(ErtsAlcType_t, void *, Uint);
void *erts_sys_realloc(ErtsAlcType_t, void *, void *, Uint);
void erts_sys_free(ErtsAlcType_t, void *, void *);
#if ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
/*
 * Note 'alignment' must remain the same in calls to
 * 'erts_sys_aligned_realloc()' and 'erts_sys_aligned_free()'
 * as in the initial call to 'erts_sys_aligned_alloc()'.
 */
void *erts_sys_aligned_alloc(UWord alignment, UWord size);
void *erts_sys_aligned_realloc(UWord alignment, void *ptr, UWord size, UWord old_size);
void erts_sys_aligned_free(UWord alignment, void *ptr);
#endif

Eterm erts_memory(fmtfn_t *, void *, void *, Eterm);
Eterm erts_allocated_areas(fmtfn_t *, void *, void *);

Eterm erts_alloc_util_allocators(void *proc);
void erts_allocator_info(fmtfn_t, void *);
Eterm erts_allocator_options(void *proc);

struct process;

int erts_request_alloc_info(struct process *c_p, Eterm ref, Eterm allocs,
			    int only_sz, int internal);

#define ERTS_ALLOC_INIT_DEF_OPTS_INITER {0}
typedef struct {
    int ncpu;
} ErtsAllocInitOpts;

typedef struct {
    Allctr_t *deallctr[ERTS_ALC_A_MAX+1];
    int delayed_dealloc_handler;
    int alc_ix;
} ErtsThrAllocData;

void erts_alloc_init(int *argc, char **argv, ErtsAllocInitOpts *eaiop);
void erts_alloc_late_init(void);

#if defined(GET_ERTS_ALC_TEST) || defined(ERTS_ALC_INTERNAL__)
/* Only for testing */
UWord erts_alc_test(UWord,
		    UWord,
		    UWord,
		    UWord);
#endif

#define ERTS_ALC_O_ALLOC		0
#define ERTS_ALC_O_REALLOC		1
#define ERTS_ALC_O_FREE			2

#define ERTS_ALC_E_NOTSUP		0
#define ERTS_ALC_E_NOMEM		1
#define ERTS_ALC_E_NOALLCTR		2

#define ERTS_ALC_MIN_LONG_LIVED_TIME	(10*60*1000)

typedef struct {
    int alloc_util;
    int enabled;
    int thr_spec;
    void *extra;
} ErtsAllocatorInfo_t;

typedef struct {
    void *	(*alloc)	(ErtsAlcType_t, void *, Uint);
    void *	(*realloc)	(ErtsAlcType_t, void *, void *, Uint);
    void	(*free)		(ErtsAlcType_t, void *, void *);
    void *extra;
} ErtsAllocatorFunctions_t;

extern erts_tsd_key_t erts_thr_alloc_data_key;
extern ErtsAllocatorFunctions_t
    ERTS_WRITE_UNLIKELY(erts_allctrs[ERTS_ALC_A_MAX+1]);
extern ErtsAllocatorInfo_t
    ERTS_WRITE_UNLIKELY(erts_allctrs_info[ERTS_ALC_A_MAX+1]);

extern Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_alloc_instances);

typedef struct {
    int enabled;
    int dd;
    int aix;
    int size;
    Allctr_t **allctr;
} ErtsAllocatorThrSpec_t;

extern ErtsAllocatorThrSpec_t erts_allctr_thr_spec[ERTS_ALC_A_MAX+1];

void erts_alloc_register_scheduler(void *vesdp);
void erts_alloc_register_delayed_dealloc_handler_thread(ErtsThrAllocData *tadp,
							int ix);
void erts_alloc_handle_delayed_dealloc(ErtsThrAllocData *thr_alloc_data,
				       int *need_thr_progress,
				       ErtsThrPrgrVal *thr_prgr_p,
				       int *more_work);
erts_aint32_t erts_alloc_fix_alloc_shrink(int ix, erts_aint32_t flgs);

__decl_noreturn void erts_alloc_enomem(ErtsAlcType_t,Uint)		
     __noreturn;
__decl_noreturn void erts_alloc_n_enomem(ErtsAlcType_t,Uint)		
     __noreturn;
__decl_noreturn void erts_realloc_enomem(ErtsAlcType_t,void*,Uint)	
     __noreturn;
__decl_noreturn void erts_realloc_n_enomem(ErtsAlcType_t,void*,Uint)	
     __noreturn;
__decl_noreturn void erts_alc_fatal_error(int,int,ErtsAlcType_t,...)	
     __noreturn;

Eterm erts_alloc_set_dyn_param(struct process*, Eterm);

#undef ERTS_HAVE_IS_IN_LITERAL_RANGE
#if defined(ARCH_32) || defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
#  define ERTS_HAVE_IS_IN_LITERAL_RANGE
#endif


/*
 * erts_alloc[_fnf](), erts_realloc[_fnf](), erts_free() works as
 * malloc(), realloc(), and free() with the following exceptions:
 *
 * * They take an extra type argument as first argument which is
 *   the memory type to operate on. Memory types are generated
 *   (as ERTS_ALC_T_[SOMETHING] defines) from the erl_alloc.types
 *   configuration file.
 * * The erts_alloc() and erts_realloc() functions terminate the
 *   emulator if memory cannot be obtained. The _fnf (Failure Not
 *   Fatal) suffixed versions return NULL if memory cannot be
 *   obtained.
 * * They may be static functions so function pointers to "the same"
 *   function may differ.
 *
 * IMPORTANT: Memory allocated or reallocated as type X, can only
 *            be reallocated or deallocated as type X.
 */

#if !ERTS_ALC_DO_INLINE

void erts_free(ErtsAlcType_t type, void *ptr);
void *erts_alloc(ErtsAlcType_t type, Uint size) ERTS_ATTR_MALLOC_USD(2,erts_free,2);
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size) ERTS_ATTR_ALLOC_SIZE(3);
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size) ERTS_ATTR_MALLOC_USD(2,erts_free,2);
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size) ERTS_ATTR_ALLOC_SIZE(3);
int erts_is_allctr_wrapper_prelocked(void);
#ifdef ERTS_HAVE_IS_IN_LITERAL_RANGE
int erts_is_in_literal_range(void* ptr);
#endif
ErtsThrAllocData *erts_get_thr_alloc_data(void);
int erts_get_thr_alloc_ix(void);

#endif /* #if !ERTS_ALC_DO_INLINE */

void *erts_alloc_permanent_cache_aligned(ErtsAlcType_t type, Uint size) ERTS_ATTR_MALLOC_US(2);

#ifndef ERTS_CACHE_LINE_SIZE
/* Assumed cache line size */
#  define ERTS_CACHE_LINE_SIZE ((UWord) ASSUMED_CACHE_LINE_SIZE)
#  define ERTS_CACHE_LINE_MASK (ERTS_CACHE_LINE_SIZE - 1)
#endif

#if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__)

ERTS_ALC_INLINE
void *erts_alloc(ErtsAlcType_t type, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
            type,
            erts_allctrs[ERTS_ALC_T2A(type)].extra,
            size);
    if (!res)
	erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	type,
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
    if (!res)
	erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
void erts_free(ErtsAlcType_t type, void *ptr)
{
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    (*erts_allctrs[ERTS_ALC_T2A(type)].free)(
	type,
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr);
    ERTS_MSACC_POP_STATE_X();
}


ERTS_ALC_INLINE
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
	type,
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}


ERTS_ALC_INLINE
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	type,
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
ErtsThrAllocData *erts_get_thr_alloc_data(void)
{
    return (ErtsThrAllocData *) erts_tsd_get(erts_thr_alloc_data_key);
}

ERTS_ALC_INLINE
int erts_get_thr_alloc_ix(void)
{
    ErtsThrAllocData *tadp = (ErtsThrAllocData *) erts_tsd_get(erts_thr_alloc_data_key);
    if (!tadp)
        return 0;
    return tadp->alc_ix;
}

#ifdef ERTS_HAVE_IS_IN_LITERAL_RANGE

ERTS_ALC_FORCE_INLINE
int erts_is_in_literal_range(void* ptr)
{
#if defined(ARCH_32)
    Uint ix = (UWord)ptr >> ERTS_MMAP_SUPERALIGNED_BITS;

    return erts_literal_vspace_map[ix / ERTS_VSPACE_WORD_BITS]
                  & ((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));

#elif defined(ARCH_64)
    extern char* erts_literals_start;
    extern UWord erts_literals_size;
    return ErtsInArea(ptr, erts_literals_start, erts_literals_size);
#else
# error No ARCH_xx
#endif
}

#endif /* ERTS_HAVE_IS_IN_LITERAL_RANGE */

#endif /* #if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__) */

typedef void (*erts_alloc_verify_func_t)(Allctr_t *);

erts_alloc_verify_func_t
erts_alloc_get_verify_unused_temp_alloc(Allctr_t **allctr);

#define ERTS_ALC_DATA_ALIGN_SIZE(SZ) \
  (((((SZ) - 1) / 8) + 1) * 8)

#if defined(ARCH_64)
#define ERTS_ALC_WORD_ALIGN_SIZE(SZ) \
    ERTS_ALC_DATA_ALIGN_SIZE((SZ))
#elif defined(ARCH_32)
#define ERTS_ALC_WORD_ALIGN_SIZE(SZ) \
    (((((SZ) - 1) / 4) + 1) * 4)
#else
#error "Not supported word size"
#endif

#define ERTS_ALC_CACHE_LINE_ALIGN_SIZE(SZ) \
  (((((SZ) - 1) / ERTS_CACHE_LINE_SIZE) + 1) * ERTS_CACHE_LINE_SIZE)

#if !defined(VALGRIND) && !defined(ADDRESS_SANITIZER)

#define ERTS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
    ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, (void) 0, (void) 0, (void) 0)


#define ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, ILCK, LCK, ULCK)	\
ERTS_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ, ILCK, LCK, ULCK)		\
static void								\
init_##NAME##_alloc(void)						\
{									\
    init_##NAME##_pre_alloc();						\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}

#define ERTS_SCHED_PREF_PALLOC_IMPL(NAME, TYPE, PASZ)			\
  ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)

#define ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)				\
ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ)

#define ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT)	\
ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)					\
static void								\
init_##NAME##_alloc(void)						\
{									\
    init_##NAME##_pre_alloc();						\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}

#define ERTS_THR_PREF_AUX(NAME, TYPE, PASZ)				\
ERTS_THR_PREF_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ)

#define ERTS_THR_PREF_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT)	        \
ERTS_THR_PREF_AUX(NAME, TYPE, PASZ)					\
static void								\
init_##NAME##_alloc(int nthreads)					\
{									\
    init_##NAME##_pre_alloc(nthreads);			                \
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}


#ifdef DEBUG
#define ERTS_PRE_ALLOC_SIZE(SZ) ((SZ) < 1000 ? (SZ)/10 + 10 : 100)
#define ERTS_PRE_ALLOC_CLOBBER(P, T) sys_memset((void *) (P), 0xfd, sizeof(T))
#else
#define ERTS_PRE_ALLOC_SIZE(SZ) ((SZ) > 1 ? (SZ) : 1)
#define ERTS_PRE_ALLOC_CLOBBER(P, T)
#endif

#define ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, ILCK, LCK, ULCK)		\
union erts_qa_##NAME##__ {						\
    TYPE type;								\
    union erts_qa_##NAME##__ *next;					\
};									\
static union erts_qa_##NAME##__						\
    qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))];			\
static union erts_qa_##NAME##__ *qa_freelist_##NAME;			\
static void								\
init_##NAME##_alloc(void)						\
{									\
    int i;								\
    qa_freelist_##NAME = &qa_prealcd_##NAME[0];				\
    for (i = 1; i < ERTS_PRE_ALLOC_SIZE((PASZ)); i++) {			\
	ERTS_PRE_ALLOC_CLOBBER(&qa_prealcd_##NAME[i-1],			\
			       union erts_qa_##NAME##__);		\
	qa_prealcd_##NAME[i-1].next = &qa_prealcd_##NAME[i];		\
    }									\
    ERTS_PRE_ALLOC_CLOBBER(&qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1],\
			   union erts_qa_##NAME##__);			\
    qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1].next = NULL;	\
    ILCK;								\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res;								\
    LCK;								\
    if (!qa_freelist_##NAME)						\
	res = NULL;							\
    else {								\
	res = &qa_freelist_##NAME->type;				\
	qa_freelist_##NAME = qa_freelist_##NAME->next;			\
    }									\
    ULCK;								\
    return res;								\
}									\
static ERTS_INLINE int							\
NAME##_free(TYPE *p)							\
{									\
    union erts_qa_##NAME##__ * up;					\
    up = ((union erts_qa_##NAME##__ *)					\
	  (((char *) p)							\
	   - ((char *) &((union erts_qa_##NAME##__ *) 0)->type)));	\
    if (up > &qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1]		\
	|| up < &qa_prealcd_##NAME[0])					\
	return 0;							\
    else {								\
	LCK;								\
	ERTS_PRE_ALLOC_CLOBBER(up, union erts_qa_##NAME##__);		\
	up->next = qa_freelist_##NAME;					\
	qa_freelist_##NAME = up;					\
	ULCK;								\
	return 1;							\
    }									\
}

#include "erl_sched_spec_pre_alloc.h"

#define ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)		\
union erts_sspa_##NAME##__ {						\
    erts_sspa_blk_t next;						\
    TYPE type;								\
};									\
									\
static erts_sspa_data_t *sspa_data_##NAME##__;				\
									\
static void								\
init_##NAME##_alloc(void)						\
{									\
    sspa_data_##NAME##__ =						\
	erts_sspa_create(sizeof(union erts_sspa_##NAME##__),		\
			 ERTS_PRE_ALLOC_SIZE((PASZ)), 			\
                         0, NULL);                                      \
}									\
									\
static TYPE *								\
NAME##_alloc(void)							\
{									\
    ErtsSchedulerData *esdp = erts_get_scheduler_data();		\
    if (!esdp || ERTS_SCHEDULER_IS_DIRTY(esdp))				\
	return NULL;							\
    return (TYPE *) erts_sspa_alloc(sspa_data_##NAME##__,		\
				    (int) esdp->no - 1);		\
}									\
									\
static int								\
NAME##_free(TYPE *p)							\
{									\
    ErtsSchedulerData *esdp = erts_get_scheduler_data();		\
    return erts_sspa_free(sspa_data_##NAME##__,				\
			  esdp ? (int) esdp->no - 1 : -1,		\
			  (char *) p);					\
}


#define ERTS_THR_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)		        \
union erts_sspa_##NAME##__ {						\
    erts_sspa_blk_t next;						\
    TYPE type;								\
};									\
									\
static erts_sspa_data_t *sspa_data_##NAME##__;				\
									\
static void								\
init_##NAME##_alloc(int nthreads)					\
{									\
    sspa_data_##NAME##__ =						\
	erts_sspa_create(sizeof(union erts_sspa_##NAME##__),		\
			 ERTS_PRE_ALLOC_SIZE((PASZ)),			\
                         nthreads,                                      \
                         #NAME);                                        \
}									\
                                                                        \
void								        \
erts_##NAME##_alloc_init_thread(void)				        \
{									\
    int id = erts_atomic_inc_read_nob(&sspa_data_##NAME##__->id_generator);\
    if (id > sspa_data_##NAME##__->nthreads) {                          \
        erts_exit(ERTS_ABORT_EXIT,                                      \
                  "%s:%d:%s(): Too many threads for '" #NAME "'\n",     \
                  __FILE__, __LINE__, __func__);                        \
    }                                                                   \
    erts_tsd_set(sspa_data_##NAME##__->tsd_key, (void*)(SWord)id);      \
}									\
									\
static TYPE *								\
NAME##_alloc(void)							\
{									\
    int id = (int)(SWord)erts_tsd_get(sspa_data_##NAME##__->tsd_key);   \
    if (id == 0)                                                        \
        return NULL;                                                    \
    return (TYPE *) erts_sspa_alloc(sspa_data_##NAME##__,		\
                                    id-1);		                \
}									\
									\
static int								\
NAME##_free(TYPE *p)							\
{									\
    int id = (int)(SWord)erts_tsd_get(sspa_data_##NAME##__->tsd_key);   \
    return erts_sspa_free(sspa_data_##NAME##__,				\
			  id - 1,		                        \
			  (char *) p);					\
}

#else /* !defined(VALGRIND) && !defined(ADDRESS_SANITIZER) */

/*
 * For VALGRIND and ADDRESS_SANITIZER we short circuit all preallocation
 * with dummy wrappers around malloc and free.
 */

#define ERTS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
    ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, (void) 0, (void) 0, (void) 0)

#define ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, ILCK, LCK, ULCK)	\
static void init_##NAME##_alloc(void)                                   \
{                                                                       \
}                                                                       \
static ERTS_INLINE TYPE* NAME##_alloc(void)			        \
{                                                                       \
    return malloc(sizeof(TYPE));                                        \
}				                                        \
static ERTS_INLINE void NAME##_free(TYPE *p)                            \
{                                                                       \
    free((void *) p);                                                   \
}

#define ERTS_SCHED_PREF_PALLOC_IMPL(NAME, TYPE, PASZ)			\
  ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)

#define ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)				\
ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ)

#define ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT)	\
        ERTS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)

#define ERTS_THR_PREF_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT)	        \
void erts_##NAME##_pre_alloc_init_thread(void)				\
{									\
}                                                                       \
static void init_##NAME##_alloc(int nthreads)				\
{									\
}									\
static ERTS_INLINE TYPE* NAME##_alloc(void)			        \
{									\
    return malloc(sizeof(TYPE));				        \
}									\
static ERTS_INLINE void NAME##_free(TYPE *p)				\
{									\
    free(p);					                        \
}

#define ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)		  \
static void init_##NAME##_alloc(void)                                     \
{                                                                         \
}                                                                         \
static TYPE* NAME##_alloc(void)                                           \
{                                                                         \
    return (TYPE *) malloc(sizeof(TYPE));                                 \
}                                                                         \
static int NAME##_free(TYPE *p)                                           \
{                                                                         \
    free(p);                                                              \
    return 1;                                                             \
}

#endif /* VALGRIND ||  ADDRESS_SANITIZER */

#ifdef DEBUG
#define ERTS_ALC_DBG_BLK_SZ(PTR) (*(((UWord *) (PTR)) - 2))
#endif /* #ifdef DEBUG */

#undef ERTS_ALC_INLINE
#undef ERTS_ALC_ATTRIBUTES

#endif /* #ifndef ERL_ALLOC_H__ */
