/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

#ifndef __PROCESS_H__
#define __PROCESS_H__

#include "sys.h"

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#if (defined(ERL_PROCESS_C__) \
     || defined(ERL_PORT_TASK_C__) \
     || (ERTS_GLB_INLINE_INCL_FUNC_DEF \
	 && defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)))
#define ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif

/* #define ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC */

#if !defined(ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC) && defined(DEBUG)
#  define ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
#endif

typedef struct process Process;

#define ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#include "erl_process_lock.h" /* Only pull out important types... */
#undef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__

#define ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_port.h"
#undef ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_vm.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitor_link.h"
#include "erl_hl_timer.h"
#include "erl_time.h"
#include "erl_atom_table.h"
#include "external.h"
#include "erl_mseg.h"
#include "erl_async.h"
#include "erl_gc.h"
#define ERTS_ONLY_INCLUDE_TRACE_FLAGS
#include "erl_trace.h"
#undef ERTS_ONLY_INCLUDE_TRACE_FLAGS
#define ERTS_ONLY_SCHED_SPEC_ETS_DATA
#include "erl_db.h"
#undef ERTS_ONLY_SCHED_SPEC_ETS_DATA

#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

// Included for ERTS_POLL_USE_SCHEDULER_POLLING
#include "erl_poll.h"

#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT	0
#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT		0

#define ERTS_MAX_NO_OF_SCHEDULERS 1024
#define ERTS_MAX_NO_OF_DIRTY_CPU_SCHEDULERS ERTS_MAX_NO_OF_SCHEDULERS
#define ERTS_MAX_NO_OF_DIRTY_IO_SCHEDULERS ERTS_MAX_NO_OF_SCHEDULERS

#define ERTS_DEFAULT_MAX_PROCESSES (1 << 20)

#define ERTS_HEAP_ALLOC(Type, Size)					\
     erts_alloc((Type), (Size))

#define ERTS_HEAP_REALLOC(Type, Ptr, OldSize, NewSize)			\
     erts_realloc((Type), (Ptr), (NewSize))

#define ERTS_HEAP_FREE(Type, Ptr, Size)					\
     erts_free((Type), (Ptr))

#include "export.h"

struct saved_calls {
   int len;
   int n;
   int cur;
   const Export *ct[1];
};

extern Export exp_send, exp_receive, exp_timeout;
extern int ERTS_WRITE_UNLIKELY(erts_sched_compact_load);
extern int ERTS_WRITE_UNLIKELY(erts_sched_balance_util);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_total_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_cpu_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_io_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_run_queues);
extern int ERTS_WRITE_UNLIKELY(erts_no_aux_work_threads);
extern int erts_sched_thread_suggested_stack_size;
extern int erts_dcpu_sched_thread_suggested_stack_size;
extern int erts_dio_sched_thread_suggested_stack_size;
#define ERTS_SCHED_THREAD_MIN_STACK_SIZE 20	/* Kilo words */
#define ERTS_SCHED_THREAD_MAX_STACK_SIZE 8192	/* Kilo words */

#include "erl_bits.h"

/* process priorities */
#define PRIORITY_MAX          0
#define PRIORITY_HIGH         1
#define PRIORITY_NORMAL       2
#define PRIORITY_LOW          3
#define ERTS_NO_PROC_PRIO_LEVELS      4
#define ERTS_NO_PROC_PRIO_QUEUES      3

#define ERTS_PORT_PRIO_LEVEL ERTS_NO_PROC_PRIO_LEVELS
#define ERTS_NO_PRIO_LEVELS (ERTS_NO_PROC_PRIO_LEVELS + 1)

#define ERTS_RUNQ_FLGS_PROCS_QMASK \
  ((((Uint32) 1) << ERTS_NO_PROC_PRIO_LEVELS) - 1)

#define ERTS_RUNQ_FLGS_QMASK \
  ((((Uint32) 1) << ERTS_NO_PRIO_LEVELS) - 1)

#define ERTS_RUNQ_FLGS_EMIGRATE_SHFT \
  ERTS_NO_PRIO_LEVELS
#define ERTS_RUNQ_FLGS_IMMIGRATE_SHFT \
  (ERTS_RUNQ_FLGS_EMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EVACUATE_SHFT \
  (ERTS_RUNQ_FLGS_IMMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_EMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_IMMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_EVACUATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_EVACUATE_SHFT)

#define ERTS_RUNQ_FLG_BASE2 \
  (ERTS_RUNQ_FLGS_EVACUATE_SHFT + ERTS_NO_PRIO_LEVELS)

#define ERTS_RUNQ_FLG_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 0))
#define ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 1))
#define ERTS_RUNQ_FLG_SUSPENDED \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 2))
#define ERTS_RUNQ_FLG_CHK_CPU_BIND \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 3))
#define ERTS_RUNQ_FLG_INACTIVE \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 4))
#define ERTS_RUNQ_FLG_NONEMPTY \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 5))
#define ERTS_RUNQ_FLG_EXEC \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 6))
#define ERTS_RUNQ_FLG_MSB_EXEC \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 7))
#define ERTS_RUNQ_FLG_MISC_OP \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 8))
#define ERTS_RUNQ_FLG_HALTING \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 9))

#define ERTS_RUNQ_FLG_MAX (ERTS_RUNQ_FLG_BASE2 + 12)

#define ERTS_RUNQ_FLGS_MIGRATION_QMASKS	\
  (ERTS_RUNQ_FLGS_EMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_IMMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_EVACUATE_QMASK)

#define ERTS_RUNQ_FLGS_MIGRATION_INFO \
  (ERTS_RUNQ_FLG_INACTIVE \
   | ERTS_RUNQ_FLG_OUT_OF_WORK \
   | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK)

#define ERTS_RUNQ_FLG_EMIGRATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_EMIGRATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_EMIGRATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_EMIGRATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_EMIGRATE((PRIO)))

#define ERTS_RUNQ_FLG_IMMIGRATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_IMMIGRATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))

#define ERTS_RUNQ_FLG_EVACUATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_EVACUATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_EVACUATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_EVACUATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_EVACUATE((PRIO)))

#define ERTS_RUNQ_FLGS_INIT(RQ, INIT)					\
    erts_atomic32_init_nob(&(RQ)->flags, (erts_aint32_t) (INIT))
#define ERTS_RUNQ_FLGS_SET(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_bor_relb(&(RQ)->flags,		\
					      (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_SET_NOB(RQ, FLGS)				\
    ((Uint32) erts_atomic32_read_bor_nob(&(RQ)->flags,		\
					     (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_BSET(RQ, MSK, FLGS)				\
    ((Uint32) erts_atomic32_read_bset_relb(&(RQ)->flags,		\
					       (erts_aint32_t) (MSK),	\
					       (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_UNSET(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_band_relb(&(RQ)->flags,		\
					       (erts_aint32_t) ~(FLGS)))
#define ERTS_RUNQ_FLGS_UNSET_NOB(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_band_nob(&(RQ)->flags,		\
					      (erts_aint32_t) ~(FLGS)))
#define ERTS_RUNQ_FLGS_GET(RQ)						\
    ((Uint32) erts_atomic32_read_acqb(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_GET_NOB(RQ)					\
    ((Uint32) erts_atomic32_read_nob(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_GET_MB(RQ)					\
    ((Uint32) erts_atomic32_read_mb(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_READ_BSET(RQ, MSK, FLGS)		  		\
    ((Uint32) erts_atomic32_read_bset_relb(&(RQ)->flags, 		\
					       (erts_aint32_t) (MSK),	\
					       (erts_aint32_t) (FLGS)))

#define ERTS_RUNQ_POINTER_MASK  (~((erts_aint_t) 3))
#define ERTS_RUNQ_BOUND_FLAG    ((erts_aint_t) 1)

typedef enum {
    ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_DONE,
    ERTS_SCHDLR_SSPND_YIELD_RESTART,
    ERTS_SCHDLR_SSPND_YIELD_DONE,
    ERTS_SCHDLR_SSPND_EINVAL
} ErtsSchedSuspendResult;

typedef enum {
    ERTS_MIGRATE_SUCCESS,
    ERTS_MIGRATE_FAILED_NOT_IN_RUNQ,
    ERTS_MIGRATE_FAILED_RUNQ_CHANGED,
    ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED
} ErtsMigrateResult;

#define ERTS_SSI_FLG_SLEEPING		(((erts_aint32_t) 1) << 0)
#define ERTS_SSI_FLG_POLL_SLEEPING 	(((erts_aint32_t) 1) << 1)
#define ERTS_SSI_FLG_TSE_SLEEPING 	(((erts_aint32_t) 1) << 2)
#define ERTS_SSI_FLG_WAITING		(((erts_aint32_t) 1) << 3)
#define ERTS_SSI_FLG_SUSPENDED	 	(((erts_aint32_t) 1) << 4)
#define ERTS_SSI_FLG_MSB_EXEC	 	(((erts_aint32_t) 1) << 5)

#define ERTS_SSI_FLGS_MAX                                       6

#define ERTS_SSI_FLGS_SLEEP_TYPE			\
 (ERTS_SSI_FLG_TSE_SLEEPING|ERTS_SSI_FLG_POLL_SLEEPING)

#define ERTS_SSI_FLGS_SLEEP				\
 (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLGS_SLEEP_TYPE)

#define ERTS_SSI_FLGS_ALL				\
 (ERTS_SSI_FLGS_SLEEP					\
  | ERTS_SSI_FLG_WAITING				\
  | ERTS_SSI_FLG_SUSPENDED                              \
  | ERTS_SSI_FLG_MSB_EXEC)

/*
 * Keep ERTS_SSI_AUX_WORK flags ordered in expected frequency
 * order relative each other. Most frequent at lowest at lowest
 * index.
 *
 * ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX *need* to be
 * highest index...
 *
 * Remember to update description in erts_pre_init_process()
 * and etp-commands when adding new flags...
 */

typedef enum {
    ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP_IX,
    ERTS_SSI_AUX_WORK_DD_IX,
    ERTS_SSI_AUX_WORK_DD_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC_IX,
    ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM_IX,
    ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP_IX,
    ERTS_SSI_AUX_WORK_CNCLD_TMRS_IX,
    ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_ASYNC_READY_IX,
    ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN_IX,
    ERTS_SSI_AUX_WORK_MISC_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_MISC_IX,
    ERTS_SSI_AUX_WORK_SET_TMO_IX,
    ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK_IX,
    ERTS_SSI_AUX_WORK_YIELD_IX,
    ERTS_SSI_AUX_WORK_REAP_PORTS_IX,
    ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX, /* SHOULD be last flag index */

    ERTS_SSI_AUX_WORK_NO_FLAGS /* Not a flag index... */
} ErtsSsiAuxWorkFlagIndex;

#define ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP_IX)
#define ERTS_SSI_AUX_WORK_DD \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DD_IX)
#define ERTS_SSI_AUX_WORK_DD_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DD_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC_IX)
#define ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM_IX)
#define ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP_IX)
#define ERTS_SSI_AUX_WORK_CNCLD_TMRS \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_CNCLD_TMRS_IX)
#define ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_ASYNC_READY \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_ASYNC_READY_IX)
#define ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN_IX)
#define ERTS_SSI_AUX_WORK_MISC_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MISC_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_MISC \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MISC_IX)
#define ERTS_SSI_AUX_WORK_SET_TMO \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_SET_TMO_IX)
#define ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK_IX)
#define ERTS_SSI_AUX_WORK_YIELD \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_YIELD_IX)
#define ERTS_SSI_AUX_WORK_REAP_PORTS \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_REAP_PORTS_IX)
#define ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX)

typedef struct ErtsSchedulerSleepInfo_ ErtsSchedulerSleepInfo;

typedef struct {
    erts_mtx_t lock;
    ErtsSchedulerSleepInfo *list; /* circular lifo list; points to last out */
} ErtsSchedulerSleepList;

struct ErtsSchedulerSleepInfo_ {
    struct ErtsSchedulerData_ *esdp;
    ErtsSchedulerSleepInfo *next;
    ErtsSchedulerSleepInfo *prev;
    erts_atomic32_t flags;
    erts_tse_t *event;
    struct erts_poll_thread *psi;
    erts_atomic32_t aux_work;
};

/* times to reschedule low prio process before running */
#define RESCHEDULE_LOW        8

#define ERTS_MAX_MISC_OPS 5

#define ERTS_FULL_REDS_HISTORY_AVG_SHFT 3
#define ERTS_FULL_REDS_HISTORY_SIZE \
   ((1 << ERTS_FULL_REDS_HISTORY_AVG_SHFT) - 1)

typedef struct ErtsProcList_ ErtsProcList;
struct ErtsProcList_ {
    union {
        Eterm pid;
        Process *p;
    } u;
    Uint64 started_interval;
    ErtsProcList* next;
    ErtsProcList* prev;
};

typedef struct ErtsMiscOpList_ ErtsMiscOpList;
struct ErtsMiscOpList_ {
    ErtsMiscOpList *next;
    void (*func)(void *arg);
    void *arg;
};

typedef struct {
    Process* first;
    Process* last;
} ErtsRunPrioQueue;

typedef enum {
    ERTS_SCHED_NORMAL = 0,
    ERTS_SCHED_DIRTY_CPU = 1,
    ERTS_SCHED_DIRTY_IO = 2,

    ERTS_SCHED_TYPE_FIRST = ERTS_SCHED_NORMAL,
    ERTS_SCHED_TYPE_LAST = ERTS_SCHED_DIRTY_IO
} ErtsSchedType;

typedef struct ErtsSchedulerData_ ErtsSchedulerData;

typedef struct ErtsRunQueue_ ErtsRunQueue;

typedef struct {
    erts_atomic32_t len;
    erts_aint32_t max_len;
    int reds;
} ErtsRunQueueInfo;


#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
#  undef ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT
#  define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT 1
#endif


#undef ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT

typedef erts_atomic64_t ErtsAtomicSchedTime;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
typedef struct {
    ErtsAtomicSchedTime last;
    struct {
	Uint64 short_interval;
	Uint64 long_interval;
    } worktime;
    int is_working;
} ErtsRunQueueSchedUtil;
#endif

typedef struct {
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    int sched_util;
#endif
    Uint32 flags;
    ErtsRunQueue *misc_evac_runq;
    struct {
	struct {
	    int here;
	    int other;
	} limit;
	ErtsRunQueue *runq;
	Uint32 flags;
    } prio[ERTS_NO_PRIO_LEVELS];
} ErtsMigrationPath;

typedef struct ErtsMigrationPaths_ ErtsMigrationPaths;

struct ErtsMigrationPaths_ {
    void *block;
    ErtsMigrationPaths *next;
    ErtsThrPrgrVal thr_prgr;
    ErtsMigrationPath mpath[1];
};


struct ErtsRunQueue_ {
    erts_mtx_t mtx;
    erts_atomic32_t flags;

    struct {
	ErtsRunQueueInfo prio_info[ERTS_NO_PROC_PRIO_LEVELS];
	/* We use the same prio queue for low and
	   normal prio processes */
	ErtsRunPrioQueue prio[ERTS_NO_PROC_PRIO_LEVELS-1];
      Uint context_switches;
      Uint reductions;
    } procs;

    erts_aint32_t max_len;
    erts_atomic32_t len;

    /* The fields above are the ones that are commonly accessed by other cores during task stealing
       They are grouped together to improve cache locality. */

    int ix;
    ErtsSchedulerSleepList sleepers;

    ErtsSchedulerData *scheduler;
    int waiting;
    int woken;
    int check_balance_reds;
    int full_reds_history_sum;
    int full_reds_history[ERTS_FULL_REDS_HISTORY_SIZE];
    int out_of_work_count;
    int wakeup_other;
    int wakeup_other_reds;

    struct {
	ErtsMiscOpList *start;
	ErtsMiscOpList *end;
	erts_atomic_t evac_runq;
    } misc;

    struct {
	ErtsRunQueueInfo info;
	Port *start;
	Port *end;
    } ports;
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    ErtsRunQueueSchedUtil sched_util;
#endif
};

extern long erts_runq_supervision_interval;

typedef union {
    ErtsRunQueue runq;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsRunQueue))];
} ErtsAlignedRunQueue;

extern ErtsAlignedRunQueue * ERTS_WRITE_UNLIKELY(erts_aligned_run_queues);

#define ERTS_PROC_REDUCTIONS_EXECUTED(SD, RQ, PRIO, REDS, AREDS)\
do {								\
    (RQ)->procs.reductions += (AREDS);				\
    (RQ)->procs.prio_info[(PRIO)].reds += (REDS);		\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (AREDS);				\
    (SD)->check_time_reds += (AREDS);				\
} while (0)

#define ERTS_PORT_REDUCTIONS_EXECUTED(SD, RQ, REDS)		\
do {								\
    (RQ)->ports.info.reds += (REDS);				\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (REDS);				\
    (SD)->check_time_reds += (REDS);				\
} while (0)

typedef struct {
    union {
        erts_atomic32_t mod; /* on dirty schedulers */
        int need; /* "+sbu true" or scheduler_wall_time enabled */
    } u;
    int enabled;
    Uint64 start;
    struct {
	Uint64 total;
	Uint64 start;
    } working;
} ErtsSchedWallTime;

typedef struct {
    int sched;
    erts_aint32_t aux_work;
} ErtsDelayedAuxWorkWakeupJob;

typedef struct ErtsAuxWorkData_ {
    int aux_work_tid;
    ErtsThrAllocData alloc_data;
    ErtsSchedulerData *esdp;
    ErtsSchedulerSleepInfo *ssi;
    ErtsThrPrgrVal current_thr_prgr;
    ErtsThrPrgrVal latest_wakeup;
    struct {
	int ix;
	ErtsThrPrgrVal thr_prgr;
    } misc;
    struct {
	ErtsThrPrgrVal thr_prgr;
    } dd;
    struct {
	ErtsThrPrgrVal thr_prgr;
    } cncld_tmrs;
    struct {
	ErtsThrPrgrVal thr_prgr;
	UWord size;
	ErtsThrPrgrLaterOp *first;
	ErtsThrPrgrLaterOp *last;
        Uint list_len;
    } later_op;
    struct {
	int need_thr_prgr;
	ErtsThrPrgrVal thr_prgr;
	void *queue;
    } async_ready;
    struct {
	Uint64 next;
	int *sched2jix;
	int jix;
	ErtsDelayedAuxWorkWakeupJob *job;
    } delayed_wakeup;
    struct {
        ErtsAlcuBlockscanYieldData alcu_blockscan;
        ErtsEtsAllYieldData ets_all;
        /* Other yielding operations... */
    } yield;
    struct {
	struct {
	    erts_aint32_t flags;
	    void (*callback)(void *);
	    void *arg;
	} wait_completed;
    } debug;
#ifdef ERTS_ENABLE_LOCK_CHECK
    void* lc_aux_arg;
#endif
} ErtsAuxWorkData;

#define ERTS_SCHED_AUX_YIELD_DATA(ESDP, NAME) \
    (&(ESDP)->aux_work_data.yield.NAME)
void erts_more_yield_aux_work(ErtsAuxWorkData *);
ErtsAuxWorkData *erts_get_aux_work_data(void);

typedef enum {
    ERTS_DIRTY_CPU_SCHEDULER,
    ERTS_DIRTY_IO_SCHEDULER
} ErtsDirtySchedulerType;

typedef struct ErtsSchedulerRegisters_ {
    union {
        struct aux_regs__ {
#ifdef BEAMASM
            /* On normal schedulers we allocate this structure on the "C stack"
             * to allow stack switching without needing to read memory or
             * occupy a register; we simply compute the stack address from the
             * register pointer.
             *
             * This is placed first because the stack grows downwards.
             *
             * In special builds that don't execute native code on the Erlang
             * stack (e.g. `valgrind`), this will instead hold the original
             * thread stack pointer when executing code that requires a certain
             * stack alignment. */
            UWord runtime_stack[1];

#ifdef ERTS_MSACC_EXTENDED_STATES
            ErtsMsAcc *erts_msacc_cache;
#endif

            /* Temporary memory used by beamasm for allocations within
             * instructions */
            UWord TMP_MEM[5];
#endif

            /* erl_bits.c state */
            struct erl_bits_state erl_bits_state;
        } d;
        char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(struct aux_regs__))];
    } aux_regs;

    union {
        Eterm d[ERTS_X_REGS_ALLOCATED];
        char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(Eterm[ERTS_X_REGS_ALLOCATED]))];
    } x_reg_array;

    union {
        FloatDef d[MAX_REG];
        char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(FloatDef[MAX_REG]))];
    } f_reg_array;

#ifdef BEAMASM
    /* Seldom-used scheduler-specific data. */
    ErtsCodePtr start_time_i;
    UWord start_time;

#if (!defined(NATIVE_ERLANG_STACK) || defined(__aarch64__)) && defined(JIT_HARD_DEBUG)
    /* Holds the initial thread stack pointer. Used to ensure that everything
     * that is pushed to the stack is also popped. */
    UWord *initial_sp;
#elif defined(NATIVE_ERLANG_STACK) && defined(DEBUG) && !defined(__aarch64__)
    /* Raw pointers to the start and end of the stack. Used to test bounds
     * without clobbering any registers. */
    UWord *runtime_stack_start;
    UWord *runtime_stack_end;
#endif

#endif
} ErtsSchedulerRegisters;

struct ErtsSchedulerData_ {
    ErtsSchedulerRegisters *registers;

    ErtsTimerWheel *timer_wheel;
    ErtsNextTimeoutRef next_tmo_ref;
    ErtsHLTimerService *timer_service;
    ethr_tid tid;		/* Thread id */
    void *match_pseudo_process; /* erl_db_util.c:db_prog_match() */
    Process *free_process;
    ErtsThrPrgrData thr_progress_data;
    ErtsSchedulerSleepInfo *ssi;
    Process *current_process;
    ErtsSchedType type;
    Uint no;			/* Scheduler number for normal schedulers */
    Uint dirty_no;  /* Scheduler number for dirty schedulers */
    int flxctr_slot_no; /* slot nr when a flxctr is used */
    struct enif_environment_t *current_nif;
    Process *dirty_shadow_process;
    Port *current_port;
    ErtsRunQueue *run_queue;
    int virtual_reds;
    int cpu_id;			/* >= 0 when bound */
    ErtsAuxWorkData aux_work_data;
    ErtsAtomCacheMap atom_cache_map;

    ErtsMonotonicTime last_monotonic_time;
#ifdef ERTS_CHECK_MONOTONIC_TIME
    ErtsMonotonicTime last_os_monotonic_time;
#endif
    int check_time_reds;

    Uint32 thr_id;
    Uint64 unique;
    Uint64 ref;

    struct {
	Uint64 out;
	Uint64 in;
    } io;
#if ERTS_POLL_USE_SCHEDULER_POLLING
    ErtsSysFdType nif_select_fds[5]; /* Used by check io */
#endif
    struct {
        ErtsSignal* sig;
        Eterm to;
#ifdef DEBUG
	Process* dbg_from;
#endif
    } pending_signal;

    Uint64 reductions;
    Uint64 rand_state;
    ErtsSchedWallTime sched_wall_time;
    ErtsGCInfo gc_info;
    ErtsPortTaskHandle nosuspend_port_task_handle;
    union {
        ErtsEtsTables ets_tables;
        erts_atomic32_t dirty_nif_halt_info;
    } u;
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_alloc_verify_func_t verify_unused_temp_alloc;
    Allctr_t *verify_unused_temp_alloc_data;
#endif
};

typedef union {
    ErtsSchedulerData esd;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerData))];
} ErtsAlignedSchedulerData;

extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_scheduler_data);
extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_cpu_scheduler_data);
extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_io_scheduler_data);


#if defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_runq_is_locked(ErtsRunQueue *);
#endif

void
erts_debug_later_op_foreach(void (*callback)(void*),
                            void (*func)(void *, ErtsThrPrgrVal, void *),
                            void *arg);
void
erts_debug_free_process_foreach(void (*func)(Process *, void *), void *arg);
void
erts_debug_proc_monitor_link_foreach(Process *proc,
                                     int (*monitor_func)(ErtsMonitor *, void *, Sint ),
                                     int (*link_func)(ErtsLink *, void *, Sint ),
                                     void *arg);

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

void erts_empty_runq(ErtsRunQueue *rq);
void erts_non_empty_runq(ErtsRunQueue *rq);


/*
 * Run queue locked during modifications. We use atomic ops since
 * other threads peek at values without run queue lock.
 */

ERTS_GLB_INLINE void erts_inc_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio);
ERTS_GLB_INLINE void erts_dec_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio);
ERTS_GLB_INLINE void erts_reset_max_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi);
ERTS_GLB_INLINE void erts_add_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio, unsigned n);
ERTS_GLB_INLINE void erts_sub_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio, unsigned n);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_add_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio, unsigned n)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rq->len);

    if (len == 0)
	erts_non_empty_runq(rq);
    len += n;
    if (rq->max_len < len)
	rq->max_len = len;
    ASSERT(len > 0);
    erts_atomic32_set_nob(&rq->len, len);

    len = erts_atomic32_read_dirty(&rqi->len);
    ASSERT(len >= 0);
    if (len == 0) {
	ASSERT((erts_atomic32_read_nob(&rq->flags)
		& ((erts_aint32_t) (1 << prio))) == 0);
	erts_atomic32_read_bor_nob(&rq->flags,
				       (erts_aint32_t) (1 << prio));
    }
    len += n;
    if (rqi->max_len < len)
	rqi->max_len = len;

    erts_atomic32_set_relb(&rqi->len, len);
}

ERTS_GLB_INLINE void
erts_inc_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio)
{
    erts_add_runq_len(rq, rqi, prio, 1);
}

ERTS_GLB_INLINE void
erts_sub_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio, unsigned n)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rq->len);
    len -= n;
    ASSERT(len >= 0);
    erts_atomic32_set_nob(&rq->len, len);

    len = erts_atomic32_read_dirty(&rqi->len);
    len -= n;
    ASSERT(len >= 0);
    if (len == 0) {
	ASSERT((erts_atomic32_read_nob(&rq->flags)
		& ((erts_aint32_t) (1 << prio))));
	erts_atomic32_read_band_nob(&rq->flags,
					~((erts_aint32_t) (1 << prio)));
    }
    erts_atomic32_set_relb(&rqi->len, len);
}

ERTS_GLB_INLINE void
erts_dec_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio)
{
    erts_sub_runq_len(rq, rqi, prio, 1);
}

ERTS_GLB_INLINE void
erts_reset_max_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rqi->len);
    ASSERT(rqi->max_len >= len);
    rqi->max_len = len;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#define RUNQ_READ_LEN(X) erts_atomic32_read_nob((X))

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

/*
 * Process Specific Data.
 *
 * NOTE: Only use PSD for very rarely used data.
 */

#define ERTS_PSD_ERROR_HANDLER			0
#define ERTS_PSD_SAVED_CALLS_BUF		1
#define ERTS_PSD_SCHED_ID			2
#define ERTS_PSD_CALL_TIME_BP			3
#define ERTS_PSD_DELAYED_GC_TASK_QS		4
#define ERTS_PSD_NFUNC_TRAP_WRAPPER		5
#define ERTS_PSD_ETS_OWNED_TABLES               6
#define ERTS_PSD_ETS_FIXED_TABLES               7
#define ERTS_PSD_DIST_ENTRY	                8
#define ERTS_PSD_CALL_MEMORY_BP	                9
#define ERTS_PSD_TS_EVENT                       10
#define ERTS_PSD_SYSMON_MSGQ_LEN_LOW            11
#define ERTS_PSD_PRIO_Q_INFO                    12
#define ERTS_PSD_PENDING_SUSPEND                13 /* keep last... */

#define ERTS_PSD_SIZE				14

typedef struct {
    void *data[ERTS_PSD_SIZE];
} ErtsPSD;

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_LC_PSD_ANY_LOCK (~ERTS_PROC_LOCKS_ALL)

#define ERTS_PSD_ERROR_HANDLER_BUF_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_ERROR_HANDLER_BUF_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_SAVED_CALLS_BUF_GET_LOCKS ((ErtsProcLocks) 0)
#define ERTS_PSD_SAVED_CALLS_BUF_SET_LOCKS ((ErtsProcLocks) 0)

#define ERTS_PSD_SCHED_ID_GET_LOCKS ERTS_PROC_LOCK_STATUS
#define ERTS_PSD_SCHED_ID_SET_LOCKS ERTS_PROC_LOCK_STATUS

#define ERTS_PSD_CALL_TIME_BP_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_CALL_TIME_BP_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_CALL_MEMORY_BP_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_CALL_MEMORY_BP_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_DELAYED_GC_TASK_QS_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_DELAYED_GC_TASK_QS_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_NFUNC_TRAP_WRAPPER_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_NFUNC_TRAP_WRAPPER_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_ETS_OWNED_TABLES_GET_LOCKS ERTS_PROC_LOCK_STATUS
#define ERTS_PSD_ETS_OWNED_TABLES_SET_LOCKS ERTS_PROC_LOCK_STATUS

#define ERTS_PSD_ETS_FIXED_TABLES_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_ETS_FIXED_TABLES_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_DIST_ENTRY_GET_LOCKS ERTS_LC_PSD_ANY_LOCK
#define ERTS_PSD_DIST_ENTRY_SET_LOCKS ERTS_PROC_LOCKS_ALL

#define ERTS_PSD_TS_EVENT_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_TS_EVENT_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_PRIO_Q_INFO_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_PRIO_Q_INFO_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_PENDING_SUSPEND_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_PENDING_SUSPEND_SET_LOCKS ERTS_PROC_LOCK_MAIN

typedef struct {
    ErtsProcLocks get_locks;
    ErtsProcLocks set_locks;
} ErtsLcPSDLocks;

extern ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];

#endif

#define ERTS_SCHED_STAT_MODIFY_DISABLE		1
#define ERTS_SCHED_STAT_MODIFY_ENABLE		2
#define ERTS_SCHED_STAT_MODIFY_CLEAR		3

typedef struct {
    erts_spinlock_t lock;
    int enabled;
    struct {
	Eterm name;
	Uint total_executed;
	Uint executed;
	Uint total_migrated;
	Uint migrated;
    } prio[ERTS_NO_PRIO_LEVELS];
} erts_sched_stat_t;

extern erts_sched_stat_t erts_sched_stat;

typedef struct {
    Eterm reason;
    ErlHeapFragment *bp;
} ErtsPendExit;

typedef struct ErtsProcSysTask_ ErtsProcSysTask;
typedef struct ErtsProcSysTaskQs_ ErtsProcSysTaskQs;

/* Defines to ease the change of memory architecture */

#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop

/* The redzone is reserved for Erlang code and runtime functions may not use it
 * on its own, but it's okay for them to run when the redzone is used.
 *
 * Therefore, we set the heap limit to HTOP or the start of the redzone,
 * whichever is higher. */
#  define HEAP_LIMIT(p)                                                        \
    (ASSERT((p)->htop <= (p)->stop),                                           \
     MAX((p)->htop, (p)->stop - S_REDZONE))

#ifdef ERLANG_FRAME_POINTERS
/* The current frame pointer on the Erlang stack. */
#  define FRAME_POINTER(p)  (p)->frame_pointer
#else
/* We define this to a trapping lvalue when frame pointers are unsupported to
 * provoke crashes when used without checking `erts_frame_layout`. The checks
 * will always be optimized out because the variable is hardcoded to
 *  `ERTS_FRAME_LAYOUT_RA`. */
#  define FRAME_POINTER(p)  (((Eterm ** volatile)0xbadf00d)[0])

#  ifndef erts_frame_layout
#    error "erts_frame_layout has not been hardcoded to ERTS_FRAME_LAYOUT_RA"
#  endif
#endif

#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
#  define STACK_START(p)    (p)->hend
#  define STACK_TOP(p)      (p)->stop
#  define STACK_END(p)      (p)->htop
#  define HIGH_WATER(p)     (p)->high_water
#  define OLD_HEND(p)       (p)->old_hend
#  define OLD_HTOP(p)       (p)->old_htop
#  define OLD_HEAP(p)       (p)->old_heap
#  define GEN_GCS(p)        (p)->gen_gcs
#  define MAX_GEN_GCS(p)    (p)->max_gen_gcs
#  define FLAGS(p)          (p)->flags
#  define MBUF(p)           (p)->mbuf
#  define MBUF_SIZE(p)      (p)->mbuf_sz
#  define MSO(p)            (p)->off_heap
#  define MIN_HEAP_SIZE(p)  (p)->min_heap_size

#  define MAX_HEAP_SIZE_GET(p)     ((p)->max_heap_size >> 3)
#  define MAX_HEAP_SIZE_SET(p, sz) ((p)->max_heap_size = ((sz) << 3) |  \
                                    MAX_HEAP_SIZE_FLAGS_GET(p))
#  define MAX_HEAP_SIZE_FLAGS_GET(p)          ((p)->max_heap_size & 0x7)
#  define MAX_HEAP_SIZE_FLAGS_SET(p, flags)   ((p)->max_heap_size = flags | \
                                               ((p)->max_heap_size & ~0x7))
#  define MAX_HEAP_SIZE_KILL 1
#  define MAX_HEAP_SIZE_LOG  2
#  define MAX_HEAP_SIZE_INCLUDE_OH_BINS 4

struct process {
    ErtsPTabElementCommon common; /* *Need* to be first in struct */

    /* Place fields that are frequently used from BEAMASM instructions near the
     * beginning of this struct so that a shorter instruction can be used to
     * access them. */

    /* These are paired to exploit the STP instruction in the ARM JIT. */
    Eterm *htop;                /* Heap top */
    Eterm *stop;                /* Stack top */

#ifdef ERLANG_FRAME_POINTERS
    Eterm *frame_pointer;       /* Frame pointer */
#endif

    /* These are paired to exploit the STP instruction in the ARM JIT. */
    Uint freason;               /* Reason for detected failure. */
    Eterm fvalue;               /* Exit & Throw value (failure reason) */

    Sint32 fcalls;              /* Number of reductions left to execute.
                                 * Only valid for the current process while it
                                 * is executing. */

    Uint32 flags;               /* Trap exit, etc */

    /* End of frequently used fields by BEAMASM code. */

    Uint32 rcount;              /* Suspend count */
    byte schedule_count;        /* Times left to reschedule a low prio process */

    /* Saved x registers. */
    byte arity;                 /* Number of live argument registers (only
                                 * valid when process is *not* running). */
    byte max_arg_reg;           /* Maximum number of argument registers
                                 * available. */
    Eterm* arg_reg;             /* Pointer to argument registers. */
    Eterm def_arg_reg[6];       /* Default array for argument registers. */

    Eterm* heap;                /* Heap start */
    Eterm* hend;                /* Heap end */

    /* If abandoned_heap is not a NULL pointer, it points to the heap
     * that was active when delay_garbage_collection() in erl_gc.c was
     * called. The high water mark that was active at that time is
     * saved in p->hend[0].
     */

    Eterm* abandoned_heap;

    Uint heap_sz;               /* Size of heap in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */
    Uint min_vheap_size;        /* Minimum size of virtual heap (in words). */
    Uint max_heap_size;         /* Maximum size of heap (in words). */

    ErtsCodePtr i;              /* Program counter. */
    Sint catches;               /* Number of catches on stack */
    Sint return_trace_frames;   /* Number of return trace frames on stack */
    Uint reds;                  /* No of reductions for this process  */
    Eterm group_leader;         /* Pid in charge (can be boxed) */
    Eterm ftrace;               /* Latest exception stack trace dump */

    Process *next;              /* Pointer to next process in run queue */

    Sint64 uniq;                 /* Used for process unique integer */
    ErtsSignalPrivQueues sig_qs; /* Signal queues */
    ErtsBifTimers *bif_timers;   /* Bif timers aiming at this process */

    ProcDict *dictionary;        /* Process dictionary, may be NULL */

    Uint seq_trace_clock;
    Uint seq_trace_lastcnt;
    Eterm seq_trace_token;	/* Sequential trace token (tuple size 5 see below) */

    union {
        struct process *real_proc;
        void *terminate;
        ErtsCodeMFA initial;	/* Initial module(0), function(1), arity(2),
                                   often used instead of pointer to funcinfo
                                   instruction. */
    } u;

    const ErtsCodeMFA* current; /* Current Erlang function, part of the
                                 * funcinfo:
                                 *
                                 * module(0), function(1), arity(2)
                                 *
                                 * (module and functions are tagged atoms;
                                 * arity an untagged integer).
                                 */

    /*
     * Information mainly for post-mortem use (erl crash dump).
     */
    Eterm parent;               /* Pid of process that created this process. */

    Uint32 static_flags;        /* Flags that do *not* change */

    /* This is the place, where all fields that differs between memory
     * architectures, have gone to.
     */

    Uint16 gen_gcs;             /* Number of (minor) generational GCs. */
    Uint16 max_gen_gcs;         /* Max minor gen GCs before fullsweep. */
    Eterm *high_water;
    Eterm *old_hend;            /* Heap pointers for generational GC. */
    Eterm *old_htop;
    Eterm *old_heap;
    ErlOffHeap off_heap;	/* Off-heap data updated by copy_struct(). */
    struct erl_off_heap_header* wrt_bins; /* Writable binaries */
    ErlHeapFragment* mbuf;	/* Pointer to heap fragment list */
    ErlHeapFragment* live_hf_end;
    ErtsMessage *msg_frag;	/* Pointer to message fragment list */
    Uint mbuf_sz;		/* Total size of heap fragments and message fragments */
    erts_atomic_t psd;		/* Rarely used process specific data */

    Uint64 bin_vheap_sz;	/* Virtual heap block size for binaries */
    Uint64 bin_old_vheap_sz;	/* Virtual old heap block size for binaries */
    Uint64 bin_old_vheap;	/* Virtual old heap size for binaries */

    ErtsProcSysTaskQs *sys_task_qs;
    ErtsProcSysTask *dirty_sys_tasks;

    erts_atomic32_t state;      /* Process state flags (see ERTS_PSFLG_*) */
    erts_atomic32_t xstate; /* Process extra state flags (see ERTS_PXSFLG_*) */
    Uint sig_inq_contention_counter;
    ErtsSignalInQueue sig_inq;
    erts_atomic_t sig_inq_buffers;
    ErlTraceMessageQueue *trace_msg_q;
    erts_proc_lock_t lock;
    ErtsSchedulerData *scheduler_data;
    erts_atomic_t run_queue;

#ifdef USE_VM_PROBES
    Eterm dt_utag;              /* Place to store the dynamic trace user tag */
    Uint dt_utag_flags;         /* flag field for the dt_utag */
#endif

#ifdef CHECK_FOR_HOLES
    Eterm* last_htop;		/* No need to scan the heap below this point. */
    ErlHeapFragment* last_mbuf;	/* No need to scan beyond this mbuf. */
    ErlHeapFragment* heap_hfrag; /* Heap abandoned, htop now lives in this frag */
#endif

#ifdef DEBUG
    Eterm* last_old_htop;	/*
				 * No need to scan the old heap below this point
				 * when looking for invalid pointers into the new heap or
				 * heap fragments.
				 */
#endif

#ifdef FORCE_HEAP_FRAGS
    Uint space_verified;        /* Avoid HAlloc forcing heap fragments when */ 
    Eterm* space_verified_from; /* we rely on available heap space (TestHeap) */
#endif

#ifdef DEBUG
    Uint debug_reds_in;
#endif
};

extern Eterm erts_init_process_id; /* pid of init process */
extern const Process erts_invalid_process;

#ifdef CHECK_FOR_HOLES
# define INIT_HOLE_CHECK(p)			\
do {						\
  (p)->last_htop = 0;				\
  (p)->last_mbuf = 0;				\
  (p)->heap_hfrag = NULL;			\
} while (0)

# define ERTS_HOLE_CHECK(p) erts_check_for_holes((p))
void erts_check_for_holes(Process* p);
#else
# define INIT_HOLE_CHECK(p)
# define ERTS_HOLE_CHECK(p)
#endif

/*
 * The MBUF_GC_FACTOR decides how easily a process is subject to GC 
 * due to message buffers allocated outside the heap.
 * The larger the factor, the easier the process gets GCed.
 * On a small memory system with lots of processes, this makes a significant 
 * difference, especially since the GCs help fragmentation quite a bit too.
 */
#if defined(SMALL_MEMORY)
#define MBUF_GC_FACTOR 4
#else
#define MBUF_GC_FACTOR 1
#endif

#define SEQ_TRACE_TOKEN(p)  ((p)->seq_trace_token)

#if ERTS_NO_PROC_PRIO_LEVELS > 4
#  error "Need to increase ERTS_PSFLG_PRIO_SHIFT"
#endif

#define ERTS_PSFLGS_PRIO_BITS 2
#define ERTS_PSFLGS_PRIO_MASK \
    ((((erts_aint32_t) 1) << ERTS_PSFLGS_PRIO_BITS) - 1)

#define ERTS_PSFLGS_ACT_PRIO_OFFSET (0*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_USR_PRIO_OFFSET (1*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_PRQ_PRIO_OFFSET (2*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_ZERO_BIT_OFFSET (3*ERTS_PSFLGS_PRIO_BITS)

#define ERTS_PSFLGS_QMASK_BITS 4
#define ERTS_PSFLGS_QMASK \
    ((((erts_aint32_t) 1) << ERTS_PSFLGS_QMASK_BITS) - 1)
#define ERTS_PSFLGS_IN_PRQ_MASK_OFFSET \
    ERTS_PSFLGS_ZERO_BIT_OFFSET

#define ERTS_PSFLG_BIT(N) \
    (((erts_aint32_t) 1) << (ERTS_PSFLGS_ZERO_BIT_OFFSET + (N)))

/*
 *
 * Update etp-proc-state-int in $ERL_TOP/erts/etc/unix/etp-commands.in
 * when changing ERTS_PSFLG_*.
 */
/* ACT_PRIO - Active prio, i.e., currently active prio. This
   prio may be higher than user prio */
#define ERTS_PSFLGS_ACT_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_ACT_PRIO_OFFSET)
/* USR_PRIO - User prio. i.e., prio the user has set */
#define ERTS_PSFLGS_USR_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_USR_PRIO_OFFSET)
/* PRQ_PRIO - Prio queue prio, i.e., prio queue this process
   struct is currently enqueued in */
#define ERTS_PSFLGS_PRQ_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_PRQ_PRIO_OFFSET)
/* ERTS_PSFLG_IN_PRQ_MAX - Process in max prio on some
   run queue (may be in multiple prio at the same time
   via proxy process structures) */
#define ERTS_PSFLG_IN_PRQ_MAX 		ERTS_PSFLG_BIT(0)
/* ERTS_PSFLG_IN_PRQ_HIGH - Process in high prio on some
   run queue (may be in multiple prio at the same time
   via proxy process structures) */
#define ERTS_PSFLG_IN_PRQ_HIGH		ERTS_PSFLG_BIT(1)
/* ERTS_PSFLG_IN_PRQ_LOW - Process in low prio on some
   run queue (may be in multiple prio at the same time
   via proxy process structures) */
#define ERTS_PSFLG_IN_PRQ_NORMAL	ERTS_PSFLG_BIT(2)
/* ERTS_PSFLG_IN_PRQ_LOW - Process in normal prio on some
   run queue (may be in multiple prio at the same time
   via proxy process structures) */
#define ERTS_PSFLG_IN_PRQ_LOW 		ERTS_PSFLG_BIT(3)
/* FREE - Process is exiting, but not visible in
   process table. Both EXITING and ACTIVE should
   always be set when FREE */
#define ERTS_PSFLG_FREE			ERTS_PSFLG_BIT(4)
/* EXITING - Process is exiting, but still visible in
   process table. Always ACTIVE while EXITING. Never
   SUSPENDED unless also FREE. */
#define ERTS_PSFLG_EXITING		ERTS_PSFLG_BIT(5)
/* MSG_SIG_IN_Q - Have unhandled message signals in signal
   in-queue */
#define ERTS_PSFLG_MSG_SIG_IN_Q         ERTS_PSFLG_BIT(6)
/* ACTIVE - Process "wants" to execute */
#define ERTS_PSFLG_ACTIVE		ERTS_PSFLG_BIT(7)
/* IN_RUNQ - Real process (not proxy) struct used in a
   run queue */
#define ERTS_PSFLG_IN_RUNQ		ERTS_PSFLG_BIT(8)
/* RUNNING - Executing in process_main() */
#define ERTS_PSFLG_RUNNING		ERTS_PSFLG_BIT(9)
/* SUSPENDED - Process suspended; suppress active but
   not active-sys nor dirty-active-sys */
#define ERTS_PSFLG_SUSPENDED		ERTS_PSFLG_BIT(10)
/* GC - gc */
#define ERTS_PSFLG_GC			ERTS_PSFLG_BIT(11)
/* SYS_TASKS - Have normal system tasks scheduled */
#define ERTS_PSFLG_SYS_TASKS		ERTS_PSFLG_BIT(12)
/* NMSG_SIG_IN_Q - Have unhandled non-message signals in
   signal in-queue */
#define ERTS_PSFLG_NMSG_SIG_IN_Q        ERTS_PSFLG_BIT(13)
/* ACTIVE_SYS - Process "wants" to execute normal system
   tasks or handle signals */
#define ERTS_PSFLG_ACTIVE_SYS		ERTS_PSFLG_BIT(14)
/* RUNNING_SYS - Process is executing normal system
   tasks or handling signals */
#define ERTS_PSFLG_RUNNING_SYS		ERTS_PSFLG_BIT(15)
/* PROXY - Current process struct is a proxy process
   struct */
#define ERTS_PSFLG_PROXY		ERTS_PSFLG_BIT(16)
/* DELAYED_SYS - Have delayed (gc) system tasks (gc
   is disabled on process) */
#define ERTS_PSFLG_DELAYED_SYS		ERTS_PSFLG_BIT(17)
/* OFF_HEAP_MSGQ - Process have off heap message queue */
#define ERTS_PSFLG_OFF_HEAP_MSGQ	ERTS_PSFLG_BIT(18)
/* SIG_Q - Have unhandled signals in private (middle)
   signal queue */
#define ERTS_PSFLG_SIG_Q		ERTS_PSFLG_BIT(19)
/* DIRTY_CPU_PROC - Process wants to reschedule onto a
   dirty cpu scheduler */
#define ERTS_PSFLG_DIRTY_CPU_PROC	ERTS_PSFLG_BIT(20)
/* DIRTY_IO_PROC - Process wants to reschedule onto a
   dirty io scheduler */
#define ERTS_PSFLG_DIRTY_IO_PROC	ERTS_PSFLG_BIT(21)
/* DIRTY_ACTIVE_SYS - Process "wants" to execute dirty
   system tasks */
#define ERTS_PSFLG_DIRTY_ACTIVE_SYS	ERTS_PSFLG_BIT(22)
/* DIRTY_RUNNING - Executing in erts_dirty_process_main() */
#define ERTS_PSFLG_DIRTY_RUNNING	ERTS_PSFLG_BIT(23)
/* DIRTY_RUNNING_SYS - Process is executing dirty system
   tasks */    
#define ERTS_PSFLG_DIRTY_RUNNING_SYS	ERTS_PSFLG_BIT(24)

#define ERTS_PSFLG_MAX  (ERTS_PSFLGS_ZERO_BIT_OFFSET + 24)

#define ERTS_PSFLGS_DIRTY_WORK		(ERTS_PSFLG_DIRTY_CPU_PROC	\
					 | ERTS_PSFLG_DIRTY_IO_PROC	\
					 | ERTS_PSFLG_DIRTY_ACTIVE_SYS)

#define ERTS_PSFLGS_IN_PRQ_MASK 	(ERTS_PSFLG_IN_PRQ_MAX		\
					 | ERTS_PSFLG_IN_PRQ_HIGH	\
					 | ERTS_PSFLG_IN_PRQ_NORMAL	\
					 | ERTS_PSFLG_IN_PRQ_LOW)

#define ERTS_PSFLGS_VOLATILE_HEAP	(ERTS_PSFLG_EXITING		\
					 | ERTS_PSFLG_DIRTY_RUNNING	\
					 | ERTS_PSFLG_DIRTY_RUNNING_SYS)

/*
 * Process is in a dirty state if it got dirty work scheduled or
 * is running dirty. We do not include the dirty-running-sys state
 * since it executing while holding the main process lock which makes
 * it hard or impossible to manipulate from the outside. The time spent
 * in the dirty-running-sys is also limited compared to the other dirty
 * states.
 *
 * For more info on why we ignore dirty running sys see
 * erts_execute_dirty_system_task() in erl_process.c.
 */
#define ERTS_PROC_IN_DIRTY_STATE(S)                                     \
    ((!!((S) & (ERTS_PSFLGS_DIRTY_WORK                                  \
                | ERTS_PSFLG_DIRTY_RUNNING)))                           \
     & (!((S) & (ERTS_PSFLG_DIRTY_RUNNING_SYS                           \
                 | ERTS_PSFLG_RUNNING_SYS                               \
                 | ERTS_PSFLG_RUNNING))))

/*
 * A process needs dirty signal handling if it has unhandled signals
 * and is in a dirty state...
 */
#define ERTS_PROC_NEED_DIRTY_SIG_HANDLING(S)                            \
    ((!!((S) & (ERTS_PSFLG_SIG_Q                                        \
                | ERTS_PSFLG_NMSG_SIG_IN_Q                              \
                | ERTS_PSFLG_MSG_SIG_IN_Q)))                            \
     & ERTS_PROC_IN_DIRTY_STATE((S)))

#define ERTS_PSFLGS_GET_ACT_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_ACT_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)
#define ERTS_PSFLGS_GET_USR_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_USR_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)
#define ERTS_PSFLGS_GET_PRQ_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_PRQ_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)


/*
 * Flags in the xstate field.
 */

#define ERTS_PXSFLG_IN_CPU_PRQ_MAX 	(((erts_aint32_t) 1) << 0)
#define ERTS_PXSFLG_IN_CPU_PRQ_HIGH	(((erts_aint32_t) 1) << 1)
#define ERTS_PXSFLG_IN_CPU_PRQ_NORMAL	(((erts_aint32_t) 1) << 2)
#define ERTS_PXSFLG_IN_CPU_PRQ_LOW 	(((erts_aint32_t) 1) << 3)
#define ERTS_PXSFLG_IN_IO_PRQ_MAX 	(((erts_aint32_t) 1) << 4)
#define ERTS_PXSFLG_IN_IO_PRQ_HIGH	(((erts_aint32_t) 1) << 5)
#define ERTS_PXSFLG_IN_IO_PRQ_NORMAL	(((erts_aint32_t) 1) << 6)
#define ERTS_PXSFLG_IN_IO_PRQ_LOW 	(((erts_aint32_t) 1) << 7)
/* MAYBE_SELF_SIGS - We might have outstanding signals
   from ourselves to ourselves. */
#define ERTS_PXSFLG_MAYBE_SELF_SIGS	(((erts_aint32_t) 1) << 8)

#define ERTS_PXSFLGS_QMASK 		ERTS_PSFLGS_QMASK
#define ERTS_PXSFLGS_IN_CPU_PRQ_MASK_OFFSET 0
#define ERTS_PXSFLGS_IN_IO_PRQ_MASK_OFFSET ERTS_PSFLGS_QMASK_BITS

#define ERTS_PXSFLG_IN_CPU_PRQ_MASK 	(ERTS_PXSFLG_IN_CPU_PRQ_MAX	\
					 | ERTS_PXSFLG_IN_CPU_PRQ_HIGH	\
					 | ERTS_PXSFLG_IN_CPU_PRQ_NORMAL\
					 | ERTS_PXSFLG_IN_CPU_PRQ_LOW)
#define ERTS_PXSFLG_IN_IO_PRQ_MASK 	(ERTS_PXSFLG_IN_CPU_PRQ_MAX	\
					 | ERTS_PXSFLG_IN_CPU_PRQ_HIGH	\
					 | ERTS_PXSFLG_IN_CPU_PRQ_NORMAL\
					 | ERTS_PXSFLG_IN_CPU_PRQ_LOW)


/*
 * Static flags that do not change after process creation.
 */
#define ERTS_STC_FLG_SYSTEM_PROC	(((Uint32) 1) << 0)
#define ERTS_STC_FLG_SHADOW_PROC	(((Uint32) 1) << 1)

/* The sequential tracing token is a tuple of size 5:
 *
 *    {Flags, Label, Serial, Sender, LastCnt}
 *
 *  WARNING: The top 5-tuple is *MUTABLE* and thus INTERNAL ONLY.
 */
#define SEQ_TRACE_TOKEN_ARITY(p)    (arityval(*(tuple_val(SEQ_TRACE_TOKEN(p)))))
#define SEQ_TRACE_TOKEN_FLAGS(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 1))
#define SEQ_TRACE_TOKEN_LABEL(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 2))
#define SEQ_TRACE_TOKEN_SERIAL(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 3))
#define SEQ_TRACE_TOKEN_SENDER(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 4))
#define SEQ_TRACE_TOKEN_LASTCNT(p)  (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 5))

/* used when we have unit32 token */
#define SEQ_TRACE_T_ARITY(token)    (arityval(*(tuple_val(token))))
#define SEQ_TRACE_T_FLAGS(token)    (*(tuple_val(token) + 1))
#define SEQ_TRACE_T_LABEL(token)    (*(tuple_val(token) + 2))
#define SEQ_TRACE_T_SERIAL(token)   (*(tuple_val(token) + 3))
#define SEQ_TRACE_T_SENDER(token)   (*(tuple_val(token) + 4))
#define SEQ_TRACE_T_LASTCNT(token)  (*(tuple_val(token) + 5))

#ifdef USE_VM_PROBES
/* The dtrace probe for seq_trace only supports 'int' labels, so we represent
 * all values that won't fit into a 32-bit signed integer as ERTS_SINT32_MIN
 * (bigints, tuples, etc). */

#define SEQ_TRACE_T_DTRACE_LABEL(token) \
    DTRACE_SEQ_TRACE_LABEL__(SEQ_TRACE_T_LABEL(token))

#define DTRACE_SEQ_TRACE_LABEL__(label_term) \
    (is_small((label_term)) ? \
        ((signed_val((label_term)) <= ERTS_SINT32_MAX && \
          signed_val((label_term)) >= ERTS_SINT32_MIN) ? \
             signed_val((label_term)) : ERTS_SINT32_MIN) \
        : ERTS_SINT32_MIN)
#endif

/*

 * Possible flags for the flags field in ErlSpawnOpts below.
 */

#define SPO_IX_LINK             0
#define SPO_IX_MONITOR          1
#define SPO_IX_SYSTEM_PROC      2
#define SPO_IX_OFF_HEAP_MSGQ    3
#define SPO_IX_ON_HEAP_MSGQ     4
#define SPO_IX_MIN_HEAP_SIZE    5
#define SPO_IX_MIN_VHEAP_SIZE   6
#define SPO_IX_PRIORITY         7
#define SPO_IX_MAX_GEN_GCS      8
#define SPO_IX_MAX_HEAP_SIZE    9
#define SPO_IX_SCHEDULER        10
#define SPO_IX_ASYNC            11
#define SPO_IX_NO_SMSG          12
#define SPO_IX_NO_EMSG          13
#define SPO_IX_ASYNC_DIST       14

#define SPO_NO_INDICES          (SPO_IX_ASYNC_DIST+1)

#define SPO_LINK                (1 << SPO_IX_LINK)
#define SPO_MONITOR             (1 << SPO_IX_MONITOR)
#define SPO_SYSTEM_PROC         (1 << SPO_IX_SYSTEM_PROC)
#define SPO_OFF_HEAP_MSGQ       (1 << SPO_IX_OFF_HEAP_MSGQ)
#define SPO_ON_HEAP_MSGQ        (1 << SPO_IX_ON_HEAP_MSGQ)
#define SPO_MIN_HEAP_SIZE       (1 << SPO_IX_MIN_HEAP_SIZE)
#define SPO_MIN_VHEAP_SIZE      (1 << SPO_IX_MIN_VHEAP_SIZE)
#define SPO_PRIORITY            (1 << SPO_IX_PRIORITY)
#define SPO_MAX_GEN_GCS         (1 << SPO_IX_MAX_GEN_GCS)
#define SPO_MAX_HEAP_SIZE       (1 << SPO_IX_MAX_HEAP_SIZE)
#define SPO_SCHEDULER           (1 << SPO_IX_SCHEDULER)
#define SPO_ASYNC               (1 << SPO_IX_ASYNC)
#define SPO_NO_SMSG             (1 << SPO_IX_NO_SMSG)
#define SPO_NO_EMSG             (1 << SPO_IX_NO_EMSG)
#define SPO_ASYNC_DIST          (1 << SPO_IX_ASYNC_DIST)

#define SPO_MAX_FLAG            SPO_ASYNC_DIST

#define SPO_USE_ARGS                 \
    (SPO_MIN_HEAP_SIZE               \
     | SPO_PRIORITY                  \
     | SPO_MAX_GEN_GCS               \
     | SPO_MAX_HEAP_SIZE             \
     | SPO_SCHEDULER)

extern int ERTS_WRITE_UNLIKELY(erts_default_spo_flags);

/*
 * The following struct contains options for a process to be spawned.
 */
typedef struct {
    int flags;
    int error_code;		/* Error code returned from create_process(). */
    Eterm mref;			/* Monitor ref returned (if SPO_MONITOR was given).
                                   (output if local; input if distributed) */

    int multi_set;

    Eterm tag;                  /* spawn_request tag (if SPO_ASYNC is set) */
    Eterm monitor_tag;          /* monitor tag (if SPO_MONITOR is set) */
    Uint32 monitor_oflags;      /* flags to bitwise-or onto origin flags */
    Uint32 link_oflags;         /* flags to bitwise-or onto origin link flags */
    Eterm opts;                 /* Option list for seq-trace... */

    /* Input fields used for distributed spawn only */
    Eterm parent_id;
    Eterm group_leader;
    Eterm mfa;
    DistEntry *dist_entry;
    Uint32 conn_id;
    ErtsMonLnkDist *mld;        /* copied from dist_entry->mld */
    ErtsDistExternal *edep;
    ErlHeapFragment *ede_hfrag;
    Eterm token;

    /*
     * The following items are only initialized if the SPO_USE_ARGS flag is set.
     */
    Uint min_heap_size;		/* Minimum heap size (must be a valued returned
				 * from next_heap_size()).  */
    Uint min_vheap_size;	/* Minimum virtual heap size  */
    int priority;		/* Priority for process. */
    Uint16 max_gen_gcs;		/* Maximum number of gen GCs before fullsweep. */
    Uint max_heap_size;         /* Maximum heap size in words */
    Uint max_heap_flags;        /* Maximum heap flags (kill | log) */
    int scheduler;

} ErlSpawnOpts;

#define ERTS_SET_DEFAULT_SPAWN_OPTS(SOP)                                \
    do {                                                                \
        (SOP)->flags = erts_default_spo_flags;                          \
        (SOP)->opts = NIL;                                              \
        (SOP)->tag = am_spawn_reply;                                    \
        (SOP)->monitor_tag = THE_NON_VALUE;                             \
        (SOP)->monitor_oflags = (Uint32) 0;                             \
        (SOP)->link_oflags = (Uint32) 0;                                \
    } while (0)

/*
 * The KILL_CATCHES(p) macro kills pending catches for process p.
 */

#define KILL_CATCHES(p) (p)->catches = -1

/* Shrink heap fragment from _last_ HAlloc.
*/
ERTS_GLB_INLINE void erts_heap_frag_shrink(Process* p, Eterm* hp);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void erts_heap_frag_shrink(Process* p, Eterm* hp)
{
    ErlHeapFragment* hf = MBUF(p);
    Uint sz;

    ASSERT(hf != NULL && (hp - hf->mem <= hf->alloc_size));

    sz = hp - hf->mem;
    p->mbuf_sz -= hf->used_size - sz;
    hf->used_size = sz;
}	
#endif /* inline */

Eterm* erts_heap_alloc(Process* p, Uint need, Uint xtra);

extern erts_rwmtx_t erts_cpu_bind_rwmtx;

extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_gc);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_schedule);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_large_heap);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_msgq_on);
extern Sint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_msgq_off);
extern Sint ERTS_WRITE_UNLIKELY(erts_system_monitor_busy_port_cnt);
extern Sint ERTS_WRITE_UNLIKELY(erts_system_monitor_busy_dist_port_cnt);
struct erts_system_monitor_flags_t {
    bool busy_port;
    bool busy_dist_port;
};

/* system_profile, same rules as for system_monitor.
	erts_profile must be != NIL when 
	erts_profile_* is set. */

extern Eterm erts_system_profile;
struct erts_system_profile_flags_t {
    unsigned int scheduler : 1;
    unsigned int runnable_procs : 1;
    unsigned int runnable_ports : 1;
    unsigned int exclusive : 1;
};
extern struct erts_system_profile_flags_t erts_system_profile_flags;
extern int erts_system_profile_ts_type;

/* process flags */
#define F_HIBERNATE_SCHED    (1 <<  0) /* Schedule out after hibernate op */
#define F_INSLPQUEUE         (1 <<  1) /* Set if in timer queue */
#define F_TIMO               (1 <<  2) /* Set if timeout */
#define F_HEAP_GROW          (1 <<  3)
#define F_NEED_FULLSWEEP     (1 <<  4)
#define F_USING_DB           (1 <<  5) /* If have created tables */
#define F_DISTRIBUTION       (1 <<  6) /* Process used in distribution */
#define F_USING_DDLL         (1 <<  7) /* Process has used the DDLL interface */
#define F_HAVE_BLCKD_MSCHED  (1 <<  8) /* Process has blocked multi-scheduling */
#define F_ETS_SUPER_USER     (1 <<  9) /* Process is ETS super user */
#define F_FORCE_GC           (1 << 10) /* Force gc at process in-scheduling */
#define F_DISABLE_GC         (1 << 11) /* Disable GC (see below) */
#define F_ABANDONED_HEAP_USE (1 << 12) /* Have usage of abandoned heap */
#define F_DELAY_GC           (1 << 13) /* Similar to disable GC (see below) */
#define F_SCHDLR_ONLN_WAITQ  (1 << 14) /* Process enqueued waiting to change schedulers online */
#define F_HAVE_BLCKD_NMSCHED (1 << 15) /* Process has blocked normal multi-scheduling */
#define F_DELAYED_DEL_PROC   (1 << 16) /* Delay delete process (dirty proc exit case) */
#define F_DIRTY_CLA          (1 << 17) /* Dirty copy literal area scheduled */
#define F_DIRTY_GC_HIBERNATE (1 << 18) /* Dirty GC hibernate scheduled */
#define F_DIRTY_MAJOR_GC     (1 << 19) /* Dirty major GC scheduled */
#define F_DIRTY_MINOR_GC     (1 << 20) /* Dirty minor GC scheduled */
#define F_HIBERNATED         (1 << 21) /* Hibernated */
#define F_TRAP_EXIT          (1 << 22) /* Trapping exit */
#define F_FRAGMENTED_SEND    (1 << 23) /* Process is doing a distributed fragmented send */
#define F_DBG_FORCED_TRAP    (1 << 24) /* DEBUG: Last BIF call was a forced trap */
#define F_DIRTY_CHECK_CLA    (1 << 25) /* Check if copy literal area GC scheduled */
#define F_ASYNC_DIST         (1 << 26) /* Truly asynchronous distribution */

/* Signal queue flags */
#define FS_OFF_HEAP_MSGQ       (1 << 0) /* Off heap msg queue */
#define FS_ON_HEAP_MSGQ        (1 << 1) /* On heap msg queue */
#define FS_OFF_HEAP_MSGQ_CHNG  (1 << 2) /* Off heap msg queue changing */
#define FS_UNUSED              (1 << 3) /* Unused */
#define FS_HANDLING_SIGS       (1 << 4) /* Process is handling signals */
#define FS_WAIT_HANDLE_SIGS    (1 << 5) /* Process is waiting to handle signals */
#define FS_UNUSED2             (1 << 6) /* Unused */
#define FS_FLUSHING_SIGS       (1 << 7) /* Currently flushing signals */
#define FS_FLUSHED_SIGS        (1 << 8) /* Flushing of signals completed */
#define FS_NON_FETCH_CNT1      (1 << 9) /* First bit of non-fetch signals counter */
#define FS_NON_FETCH_CNT2      (1 << 10)/* Second bit of non-fetch signals counter */
#define FS_NON_FETCH_CNT4      (1 << 11)/* Third bit of non-fetch signals counter */
#define FS_MON_MSGQ_LEN_HIGH   (1 << 12)/* Monitor of msgq high limit for some session(s) */
#define FS_MON_MSGQ_LEN_LOW    (1 << 13)/* Monitor of msgq low limit for some session(s) */
#define FS_SET_SAVE_INFO_1     (1 << 14)/* set save info bit 1 */
#define FS_SET_SAVE_INFO_2     (1 << 15)/* set save info bit 2 */
#define FS_PRIO_MQ_SAVE        (1 << 16)/* Save points into prio queue */
#define FS_PRIO_MQ             (1 << 17)/* Prio message queue installed */
#define FS_PRIO_MQ_END_MARK    (1 << 18)/* Prio message queue end marker in queue */

#define FS_DBG_MQ_PTRS_UNREL   (1 << 31)/* MsgQ pointers unreliable (signal handling) */

/*
 * The FS_SET_SAVE_INFO_* bits of the signal queue flags map to the following
 * values. This information determines how to handle the save pointer with
 * regards to the priority (part of the message) queue.
 *
 * - FS_SET_SAVE_INFO_FIRST  - We began searching for messages at the start
 *                             of the message queue.
 * - FS_SET_SAVE_INFO_LAST   - We began searching for a message at the current
 *                             end of the message queue. This is an ERTS
 *                             internal receive optimization where we trap out
 *                             to a known receive. When set, the save pointer
 *                             should always point past the end of the prio
 *                             queue. 
 * - FS_SET_SAVE_INFO_RCVM   - We began searching for messages at receive
 *                             marker identified by the 'set_save_ix' field in
 *                             the receive marker block.
 * - FS_SET_SAVE_INFO_MARK   - The prio queue continuation marker is inserted
 *                             in the message queue. When we reach the end of
 *                             the prio queue, we should continue at the marker.
 *                             Information about the previous set save info
 *                             state can be found in the 'saved_save_info' field
 *                             in the priority queue info block.
 */

#define FS_SET_SAVE_INFO_MASK  (FS_SET_SAVE_INFO_1 | FS_SET_SAVE_INFO_2)

#define FS_SET_SAVE_INFO_FIRST (0)
#define FS_SET_SAVE_INFO_RCVM  (FS_SET_SAVE_INFO_1)
#define FS_SET_SAVE_INFO_MARK  (FS_SET_SAVE_INFO_2)
#define FS_SET_SAVE_INFO_LAST  (FS_SET_SAVE_INFO_1 | FS_SET_SAVE_INFO_2)

#define ERTS_MQ_SET_SAVE_INFO(P, SI)                                    \
    do {                                                                \
        ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN                              \
                       & erts_proc_lc_my_proc_locks((P)));              \
        ASSERT(((SI) & ~FS_SET_SAVE_INFO_MASK) == 0);                   \
        (P)->sig_qs.flags &= ~FS_SET_SAVE_INFO_MASK;                    \
        (P)->sig_qs.flags |= (SI);                                      \
    } while (0)

#define ERTS_MQ_GET_SAVE_INFO(P)                                        \
    ((P)->sig_qs.flags & FS_SET_SAVE_INFO_MASK)

#define FS_NON_FETCH_CNT_MASK \
    (FS_NON_FETCH_CNT1|FS_NON_FETCH_CNT2|FS_NON_FETCH_CNT4)

/*
 * F_DISABLE_GC and F_DELAY_GC are similar. Both will prevent
 * GC of the process, but it is important to use the right
 * one:
 * - F_DISABLE_GC should *only* be used by BIFs. This when
 *   the BIF needs to yield while preventig a GC.
 * - F_DELAY_GC should only be used when GC is temporarily
 *   disabled while the process is scheduled. A process must
 *   not be scheduled out while F_DELAY_GC is set.
 */

#define ERTS_TRACE_FLAGS_TS_TYPE_SHIFT			0

#define F_TRACE_FLAG(N)      (1 << (ERTS_TRACE_TS_TYPE_BITS + (N)))

/* process trace_flags */
#define F_NOW_TS             (ERTS_TRACE_FLG_NOW_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_STRICT_MON_TS      (ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_MON_TS             (ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_SENSITIVE          F_TRACE_FLAG(0)
#define F_TRACE_SEND         F_TRACE_FLAG(1)
#define F_TRACE_RECEIVE      F_TRACE_FLAG(2)
#define F_TRACE_SOS          F_TRACE_FLAG(3) /* Set on spawn       */
#define F_TRACE_SOS1         F_TRACE_FLAG(4) /* Set on first spawn */
#define F_TRACE_SOL          F_TRACE_FLAG(5) /* Set on link        */
#define F_TRACE_SOL1         F_TRACE_FLAG(6) /* Set on first link  */
#define F_TRACE_CALLS        F_TRACE_FLAG(7)
#define F_TRACE_PROCS        F_TRACE_FLAG(8)
#define F_TRACE_FIRST_CHILD  F_TRACE_FLAG(9)
#define F_TRACE_SCHED        F_TRACE_FLAG(10)
#define F_TRACE_GC           F_TRACE_FLAG(11)
#define F_TRACE_ARITY_ONLY   F_TRACE_FLAG(12)
#define F_TRACE_RETURN_TO    F_TRACE_FLAG(13) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       F_TRACE_FLAG(14) /* No call trace msg suppress */
#define F_TRACE_DBG_CANARY   F_TRACE_FLAG(15)
/* port trace flags, currently the same as process trace flags */
#define F_TRACE_SCHED_PORTS  F_TRACE_FLAG(17) /* Trace of port scheduling */
#define F_TRACE_SCHED_PROCS  F_TRACE_FLAG(18) /* With virtual scheduling */
#define F_TRACE_PORTS	     F_TRACE_FLAG(19) /* Ports equivalent to F_TRACE_PROCS */
#define F_TRACE_SCHED_NO     F_TRACE_FLAG(20) /* Trace with scheduler id */
#define F_TRACE_SCHED_EXIT   F_TRACE_FLAG(21)
#define F_TRACE_RETURN_TO_MARK  F_TRACE_FLAG(22) /* temporary marker */


#define F_NUM_FLAGS          (ERTS_TRACE_TS_TYPE_BITS + 23)
#ifdef DEBUG
// Was there a point with this high 5?
#  define F_INITIAL_TRACE_FLAGS 0 //(5 << F_NUM_FLAGS)
#else
#  define F_INITIAL_TRACE_FLAGS 0
#endif

/* F_TIMESTAMP_MASK is a bit-field of all timestamp types */
#define F_TIMESTAMP_MASK \
    (ERTS_TRACE_TS_TYPE_MASK << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)

#define TRACEE_FLAGS ( F_TRACE_PROCS | F_TRACE_CALLS \
		     | F_TRACE_SOS |  F_TRACE_SOS1| F_TRACE_RECEIVE  \
		     | F_TRACE_SOL | F_TRACE_SOL1 | F_TRACE_SEND \
		     | F_TRACE_SCHED | F_TIMESTAMP_MASK | F_TRACE_GC \
		     | F_TRACE_ARITY_ONLY | F_TRACE_RETURN_TO \
                     | F_TRACE_SILENT | F_TRACE_SCHED_PROCS | F_TRACE_PORTS \
		     | F_TRACE_SCHED_PORTS | F_TRACE_SCHED_NO \
		     | F_TRACE_SCHED_EXIT )


#define ERTS_TRACEE_MODIFIER_FLAGS \
    (F_TRACE_SILENT | F_TIMESTAMP_MASK | F_TRACE_SCHED_NO \
     | F_TRACE_RECEIVE | F_TRACE_SEND)
#define ERTS_PORT_TRACEE_FLAGS                                     \
    (ERTS_TRACEE_MODIFIER_FLAGS | F_TRACE_PORTS | F_TRACE_SCHED_PORTS)
#define ERTS_PROC_TRACEE_FLAGS \
    ((TRACEE_FLAGS & ~ERTS_PORT_TRACEE_FLAGS) | ERTS_TRACEE_MODIFIER_FLAGS)

#define SEQ_TRACE_FLAG(N)        (1 << (ERTS_TRACE_TS_TYPE_BITS + (N)))

#define ERTS_SIG_ENABLE_TRACE_FLAGS \
    ( F_TRACE_RECEIVE | F_TRACE_PROCS)

/*
 * F_TRACE_RECEIVE is always enabled/disable via signaling.
 * F_TRACE_PROCS enable/disable F_TRACE_PROCS_SIG via signaling.
 */

/* Sequential trace flags */

/* SEQ_TRACE_TIMESTAMP_MASK is a bit-field */
#define SEQ_TRACE_TIMESTAMP_MASK \
    (ERTS_TRACE_TS_TYPE_MASK << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)

#define SEQ_TRACE_SEND     (1 << 0)
#define SEQ_TRACE_RECEIVE  (1 << 1)
#define SEQ_TRACE_PRINT    (1 << 2)
/* (This three-bit gap contains the timestamp.) */

#define ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT 3

#define SEQ_TRACE_NOW_TS   (ERTS_TRACE_FLG_NOW_TIMESTAMP \
			    << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)
#define SEQ_TRACE_STRICT_MON_TS (ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP \
				 << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)
#define SEQ_TRACE_MON_TS (ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP \
			  << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)

#ifdef USE_VM_PROBES
#define DT_UTAG_PERMANENT (1 << 0)
#define DT_UTAG_SPREADING (1 << 1)
#define DT_UTAG(P) ((P)->dt_utag)
#define DT_UTAG_FLAGS(P)  ((P)->dt_utag_flags) 
#endif

#define CANCEL_TIMER(P)					\
    do {						\
	if ((P)->flags & (F_INSLPQUEUE|F_TIMO)) {	\
	    if ((P)->flags & F_INSLPQUEUE)		\
		erts_cancel_proc_timer((P));		\
	    else					\
		(P)->flags &= ~F_TIMO;			\
	}						\
    } while (0)

#define ERTS_NUM_DIRTY_CPU_RUNQS 1
#define ERTS_NUM_DIRTY_IO_RUNQS 1

#define ERTS_NUM_DIRTY_RUNQS (ERTS_NUM_DIRTY_CPU_RUNQS+ERTS_NUM_DIRTY_IO_RUNQS)

#define ERTS_RUNQ_IX(IX)						\
  (ASSERT(0 <= (IX) && (IX) < erts_no_run_queues+ERTS_NUM_DIRTY_RUNQS), \
   &erts_aligned_run_queues[(IX)].runq)
#define ERTS_RUNQ_IX_IS_DIRTY(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_run_queues+ERTS_NUM_DIRTY_RUNQS), \
   (erts_no_run_queues <= (IX)))
#define ERTS_DIRTY_RUNQ_IX(IX)						\
  (ASSERT(ERTS_RUNQ_IX_IS_DIRTY(IX)),					\
   &erts_aligned_run_queues[(IX)].runq)
#define ERTS_DIRTY_CPU_RUNQ (&erts_aligned_run_queues[erts_no_run_queues].runq)
#define ERTS_DIRTY_IO_RUNQ  (&erts_aligned_run_queues[erts_no_run_queues+1].runq)
#define ERTS_RUNQ_IS_DIRTY_CPU_RUNQ(RQ) ((RQ) == ERTS_DIRTY_CPU_RUNQ)
#define ERTS_RUNQ_IS_DIRTY_IO_RUNQ(RQ) ((RQ) == ERTS_DIRTY_IO_RUNQ)
#define ERTS_SCHEDULER_IX(IX)						\
  (ASSERT(0 <= (IX) && (IX) < erts_no_schedulers),			\
   &erts_aligned_scheduler_data[(IX)].esd)
#define ERTS_DIRTY_CPU_SCHEDULER_IX(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_dirty_cpu_schedulers),		\
   &erts_aligned_dirty_cpu_scheduler_data[(IX)].esd)
#define ERTS_DIRTY_IO_SCHEDULER_IX(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_dirty_io_schedulers),		\
   &erts_aligned_dirty_io_scheduler_data[(IX)].esd)
#define ERTS_SCHEDULER_IS_DIRTY(ESDP)					\
  ((ESDP)->type != ERTS_SCHED_NORMAL)
#define ERTS_SCHEDULER_IS_DIRTY_CPU(ESDP)				\
    ((ESDP)->type == ERTS_SCHED_DIRTY_CPU)
#define ERTS_SCHEDULER_IS_DIRTY_IO(ESDP)				\
    ((ESDP)->type == ERTS_SCHED_DIRTY_IO)

void erts_pre_init_process(void);
void erts_late_init_process(void);
void erts_early_init_scheduling(int);
void erts_init_scheduling(int, int, int, int, int, int);
void erts_execute_dirty_system_task(Process *c_p);
int erts_set_gc_state(Process *c_p, int enable);
Eterm erts_sched_wall_time_request(Process *c_p, int set, int enable,
                                   int dirty_cpu, int want_dirty_io);
Eterm erts_system_check_request(Process *c_p);
Eterm erts_gc_info_request(Process *c_p);
Uint64 erts_get_proc_interval(void);
Uint64 erts_ensure_later_proc_interval(Uint64);
Uint64 erts_step_proc_interval(void);

void erts_proclist_destroy(ErtsProcList *);
ErtsProcList *erts_proclist_create(Process *) ERTS_ATTR_MALLOC_D(erts_proclist_destroy,1);
ErtsProcList *erts_proclist_copy(ErtsProcList *);
void erts_proclist_dump(fmtfn_t to, void *to_arg, ErtsProcList*);

ERTS_GLB_INLINE int erts_proclist_same(ErtsProcList *, Process *);
ERTS_GLB_INLINE void erts_proclist_store_first(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE void erts_proclist_store_last(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_first(ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_last(ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_next(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_prev(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_first(ErtsProcList **);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_last(ErtsProcList **);
ERTS_GLB_INLINE int erts_proclist_fetch(ErtsProcList **, ErtsProcList **);
ERTS_GLB_INLINE void erts_proclist_remove(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_empty(ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_first(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_last(ErtsProcList *, ErtsProcList *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_proclist_same(ErtsProcList *plp, Process *p)
{
    return ((plp->u.pid == p->common.id || plp->u.p == p)
	    && (plp->started_interval
		== p->common.u.alive.started_interval));
}

ERTS_GLB_INLINE void erts_proclist_store_first(ErtsProcList **list,
					       ErtsProcList *element)
{
    if (!*list)
	element->next = element->prev = element;
    else {
	element->prev = (*list)->prev;
	element->next = *list;
	element->prev->next = element;
	element->next->prev = element;
    }
    *list = element;
}

ERTS_GLB_INLINE void erts_proclist_store_last(ErtsProcList **list,
					      ErtsProcList *element)
{
    if (!*list) {
	element->next = element->prev = element;
	*list = element;
    }
    else {
	element->prev = (*list)->prev;
	element->next = *list;
	element->prev->next = element;
	element->next->prev = element;
    }
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_first(ErtsProcList *list)
{
    return list;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_last(ErtsProcList *list)
{
    if (!list)
	return NULL;
    else
	return list->prev;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_next(ErtsProcList *list,
						      ErtsProcList *element)
{
    ErtsProcList *next;
    ASSERT(list && element);
    next = element->next;
    return list == next ? NULL : next;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_prev(ErtsProcList *list,
						      ErtsProcList *element)
{
    ErtsProcList *prev;
    ASSERT(list && element);
    prev = element->prev;
    return list == element ? NULL : prev;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_first(ErtsProcList **list)
{
    if (!*list)
	return NULL;
    else {
	ErtsProcList *res = *list;
	if (res->next == *list)
	    *list = NULL;
	else
	    *list = res->next;
	res->next->prev = res->prev;
	res->prev->next = res->next;
	return res;
    }
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_last(ErtsProcList **list)
{
    if (!*list)
	return NULL;
    else {
	ErtsProcList *res = (*list)->prev;
	if (res == *list)
	    *list = NULL;
	res->next->prev = res->prev;
	res->prev->next = res->next;
	return res;
    }
}

ERTS_GLB_INLINE int erts_proclist_fetch(ErtsProcList **list_first,
					ErtsProcList **list_last)
{
    if (!*list_first) {
	if (list_last)
	    *list_last = NULL;
	return 0;
    }
    else {
	if (list_last)
	    *list_last = (*list_first)->prev;
	(*list_first)->prev->next = NULL;
	(*list_first)->prev = NULL;
	return !0;
    }
}

ERTS_GLB_INLINE void erts_proclist_remove(ErtsProcList **list,
					  ErtsProcList *element)
{
    ASSERT(list && *list);
    if (*list == element) {
	*list = element->next;
	if (*list == element)
	    *list = NULL;
    }
    element->next->prev = element->prev;
    element->prev->next = element->next;
}

ERTS_GLB_INLINE int erts_proclist_is_empty(ErtsProcList *list)
{
    return list == NULL;
}

ERTS_GLB_INLINE int erts_proclist_is_first(ErtsProcList *list,
					   ErtsProcList *element)
{
    ASSERT(list && element);
    return list == element;
}

ERTS_GLB_INLINE int erts_proclist_is_last(ErtsProcList *list,
					  ErtsProcList *element)
{
    ASSERT(list && element);
    return list->prev == element;
}

#endif

int erts_sched_set_wakeup_other_threshold(ErtsSchedType sched_type, char *str);
int erts_sched_set_wakeup_other_type(ErtsSchedType sched_type, char *str);
int erts_sched_set_busy_wait_threshold(ErtsSchedType sched_type, char *str);
int erts_sched_set_wake_cleanup_threshold(char *);

void erts_schedule_thr_prgr_later_op(void (*)(void *),
				     void *,
				     ErtsThrPrgrLaterOp *);
void erts_schedule_thr_prgr_later_cleanup_op(void (*)(void *),
					     void *,
					     ErtsThrPrgrLaterOp *,
					     UWord);
void erts_schedule_complete_off_heap_message_queue_change(Eterm pid);
void erts_schedule_cla_gc(Process *c_p, Eterm to, Eterm req_id, int check);
struct db_fixation;
void erts_schedule_ets_free_fixation(Eterm pid, struct db_fixation*);
void erts_schedule_flush_trace_messages(Process *proc, int force_on_proc);
int erts_flush_trace_messages(Process *c_p, ErtsProcLocks locks);
int erts_sig_prio(Eterm pid, int prio);

#if defined(ERTS_ENABLE_LOCK_CHECK)
int erts_dbg_check_halloc_lock(Process *p);
#endif
void
erts_schedulers_state(Uint *, Uint *, Uint *, Uint *, Uint *, Uint *, Uint *, Uint *);
ErtsSchedSuspendResult
erts_set_schedulers_online(Process *p,
			   ErtsProcLocks plocks,
			   Sint new_no,
			   Sint *old_no,
			   int dirty_only);
ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *, ErtsProcLocks, int, int, int);
int erts_is_multi_scheduling_blocked(void);
Eterm erts_multi_scheduling_blockers(Process *, int);
void erts_start_schedulers(void);
void erts_alloc_notify_delayed_dealloc(int);
void erts_alloc_ensure_handle_delayed_dealloc_call(int);
void erts_notify_canceled_timer(ErtsSchedulerData *, int);
void erts_notify_check_async_ready_queue(void *);
void erts_notify_code_ix_activation(Process* p, ErtsThrPrgrVal later);
void erts_notify_finish_breakpointing(Process* p);
void erts_schedule_misc_aux_work(int sched_id,
				 void (*func)(void *),
				 void *arg);
void erts_schedule_multi_misc_aux_work(int ignore_self,
                                       int min_tid,
				       int max_tid,
				       void (*func)(void *),
				       void *arg);
erts_aint32_t erts_set_aux_work_timeout(int, erts_aint32_t, int);
void erts_aux_work_timeout_late_init(ErtsSchedulerData *esdp);
void erts_sched_notify_check_cpu_bind(void);
Uint erts_active_schedulers(void);
void erts_init_process(int, int, int);
Eterm erts_process_state2status(erts_aint32_t);
Eterm erts_process_status(Process *, Eterm);
Uint erts_run_queues_len(Uint *, int, int, int);
void erts_add_to_runq(Process *);
Eterm erts_bound_schedulers_term(Process *c_p);
Eterm erts_get_cpu_topology_term(Process *c_p, Eterm which);
Eterm erts_get_schedulers_binds(Process *c_p);
Eterm erts_set_cpu_topology(Process *c_p, Eterm term);
Eterm erts_bind_schedulers(Process *c_p, Eterm how);
ErtsRunQueue *erts_schedid2runq(Uint);
Process *erts_schedule(ErtsSchedulerData *, Process*, int);
void erts_schedule_misc_op(void (*)(void *), void *);
int erts_parse_spawn_opts(ErlSpawnOpts *sop, Eterm opts_list, Eterm *tag,
                          int success_message_opt);
void
erts_send_local_spawn_reply(Process *parent, ErtsProcLocks parent_locks,
                            Process *child, Eterm tag, Eterm ref,
                            Eterm result, Eterm token);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_set_self_exiting(Process *, Eterm);
void erts_do_exit_process(Process*, Eterm);
void erts_continue_exit_process(Process *);
void erts_proc_exit_link(Process *, ErtsLink *, Uint16, Eterm, Eterm);
/* Begin System profile */
Uint erts_runnable_process_count(void);
/* End System profile */
void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);
#ifdef DEBUG
void erts_debug_verify_clean_empty_process(Process* p);
#endif
void erts_stack_dump(fmtfn_t to, void *to_arg, Process *);
void erts_limited_stack_trace(fmtfn_t to, void *to_arg, Process *);
void erts_program_counter_info(fmtfn_t to, void *to_arg, Process *);
void erts_print_scheduler_info(fmtfn_t to, void *to_arg, ErtsSchedulerData *esdp);
void erts_print_run_queue_info(fmtfn_t, void *to_arg, ErtsRunQueue*);
void erts_dump_extended_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg);
void erts_dump_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg);
const char* erts_internal_fun_description_from_pc(ErtsCodePtr);

#define ERTS_PI_FLAG_SINGELTON                          (1 << 0)
#define ERTS_PI_FLAG_ALWAYS_WRAP                        (1 << 1)
#define ERTS_PI_FLAG_WANT_MSGS                          (1 << 2)
#define ERTS_PI_FLAG_NEED_MSGQ                          (1 << 3)
#define ERTS_PI_FLAG_FORCE_SIG_SEND                     (1 << 4)
#define ERTS_PI_FLAG_REQUEST_FOR_OTHER                  (1 << 5)
#define ERTS_PI_FLAG_KEY_TUPLE2                         (1 << 6)

Eterm erts_process_info(Process *c_p, ErtsHeapFactory *hfact,
                        Process *rp, ErtsProcLocks rp_locks,
                        int *item_ix, Eterm *item_extra, int item_len,
                        int flags, Uint reserve_size, Uint *reds);

typedef struct {
    Process *c_p;
    Eterm reason;
    ErtsLink *dist_links;
    ErtsMonitor *dist_monitors;
    ErtsMonitor *pend_spawn_monitors;
    ErtsMonitor *wait_pend_spawn_monitor;
    Eterm dist_state;
    int yield;
} ErtsProcExitContext;
int erts_proc_exit_handle_monitor(ErtsMonitor *mon, void *vctxt, Sint reds);
int erts_proc_exit_handle_link(ErtsLink *lnk, void *vctxt, Sint reds);

void erts_proc_exit_dist_demonitor(Process *c_p, DistEntry *dep, Uint32 conn_id,
                                   Eterm ref, Eterm watched);

Eterm erts_get_process_priority(erts_aint32_t state);
Eterm erts_set_process_priority(Process *p, Eterm prio);

Uint erts_get_total_context_switches(void);
void erts_get_total_reductions(Uint *, Uint *);
void erts_get_exact_total_reductions(Process *, Uint *, Uint *);

Eterm erts_fake_scheduler_bindings(Process *p, Eterm how);

void erts_sched_stat_modify(int what);
Eterm erts_sched_stat_term(Process *p, int total);

void erts_free_proc(Process *);

void erts_suspend(Process*, ErtsProcLocks, Port*);
void erts_resume(Process*, ErtsProcLocks);
int erts_resume_processes(ErtsProcList *);

void erts_deep_process_dump(fmtfn_t, void *);

Eterm erts_get_reader_groups_map(Process *c_p);
Eterm erts_get_decentralized_counter_groups_map(Process *c_p);
Eterm erts_debug_reader_groups_map(Process *c_p, int groups);

Uint erts_debug_nbalance(void);

#define ERTS_DEBUG_WAIT_COMPLETED_DEALLOCATIONS		(1 << 0)
#define ERTS_DEBUG_WAIT_COMPLETED_TIMER_CANCELLATIONS	(1 << 1)
#define ERTS_DEBUG_WAIT_COMPLETED_AUX_WORK		(1 << 2)
#define ERTS_DEBUG_WAIT_COMPLETED_THREAD_PROGRESS       (1 << 3)

int erts_debug_wait_completed(Process *c_p, int flags);

Uint erts_process_memory(Process *c_p, int include_sigs_in_transit);

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(P)					\
do {										\
    ErtsSchedulerData *esdp__ = erts_get_scheduler_data();			\
    if (esdp__ && !ERTS_SCHEDULER_IS_DIRTY(esdp__))				\
	esdp__->verify_unused_temp_alloc(					\
	    esdp__->verify_unused_temp_alloc_data);				\
} while (0)
#else
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(ESDP)
#endif

ErtsSchedulerData *erts_get_scheduler_data(void);

void erts_schedule_process(Process *, erts_aint32_t, ErtsProcLocks);
erts_aint32_t erts_proc_sys_schedule(Process *p, erts_aint32_t state,
                                     erts_aint32_t enable_flag);
int erts_have_non_prio_elev_sys_tasks(Process *c_p, ErtsProcLocks locks);

ERTS_GLB_INLINE void erts_schedule_dirty_sys_execution(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_schedule_dirty_sys_execution(Process *c_p)
{
    erts_aint32_t a, n, e;

    a = erts_atomic32_read_nob(&c_p->state);

    /*
     * Only a currently executing process schedules
     * itself for dirty-sys execution...
     */

    ASSERT(a & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS));

    /* Don't set dirty-active-sys if we are about to exit... */

    while (!(a & (ERTS_PSFLG_DIRTY_ACTIVE_SYS
                  | ERTS_PSFLG_EXITING))) {
        e = a;
        n = a | ERTS_PSFLG_DIRTY_ACTIVE_SYS;
        a = erts_atomic32_cmpxchg_mb(&c_p->state, n, e);
        if (a == e)
            break; /* dirty-active-sys set */
    }
}

#endif

#if defined(ERTS_ENABLE_LOCK_CHECK)

#define ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__
#include "erl_process_lock.h"
#undef ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__

#define ERTS_LC_CHK_RUNQ_LOCK(RQ, L)				\
do {									\
    if ((L))								\
	ERTS_LC_ASSERT(erts_lc_runq_is_locked((RQ)));		\
    else								\
	ERTS_LC_ASSERT(!erts_lc_runq_is_locked((RQ)));		\
} while (0)
#else
#define ERTS_LC_CHK_RUNQ_LOCK(RQ, L)
#endif

void *erts_psd_set_init(Process *p, int ix, void *data);

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix);
ERTS_GLB_INLINE void *
erts_psd_set(Process *p, int ix, void *data);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix)
{
    ErtsPSD *psd;
#if defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].get_locks)
	ERTS_LC_ASSERT(locks || erts_thr_progress_is_blocking());
    else {
	locks &= erts_psd_required_locks[ix].get_locks;
	ERTS_LC_ASSERT(erts_psd_required_locks[ix].get_locks == locks
			   || erts_thr_progress_is_blocking());
    }
#endif

    psd = (ErtsPSD *) erts_atomic_read_nob(&p->psd);
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    if (!psd)
	return NULL;
    ERTS_THR_DATA_DEPENDENCY_READ_MEMORY_BARRIER;
    return psd->data[ix];
}

ERTS_GLB_INLINE void *
erts_psd_set(Process *p, int ix, void *data)
{
    ErtsPSD *psd;
#if defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    erts_aint32_t state = state = erts_atomic32_read_nob(&p->state);
    if (!(state & ERTS_PSFLG_FREE)) {
	if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].set_locks)
	    ERTS_LC_ASSERT(locks || erts_thr_progress_is_blocking());
	else {
	    locks &= erts_psd_required_locks[ix].set_locks;
	    ERTS_LC_ASSERT(erts_psd_required_locks[ix].set_locks == locks
			       || erts_thr_progress_is_blocking());
	}
    }
#endif
    psd = (ErtsPSD *) erts_atomic_read_nob(&p->psd);
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    if (psd) {
	void *old;
#ifdef ETHR_ORDERED_READ_DEPEND
	ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
#else
	ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreStore);
#endif
	old = psd->data[ix];
	psd->data[ix] = data;
	return old;
    }

    if (!data)
	return NULL;

    return erts_psd_set_init(p, ix, data);
}

#endif

#define ERTS_PROC_SCHED_ID(P, ID) \
  ((UWord) erts_psd_set((P), ERTS_PSD_SCHED_ID, (void *) (ID)))

#define ERTS_PROC_GET_SAVED_CALLS_BUF(P) \
  ((struct saved_calls *) erts_psd_get((P), ERTS_PSD_SAVED_CALLS_BUF))
#define ERTS_PROC_SET_SAVED_CALLS_BUF(P, SCB) \
  ((struct saved_calls *) erts_psd_set((P), ERTS_PSD_SAVED_CALLS_BUF, (void *) (SCB)))

#define ERTS_PROC_GET_CALL_TIME(P) \
  ((process_breakpoint_trace_t *) erts_psd_get((P), ERTS_PSD_CALL_TIME_BP))
#define ERTS_PROC_SET_CALL_TIME(P, PBT) \
  ((process_breakpoint_trace_t *) erts_psd_set((P), ERTS_PSD_CALL_TIME_BP, (void *) (PBT)))

#define ERTS_PROC_GET_DELAYED_GC_TASK_QS(P) \
    ((ErtsProcSysTaskQs *) erts_psd_get((P), ERTS_PSD_DELAYED_GC_TASK_QS))
#define ERTS_PROC_SET_DELAYED_GC_TASK_QS(P, PBT) \
    ((ErtsProcSysTaskQs *) erts_psd_set((P), ERTS_PSD_DELAYED_GC_TASK_QS, (void *) (PBT)))

#define ERTS_PROC_GET_NFUNC_TRAP_WRAPPER(P) \
    ((ErtsNativeFunc*)erts_psd_get((P), ERTS_PSD_NFUNC_TRAP_WRAPPER))
#define ERTS_PROC_SET_NFUNC_TRAP_WRAPPER(P, NTE) \
    erts_psd_set((P), ERTS_PSD_NFUNC_TRAP_WRAPPER, (void *) (NTE))

#define ERTS_PROC_GET_DIST_ENTRY(P) \
    ((DistEntry *) erts_psd_get((P), ERTS_PSD_DIST_ENTRY))
#define ERTS_PROC_SET_DIST_ENTRY(P, DE) \
    ((DistEntry *) erts_psd_set((P), ERTS_PSD_DIST_ENTRY, (void *) (DE)))

#define ERTS_PROC_GET_TS_EVENT(P) \
    ((erts_tse_t *) erts_psd_get((P), ERTS_PSD_TS_EVENT))
#define ERTS_PROC_SET_TS_EVENT(P, TSE) \
    ((erts_tse_t *) erts_psd_set((P), ERTS_PSD_TS_EVENT, (void *) (TSE)))

#define ERTS_PROC_GET_PRIO_Q_INFO(P) \
    ((ErtsPrioQInfo *) erts_psd_get((P), ERTS_PSD_PRIO_Q_INFO))
#define ERTS_PROC_SET_PRIO_Q_INFO(P, PRIO_Q_INFO) \
    ((ErtsPrioQInfo *) erts_psd_set((P), ERTS_PSD_PRIO_Q_INFO, (void *) (PRIO_Q_INFO)))

#define ERTS_PROC_GET_PENDING_SUSPEND(P) \
    ((void *) erts_psd_get((P), ERTS_PSD_PENDING_SUSPEND))
#define ERTS_PROC_SET_PENDING_SUSPEND(P, PS) \
    ((void *) erts_psd_set((P), ERTS_PSD_PENDING_SUSPEND, (void *) (PS)))

#define ERTS_PROC_GET_CALL_MEMORY(P) \
    ((process_breakpoint_trace_t *) erts_psd_get((P), ERTS_PSD_CALL_MEMORY_BP))
#define ERTS_PROC_SET_CALL_MEMORY(P, PBT) \
    ((process_breakpoint_trace_t *) erts_psd_set((P), ERTS_PSD_CALL_MEMORY_BP, (void *) (PBT)))

ERTS_GLB_INLINE Eterm erts_proc_get_error_handler(Process *p);
ERTS_GLB_INLINE Eterm erts_proc_set_error_handler(Process *p, Eterm handler);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm
erts_proc_get_error_handler(Process *p)
{
    void *val = erts_psd_get(p, ERTS_PSD_ERROR_HANDLER);
    if (!val)
	return am_error_handler;
    else {
	ASSERT(is_atom(((Eterm) (UWord) val)));
	return (Eterm) (UWord) val;
    }
}

ERTS_GLB_INLINE Eterm
erts_proc_set_error_handler(Process *p, Eterm handler)
{
    void *old_val;
    void *new_val;
    ASSERT(is_atom(handler));
    new_val = (handler == am_error_handler) ? NULL : (void *) (UWord) handler;
    old_val = erts_psd_set(p, ERTS_PSD_ERROR_HANDLER, new_val);
    if (!old_val)
	return am_error_handler;
    else {
	ASSERT(is_atom(((Eterm) (UWord) old_val)));
	return (Eterm) (UWord) old_val;
    }
}

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS


#include "erl_thr_progress.h"

extern erts_atomic_t erts_migration_paths;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
int erts_get_sched_util(ErtsRunQueue *rq,
			int initially_locked,
			int short_interval);
#endif


ERTS_GLB_INLINE ErtsMigrationPaths *erts_get_migration_paths_managed(void);
ERTS_GLB_INLINE ErtsMigrationPaths *erts_get_migration_paths(void);
ERTS_GLB_INLINE ErtsRunQueue *erts_check_emigration_need(ErtsRunQueue *c_rq,
							 int prio);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsMigrationPaths *
erts_get_migration_paths_managed(void)
{
    return (ErtsMigrationPaths *) erts_atomic_read_ddrb(&erts_migration_paths);
}

ERTS_GLB_INLINE ErtsMigrationPaths *
erts_get_migration_paths(void)
{
    if (erts_thr_progress_is_managed_thread())
	return erts_get_migration_paths_managed();
    else
	return NULL;
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_check_emigration_need(ErtsRunQueue *c_rq, int prio)
{
    ErtsMigrationPaths *mps = erts_get_migration_paths();
    ErtsMigrationPath *mp;
    Uint32 flags;

    if (!mps)
	return NULL;

    mp = &mps->mpath[c_rq->ix];
    flags = mp->flags;

    if (ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, prio)) {
	int len;

	if (ERTS_CHK_RUNQ_FLG_EVACUATE(flags, prio)) {
	    /* force emigration */
	    return mp->prio[prio].runq;
	}

	if (flags & ERTS_RUNQ_FLG_INACTIVE) {
	    /*
	     * Run queue was inactive at last balance. Verify that
	     * it still is before forcing emigration.
	     */
	    if (ERTS_RUNQ_FLGS_GET(c_rq) & ERTS_RUNQ_FLG_INACTIVE)
		return mp->prio[prio].runq;
	}

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	if (mp->sched_util) {
	    ErtsRunQueue *rq = mp->prio[prio].runq;
	    /* No migration if other is non-empty */
	    if (!(ERTS_RUNQ_FLGS_GET(rq) & ERTS_RUNQ_FLG_NONEMPTY)
		&& erts_get_sched_util(rq, 0, 1) < mp->prio[prio].limit.other
		&& erts_get_sched_util(c_rq, 0, 1) > mp->prio[prio].limit.here) {
		return rq;
	    }
	}
	else
#endif
	{

	    if (prio == ERTS_PORT_PRIO_LEVEL)
		len = RUNQ_READ_LEN(&c_rq->ports.info.len);
	    else
		len = RUNQ_READ_LEN(&c_rq->procs.prio_info[prio].len);

	    if (len > mp->prio[prio].limit.here) {
		ErtsRunQueue *n_rq = mp->prio[prio].runq;
		if (n_rq) {
		    if (prio == ERTS_PORT_PRIO_LEVEL)
			len = RUNQ_READ_LEN(&n_rq->ports.info.len);
		    else
			len = RUNQ_READ_LEN(&n_rq->procs.prio_info[prio].len);

		    if (len < mp->prio[prio].limit.other)
			return n_rq;
		}
	    }
	}
    }
    return NULL;
}

#endif


#endif

ERTS_GLB_INLINE ErtsSchedulerData *erts_proc_sched_data(Process *c_p);
ERTS_GLB_INLINE int erts_is_scheduler_bound(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE Process *erts_get_current_process(void);
ERTS_GLB_INLINE Eterm erts_get_current_pid(void);
ERTS_GLB_INLINE Uint erts_get_scheduler_id(void);
ERTS_GLB_INLINE void erts_init_runq_proc(Process *p, ErtsRunQueue *rq, int bnd);
ERTS_GLB_INLINE ErtsRunQueue *erts_set_runq_proc(Process *p, ErtsRunQueue *rq, int *boundp);
ERTS_GLB_INLINE int erts_try_change_runq_proc(Process *p, ErtsRunQueue *rq);
ERTS_GLB_INLINE ErtsRunQueue *erts_bind_runq_proc(Process *p, int bind);
ERTS_GLB_INLINE int erts_proc_runq_is_bound(Process *p);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_proc(Process *p, int *boundp);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_current(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE void erts_runq_lock(ErtsRunQueue *rq);
ERTS_GLB_INLINE int erts_runq_trylock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_runq_unlock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);
ERTS_GLB_INLINE void erts_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);

ERTS_GLB_INLINE ErtsMessage *erts_alloc_message_heap_state(Process *pp,
							   erts_aint32_t *psp,
							   ErtsProcLocks *plp,
							   Uint sz,
							   Eterm **hpp,
							   ErlOffHeap **ohpp);
ERTS_GLB_INLINE ErtsMessage *erts_alloc_message_heap(Process *pp,
						     ErtsProcLocks *plp,
						     Uint sz,
						     Eterm **hpp,
						     ErlOffHeap **ohpp);

ERTS_GLB_INLINE void erts_shrink_message_heap(ErtsMessage **msgpp, Process *pp,
					      Eterm *start_hp, Eterm *used_hp, Eterm *end_hp,
					      Eterm *brefs, Uint brefs_size);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsSchedulerData *erts_proc_sched_data(Process *c_p)
{
    ErtsSchedulerData *esdp;
    ASSERT(c_p);
    esdp = c_p->scheduler_data;
    if (esdp) {
	ASSERT(esdp == erts_get_scheduler_data());
	ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
    }
    else {
	esdp = erts_get_scheduler_data();
	ASSERT(esdp);
	/*
	 * Not always true that we are on a dirty
	 * scheduler; we may be executing on
	 * behalf of another process...
	 *
	 * ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
	 */
    }
    ASSERT(esdp);
    return esdp;
}

ERTS_GLB_INLINE
int erts_is_scheduler_bound(ErtsSchedulerData *esdp)
{
    if (!esdp)
	esdp = erts_get_scheduler_data();
    ASSERT(esdp);
    return esdp->cpu_id >= 0;
}

ERTS_GLB_INLINE
Process *erts_get_current_process(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (!esdp)
        return NULL;
    if (esdp->current_process)
        return esdp->current_process;
    if (esdp->free_process)
        return esdp->free_process;
    return NULL;
}

ERTS_GLB_INLINE
Eterm erts_get_current_pid(void)
{
    Process *proc = erts_get_current_process();
    return proc ? proc->common.id : THE_NON_VALUE;
}

ERTS_GLB_INLINE
Uint erts_get_scheduler_id(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (esdp && ERTS_SCHEDULER_IS_DIRTY(esdp))
	return 0;
    else
	return esdp ? esdp->no : (Uint) 0;
}

/**
 * Init run-queue of process.
 *
 * @param p[in,out]     Process
 * @param rq[in]        Run-queue that process will be assigned to
 * @param bnd[in,out]   If non-zero binds process to run-queue.
 */

ERTS_GLB_INLINE void
erts_init_runq_proc(Process *p, ErtsRunQueue *rq, int bnd)
{
    erts_aint_t rqint = (erts_aint_t) rq;
    if (bnd)
        rqint |= ERTS_RUNQ_BOUND_FLAG;
    erts_atomic_init_nob(&p->run_queue, rqint);
}

/**
 * Forcibly set run-queue of process.
 *
 * @param p[in,out]     Process
 * @param rq[in]        Run-queue that process will be assigned to
 * @param bndp[in,out]  Pointer to integer. On input non-zero
 *                      value causes the process to be bound to
 *                      the run-queue. On output, indicating
 *                      whether process previously was bound or
 *                      not.
 * @return              Previous run-queue.
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_set_runq_proc(Process *p, ErtsRunQueue *rq, int *bndp)
{
    erts_aint_t rqint = (erts_aint_t) rq;
    ASSERT(bndp);
    ASSERT(rq);
    if (*bndp)
        rqint |= ERTS_RUNQ_BOUND_FLAG;
    rqint = erts_atomic_xchg_nob(&p->run_queue, rqint);
    *bndp = (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
    return (ErtsRunQueue *) (rqint & ERTS_RUNQ_POINTER_MASK);
}

/**
 * Try to change run-queue assignment of a process.
 *
 * @param p[in,out]     Process
 * @param rq[int]       Run-queue that process will be assigned to
 * @return              Non-zero if the run-queue assignment was
 *                      successfully changed.
 */

ERTS_GLB_INLINE int
erts_try_change_runq_proc(Process *p, ErtsRunQueue *rq)
{
    erts_aint_t old_rqint, new_rqint;

    ASSERT(rq);

    new_rqint = (erts_aint_t) rq;
    old_rqint = (erts_aint_t) erts_atomic_read_nob(&p->run_queue);
    while (1) {
        erts_aint_t act_rqint;

        if (old_rqint & ERTS_RUNQ_BOUND_FLAG)
            return 0;

        act_rqint = erts_atomic_cmpxchg_nob(&p->run_queue,
                                            new_rqint,
                                            old_rqint);
        if (act_rqint == old_rqint)
            return !0;

        old_rqint = act_rqint;
    }
}

/**
 *
 * Bind or unbind process to/from currently used run-queue.
 *
 * @param p             Process
 * @param bind          Bind if non-zero; otherwise unbind
 * @return              Pointer to previously bound run-queue,
 *                      or NULL if previously unbound
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_bind_runq_proc(Process *p, int bind)
{
    erts_aint_t rqint;
    if (bind)
        rqint = erts_atomic_read_bor_nob(&p->run_queue,
                                         ERTS_RUNQ_BOUND_FLAG);
    else
        rqint = erts_atomic_read_band_nob(&p->run_queue,
                                          ~ERTS_RUNQ_BOUND_FLAG);
    if (rqint & ERTS_RUNQ_BOUND_FLAG)
        return (ErtsRunQueue *) (rqint & ERTS_RUNQ_POINTER_MASK);
    else
        return NULL;
}

/**
 * Determine whether a process is bound to a run-queue or not.
 *
 * @return              Returns a non-zero value if bound,
 *                      and zero of not bound.
 */

ERTS_GLB_INLINE int
erts_proc_runq_is_bound(Process *p)
{
    erts_aint_t rqint = erts_atomic_read_nob(&p->run_queue);
    return (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
}

/**
 * Set run-queue of process.
 *
 * @param p[in,out]     Process
 * @param bndp[out]     Pointer to integer. If non-NULL pointer,
 *                      the integer will be set to a non-zero
 *                      value if the process is bound to the
 *                      run-queue.
 * @return              Pointer to the normal run-queue that
 *                      the process currently is assigned to.
 *                      A process is always assigned to a
 *                      normal run-queue.
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_proc(Process *p, int *bndp)
{
    erts_aint_t rqint = erts_atomic_read_nob(&p->run_queue);
    ErtsRunQueue *rq;
    if (bndp)
        *bndp = (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
    rqint &= ERTS_RUNQ_POINTER_MASK;
    rq = (ErtsRunQueue *) rqint;
    ASSERT(rq);
    return rq;
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_current(ErtsSchedulerData *esdp)
{
    ASSERT(!esdp || esdp == erts_get_scheduler_data());
    if (!esdp)
	esdp = erts_get_scheduler_data();
    return esdp->run_queue;
}

ERTS_GLB_INLINE void
erts_runq_lock(ErtsRunQueue *rq)
{
    erts_mtx_lock(&rq->mtx);
}

ERTS_GLB_INLINE int
erts_runq_trylock(ErtsRunQueue *rq)
{
    return erts_mtx_trylock(&rq->mtx);
}

ERTS_GLB_INLINE void
erts_runq_unlock(ErtsRunQueue *rq)
{
    erts_mtx_unlock(&rq->mtx);
}

ERTS_GLB_INLINE void
erts_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&rq->mtx));
    if (xrq != rq) {
	if (erts_mtx_trylock(&xrq->mtx) == EBUSY) {
	    if (rq < xrq)
		erts_mtx_lock(&xrq->mtx);
	    else {
		erts_mtx_unlock(&rq->mtx);
		erts_mtx_lock(&xrq->mtx);
		erts_mtx_lock(&rq->mtx);
	    }
	}
    }
}

ERTS_GLB_INLINE void
erts_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
    if (xrq != rq)
	erts_mtx_unlock(&xrq->mtx);
}

ERTS_GLB_INLINE void
erts_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
    ASSERT(rq1 && rq2);
    if (rq1 == rq2)
	erts_mtx_lock(&rq1->mtx);
    else if (rq1 < rq2) {
	erts_mtx_lock(&rq1->mtx);
	erts_mtx_lock(&rq2->mtx);
    }
    else {
	erts_mtx_lock(&rq2->mtx);
	erts_mtx_lock(&rq1->mtx);
    }
}

ERTS_GLB_INLINE void
erts_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
    ASSERT(rq1 && rq2);
    erts_mtx_unlock(&rq1->mtx);
    if (rq1 != rq2)
	erts_mtx_unlock(&rq2->mtx);
}

ERTS_GLB_INLINE ErtsMessage *
erts_alloc_message_heap_state(Process *pp,
			      erts_aint32_t *psp,
			      ErtsProcLocks *plp,
			      Uint sz,
			      Eterm **hpp,
			      ErlOffHeap **ohpp)
{
    int on_heap;
    ErtsMessage *mp;

    if ((*psp) & ERTS_PSFLG_OFF_HEAP_MSGQ) {
	mp = erts_alloc_message(sz, hpp);
	*ohpp = sz == 0 ? NULL : &mp->hfrag.off_heap;
	return mp;
    }

    mp = erts_try_alloc_message_on_heap(pp, psp, plp, sz, hpp, ohpp, &on_heap);
    ASSERT(pp || !on_heap);
    return mp;
}

ERTS_GLB_INLINE ErtsMessage *
erts_alloc_message_heap(Process *pp,
			ErtsProcLocks *plp,
			Uint sz,
			Eterm **hpp,
			ErlOffHeap **ohpp)
{
    erts_aint32_t state = pp ? erts_atomic32_read_nob(&pp->state) : 0;
    return erts_alloc_message_heap_state(pp, &state, plp, sz, hpp, ohpp);
}

ERTS_GLB_INLINE void
erts_shrink_message_heap(ErtsMessage **msgpp, Process *pp,
			 Eterm *start_hp, Eterm *used_hp, Eterm *end_hp,
			 Eterm *brefs, Uint brefs_size)
{
    ASSERT(start_hp <= used_hp && used_hp <= end_hp);
    if ((*msgpp)->data.attached == ERTS_MSG_COMBINED_HFRAG)
	*msgpp = erts_shrink_message(*msgpp, used_hp - start_hp,
				     brefs, brefs_size);
    else if (!(*msgpp)->data.attached) {
	ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   & erts_proc_lc_my_proc_locks(pp));
	HRelease(pp, end_hp, used_hp);
    }
    else {
	ErlHeapFragment *hfrag = (*msgpp)->data.heap_frag;
	if (start_hp != used_hp)
	    hfrag = erts_resize_message_buffer(hfrag, used_hp - start_hp,
					       brefs, brefs_size);
	else {
	    free_message_buffer(hfrag);
	    hfrag = NULL;
	}
	(*msgpp)->data.heap_frag = hfrag;
    }
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

ERTS_GLB_INLINE ErtsAtomCacheMap *erts_get_atom_cache_map(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE ErtsAtomCacheMap *
erts_get_atom_cache_map(Process *c_p)
{
    ErtsSchedulerData *esdp = (c_p
			       ? erts_proc_sched_data(c_p)
			       : erts_get_scheduler_data());
    ASSERT(esdp);
    return &esdp->atom_cache_map;
}
#endif

#ifdef __WIN32__
/*
 * Don't want erts_time2reds() inlined in beam_emu.c on windows since
 * it is compiled with gcc which fails on it. Implementation is in
 * erl_process.c on windows.
 */
#  define ERTS_TIME2REDS_IMPL__ erts_time2reds__
Sint64 erts_time2reds(ErtsMonotonicTime start, ErtsMonotonicTime end);
#else
#  define ERTS_TIME2REDS_IMPL__ erts_time2reds
#endif

ERTS_GLB_INLINE Sint64 ERTS_TIME2REDS_IMPL__(ErtsMonotonicTime start,
					     ErtsMonotonicTime end);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Sint64
ERTS_TIME2REDS_IMPL__(ErtsMonotonicTime start, ErtsMonotonicTime end)
{
    ErtsMonotonicTime time = end - start;
    ASSERT(time >= 0);
    time = ERTS_MONOTONIC_TO_USEC(time);
    if (time == 0)
	return (Sint64) 1; /* At least one reduction */
    /* Currently two reductions per micro second */
    time *= (CONTEXT_REDS-1)/1000 + 1;
    return (Sint64) time;
}
#endif

Process *erts_try_lock_sig_free_proc(Eterm pid,
                                     ErtsProcLocks locks,
                                     erts_aint32_t *statep);

#ifdef DEBUG
#define ERTS_ASSERT_IS_NOT_EXITING(P) \
    do { ASSERT(!ERTS_PROC_IS_EXITING((P))); } while (0)
#else
#define ERTS_ASSERT_IS_NOT_EXITING(P)
#endif


#define ERTS_PROC_IS_EXITING(P) \
    (ERTS_PSFLG_EXITING & erts_atomic32_read_acqb(&(P)->state))


/* Minimum NUMBER of processes for a small system to start */
#define ERTS_MIN_PROCESSES		1024
#if ERTS_MIN_PROCESSES < ERTS_NO_OF_PIX_LOCKS
#undef ERTS_MIN_PROCESSES
#define ERTS_MIN_PROCESSES		ERTS_NO_OF_PIX_LOCKS
#endif

void erts_notify_inc_runq(ErtsRunQueue *runq);

void erts_sched_finish_poke(ErtsSchedulerSleepInfo *, erts_aint32_t);
ERTS_GLB_INLINE void erts_sched_poke(ErtsSchedulerSleepInfo *ssi);
void erts_aux_thread_poke(void);
ERTS_GLB_INLINE Uint32 erts_sched_local_random_hash_64_to_32_shift(Uint64 key);
ERTS_GLB_INLINE Uint32 erts_sched_local_random(Uint additional_seed);
#ifdef DEBUG
ERTS_GLB_INLINE float erts_sched_local_random_float(Uint additional_seed);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_sched_poke(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t flags;
    ERTS_THR_MEMORY_BARRIER;
    flags = erts_atomic32_read_nob(&ssi->flags);
    if (flags & ERTS_SSI_FLG_SLEEPING) {
	flags = erts_atomic32_read_band_nob(&ssi->flags, ~ERTS_SSI_FLGS_SLEEP);
	erts_sched_finish_poke(ssi, flags);
    }
}


/*
 * Source: https://gist.github.com/badboy/6267743
 *         http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm
 */
ERTS_GLB_INLINE
Uint32 erts_sched_local_random_hash_64_to_32_shift(Uint64 key)
{
    key = (~key) + (key << 18); /* key = (key << 18) - key - 1; */
    key = key ^ (key >> 31);
    key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (Uint32) key;
}

/*
 * This function attempts to return a random number based on the state
 * of the scheduler and the additional_seed parameter.
 */
ERTS_GLB_INLINE
Uint32 erts_sched_local_random(Uint additional_seed)
{
    extern erts_atomic_t erts_sched_local_random_nosched_state;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    Uint64 rand_state;

    if(ERTS_UNLIKELY(esdp == NULL)) {
        rand_state = erts_atomic_inc_read_nob(&erts_sched_local_random_nosched_state);
    } else {
        rand_state = esdp->rand_state++;
    }
    return erts_sched_local_random_hash_64_to_32_shift(rand_state
                                                       + additional_seed);
}

#ifdef DEBUG

/*
 * This function returns a random float between 0.0 and 1.0.
 */
ERTS_GLB_INLINE
float erts_sched_local_random_float(Uint additional_seed)
{
    Uint32 rnd = erts_sched_local_random(additional_seed);
    return (float)(((double)rnd)/((double)ERTS_UINT32_MAX));
}

#endif /* #ifdef DEBUG */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#include "erl_process_lock.h"

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS

#endif


void erts_halt(int code, ErtsMonotonicTime tmo);
extern erts_atomic32_t erts_halt_progress;
extern int erts_halt_code;

extern Eterm
erts_build_stacktrace(ErtsHeapFactory *hfact, Process* rp,
                      Uint reserve_size, int max_depth, int include_i);
