/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2011-2025. All Rights Reserved.
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

/*
 * Description: Memory barriers for x86/x86-64
 * Author: Rickard Green
 */

#ifndef ETHR_X86_MEMBAR_H__
#define ETHR_X86_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

#ifdef ETHR_X86_RUNTIME_CONF__
#define ETHR_INSTRUCTION_BARRIER ethr_instruction_fence__()

static __inline__ __attribute__((__always_inline__)) void
ethr_instruction_fence__(void)
{
    ETHR_ASSERT(ETHR_X86_RUNTIME_CONF_HAVE_CPUID__);

#if ETHR_SIZEOF_PTR == 4
__asm__ __volatile__("cpuid\n\t" :: "a"(0) : "ebx", "ecx", "edx", "memory");
#else
__asm__ __volatile__("cpuid\n\t" :: "a"(0) : "rbx", "rcx", "rdx", "memory");
#endif
}
#else
/* !! Note that we DO NOT define a fallback !!
 *
 * See the definition of ERTS_THR_INSTRUCTION_BARRIER in erl_threads.h for
 * details. */
#endif

#define ETHR_NO_SSE2_MEMORY_BARRIER__			\
do {							\
    volatile ethr_sint32_t x__ = 0;			\
    __asm__ __volatile__ ("lock; orl $0x0, %0\n\t"	\
			  : "=m"(x__)			\
			  : "m"(x__)			\
			  : "memory");			\
} while (0)

static __inline__ void
ethr_cfence__(void)
{
    __asm__ __volatile__ ("" : : : "memory");
}

static __inline__ void
ethr_mfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MEMORY_BARRIER__;
    else
#endif
	__asm__ __volatile__ ("mfence\n\t" : : : "memory");
}

static __inline__ void
ethr_sfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MEMORY_BARRIER__;
    else
#endif
	__asm__ __volatile__ ("sfence\n\t" : : : "memory");
}

static __inline__ void
ethr_lfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MEMORY_BARRIER__;
    else
#endif
	__asm__ __volatile__ ("lfence\n\t" : : : "memory");
}

#define ETHR_X86_OUT_OF_ORDER_MEMBAR(B)				\
  ETHR_CHOOSE_EXPR((B) == ETHR_StoreStore,			\
		   ethr_sfence__(),				\
		   ETHR_CHOOSE_EXPR((B) == ETHR_LoadLoad,	\
				    ethr_lfence__(),		\
				    ethr_mfence__()))

#ifdef ETHR_X86_OUT_OF_ORDER

#define ETHR_MEMBAR(B) \
  ETHR_X86_OUT_OF_ORDER_MEMBAR((B))

#else /* !ETHR_X86_OUT_OF_ORDER (the default) */

/*
 * We assume that only stores before loads may be reordered. That is,
 * we assume that *no* instructions like these are used:
 * - CLFLUSH,
 * - streaming stores executed with non-temporal move,
 * - string operations, or
 * - other instructions which aren't LoadLoad, LoadStore, and StoreStore
 *   ordered by themselves
 * If such instructions are used, either insert memory barriers
 * using ETHR_X86_OUT_OF_ORDER_MEMBAR() at appropriate places, or
 * define ETHR_X86_OUT_OF_ORDER. For more info see Intel 64 and IA-32
 * Architectures Software Developer's Manual; Vol 3A; Chapter 8.2.2.
 */

#define ETHR_MEMBAR(B) \
  ETHR_CHOOSE_EXPR((B) & ETHR_StoreLoad, ethr_mfence__(), ethr_cfence__())

#endif /* !ETHR_X86_OUT_OF_ORDER */

#endif /* ETHR_X86_MEMBAR_H__ */
