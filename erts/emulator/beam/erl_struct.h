/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024. All Rights Reserved.
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

#ifndef __ERL_STRUCT_H__
#define __ERL_STRUCT_H__

#include "sys.h"
#include "code_ix.h"

/* Struct entry, these are always allocated with an alignment that allows them
 * to be IMMED1-tagged, letting us keep struct definitions as transparent heap
 * objects much like tuples. */
typedef struct {
    Eterm module;
    Eterm name;
    Uint arity;

    /* Literal-tagged pointers to the canonical ErtsStructDefinition for each
     * code generation. */
    Eterm definitions[ERTS_ADDRESSV_SIZE];
} ErtsStructEntry;

/* Struct definitions are tagged as tuples to simplify the GC. They should
 * never be presented to the user. */
typedef struct {
    Eterm thing_word;

    /* Small-tagged ErtsStructEntry* */
    Eterm entry;

    struct {
        Eterm key;
        Eterm value;
    } fields[];
} ErtsStructDefinition;

/* Struct objects on the heap have the following structure:
 *
 * [MAKE_STRUCT_HEADER(FieldCount), struct definition, values ...]*/

void erts_struct_init_table(void);

ERTS_GLB_INLINE ErtsStructEntry *erts_struct_active_entry(Eterm module,
                                                     Eterm name,
                                                     Uint arity);

ErtsStructEntry *erts_struct_put(Eterm module,
                                 Eterm name,
                                 Uint arity);
ErtsStructEntry *erts_struct_get_or_make_stub(Eterm module,
                                              Eterm name,
                                              Uint arity);

void erts_struct_start_staging(void);
void erts_struct_end_staging(int commit);

extern erts_mtx_t struct_staging_lock;
#define erts_struct_staging_lock()   erts_mtx_lock(&struct_staging_lock)
#define erts_struct_staging_unlock() erts_mtx_unlock(&struct_staging_lock)

/* ************************************************************************* */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsStructEntry*
erts_struct_active_entry(Eterm module, Eterm name, Uint arity)
{
    extern ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                                   Eterm name,
                                                   Uint arity,
                                                   ErtsCodeIndex code_ix);
    return erts_struct_find_entry(module, name, arity, erts_active_code_ix());
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __ERL_STRUCT_H__ */

