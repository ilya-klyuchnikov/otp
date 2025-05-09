<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# relup

Release upgrade file

## Description

The _release upgrade file_ describes how a release is upgraded in a running
system.

This file is automatically generated by
[`systools:make_relup/3,4`](`systools:make_relup/3`), using a release resource
file (`.rel`), application resource files (`.app`), and application upgrade
files (`.appup`) as input.

## File Syntax

In a target system, the release upgrade file is to be located in directory
`$ROOT/releases/Vsn`.

The `relup` file contains one single Erlang term, which defines the instructions
used to upgrade the release. The file has the following syntax:

```c
{Vsn,
  [{UpFromVsn, Descr, Instructions}, ...],
  [{DownToVsn, Descr, Instructions}, ...]}.
```

- **`Vsn = string()`** - Current release version.

- **`UpFromVsn = string()`** - Earlier version of the release to upgrade from.

- **`Descr = term()`** - A user-defined parameter passed from the function
  [`systools:make_relup/3,4`](`systools:make_relup/3`). It is used in the return
  value of
  [`release_handler:install_release/1,2`](`release_handler:install_release/1`).

- **`Instructions`** - A list of low-level release upgrade instructions, see
  [`appup(4)`](appup.md). It consists of the release upgrade instructions from
  the respective application upgrade files (high-level instructions are
  translated to low-level instructions), in the same order as in the start
  script.

- **`DownToVsn = string()`** - Earlier version of the release to downgrade to.

## See Also

[`app(4)`](`e:kernel:app.md`), [`appup(4)`](appup.md), [`rel(4)`](rel.md),
`m:release_handler`, `m:systools`
