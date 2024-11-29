Misdeed version 0.1.0 alpha, by vfig.

Usage:

    misdeed.exe fixup_cell_lights <input.mis> -o <output.mis>

What it does:

  - Trims cell lights for a perf boost.
  - Fixes bad object lighting, where an object would be too dark but look
    right if you walked into it.

This should only be run as a post-process on a .mis, before testing/release, as
the fixes that this tool applies are wiped out whenever you rebuild lighting in
dromed.

Credit to Jayrude for discovering the perf boost, and consulting with me when
implementing it. And hopefully for making this tool obsolete soon, too :)
