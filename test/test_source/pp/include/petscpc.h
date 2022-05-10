#if !defined (PETSCPCDEF_H)
#define PETSCPCDEF_H

#include "petscerror.h"

#define PC type(tPC)
#define PCType character*(80)
#define ewrite(priority, format) if (priority <= 3) write((priority), format)
#define ewrite2(priority, format) \
    if (priority <= 3) write((priority), format)
#define varVar \
        55
#endif
