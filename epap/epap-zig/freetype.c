#define FT2_BUILD_LIBRARY 1

#include <ft2build.h>
#include <freetype/freetype.h>

#define FTOPTION_H_
#define FT_MAX_MODULES 32
#define T1_MAX_SUBRS_CALLS 16
#define T1_MAX_CHARSTRINGS_OPERANDS 256
#define FT_CONFIG_OPTION_ERROR_STRINGS
#define TT_CONFIG_CMAP_FORMAT_0
#define TT_CONFIG_CMAP_FORMAT_2
#define TT_CONFIG_CMAP_FORMAT_4
#define TT_CONFIG_CMAP_FORMAT_6
#define TT_CONFIG_CMAP_FORMAT_8
#define TT_CONFIG_CMAP_FORMAT_10
#define TT_CONFIG_CMAP_FORMAT_12
#define TT_CONFIG_CMAP_FORMAT_13
#define TT_CONFIG_CMAP_FORMAT_14

#define FT_CONFIG_MODULES_H <freetype-modules.h>

#include <base/ftbase.c>
#include <base/ftinit.c>
#include <base/ftsystem.c>
#include <base/ftbitmap.c>
#include <base/ftbbox.c>
#include <truetype/truetype.c>
#include <cff/cff.c>
#include <cid/type1cid.c>
#include <psnames/psmodule.c>
#include <psaux/psaux.c>
#include <pshinter/pshinter.c>
#include <sfnt/sfnt.c>
#include <bdf/bdf.c>
#include <raster/raster.c>
#include <gzip/ftgzip.c>
#include <autofit/autofit.c>