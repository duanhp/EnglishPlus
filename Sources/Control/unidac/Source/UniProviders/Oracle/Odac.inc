
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

// Type of build

{$I Dac.inc}

{$IFDEF TRIAL}            // TRIAL
  {$DEFINE _TRIAL_T}      // TRIAL_T
{$ENDIF}

{$IFNDEF NONET}
{$IFNDEF FPC}
  {$DEFINE NET}           // NET
{$ENDIF}
{$ENDIF}

{$DEFINE _DEBUG}          // DEBUG

{$IFDEF MSWINDOWS}
  {$DEFINE USE_ORAMEMO}   // Use TOraMemo if it is possible

  {$IFNDEF FPC}
    {$DEFINE ORATOOLS}      // include OraToolsClient unit and related code
  {$ENDIF}
  {$IFNDEF STD}
    {$IFNDEF FPC}
      {$DEFINE SMART_REFRESH}
      {$IFNDEF WIN64}
        {$DEFINE BDESESSION}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
  {$J+}                   // allows to write typed constants
{$ENDIF}

{$UNDEF HAVE_COMPRESS_INTERNAL}
