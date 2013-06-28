
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////


{$I Dac.inc}

{$IFDEF TRIAL}            // TRIAL
  {$DEFINE _TRIAL_T}      // TRIAL_T
{$ENDIF}

{$DEFINE _DEBUG}          // DEBUG

{$IFDEF LINUX}
  {$J+}                   // allows to write typed constants
{$ENDIF}
