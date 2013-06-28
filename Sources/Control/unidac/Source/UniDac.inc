
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

// Type of build

{$I Dac.inc}

{$IFDEF TRIAL}            // TRIAL
  {$DEFINE _TRIAL_T}      // TRIAL_T
{$ENDIF}

{$DEFINE _DEBUG}          // DEBUG

{$IFDEF LINUX}
  {$J+}                   // allows to write typed constants
{$ENDIF}
