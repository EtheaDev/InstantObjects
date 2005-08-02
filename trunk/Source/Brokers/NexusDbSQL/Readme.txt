NexusDbSQL Brokers for InstantObjects
Steven Mitchell - 29 Jul 2005

To use these brokers with NexusDb V1 set the compiler conditional define of "NX1" in the 'InstantNxDbDefines.inc' file and (re)build all of the NexusDb related broker packages.

To use these brokers with NexusDb V2 comment the compiler conditional define of "NX1" in the 'InstantNxDbDefines.inc' file and (re)build all of the NexusDb related broker packages.

The current default is to compile for NexusDb V2.

Note: Minimal testing has been done with NexusDb V2 as this version has only recently been released.