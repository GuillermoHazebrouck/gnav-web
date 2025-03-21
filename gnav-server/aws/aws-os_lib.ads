------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Style_Checks ("M32766");
--  Allow long lines

--  This package provides target dependent definitions of constant/types for
--  use by the AWS library. This package should not be directly with'd
--  by an application program.

--  This file is generated automatically, do not modify it by hand! Instead,
--  make changes to aws-os_lib-tmplt.c and rebuild the AWS library.
--  This is the version for i686-pc-linux-gnu

with Interfaces.C.Strings;
with System;
with GNAT.OS_Lib;

package AWS.OS_Lib is

   use Interfaces;

   ---------------------------------
   -- General platform parameters --
   ---------------------------------

   type OS_Type is (Windows, VMS, Other_OS);
   Target_OS            : constant OS_Type := Other_OS;
   pragma Warnings (Off, Target_OS);
   --  Suppress warnings on Target_OS since it is in general tested for
   --  equality with a constant value to implement conditional compilation,
   --  which normally generates a constant condition warning.

   Target_Name          : constant String  := "i686-pc-linux-gnu";

   Executable_Extension : constant String    := "";
   Directory_Separator  : constant Character := '/';
   Path_Separator       : constant Character := ':';

   --  Sizes of various data types

   SIZEOF_unsigned_int  : constant := 4;     --  Size of unsigned int
   SIZEOF_fd_set        : constant := 128;   --  fd_set
   FD_SETSIZE           : constant := 1024;  --  Max fd value
   SIZEOF_sin_family    : constant := 16;    --  Size of sa.sin_family
   SIZEOF_nfds_t        : constant := 32;    --  Size of nfds_t
   SIZEOF_pollfd_events : constant := 16;    --  Size of pollfd.events
   SIZEOF_fd_type       : constant := 32;    --  Size of socket fd
   SIZEOF_socklen_t     : constant := 32;    --  Size of socklen_t

   --  Sizes (in bytes) of the components of struct timeval
   SIZEOF_tv_sec        : constant := 32;    --  tv_sec
   SIZEOF_tv_usec       : constant := 32;    --  tv_usec

   --  Poll values

   POLLIN               : constant := 1;     --  There is data to read
   POLLPRI              : constant := 2;     --  Urgent data to read
   POLLOUT              : constant := 4;     --  Writing will not block
   POLLERR              : constant := 8;     --  Error (output only)
   POLLHUP              : constant := 16;    --  Hang up (output only)
   POLLNVAL             : constant := 32;    --  Invalid request

   -----------------
   -- Fcntl flags --
   -----------------

   FNDELAY              : constant := 2048;  --  Nonblocking

   ----------------------
   -- Ioctl operations --
   ----------------------

   FIONBIO              : constant := 21537; --  Set/clear non-blocking io
   FIONREAD             : constant := 21531; --  How many bytes to read
   FIONWRITE            : constant := 21521; --  How many bytes in the send queue
   FIONSPACE            : constant := -1;    --  Free space in the send queue

   ---------------------------------------
   -- getaddrinfo getnameinfo constants --
   ---------------------------------------

   AI_PASSIVE           : constant := 1;     --  NULL nodename for accepting
   AI_CANONNAME         : constant := 2;     --  Get the host official name
   AI_NUMERICSERV       : constant := 1024;  --  Service is a numeric string
   AI_NUMERICHOST       : constant := 4;     --  Node is a numeric IP address
   EAI_SYSTEM           : constant := -11;   --  Check errno for details
   NI_NUMERICHOST       : constant := 1;     --  Numeric form of the hostname

   ------------------
   -- Errno values --
   ------------------

   --  The following constants are defined from <errno.h>

   EAGAIN               : constant := 11;    --  Try again
   ENOENT               : constant := 2;     --  File not found
   ENOMEM               : constant := 12;    --  Out of memory
   EADDRNOTAVAIL        : constant := 99;    --  Cannot assign address
   EINPROGRESS          : constant := 115;   --  Operation now in progress
   EINTR                : constant := 4;     --  Interrupted system call
   EINVAL               : constant := 22;    --  Invalid argument
   ENAMETOOLONG         : constant := 36;    --  Name too long
   ENOBUFS              : constant := 105;   --  No buffer space available
   ENOTCONN             : constant := 107;   --  Socket not connected
   ESHUTDOWN            : constant := 108;   --  Cannot send once shutdown
   ESOCKTNOSUPPORT      : constant := 94;    --  Socket type not supported
   ETIMEDOUT            : constant := 110;   --  Connection timed out
   ETOOMANYREFS         : constant := 109;   --  Too many references
   EWOULDBLOCK          : constant := 11;    --  Operation would block
   ECONNRESET           : constant := 104;   --  Connection reset by peer
   EACCES               : constant := 13;    --  Permission denied
   EADDRINUSE           : constant := 98;    --  Address already in use
   EAFNOSUPPORT         : constant := 97;    --  Addr family not supported
   EALREADY             : constant := 114;   --  Operation in progress
   EBADF                : constant := 9;     --  Bad file descriptor
   ECONNABORTED         : constant := 103;   --  Connection aborted
   ECONNREFUSED         : constant := 111;   --  Connection refused
   EDESTADDRREQ         : constant := 89;    --  Destination addr required
   EFAULT               : constant := 14;    --  Bad address
   EHOSTDOWN            : constant := 112;   --  Host is down
   EHOSTUNREACH         : constant := 113;   --  No route to host
   EIO                  : constant := 5;     --  Input output error
   EISCONN              : constant := 106;   --  Socket already connected
   ELOOP                : constant := 40;    --  Too many symbolic links
   EMFILE               : constant := 24;    --  Too many open files
   EMSGSIZE             : constant := 90;    --  Message too long
   EPIPE                : constant := 32;    --  Broken pipe
   EPFNOSUPPORT         : constant := 96;    --  Unknown protocol family
   EPROTONOSUPPORT      : constant := 93;    --  Unknown protocol
   EPROTOTYPE           : constant := 91;    --  Unknown protocol type
   ENETDOWN             : constant := 100;   --  Network is down
   ENETRESET            : constant := 102;   --  Disconn. on network reset
   ENETUNREACH          : constant := 101;   --  Network is unreachable
   ENOPROTOOPT          : constant := 92;    --  Protocol not available
   ENOTSOCK             : constant := 88;    --  Operation on non socket
   EOPNOTSUPP           : constant := 95;    --  Operation not supported

   --------------
   -- Families --
   --------------

   AF_INET              : constant := 2;     --  IPv4 address family
   AF_INET6             : constant := 10;    --  IPv6 address family
   AF_UNSPEC            : constant := 0;     --  Unspecified address family

   ------------------
   -- Socket modes --
   ------------------

   SOCK_STREAM          : constant := 1;     --  Stream socket
   SOCK_DGRAM           : constant := 2;     --  Datagram socket

   -----------------
   -- Host errors --
   -----------------

   HOST_NOT_FOUND       : constant := 1;     --  Unknown host
   TRY_AGAIN            : constant := 2;     --  Host name lookup failure
   NO_DATA              : constant := 4;     --  No data record for name
   NO_RECOVERY          : constant := 3;     --  Non recoverable errors

   --------------------
   -- Shutdown modes --
   --------------------

   SHUT_RD              : constant := 0;     --  No more recv
   SHUT_WR              : constant := 1;     --  No more send
   SHUT_RDWR            : constant := 2;     --  No more recv/send

   ---------------------
   -- Protocol levels --
   ---------------------

   SOL_SOCKET           : constant := 1;     --  Options for socket level
   IPPROTO_IP           : constant := 0;     --  Dummy protocol for IP
   IPPROTO_IPV6         : constant := 41;    --  IPv6 socket option level
   IPPROTO_UDP          : constant := 17;    --  UDP
   IPPROTO_TCP          : constant := 6;     --  TCP

   -------------------
   -- Request flags --
   -------------------

   MSG_OOB              : constant := 1;     --  Process out-of-band data
   MSG_PEEK             : constant := 2;     --  Peek at incoming data
   MSG_EOR              : constant := 128;   --  Send end of record
   MSG_WAITALL          : constant := 256;   --  Wait for full reception
   MSG_NOSIGNAL         : constant := 16384; --  No SIGPIPE on send
   MSG_Forced_Flags     : constant := MSG_NOSIGNAL;
   --  Flags set on all send(2) calls

   --------------------
   -- Socket options --
   --------------------

   TCP_NODELAY          : constant := 1;     --  Do not coalesce packets
   SO_REUSEADDR         : constant := 2;     --  Bind reuse local address
   SO_KEEPALIVE         : constant := 9;     --  Enable keep-alive msgs
   SO_LINGER            : constant := 13;    --  Defer close to flush data
   SO_BROADCAST         : constant := 6;     --  Can send broadcast msgs
   SO_SNDBUF            : constant := 7;     --  Set/get send buffer size
   SO_RCVBUF            : constant := 8;     --  Set/get recv buffer size
   SO_SNDTIMEO          : constant := 21;    --  Emission timeout
   SO_RCVTIMEO          : constant := 20;    --  Reception timeout
   SO_ERROR             : constant := 4;     --  Get/clear error status
   IP_MULTICAST_IF      : constant := 32;    --  Set/get mcast interface
   IP_MULTICAST_TTL     : constant := 33;    --  Set/get multicast TTL
   IP_MULTICAST_LOOP    : constant := 34;    --  Set/get mcast loopback
   IP_ADD_MEMBERSHIP    : constant := 35;    --  Join a multicast group
   IP_DROP_MEMBERSHIP   : constant := 36;    --  Leave a multicast group
   IP_PKTINFO           : constant := 8;     --  Get datagram info
   IPV6_V6ONLY          : constant := 26;    --  Restricted to IPv6 communications only

   --  Some types

   type nfds_t is mod 2 ** SIZEOF_nfds_t;
   for nfds_t'Size use SIZEOF_nfds_t;

   type FD_Type
     is range -(2 ** (SIZEOF_fd_type - 1)) .. 2 ** (SIZEOF_fd_type - 1) - 1;
   for FD_Type'Size use SIZEOF_fd_type;

   type Events_Type is mod 2 ** SIZEOF_pollfd_events;
   for Events_Type'Size use SIZEOF_pollfd_events;

   type socklen_t is mod 2 ** SIZEOF_socklen_t;
   for socklen_t'Size use SIZEOF_socklen_t;

   type timeval_tv_sec_t
     is range -(2 ** (SIZEOF_tv_sec - 1)) .. 2 ** (SIZEOF_tv_sec - 1) - 1;
   for timeval_tv_sec_t'Size use SIZEOF_tv_sec;

   type timeval_tv_usec_t
     is range -(2 ** (SIZEOF_tv_usec - 1)) .. 2 ** (SIZEOF_tv_usec - 1) - 1;
   for timeval_tv_usec_t'Size use SIZEOF_tv_usec;

   type Timeval is record
      tv_sec  : timeval_tv_sec_t;  -- Seconds
      tv_usec : timeval_tv_usec_t; -- Microseconds
   end record;
   pragma Convention (C, Timeval);

   type sa_family_t is mod 2 ** SIZEOF_sin_family;
   for sa_family_t'Size use SIZEOF_sin_family;

   type In6_Addr is array (1 .. 8) of Interfaces.Unsigned_16;
   pragma Convention (C, In6_Addr);

   type Sockaddr_In6 is record
      Family   : sa_family_t := 0;
      Port     : Interfaces.C.unsigned_short := 0;
      FlowInfo : Interfaces.Unsigned_32 := 0;
      Addr     : In6_Addr := (others => 0);
      Scope_Id : Interfaces.Unsigned_32 := 0;
   end record;
   pragma Convention (C, Sockaddr_In6);

   type Addr_Info;
   type Addr_Info_Access is access all Addr_Info;

   type Addr_Info is record
      ai_flags     : C.int;
      ai_family    : C.int;
      ai_socktype  : C.int;
      ai_protocol  : C.int;
      ai_addrlen   : socklen_t;
      ai_addr      : System.Address;
      ai_canonname : C.Strings.chars_ptr;
      ai_next      : Addr_Info_Access;
   end record;
   pragma Convention (C, Addr_Info);

   --  Some routines

   function GetAddrInfo
     (node    : C.Strings.chars_ptr;
      service : C.Strings.chars_ptr;
      hints   : Addr_Info;
      res     : not null access Addr_Info_Access) return C.int;

   procedure FreeAddrInfo (res : Addr_Info_Access);

   function GAI_StrError (ecode : C.int) return C.Strings.chars_ptr;

   function Socket_StrError (ecode : Integer) return C.Strings.chars_ptr;

   function Set_Sock_Opt
     (S       : C.int;
      Level   : C.int;
      OptName : C.int;
      OptVal  : System.Address;
      OptLen  : C.int) return C.int;

   function C_Ioctl (S : C.int; Req : C.int; Arg : access C.int) return C.int;

   function C_Close (Fd : C.int) return C.int;
   procedure WSA_Startup (Version : C.int; Data : System.Address) is null;
   function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;

private

   pragma Import (C, GetAddrInfo, "getaddrinfo");
   pragma Import (C, FreeAddrInfo, "freeaddrinfo");
   pragma Import (C, Set_Sock_Opt, "setsockopt");
   pragma Import (C, GAI_StrError, "gai_strerror");
   pragma Import (C, Socket_StrError, "strerror");
   pragma Import (C, C_Ioctl, "ioctl");
   pragma Import (C, C_Close, "close");

end AWS.OS_Lib;
