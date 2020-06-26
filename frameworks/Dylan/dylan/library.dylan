Module: dylan-user

define library dylan-server
  use common-dylan;
  use io, import: { streams };
  use http-common;
  use http-server;
  use system, import: { date };
end library dylan-server;

define module dylan-server
  use common-dylan;
  use date, import: { as-rfc1123-string, current-date };
  use http-common;
  use http-server;
  use streams, import: { write };
end module dylan-server;
