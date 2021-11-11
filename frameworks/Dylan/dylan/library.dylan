Module: dylan-user

define library dylan-server
  use common-dylan;
  use collections;
  use http-common;
  use http-server;
  use io, import: { streams };
  use json;
  use system, import: { date };
end library dylan-server;

define module dylan-server
  use common-dylan;
  use date, import: { as-rfc1123-string, current-date };
  use http-common;
  use http-server;
  use json;
  use streams, import: { write };
  use table-extensions;
end module dylan-server;
