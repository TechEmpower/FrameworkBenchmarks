// Copyright, https://github.com/QubitProducts/urlite

// deno-lint-ignore-file no-explicit-any
export interface Url {
  href?: string;
  path?: string;
  search?: string;
  protocol?: string;
  auth?: string;
  hostname?: string;
  port?: number;
  pathname?: string;
  hash?: string;

  query?: QsMap;
}

const fragments = ['protocol', 'auth', 'hostname', 'port', 'pathname', 'search', 'hash']
const pattern = /([^:/?#]+:)?(?:(?:\/\/)(?:([^/?#]*:[^@/]+)@)?([^/:?#]+)(?:(?::)(\d+))?)?(\/?[^?#]*)?(\?[^#]*)?(#[^\s]*)?/
//const pattern = /^([^:/?#]+\:?(?:(?:\/\/).*?[^/:?#]+(?:(?::)\d+)?))?(\/?[^?#]*)?(\?[^#]*)?.*?$/s;
const memory = new Map<string, Url>();

export function parse(url: string, qs = false): Url {
  if (memory.has(url)) return memory.get(url)!;
  const parts: any = {}
  parts.href = url
  const matches = url.match(pattern)
  let l = fragments.length
  while (l--) { parts[fragments[l]] = matches![l + 1] }
  parts.path = parts.search ? (parts.pathname ? parts.pathname + parts.search : parts.search) : parts.pathname

  if (qs && parts.search != null) {
    parts.query = Qs_parse(parts.search.slice(1));
  }

  memory.set(url, parts);

  return parts
}

export type QsMap = Record<string, string | string[] | boolean | number>

// const Qs_memory = new Map<string, QsMap>();

export function Qs_parse (qs: string): QsMap {
  //if (Qs_memory.has(qs)) return Qs_memory.get(qs)!;

  const obj: any = {}
  const params = decodeURI(qs || '').split(/&amp;|&/)
  const l = params.length
  for (let i = 0; i < l; i++) {
    if (params[i]) {
      let index = params[i].indexOf('=')
      if (index === -1) index = params[i].length
      const key = params[i].substring(0, index)
      const val = params[i].substring(index + 1)
      if (Object.hasOwn(obj, key)) {
        if (obj[key] instanceof Array) obj[key] = [obj[key]]
        obj[key].push(val)
      } else {
        obj[key] = val || true
      }
    }
  }
  //Qs_memory.set(qs, obj)
  return obj
}
