import { bunHandle, nodeHandle } from '$lib/server/handles';

const { RUNTIME } = process.env;

export const handle = RUNTIME === 'bun' ? bunHandle : nodeHandle;
