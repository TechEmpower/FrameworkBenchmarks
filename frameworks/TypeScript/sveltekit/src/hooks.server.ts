import { bunHandler, nodeHandler } from '$lib/server/handlers';

const { RUNTIME } = process.env;

export const handle = RUNTIME === 'bun' ? bunHandler : nodeHandler;
