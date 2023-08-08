import type { Handle } from '@sveltejs/kit';

export const nodeHandler: Handle = (async ({ event, resolve }) => {
	const response = await resolve(event);

	response.headers.set('Server', 'SvelteKit');
	response.headers.set('Date', new Date().toUTCString());

	return response;
}) satisfies Handle;

// bun webserver adds 'Date' header by default
export const bunHandler: Handle = (async ({ event, resolve }) => {
	const response = await resolve(event);

	response.headers.set('Server', 'SvelteKit');

	return response;
}) satisfies Handle;
