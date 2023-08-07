import type { Handle } from '@sveltejs/kit';

export const nodeHandle: Handle = (async ({ event, resolve }) => {
	const response = await resolve(event);

	response.headers.set('Server', 'SvelteKit');
	response.headers.set('Date', new Date().toUTCString());

	return response;
}) satisfies Handle;

// bun webserver adds 'Date' header by default
export const bunHandle: Handle = (async ({ event, resolve }) => {
	const response = await resolve(event);

	response.headers.set('Server', 'SvelteKit');

	return response;
}) satisfies Handle;
