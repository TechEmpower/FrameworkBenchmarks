import { headerPlainTextInit } from '$lib/server/headers';
import { text, type RequestHandler } from '@sveltejs/kit';

export const GET: RequestHandler = () => {
	return text('Hello, world!', {
		headers: headerPlainTextInit
	});
};
